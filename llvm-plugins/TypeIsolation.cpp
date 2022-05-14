#include <llvm/ADT/SmallSet.h>
#include <llvm/Analysis/AssumptionCache.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/ValueTracking.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PatternMatch.h>
#include <llvm/Pass.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/CodeGen/TargetLowering.h>
#include <llvm/CodeGen/TargetSubtargetInfo.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Utils/Local.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

#include <string>
#include <list>
#include <queue>
#include <set>
#include <unordered_set>
#include <vector>

#include <Utils.h>
//#include <metadata.h>

#include "SizeofTypes.h"

#define DEBUG_TYPE "TypeIsolation"

// This is what I had enabled for most of my work (including early benchmarking builds), might be useful to turn on.
//#define HELPFUL_DEBUG

#if 0
#define DUMP_OPTIMISM // TODO check
#define SIMPLE_DEBUG
#define EXTRA_DEBUG
#define DISTANCE_DEBUG
#endif

using namespace llvm;
using namespace llvm::PatternMatch;

cl::opt<bool> CheckResult ("typeisolation-check", cl::desc("Check result and crash on faulty GEPs"), cl::init(false));
cl::opt<bool> DummyOperation ("typeisolation-dummy", cl::desc("Dummy operation"), cl::init(false));
cl::opt<bool> ProtectStores ("typeisolation-stores", cl::desc("Protect stores"), cl::init(false));
cl::opt<bool> DominatorOpts ("typeisolation-domopt", cl::desc("Use dominator optimization"), cl::init(true));
cl::opt<bool> debugPointerClasses ("typeisolation-debugptrclasses", cl::desc("Print each GEP classification"), cl::init(false));

#define ARENA_SIZE (1L << 32)

struct TypeIsolation : public FunctionPass {
	static char ID;

	std::set<Type *> blacklistedTypes;

	enum pointer_class {
		ptr_cls_unknown, /* pointer safety depends on classification of instructions yet unknown */
		ptr_cls_unsafe,  /* may be invalid outside red zone */
		ptr_cls_safe_base, /* safe if we mask the index */
		ptr_cls_safe,    /* either valid or within red zone */
		ptr_cls_valid,   /* valid pointer or NULL */
	};

	enum value_category {
		vc_unknown,
		vc_pointer, // pointer
		vc_pointerdelta, // pointer delta
		vc_negpointer, // negated pointer
		vc_nonpointer, // not a pointer
		vc_ambigious // ambigious merge :(
	};
	const char *nameForValueCat(value_category cat) {
		switch (cat) {
		case vc_unknown:
			return "unknown";
		case vc_ambigious:
			return "ambigious";
		case vc_pointer:
			return "pointer";
		case vc_pointerdelta:
			return "pointerdelta";
		case vc_negpointer:
			return "negpointer";
		case vc_nonpointer:
			return "nonpointer";
		}
		return "error";
	}

	ValueMap<Value *, pointer_class> pointerClasses;
	ValueToValueMapTy pointerBases;

	ValueMap<Value *, value_category> valueCategories;
	std::set<Instruction *> pointerArithInsts;

	/*
	 * Categorize one specific value.
	 * It should be a constant, because we categorized all instructions earlier.
	 */
	value_category categorizeOneValue(Value *V) {
		assert(!isa<Instruction>(V));
		assert(valueCategories[V] == vc_unknown);

		value_category newCat = vc_unknown;
		Type *Ty = V->getType();
		if (Ty->isPointerTy())
			newCat = vc_pointer;
		else
			newCat = vc_nonpointer;
		valueCategories[V] = newCat;
		return newCat;
	}

	// helper functions for value categories
	bool catIsPointer(Value *V) {
		value_category cat = valueCategories[V];
		if (cat == vc_unknown)
			cat = categorizeOneValue(V);
		return (cat == vc_pointer);
	}
	bool catIsNotPointer(Value *V) {
		value_category cat = valueCategories[V];
		if (cat == vc_unknown)
			cat = categorizeOneValue(V);
		return (cat == vc_nonpointer || cat == vc_pointerdelta);
	}

	bool isLoadProbablyPointer(const LoadInst *I) {
		Type *VTy = I->getType();
		if (VTy->isPointerTy())
			return true;

		AAMDNodes AAMD;
		I->getAAMetadata(AAMD);
		if (AAMD.TBAA) {
			MDNode *accessTypeNode = dyn_cast<MDNode>(AAMD.TBAA->getOperand(1));
			if (accessTypeNode) {
				MDString *typeStr = dyn_cast<MDString>(accessTypeNode->getOperand(0));
				if (typeStr && typeStr->getString() == "any pointer") {
					return true;
				}
			}
		}

		return false;
	}

	bool isStoredValueProbablyPointer(const StoreInst *I) {
		Type *VTy = I->getValueOperand()->getType();
		if (VTy->isPointerTy())
			return true;

		AAMDNodes AAMD;
		I->getAAMetadata(AAMD);
		if (AAMD.TBAA) {
			MDNode *accessTypeNode = dyn_cast<MDNode>(AAMD.TBAA->getOperand(1));
			if (accessTypeNode) {
				MDString *typeStr = dyn_cast<MDString>(accessTypeNode->getOperand(0));
				if (typeStr && typeStr->getString() == "any pointer") {
					return true;
				}
			}
		}

		return false;
	}

	void categorizePointerTypes(Function &F) {
		valueCategories.clear();
		pointerArithInsts.clear();

		const DataLayout &DL = F.getParent()->getDataLayout();

		// perlbench's S_method_common uses vars as integer and pointers
//#define CAT_DEBUG
#ifdef CAT_DEBUG
		bool debugThis = (F.getName() == "S_method_common");
#endif

		std::unordered_set<Value *> todo;

		std::function<void(Value *, Value *, value_category)> SetCategoryForUse;

		// set the category of a value
		std::function<void(Value *, value_category)> SetCategoryForValue = [&](Value *V, value_category cat) -> void {
			// Constants may be shared. We special-case them.
			// (e.g. i64 0 for obvious example, but also e.g. operands to and/or)
			//if (isa<ConstantInt>(V) || isa<ConstantAggregateZero>(V) || isa<ConstantPointerNull>(V) || isa<UndefValue>(V)) {
			if (isa<ConstantData>(V)) {
				return;
			}
			if (isa<Function>(V)) {
				// TODO: whaaaat
				return;
			}

			Type *VT = V->getType();
			if (isa<PointerType>(VT) && blacklistedTypes.count(cast<PointerType>(VT)->getElementType()))
				return;
			if (isa<GetElementPtrInst>(V)) {
				GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(V);
				VT = GEP->getPointerOperand()->getType();
				if (isa<PointerType>(VT) && blacklistedTypes.count(cast<PointerType>(VT)->getElementType()))
					return;
			}

#ifdef CAT_DEBUG
			if (debugThis)
				dbgs() << "[catdebug] SetCat " << *V << " to " << nameForValueCat(cat) << "\n";
#endif
			value_category oldCat = valueCategories[V];
			if (oldCat != vc_unknown) {
				if (oldCat == cat)
					return;
				// don't warn about delta->nonpointer, that's fine
				if (oldCat == vc_pointerdelta && cat == vc_nonpointer)
					return;
				// otherwise, warn about mismatches
				if (oldCat != cat) {
#ifdef HELPFUL_DEBUG
					dbgs() << "[cat] " << *V << " has ambigious categories! was " << nameForValueCat(oldCat) << ", now " << nameForValueCat(cat) << "\n";
#endif
				}
				// We allow 'upgrades' to pointer, but we complain about them.
				if (oldCat != cat && cat != vc_pointer)
					return;
			}

			if (V->getType()->isVoidTy()) {
				dbgs() << "[cat] trying to categorize void expr " << *V << "\n";
				return;
			}

			if (CastInst *castInst = dyn_cast<CastInst>(V)) {
				Type *srcType = castInst->getSrcTy();
				uint64_t srcBits = DL.getTypeSizeInBits(srcType);

				if (cat == vc_pointer && srcBits < 64) {
					// TODO: This is because we're too trusting of stored pointer types.
					dbgs() << "[cat] trying to categorize cast from non-pointer-sized " << *V << " as pointer\n";
					return;
				}
			}

			valueCategories[V] = cat;
			if (todo.find(V) != todo.end())
				todo.erase(V);

			// forward the category to uses
			for (auto &use : V->uses())
				SetCategoryForUse(V, use.getUser(), cat);

			// If we're being marked directly (e.g. due to our type) as a pointer,
			// we have to assume this is pointer arithmetic.
			if (cat == vc_pointer && (isa<GetElementPtrInst>(V) || isa<BinaryOperator>(V))) {
				pointerArithInsts.insert(cast<Instruction>(V));

				// Mark our pointer operand as a pointer if it's definitely a pointer.
				// Note about the TODOs: There are debug prints below which hopefully warn about any other cases.
				// TODO: We should do a pass later which fixes up other GEPs.
				GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(V);
				if (GEP) {
					Value *gepp = GEP->getPointerOperand();
					if (GEP->isInBounds() || GEP->hasAllConstantIndices()) {
						SetCategoryForValue(gepp, vc_pointer);
					}
				}

				// TODO: We should do a pass later which fixes up other BinaryOperators.
				BinaryOperator *bio = dyn_cast<BinaryOperator>(V);
				if (bio) {
					Value *op0 = bio->getOperand(0);
					Value *op1 = bio->getOperand(1);
					if (isa<ConstantInt>(op0)) {
						SetCategoryForValue(op1, vc_pointer);
					} else if (isa<ConstantInt>(op1)) {
						SetCategoryForValue(op0, vc_pointer);
					} else {
						// TODO?
					}
				}
			}

			// if relevant, also backwards to sources
			if (PHINode *phiNode = dyn_cast<PHINode>(V)) {
				for (auto &use : phiNode->incoming_values())
					SetCategoryForValue(use.get(), cat);
			}

			if (SelectInst *selectInst = dyn_cast<SelectInst>(V)) {
				SetCategoryForValue(selectInst->getFalseValue(), cat);
				SetCategoryForValue(selectInst->getTrueValue(), cat);
			}

			if (CastInst *castInst = dyn_cast<CastInst>(V)) {
				Type *srcType = castInst->getSrcTy();
				Type *destType = castInst->getDestTy();
				uint64_t srcBits = DL.getTypeSizeInBits(srcType);
				uint64_t destBits = DL.getTypeSizeInBits(destType);

				// due to %112 = ptrtoint <2 x i8*> %111 to <2 x i64>
				if ((!srcType->isIntOrIntVectorTy() && !srcType->isPtrOrPtrVectorTy()) || (!destType->isIntOrIntVectorTy() && !destType->isPtrOrPtrVectorTy())) {
					if (cat != vc_nonpointer) {
						dbgs() << "[cat] Warning: " << *V << " is non-int/ptr typed!\n";
						F.dump();
						return;
					} else {
						SetCategoryForValue(castInst->getOperand(0), cat);
					}
				}

				// check for pointer -> non-pointer transformations
				if (cat != vc_pointer) {
					if (srcBits >= 64 && destBits < 64) {
						// we might be truncating a pointer
						return;
					}
				}

				SetCategoryForValue(castInst->getOperand(0), cat);
			}
		};

		auto CalculateCategoryForMerge = [&](Instruction *I) -> value_category {
			SmallVector<Value *, 8> queue;
			SelectInst *seli = dyn_cast<SelectInst>(I);
			PHINode *phi = dyn_cast<PHINode>(I);
			assert(seli || phi);

			if (seli) {
				queue.push_back(seli->getFalseValue());
				queue.push_back(seli->getTrueValue());
			} else if (phi) {
				for (auto &use : phi->incoming_values()) {
					queue.push_back(use.get());
				}
			}

			value_category mergedCat = vc_unknown;

			for (auto ivi = queue.begin(); ivi != queue.end(); ++ivi) {
				Value *vi = *ivi;
				// Skip undefs.
				if (isa<UndefValue>(vi))
					continue;
				// Don't include the phinode itself.
				if (vi == I)
					continue;

				value_category otherCat = valueCategories[vi];
				if (isa<Constant>(vi)) {
					// zero/null can be anything, ignore them
					if (cast<Constant>(vi)->isZeroValue())
						continue;
					// otherwise trust the type?
					if (vi->getType()->isPtrOrPtrVectorTy())
						otherCat = vc_pointer;
					else
						otherCat = vc_nonpointer;
				}

				if (otherCat == vc_unknown)
					return vc_unknown;

				if (mergedCat == vc_negpointer || otherCat == vc_negpointer) {
					// first round?
					if (mergedCat == vc_unknown && otherCat == vc_negpointer)
						mergedCat = vc_negpointer;

					// otherwise: complain loudly if we're merging different types
					if (mergedCat != vc_negpointer || otherCat != vc_negpointer) {
						dbgs() << "[cat] Warning: merge of negpointer with other types: " << *I << "\n";
						F.dump();
						return vc_ambigious;
					}
					continue;
				}

				switch (mergedCat) {
				case vc_unknown:
					// first round
					mergedCat = otherCat;
					break;
				case vc_pointer:
					if (otherCat != vc_pointer)
						return vc_ambigious;
					break;
				case vc_nonpointer:
				case vc_pointerdelta:
					if (otherCat == vc_pointer || otherCat == vc_negpointer)
						return vc_ambigious;
					if (mergedCat == vc_pointerdelta && otherCat == vc_nonpointer)
						mergedCat = vc_nonpointer; // squash
					break;
				case vc_negpointer:
					// handled above
					break;
				case vc_ambigious: break; // impossible
				}
			}

			// If all inputs are zero/undef/ourselves, we can safely mark ourselves as nonpointer.
			if (mergedCat == vc_unknown)
				mergedCat = vc_nonpointer;
			return mergedCat;
		};

		// we just set a category for V, which is used by user: should we propagate?
		// TODO: why do we even pass cat, V is categorized now
		SetCategoryForUse = [&](Value *V, Value *user, value_category cat) -> void {
			if (user->getType()->isVoidTy())
				return;
			if (Instruction *I = dyn_cast<Instruction>(user))
				if (I->getParent()->getParent() != &F)
					return;
			// We want to propagate new categories upward
			// (e.g. pointers used in loads/stores are DEFINITELY pointers),
			// but here we propagate downward, so we don't override categories.
			// TODO: Do we need to for some cases?
			if (valueCategories[user] != vc_unknown)
				return;

#ifdef CAT_DEBUG
			if (debugThis)
				dbgs() << "[catdebug] SetCat " << *V << " of " << *user << " is now " << nameForValueCat(cat) << "\n";
#endif

			if (Instruction *I = dyn_cast<Instruction>(user)) {
				switch (I->getOpcode()) {
				case Instruction::ICmp:
				case Instruction::FCmp:
					/* We'll categorize these automatically later. */
					return;
				}

				SelectInst *seli = dyn_cast<SelectInst>(I);
				PHINode *phi = dyn_cast<PHINode>(I);
				if (seli || phi) {
					value_category newCat = CalculateCategoryForMerge(I);
					if (newCat != vc_unknown && newCat != vc_ambigious)
						SetCategoryForValue(I, newCat);
					return;
				}

				Value *otherVal = nullptr;
				value_category newCat = vc_unknown;
				value_category otherCat = vc_unknown;

				if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(I)) {
					Value *ptrOperand = GEP->getPointerOperand();
					value_category ptrCat = valueCategories[ptrOperand];

					if (ptrCat == vc_unknown) {
						// pending
						return;
					}

					// avoid more expensive check below for simple cases
					if (ptrCat == vc_pointer) {
						if (GEP->isInBounds() || GEP->hasAllConstantIndices()) {
							pointerArithInsts.insert(I);
							SetCategoryForValue(user, vc_pointer);
							return;
						}
					}

					// work out the result of all the indices
					newCat = ptrCat;
					for (unsigned n = 0; n < GEP->getNumIndices(); ++n) {
						Value *index = GEP->getOperand(n + 1);
						if (isa<ConstantInt>(index)) continue;
						value_category indexCat = valueCategories[index];
						switch (indexCat) {
						case vc_unknown: return; // pending
						case vc_negpointer:
							if (GEP->getNumIndices() != 1) {
								dbgs() << "[cat] Warning: non-trivial negpointer use by " << *GEP << "\n";
								F.dump();
							}
							if (newCat == vc_pointer)
								newCat = vc_pointerdelta;
							break;
						case vc_nonpointer:
						case vc_pointerdelta:
							// no change
							break;
						case vc_pointer:
							if (newCat == vc_pointer) {
								// probably something went wrong here
								dbgs() << "[cat] Warning: pointer+pointer by " << *GEP << "\n";
								F.dump();
							}
							if (newCat == vc_negpointer) {
								if (GEP->getNumIndices() != 1) {
									dbgs() << "[cat] Warning: non-trivial negpointer use by " << *GEP << "\n";
									F.dump();
								}
								newCat = vc_pointerdelta;
							}
							break;
						case vc_ambigious: break; // impossible
						}
						// First non-constant index is enough to determine whether or not this is a pointer.
						// Any remaining pointer/negpointer indexes will end up shifted beyond recognition.
						break;
					}

					if (newCat == vc_pointer)
						pointerArithInsts.insert(I);

					if (newCat != vc_unknown) {
						SetCategoryForValue(user, newCat);
						return;
					}
				}

				if (I->isBinaryOp()) {
					if (I->getOperand(0) == V)
						otherVal = I->getOperand(1);
					else
						otherVal = I->getOperand(0);

					// Constants are non-pointers in this context.
					if (isa<ConstantInt>(otherVal)) {
						otherCat = vc_nonpointer;
					}

					if (otherCat == vc_unknown)
						otherCat = valueCategories[otherVal];

					if (otherCat == vc_unknown) {
						// can't categorize yet
						return;
					}

					if (otherCat == vc_nonpointer && cat == vc_nonpointer) {
						newCat = vc_nonpointer;
					}
					if (newCat != vc_unknown) {
						SetCategoryForValue(user, newCat);
						return;
					}
				}

				// sub is special
				if (I->getOpcode() == Instruction::Sub) {
					if (otherCat == vc_pointer && cat == vc_pointer) {
						SetCategoryForValue(I, vc_pointerdelta);
						return;
					}

					if ((cat == vc_pointer && I->getOperand(1) == V)
						|| (otherCat == vc_pointer && I->getOperand(0) == V)) {
						// This happens with various non-pointers in the other operand.
						SetCategoryForValue(I, vc_negpointer);
						return;
					} else {
						// if first operand is pointer, then second is non-ptr --> pointer
						// if first operand is non-pointer then the case above should have dealt with pointer
						assert(cat != vc_pointer || otherCat != vc_pointer);
						SetCategoryForValue(I, cat);
						return;
					}
				}

				if (I->getOpcode() == Instruction::Add) {
					// handle negpointer propagation
					if (cat == vc_negpointer) {
						if (otherCat == vc_pointer)
							newCat = vc_pointerdelta;
						if (otherCat == vc_pointerdelta || otherCat == vc_nonpointer)
							newCat = vc_negpointer;
					} else if (otherCat == vc_negpointer) {
						if (cat == vc_pointer)
							newCat = vc_pointerdelta;
						if (cat == vc_pointerdelta || cat == vc_nonpointer)
							newCat = vc_negpointer;
					}

					if (newCat != vc_unknown) {
						SetCategoryForValue(user, newCat);
						return;
					}
				}

				if (I->getOpcode() == Instruction::Shl ||
					I->getOpcode() == Instruction::AShr ||
					I->getOpcode() == Instruction::LShr ||
					I->getOpcode() == Instruction::SDiv ||
					I->getOpcode() == Instruction::SRem ||
					I->getOpcode() == Instruction::UDiv ||
					I->getOpcode() == Instruction::URem ||
					I->getOpcode() == Instruction::Mul) {
					// These are always non-pointers/deltas (or else the code is too weird to analyze correctly).
					// TODO: Maybe assert the shift is non-zero.
					SetCategoryForValue(user, vc_nonpointer);
					return;
				}

				// AND with non-pointer values?
				if (I->getOpcode() == Instruction::And) {
					ConstantInt *Y;
					if (match(user, m_c_And(m_Specific(V), m_ConstantInt(Y)))) {
						uint64_t andv = Y->getZExtValue();
						if (andv <= 0xffffffff) {
							SetCategoryForValue(user, vc_nonpointer);
							return;
						}
					}
				}

				// XOR pointer with -1 is a negpointer
				// (see e.g. perlbench)
				if (I->getOpcode() == Instruction::Xor && cat == vc_pointer) {
					ConstantInt *Y;
					if (match(user, m_c_Xor(m_Specific(V), m_ConstantInt(Y)))) {
						if (Y->isMinusOne()) {
							SetCategoryForValue(user, vc_negpointer);
							return;
						}
					}
				}


				if (I->getOpcode() == Instruction::Add ||
					I->getOpcode() == Instruction::Or ||
					I->getOpcode() == Instruction::Xor ||
					I->getOpcode() == Instruction::And) {
					if (otherCat == vc_pointer && cat != vc_pointer) {
						newCat = vc_pointer;
					} else if (otherCat != vc_pointer && cat == vc_pointer) {
						newCat = vc_pointer;
					} else if (otherCat == vc_pointer && cat == vc_pointer) {
						// WTF
						dbgs() << "[cat] Warning: pointers combined: " << *user << "\n";
						F.dump();
						newCat = vc_pointer;
					} else {
						// both non-pointers, we stop tracking deltas
						newCat = vc_nonpointer;
					}
					if (newCat == vc_pointer)
						pointerArithInsts.insert(I);

					if (newCat != vc_unknown) {
						SetCategoryForValue(user, newCat);
						return;
					}
				}
			}

			if (CastInst *castInst = dyn_cast<CastInst>(user)) {
				Type *srcType = castInst->getSrcTy();
				Type *destType = castInst->getDestTy();
				uint64_t srcBits = DL.getTypeSizeInBits(srcType);
				uint64_t destBits = DL.getTypeSizeInBits(destType);

				// early reject things which can't be pointers
				// due to %112 = ptrtoint <2 x i8*> %111 to <2 x i64>
				if ((!srcType->isIntOrIntVectorTy() && !srcType->isPtrOrPtrVectorTy()) || (!destType->isIntOrIntVectorTy() && !destType->isPtrOrPtrVectorTy())) {
					if (cat != vc_nonpointer) {
						dbgs() << "[cat] Warning: " << *user << " is non-int/ptr typed!\n";
						F.dump();
					} else {
						SetCategoryForValue(user, cat);
					}
					return;
				}

				// warning
				// Note: cpu2006's xalancbmk uses an i128 load and truncates it to get a pointer, so we don't check srcBits != 64 here.
				if (cat == vc_pointer && (srcBits < 64)) {
					dbgs() << "[cat] Warning: " << *user << " is ptr of wrong size!\n";
					F.dump();
				}

				if (cat == vc_pointer && destBits < 64) {
					// if we're casting a pointer to <64 bits, it's a narrow -> non-pointer
					SetCategoryForValue(user, vc_nonpointer);
				} else {
					// otherwise, propagate the type
					SetCategoryForValue(user, cat);
				}
				return;
			}

			if (user->getType()->isVoidTy())
				return;

#ifdef CAT_DEBUG
			if (debugThis)
				dbgs() << "[catdebug] can't categorize user " << *user << " of " << *V << "\n";
#endif
		};

		for (auto &BB : F) {
			for (auto &I : BB) {
				Instruction *inst = &I;
				Type *instType = inst->getType();

				if (isa<DbgInfoIntrinsic>(inst))
					continue;

				// already categorized
				if (valueCategories[inst] != vc_unknown)
					continue;

				// Pre-categorize some constant expressions.
				// WARNING: Constants are shared and may not be consistently categorizable!
				for (unsigned n = 0; n < inst->getNumOperands(); ++n) {
					Value *operand = inst->getOperand(n);
					Constant *constOperand = dyn_cast<Constant>(operand);
					if (!constOperand)
						continue;
					// stripPointerCasts doesn't look through ptrtoint
					if (Operator::getOpcode(constOperand) == Instruction::PtrToInt)
						constOperand = cast<Constant>(cast<Operator>(constOperand)->getOperand(0));
					constOperand = constOperand->stripPointerCasts();
					if (constOperand->getType()->isPointerTy())
						SetCategoryForValue(operand, vc_pointer);
				}

				value_category cat;

				LoadInst *LI = dyn_cast<LoadInst>(inst);
				StoreInst *SI = dyn_cast<StoreInst>(inst);
				CallBase *CB = dyn_cast<CallBase>(inst);

				switch (inst->getOpcode()) {
				case Instruction::Alloca:
					SetCategoryForValue(inst, vc_pointer);
					break;
				case Instruction::Call:
				case Instruction::Invoke:
					if (instType->isVoidTy())
						break;
					// We trust types specified outside our function.
					// Therefore, trust return type:
					if (instType->isPtrOrPtrVectorTy())
						cat = vc_pointer;
					else
						cat = vc_nonpointer;
					SetCategoryForValue(inst, cat);
					// And argument types:
					for (auto &arg : CB->args()) {
						Value *argV = arg.get();
						if (argV->getType()->isPtrOrPtrVectorTy()) {
							cat = vc_pointer;
						} else {
							cat = vc_nonpointer;
						}
						SetCategoryForValue(argV, cat);
					}
					break;
				case Instruction::ICmp:
				case Instruction::FCmp:
					SetCategoryForValue(inst, vc_nonpointer);
					break;
				case Instruction::Load:
					SetCategoryForValue(LI->getPointerOperand(), vc_pointer);
					if (isLoadProbablyPointer(LI))
						SetCategoryForValue(LI, vc_pointer);
					else
						SetCategoryForValue(LI, vc_nonpointer);
					break;
				case Instruction::Store:
					SetCategoryForValue(SI->getPointerOperand(), vc_pointer);
					if (isStoredValueProbablyPointer(SI))
						SetCategoryForValue(SI->getValueOperand(), vc_pointer);
					else
						SetCategoryForValue(SI->getValueOperand(), vc_nonpointer);
					break;
				case Instruction::Ret:
					if (Value *retValue = dyn_cast<ReturnInst>(inst)->getReturnValue()) {
						SetCategoryForValue(retValue, retValue->getType()->isPtrOrPtrVectorTy() ? vc_pointer : vc_nonpointer);
					}
					break;
				default:
					if (instType->isVoidTy())
						break;
					if (instType->isFloatingPointTy()) {
						SetCategoryForValue(inst, vc_nonpointer);
						continue;
					}

					// Instructions can be categorized when we categorize their operands;
					// only insert into the todo list if that didn't happen.
					if (valueCategories[inst] == vc_unknown)
						todo.insert(inst);
					break;
				}
			}
		}

		//dbgs() << "*** arguments\n";

		// We trust pointer-typed arguments to be pointers.
		for (auto ai = F.arg_begin(); ai != F.arg_end(); ++ai) {
			value_category cat = vc_nonpointer;
			Argument *arg = &*ai;
			if (arg->getType()->isPtrOrPtrVectorTy())
				cat = vc_pointer;
			SetCategoryForValue(arg, cat);
		}

#if 0
		for (auto II = todo.begin(); II != todo.end(); ++II) {
			dbgs() << "remains uncategorized before finishing: " << *(*II) << "\n";
		}
#endif

		//dbgs() << "*** type info\n";

		// Use the type information to fill out the remainder.
		std::vector<Value *> todo2(todo.begin(), todo.end());
		for (auto II = todo2.begin(); II != todo2.end(); ++II) {
			if (todo.find(*II) == todo.end())
				continue;
			assert(valueCategories[*II] == vc_unknown);
			Instruction *I = dyn_cast<Instruction>(*II);

			// typed as a pointer? at this point, mark it as a pointer
			// TODO: do we need this?
			if (I->getType()->isPtrOrPtrVectorTy()) {
#ifdef HELPFUL_DEBUG
				dbgs() << "DEBUG set " << *I << " as pointer\n";
#endif
				SetCategoryForValue(I, vc_pointer);
			}
		}

		// We don't want to miss pointers, so we mark non-pointers later.
		todo2 = std::vector<Value *>(todo.begin(), todo.end());
		for (auto II = todo2.begin(); II != todo2.end(); ++II) {
			if (todo.find(*II) == todo.end())
				continue;
			assert(valueCategories[*II] == vc_unknown);
			Instruction *I = dyn_cast<Instruction>(*II);
			// not a 64-bit integer/pointer? then mark it as not a pointer
			if (!I->getType()->isIntOrPtrTy() || I->getType()->getPrimitiveSizeInBits() != 64) {
				SetCategoryForValue(I, vc_nonpointer);
			}
		}

		//dbgs() << "*** finish phi/selects\n";

		// If we didn't resolve phi/selects as pointers yet, they're not pointers.
		// (We refuse to resolve loops earlier if they have any non-pointer incoming values.)
		// TODO: Check this.
		todo2 = std::vector<Value *>(todo.begin(), todo.end());
		for (auto II = todo2.begin(); II != todo2.end(); ++II) {
			if (todo.find(*II) == todo.end())
				continue;
			assert(valueCategories[*II] == vc_unknown);
			if (Instruction *I = dyn_cast<Instruction>(*II)) {
				SelectInst *seli = dyn_cast<SelectInst>(I);
				PHINode *phi = dyn_cast<PHINode>(I);
				if (seli || phi) {
					value_category newCat = CalculateCategoryForMerge(I);
					if (newCat == vc_ambigious) {
						// TODO: Make sure this is a union?
#ifdef HELPFUL_DEBUG
						dbgs() << "[cat] ambigious merge: " << *I << "\n";
#endif
						SetCategoryForValue(I, vc_pointer);
					} else if (newCat != vc_unknown)
						SetCategoryForValue(I, newCat);
					else
						SetCategoryForValue(I, vc_nonpointer);
				}
			}
		}

#ifdef CAT_DEBUG_ALL
		for (auto II = valueCategories.begin(); II != valueCategories.end(); ++II) {
			dbgs() << nameForValueCat(II->second) << ": " << *II->first << "\n";
		}
#endif

		// FIXME: Do a final pass and make sure that pointer arithmetic is marked as such?

#ifdef HELPFUL_DEBUG
		for (auto II = todo.begin(); II != todo.end(); ++II) {
			dbgs() << "[cat] remains uncategorized: " << *(*II) << "\n";
		}
#endif
#if 0
		for (auto II = pointerArithInsts.begin(); II != pointerArithInsts.end(); ++II) {
			dbgs() << "[cat] ptr arith: " << *(*II) << "\n";
		}
#endif

		// Debug prints to show GEPs/BinaryOperators where we didn't propagate upward.
		for (auto II = pointerArithInsts.begin(); II != pointerArithInsts.end(); ++II) {
			// These are very uncommon, so we leave them in to make sure we don't miss something.
			// These are probably NOT safe if you see them in your output for GEPs, so maybe TODO error out in that case.
			// (Binary operators can sometimes have both sides being non-pointers, we will die later if we try masking those.)
			GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(*II);
			if (GEP) {
				Value *gepp = GEP->getPointerOperand();
				if (isa<ConstantPointerNull>(gepp))
					continue;
				if (valueCategories[gepp] != vc_pointer) {
#ifdef HELPFUL_DEBUG
					dbgs() << "[cat] Warning: " << *GEP << " has ptr operand " << *gepp << " but it has category " << nameForValueCat(valueCategories[gepp]) << "\n";
#endif
				}
			}
			BinaryOperator *BO = dyn_cast<BinaryOperator>(*II);
			if (BO) {
				Value *op0 = BO->getOperand(0);
				Value *op1 = BO->getOperand(1);
				value_category cat0 = valueCategories[op0];
				value_category cat1 = valueCategories[op1];
				if (cat0 != vc_pointer && cat1 != vc_pointer) {
#ifdef HELPFUL_DEBUG
					dbgs() << "[cat] Warning: " << *BO << " has operands " << *op0 << ", " << *op1 << ", but they have categories " << nameForValueCat(cat0) << ", " << nameForValueCat(cat1) << "\n";
#endif
				}
			}
		}
	}

	// alyssa needs globals
	LoopInfo *GLI;
	DominatorTree *GDT;
	//std::set<GetElementPtrInst *> gepsToMaskIndex;

	bool m_changed;

	void blacklistType(Type *T) {
		std::set<Type *> seen;
		std::queue<Type *> todo;

		todo.push(T);
		while (!todo.empty()) {
			Type *T = todo.front();
			todo.pop();
			if (T->isStructTy()) {
				dbgs() << "TypeIsolation is blacklisting type " << T->getStructName() << "\n";
				blacklistedTypes.insert(T);
			}

			if (seen.count(T)) continue;
			seen.insert(T);

			if (T->isArrayTy()) {
				Type *AT = T->getArrayElementType();
				todo.push(AT);
			}

			if (T->isStructTy()) {
				for (unsigned n = 0; n < T->getNumContainedTypes(); ++n) {
					Type *CT = T->getContainedType(n);
					todo.push(CT);
				}
			}

			// TODO: sub struct types pls
		}
	}

	bool doInitialization(Module &M) override {
		// deepsjeng allocates a huge TTable
		if (GlobalValue *V = M.getNamedValue("TTable")) {
			Type *T = V->getValueType();
			if (T->isPointerTy())
				blacklistType(T->getPointerElementType());
		}

		return false;
	}

	TypeIsolation() : FunctionPass(ID) {
	}

	void getAnalysisUsage(AnalysisUsage &AU) const override {
		AU.addPreserved<SizeofTypes>();
	}

	bool isMallocCall(const CallInst *ci) {
		Function *callee = ci->getCalledFunction();

		/* we cannot handle indirect calls */
		if (!callee) return false;

		/* we cannot handle locally implemented functions without
		 * interprocedural analysis
		 */
		if (!callee->isDeclaration()) return false;

		/* look for known malloc-like functions */
		std::string name = callee->getName();
		size_t argcount = callee->arg_size();
		if ((name == "_Znwj" && argcount == 1) ||
			(name == "_Znwm" && argcount == 1) ||
			(name == "_Znaj" && argcount == 1) ||
			(name == "_Znam" && argcount == 1) ||
			(name == "malloc" && argcount == 1) ||
			(name == "realloc" && argcount == 2) ||
			(name == "calloc" && argcount == 2) ||
			/* typed allocs have an extra param, the type */
			(name == "tc_typed_malloc" && argcount == 2) ||
			(name == "tc_typed_calloc" && argcount == 3) ||
			(name == "tc_typed_realloc" && argcount == 3) ||
			(name == "tc_typed_new" && argcount == 2)
			) {
			return true;
		}
		
		// TODO detect wrappers? interprocedural analysis? inlining?

		return false;
	}

	void accumulateGepStats(GetElementPtrInst *gep) {
		Value *ptr = gep->getPointerOperand();
		int ptrType;
		if (dyn_cast<GlobalValue>(ptr)) {
			ptrType = 0;
		} else if (dyn_cast<PHINode>(ptr)) {
			ptrType = 1;
		} else if (dyn_cast<GetElementPtrInst>(ptr)) {
			ptrType = 2;
		} else if (dyn_cast<LoadInst>(ptr)) {
			ptrType = 3;
		} else if (dyn_cast<CastInst>(ptr)) {
			ptrType = 4;
		} else if (dyn_cast<Constant>(ptr)) {
			ptrType = 5;
		} else if (dyn_cast<Instruction>(ptr)) {
			ptrType = 6;
		} else {
			ptrType = 7;
		}

		dbgs() << "TypeIsolation DEBUG pointer"
			" ptrType=" << ptrType <<
			"\n";

		int hasInt32 = 0;
		int hasInt64 = 0;
		int hasCast = 0;
		int hasConst = 0;
		int hasLoad = 0;
		int hasPHI = 0;
		int hasOther = 0;
		for (auto &index : gep->indices()) {
			Value *indexValue = index.get();
			Type *indexType = indexValue->getType();
			
			if (indexType->isIntegerTy()) {
				if (indexType->getIntegerBitWidth() > 32) {
					if (dyn_cast<CastInst>(indexValue)) {
						hasCast = 1;
					} else if (dyn_cast<Constant>(indexValue)) {
						hasConst = 1;
					} else if (dyn_cast<LoadInst>(indexValue)) {
						hasLoad = 1;
					} else if (dyn_cast<PHINode>(indexValue)) {
						hasPHI = 1;
					} else {
						hasInt64 = 1;
					}
				} else {
					hasInt32 = 1;
				}
			} else {
				hasOther = 1;
			}
		}

		dbgs() << "TypeIsolation DEBUG index"
			" int32=" << hasInt32 <<
			" int64=" << hasInt64 <<
			" cast=" << hasCast <<
			" const=" << hasConst <<
			" load=" << hasLoad <<
			" PHI=" << hasPHI <<
			" other=" << hasOther <<
			"\n";
		
		int useCast = 0;
		int useGep = 0;
		int useLoad = 0;
		int useStorePtr = 0;
		int useStoreValue = 0;
		int useOther = 0;
		for (auto &use : gep->uses()) {
			Value *user = use.getUser();
			if (dyn_cast<LoadInst>(user)) {
				useLoad = 1;
			} else if (dyn_cast<StoreInst>(user)) {
				if (dyn_cast<StoreInst>(user)->getPointerOperand()) useStorePtr = 1;
				if (dyn_cast<StoreInst>(user)->getValueOperand()) useStoreValue = 1;
			} else if (dyn_cast<GetElementPtrInst>(user)) {
				useGep = 1;
			} else if (dyn_cast<CastInst>(user)) {
				useCast = 1;
			} else {
				useOther = 1;
			}
		}
		
		dbgs() << "TypeIsolation DEBUG use"
			" cast=" << useCast <<
			" gep=" << useGep <<
			" load=" << useLoad <<
			" storeptr=" << useStorePtr <<
			" storeval=" << useStoreValue <<
			" other=" << useOther <<
			"\n";
	}
	
	const char *valueTypeToStr(const Value *ptr) {
		if (dyn_cast<AllocaInst>(ptr)) return "AllocaInst";
		if (dyn_cast<CastInst>(ptr)) return "CastInst";
		if (dyn_cast<LoadInst>(ptr)) return "LoadInst";
		if (dyn_cast<StoreInst>(ptr)) return "StoreInst";

		if (dyn_cast<GlobalValue>(ptr)) return "GlobalValue";
		if (dyn_cast<BinaryOperator>(ptr)) return "BinaryOperator";
		if (dyn_cast<CallInst>(ptr)) return "CallInst";
		if (dyn_cast<InvokeInst>(ptr)) return "InvokeInst";
		if (dyn_cast<ExtractElementInst>(ptr)) return "ExtractElementInst";
		if (dyn_cast<ExtractValueInst>(ptr)) return "ExtractValueInst";
		if (dyn_cast<GetElementPtrInst>(ptr)) return "GetElementPtrInst";
		if (dyn_cast<PHINode>(ptr)) return "PHINode";
		if (dyn_cast<SelectInst>(ptr)) return "SelectInst";
		if (dyn_cast<VAArgInst>(ptr)) return "VAArgInst";
		if (dyn_cast<UnaryInstruction>(ptr)) return "UnaryInstruction";

		if (dyn_cast<UndefValue>(ptr)) return "UndefValue";
		if (dyn_cast<ConstantInt>(ptr)) return "ConstantInt";
		if (dyn_cast<ConstantData>(ptr)) return "ConstantData";
		if (dyn_cast<ConstantExpr>(ptr)) return "ConstantExpr";
		if (dyn_cast<ConstantAggregate>(ptr)) return "ConstantAggregate";
		if (dyn_cast<Constant>(ptr)) return "Constant";
		if (dyn_cast<Instruction>(ptr)) return "Instruction";

		if (dyn_cast<Argument>(ptr)) return "Argument";
		if (dyn_cast<User>(ptr)) return "User";
		return "(other)";
	}

// this might be buried a couple of levels deep
// TODO: this is ok for current benchmarks but we should fix for general case
#define ARBITRARY_SIZE 32768

	// used by lowDistanceToPointer, below
	// naive check if a load/store of the gep ALWAYS dominates the domInst given
	// (domInst is at the backedge of the loop; we want to know if there is always a load/store in the loop)
	// note: use == load/store!!!
	// TODO: this should prbly use phi nodes like the code in hasDominatingPointerAccess
	bool alwaysHasUseDominating(
		GetElementPtrInst *gep,
		Instruction *domInst) {
		std::queue<std::pair<const Value *, const Value *>> todo;
		std::set<std::pair<const Value *, const Value *>> seen;

		for (auto &use : gep->uses()) {
			todo.push(std::pair<const Value *, const Value *>(gep, use.getUser()));
		}

		while (!todo.empty()) {
			std::pair<const Value *, const Value *> pair = todo.front();
			const Value *pointer = pair.first;
			const Value *instruction = pair.second;
			todo.pop();

			if (seen.count(pair)) continue;
			seen.insert(pair);

			if (dyn_cast<CastInst>(instruction)) {
				for (auto &use : instruction->uses()) {
					todo.push(std::pair<const Value *, const Value *>(instruction, use.getUser()));
				}
				continue;
			}

			const StoreInst *storeInst = dyn_cast<StoreInst>(instruction);
			if (storeInst) {
				// note: we don't gate this by ProtectStores
				// we don't want it to be the value :)
				if (pointer != storeInst->getValueOperand()) {
					if (GDT->dominates(storeInst, domInst))
						return true;
				}
			}
			const LoadInst *loadInst = dyn_cast<LoadInst>(instruction);
			if (loadInst) {
				if (GDT->dominates(loadInst, domInst))
					return true;
			}
		}

		return false;
	}

	/*
	 * checks whether a GEP is using a loop induction variable which we know is valid (or can make valid)
	 *
	 * (also some SCEV-based check which is ignored by the pointer, only valid is relevant right now)
	 */
	enum pointer_class lowDistanceToPointer(
		ScalarEvolution &SE,
		Value *ptr,
		GetElementPtrInst *gep) {

		if (!DominatorOpts)
			return ptr_cls_unsafe;

		const DataLayout &DL = gep->getModule()->getDataLayout();

		const Type *ptrtype = ptr->getType();
		if (!ptrtype->isPtrOrPtrVectorTy()) {
			dbgs() << *ptrtype << " is not pointer!\n";
			return ptr_cls_unsafe;
		}

		int64_t PtrOffset = 0;
		Value *PtrBase = GetPointerBaseWithConstantOffset(ptr, PtrOffset, DL);
		if (!PtrBase)
			return ptr_cls_unsafe;

		/* is the distance low? */
		if (PtrBase == ptr) {
			bool resolved_all = true;
			bool saw_phi = false;

#ifdef DISTANCE_DEBUG
			dbgs() << "distance between base of '" << *gep << "' and " << *PtrBase << " is " << PtrOffset << "\n";
#endif
			/* let's look for an index induction variable.. */
			Loop *loop = GLI->getLoopFor(gep->getParent());
			if (loop && PtrOffset < ARBITRARY_SIZE && PtrOffset > -ARBITRARY_SIZE) {

				BasicBlock *Incoming = nullptr, *Backedge = nullptr;
				if (!loop->getIncomingAndBackEdge(Incoming, Backedge))
					Incoming = nullptr;
#ifdef DISTANCE_DEBUG
				else
					dbgs() << "got edges\n";
#endif

				for (auto &index : gep->indices()) {
					if (!resolved_all)
						break;
					Value *gepidx = index.get();
#ifdef DISTANCE_DEBUG
					dbgs() << "idx: " << *gepidx << "\n";
#endif
					ConstantInt *CIG = dyn_cast<ConstantInt>(gepidx);
					PHINode *phi = dyn_cast<PHINode>(gepidx);

					if (!phi) {
						/* cope with situations like hmmer where we add -1 to the induction var for some GEPs */
						Value *maybePhi;
						ConstantInt *addedCI;
						// match doesn't let us match phi nodes? :o
						if (match(gepidx, m_c_Add(m_Value(maybePhi), m_ConstantInt(addedCI)))) {
							if (PHINode *ooh = dyn_cast<PHINode>(maybePhi)) {
								if (addedCI->isOne() || addedCI->isMinusOne()) {
									phi = ooh;
								}
							}
						}
					}

					// we need PtrBase to come from *outside* the loop, otherwise it might change in the loop
					if (phi && Incoming && dyn_cast<Instruction>(PtrBase) && !GDT->dominates(dyn_cast<Instruction>(PtrBase), Incoming->getTerminator())) {
#ifdef DISTANCE_DEBUG_BASIC
						dbgs() << *phi << " can't be checked because " << *PtrBase << " doesn't dominate the incoming edge\n";
#endif
						Incoming = nullptr;
					}

					if (phi && Incoming && Backedge && (phi->getBasicBlockIndex(Incoming) == -1 || phi->getBasicBlockIndex(Backedge) == -1)) {
						// FIXME: maybe we should grab the loop from the phi node.....
						dbgs() << "phi here is not in the loop as we expected\n";
						// we will fail below
					}
					if (CIG) {
						// TODO: for now we just assume constant increments are constant
					} else
					if (phi && Incoming && Backedge && phi->getBasicBlockIndex(Incoming) != -1 && phi->getBasicBlockIndex(Backedge) != -1) {
						// this is like getCanonicalInductionVariable, but not so strict on the requirements
						ConstantInt *CI = dyn_cast<ConstantInt>(phi->getIncomingValueForBlock(Incoming));
						Instruction *Inc = dyn_cast<Instruction>(phi->getIncomingValueForBlock(Backedge));

						// we only cope with GEPs which are run EVERY time around the loop
						// (because we need a load or a store which also dominates the loop)
						// FIXME: hmmer would gain massively if we also check nearby ptr..
						bool dominates = GDT->dominates(gep, Backedge->getTerminator());

						if (dominates && CI && Inc) {
#ifdef DISTANCE_DEBUG
							dbgs() << "in: " << *CI << "\n";
							dbgs() << "back: " << *Inc << "\n";
#endif
							// TODO: is the CI->isOne() safe? (should be, because it's a constant, see also above)
							if ((CI->isZero() || CI->isOne()) && Inc->getOpcode() == Instruction::Add && Inc->getOperand(0) == phi) {
								if (ConstantInt *CI2 = dyn_cast<ConstantInt>(Inc->getOperand(1))) {
									APInt val = CI2->getValue();
#ifdef DISTANCE_DEBUG
									dbgs() << "stride is " << val << "\n";
#endif
									if (val.isNegative())
										val = -val;
									if (val.ule(ARBITRARY_SIZE)) {
#ifdef DISTANCE_DEBUG
										dbgs() << "yay!\n";
#endif

										// do we have a load/store which ALWAYS runs in the loop?
										// FIXME: hmmer would gain massively if we also check nearby ptr
										if (alwaysHasUseDominating(gep, Backedge->getTerminator())) {
#ifdef DISTANCE_DEBUG
											dbgs() << "double yay!\n";
#endif
											// is the origin pointer valid?
											// FIXME I forget why the last param is like this
											enum pointer_class basecls = classifyPointerOperand(PtrBase, !gep->hasAllZeroIndices());
											if (basecls == ptr_cls_unknown) {
												// this is pretty rare, interestingly
#ifdef DISTANCE_DEBUG
												dbgs() << "waiting for this to be valid: " << *PtrBase << ", via phi " << *phi << "\n";
#endif
												return ptr_cls_unknown;
											}
											else if (basecls == ptr_cls_safe) {
												// FIXME for debugging only
												// (should never happen in current code)
												dbgs() << "wanted this to be valid: " << *PtrBase << ", via phi " << *phi << "\n";
												resolved_all = false;
											}
											else if (basecls != ptr_cls_valid)
												resolved_all = false;
											else {
												saw_phi = true;
#ifdef DISTANCE_DEBUG
												dbgs() << "TRIPLE YAY!\n";
#endif
											}

										} else resolved_all = false;
									} else {
#ifdef DISTANCE_DEBUG
										dbgs() << "nope, not bounded\n";
#endif
										resolved_all = false;
									}
								}
							}
						}
					} else resolved_all = false;
					// TODO: do something with other indices
#if 0
					const SCEV *SCEV_gepidx = SE.getSCEV(gepidx);
					dbgs() << "idx: " << *gepidx << ", scev is " << *SCEV_gepidx << "\n";

					std::pair<const SCEV *, const SCEV *> init_post = SE.SplitIntoInitAndPostInc(loop, SCEV_gepidx);
					if (init_post.first != SE.getCouldNotCompute()) {
						// TODO
						dbgs() << "split idx: " << *init_post.first << ", " << *init_post.second << "\n";
					}
#endif
				}
			}

			if (resolved_all && saw_phi) {
				dbgs() << "resolved all indices, including a phi node!\n";
				if (!gep->hasAllZeroIndices())
					return ptr_cls_valid; // this is ok ONLY because a caller at a higher level will patch up
				else
					dbgs() << "GEP was zero :o\n"; // TODO think about this if it happens
			}
		}

		// FIXME: alyssa disabled this for now because it may be unsound when the PtrBase is a phi (or via a phi)
		// the problem is: we don't trigger classifyPointerOperand, so the 'real' base GEP may not get masked :(
		return ptr_cls_unsafe;

		// just in case (shouldn't trigger in this situation..)
		if (!SE.isSCEVable(ptr->getType()) || !SE.isSCEVable(PtrBase->getType()))
			return ptr_cls_unsafe;

#if 0
		const SCEV *SCEVOrig;
		const SCEV *SCEVNew;

		const Loop *loop = 0;
		if ((dyn_cast<const Instruction>(inst)->getParent() == baseBB) && (loop = GLI->getLoopFor(baseBB))) {
#ifdef EXTRA_DEBUG
			dbgs() << "same BB, got a loop\n";
#endif
		      		SCEVOrig = SE.getSCEVAtScope(const_cast<Value *>(OrigBase), loop);
		      		SCEVNew = SE.getSCEVAtScope(const_cast<Value *>(NewBase), loop);

				std::pair<const SCEV *, const SCEV *> init_post = SE.SplitIntoInitAndPostInc(loop, SCEVNew);
				if (init_post.first != SE.getCouldNotCompute()) {
					// if the second is not too distant, could be useful
					const SCEV *distance = SE.getMinusSCEV(SCEVOrig, init_post.second);
					dbgs() << *SCEVOrig << " " << *SCEVNew << " split into " << *init_post.first << " and " << *init_post.second << ": computed distance " << *distance << "\n";
					if (
			 		  (SE.isKnownPredicate(ICmpInst::ICMP_SLT, distance, SE.getConstant(APInt(64, (int64_t)32768)))) && 
			  		  SE.isKnownPredicate(ICmpInst::ICMP_SLT, SE.getNegativeSCEV(distance), SE.getConstant(APInt(64, (int64_t)32768)))
			  		 ) {
#ifdef EXTRA_DEBUG
						dbgs() << "hmmm\n";
#endif
					}
				}
			} else {
		       		SCEVOrig = SE.getSCEV(const_cast<Value *>(OrigBase));
		       		SCEVNew = SE.getSCEV(const_cast<Value *>(NewBase));
			}
#endif
			// problems in hmmer??
			if (!SE.isSCEVable(gep->getType()))
				return ptr_cls_unsafe;

			const SCEV *SCEV_gep = SE.getSCEV(gep);
			const SCEV *SCEV_ptr = SE.getSCEV(ptr);
			if (SE.getEffectiveSCEVType(SCEV_gep->getType()) != SE.getEffectiveSCEVType(SCEV_ptr->getType()))
				return ptr_cls_unsafe;
			const SCEV *distance = SE.getMinusSCEV(SCEV_gep, SCEV_ptr);
#ifdef EXTRA_DEBUG
			dbgs() << "distance is " << *distance << "\n";
#endif
			if (
			   (SE.isKnownPredicate(ICmpInst::ICMP_SLT, distance, SE.getConstant(APInt(64, (int64_t)ARBITRARY_SIZE)))) && 
			    SE.isKnownPredicate(ICmpInst::ICMP_SLT, SE.getNegativeSCEV(distance), SE.getConstant(APInt(64, (int64_t)ARBITRARY_SIZE)))
			   ) {
#ifdef EXTRA_DEBUG
				dbgs() << "distance between '" << *gep << "' and " << *PtrBase << " is bounded by SCEV :)\n";
#endif
				return ptr_cls_safe; /* this is OK *only* if the base pointer is valid */
			}

		return ptr_cls_unsafe; /* distance can't be proven, must be masked */
	}

	enum pointer_class classifyPointerOperand(Value *ptr,
		bool rewriteIfNeeded) {
		CallBase *callInst;
		CastInst *castInst;
		ConstantInt *constantInt;
		Value *current;
		PHINode *phiNode;
		enum pointer_class pointerClass = ptr_cls_valid;
		std::queue<Value *> queue;
		std::set<Value *> seen;
		SelectInst *selectInst;

		int debugSeen = 0, debugAlloca = 0, debugArgument = 0,
			debugGlobal = 0, debugLoad = 0, debugNull = 0,
			debugCast = 0, debugSelect = 0, debugPHI = 0,
			debugMalloc = 0, debugGepValid = 0, debugGepSafe = 0,
			debugGepUnsafe = 0, debugOther = 0, debugCall = 0;

		std::queue<Instruction *> toDelete;

		queue.push(ptr);
		while (!queue.empty()) {
			current = queue.front();
			queue.pop();

			/* don't handle the same input value twice */
			if (seen.count(current)) { debugSeen = 1; continue; }
			seen.insert(current);

			/* known valid pointers */
			if (dyn_cast<AllocaInst>(current)) { debugAlloca = 1; continue; }
			if (dyn_cast<Argument>(current)) { debugArgument = 1; continue; }
			if (dyn_cast<GlobalValue>(current)) { debugGlobal = 1; continue; }
			if (dyn_cast<LoadInst>(current)) { debugLoad = 1; continue; }
			if (dyn_cast<ConstantPointerNull>(current)) { debugNull = 1; continue; }

			// FIXME: is this ok? (we hit this >400 times in cpu2017's perlbench)
			if (dyn_cast<UndefValue>(current)) { continue; }

			/* NULL pointers from casts */
			constantInt = dyn_cast<ConstantInt>(current);
			if (constantInt && constantInt->isZero()) { debugNull = 1; continue; }

			ConstantExpr *constantExpr = dyn_cast<ConstantExpr>(current);
			if (constantExpr) {
				// TODO: this is NOT ok, we should prove it valid or not immediately
				// otherwise we end up with getelementptr instructions which are marked invalid despite being likely-valid
				Instruction *fakeInst = constantExpr->getAsInstruction();
				queue.push(fakeInst);
				toDelete.push(fakeInst);
				continue;
			}

			/* values based on other values */
			castInst = dyn_cast<CastInst>(current);
			if (castInst) {
				for (auto &use : castInst->operands()) queue.push(use.get());
				debugCast = 1;
				continue;
			}

			phiNode = dyn_cast<PHINode>(current);
			if (phiNode) {
				for (auto &use : phiNode->incoming_values()) queue.push(use.get());
				debugPHI = 1;
				continue;
			}

			selectInst = dyn_cast<SelectInst>(current);
			if (selectInst) {
				queue.push(selectInst->getFalseValue());
				queue.push(selectInst->getTrueValue());
				debugSelect = 1;
				continue;
			}

			// vector insts: push all the sources
			// [note: the inputs should be OK so the outputs should also be OK]
			InsertElementInst *iei = dyn_cast<InsertElementInst>(current);
			if (iei) {
				queue.push(iei->getOperand(0));
				queue.push(iei->getOperand(1));
				continue;
			}
			ExtractElementInst *eei = dyn_cast<ExtractElementInst>(current);
			if (eei) {
				queue.push(eei->getOperand(0));
				continue;
			}
			ShuffleVectorInst *svi = dyn_cast<ShuffleVectorInst>(current);
			if (svi) {
				queue.push(svi->getOperand(0));
				queue.push(svi->getOperand(1));
				continue;
			}

			/* extractvalue is valid because we treat insertvalue as escapes, and you can't bitcast aggregates */
			ExtractValueInst *evi = dyn_cast<ExtractValueInst>(current);
			if (evi) {
				continue;
			}

#if 1
			/* function return values are always valid */
			callInst = dyn_cast<CallBase>(current);
			if (callInst) { debugCall = 1; continue; }
#else
			/* malloc and new result is valid */
			callInst = dyn_cast<CallInst>(current);
			if (callInst && isMallocCall(callInst)) { debugMalloc = 1; continue; }
#endif

			/* Pointer arithmetic based on other pointer arithmetic */
			Instruction *currinst = dyn_cast<Instruction>(current);
			if (currinst && pointerArithInsts.count(currinst)) {
				/* unsafe ptr arith will be checked, and therefore
				 * guaranteed to result in a valid pointer
				 */
				pointer_class otherGEPClass = pointerClasses[currinst];
				if (otherGEPClass == ptr_cls_safe || otherGEPClass == ptr_cls_safe_base) {
					debugGepSafe = 1;
					if (rewriteIfNeeded) {
						pointerClasses[currinst] = ptr_cls_unsafe;
					} else {
						if (pointerClass > ptr_cls_safe) pointerClass = ptr_cls_safe;
					}
				} else if (otherGEPClass == ptr_cls_valid) {
					debugGepValid = 1;
				} else if (otherGEPClass == ptr_cls_unsafe) {
					debugGepUnsafe = 1;
					/* pointer will become valid */
				} else {
					if (pointerClass > ptr_cls_unknown) pointerClass = ptr_cls_unknown;
				}
				continue;
			}

#if 0
			// check for alignment ANDs
			Value *ourRoot;
			ConstantInt *andVal;
			if (match(current, m_c_And(m_Value(ourRoot), m_ConstantInt(andVal)))) {
				int64_t andv = andVal->getSExtValue();
				if (andv == -16) {
					// also handle one particular case: (ptr + 15) & -16
					Value *newRoot;
					if (match(ourRoot, m_c_Add(m_Value(newRoot), m_ConstantInt(andVal)))) {
						andv = andVal->getSExtValue();
						if (andv == 15) {
							ourRoot = newRoot;
						}
					}

					// TODO: think carefully about the safety of this
					// our allocations are aligned
					queue.push(ourRoot);
					continue;
				}
			}
#endif

			/* other origins considered unsafe */
			/* TODO: we need to special-case this to handle all real-world code. for now, we just print them, to make sure we don't have any. */
			debugOther = 1;
#ifdef HELPFUL_DEBUG
			dbgs() << "GEP unsafe pointerclass " << valueTypeToStr(current) << " " << *current << "\n";
#endif
#if 0
			if (callInst) {
				dbgs() << "GEP call for unsafe pointerclass " << (callInst->getCalledFunction() ? callInst->getCalledFunction()->getName() : "(indirect)") << "\n";
			}
#endif
			if (pointerClass > ptr_cls_unsafe) pointerClass = ptr_cls_unsafe;
		}

		while (!toDelete.empty()) {
			toDelete.front()->deleteValue();
			toDelete.pop();
		}

		if (pointerClass != ptr_cls_unknown && debugPointerClasses) {
				dbgs() << "GEP pointerclass " <<
					((pointerClass == ptr_cls_valid) ? " valid" : "") <<
					((pointerClass == ptr_cls_safe) ? " safe" : "") <<
					((pointerClass == ptr_cls_unsafe) ? " unsafe" : "") <<
					(debugSeen ? " seen" : "") <<
					(debugAlloca ? " alloca" : "") <<
					(debugArgument ? " argument" : "") <<
					(debugGlobal ? " global" : "") <<
					(debugLoad ? " load" : "") <<
					(debugNull ? " null" : "") <<
					(debugCast ? " cast" : "") <<
					(debugSelect ? " select" : "") <<
					(debugPHI ? " phi" : "") <<
					(debugMalloc ? " malloc" : "") <<
					(debugCall ? " call" : "") <<
					(debugGepValid ? " gep-valid" : "") <<
					(debugGepSafe ? " gep-safe" : "") <<
					(debugGepUnsafe ? " gep-unsafe" : "") <<
					(debugOther ? " other" : "") <<
					"\n";
		}
		return pointerClass;
	}

	// we start bound at 0, which means 'no bound' (returns false later)
	// and then we narrow it when we can
	// return true = bound is the maximum
	bool maxBoundForValue(const Value *value, uint64_t &bound, std::set<const Value *> &seen) {
		// TODO: the phi node loop is not so great for obvious cases like constants....
		if (seen.count(value)) return false; /* phi node loop (or diamond?), give up */
		seen.insert(value);

		for (;;) {
			// TODO: we could check constants
			const Type *type = value->getType();
			if (type->isIntegerTy()) {
 				uint64_t bw = type->getIntegerBitWidth();
				if (bw <= 32) {
					uint64_t w = (((uint64_t)1) << bw) - 1;
					if (!bound || w < bound)
						bound = w;
				}
			}

			Value *X;
			ConstantInt *Y;
			if (match(value, m_c_And(m_Value(X), m_ConstantInt(Y)))) {
				uint64_t v = Y->getZExtValue();
				if (v == 0) {
					bound = 0;
					return true;
				}
				if (!bound || v < bound)
					bound = v;
				value = X;
				continue;
			}

			if (auto ConstVal = dyn_cast<ConstantInt>(value)) {
				uint64_t v = ConstVal->getZExtValue();
				// TODO: negative bounds? (otherwise we get huge value, that is ok)
				if (ConstVal->isMinusOne())
					v = 1;
				// narrow the bound
				if (!bound || v < bound) {
					bound = v;
#if 0
					dbgs() << "found a const bound for " << *value << ", it was " << bound << "\n";
#endif
				}
				return true;
			}

			// handle select/phi
			std::queue<const Value *> queue;
			const SelectInst *seli = dyn_cast<const SelectInst>(value);
			const PHINode *phi = dyn_cast<const PHINode>(value);
			if (seli) {
				queue.push(seli->getFalseValue());
				queue.push(seli->getTrueValue());
			} else if (phi) {
				seen.insert(phi);
				for (auto &use : phi->incoming_values()) {
					queue.push(use.get());
				}
			}
			if (seli || phi) {
				uint64_t allPhiBound = 0;

				// get the maximum bound from all incoming phi/select values
				while (!queue.empty()) {
					const Value *current = queue.front();
					queue.pop();

					uint64_t phiBound = 0;
					bool x = maxBoundForValue(current, phiBound, seen);
					if (!x)
						return (bound != 0);
					// recursion returned *true*, so bound is valid even if it's zero!
					// but remember: we want the *max* bound (worst-case) from this phi
					if (phiBound > allPhiBound)
						allPhiBound = phiBound;
				}
				// then narrow the bound
				if (!bound || allPhiBound < bound) {
					bound = allPhiBound;
#if 0
					dbgs() << "found a phi bound for " << *value << ", it was " << bound << "\n";
#endif
				}
				return true;
			}

			const CastInst *castInst = dyn_cast<CastInst>(value);
			if (castInst) {
				value = *castInst->op_begin();
				continue;
			}

			break;
		}

		if (bound == 0)
			return false;
		return true;
	}

	bool valueBoundedTo32Bit(const Value *value) {
		const CastInst *castInst;
		const Constant *constant;
		const Type *type;

		// TODO we could reason across PHInodes to look for more casts,
		// though these cases seem very uncommon based on stats gathered
		// on SPEC CPU2006

		for (;;) {
			type = value->getType();
			if (type->isIntegerTy() && type->getIntegerBitWidth() <= 32) return true;

			castInst = dyn_cast<CastInst>(value);
			if (castInst) {
				value = *castInst->op_begin();
				continue;
			}

			// TODO we assume the constant fits without checking its
			// value (in practice we will never encounter constant
			// offsets that large though)
			constant = dyn_cast<Constant>(value);
			if (constant) return true;

			return false;
		}
	}
	
	bool gepIndexBoundedByGuardZone(
			ScalarEvolution &SE,
			const GetElementPtrInst *gep
			) {
		/* we assume constants are ok */
		if (gep->hasAllConstantIndices())
			return true;

		/* heavily modified GEPOperator::accumulateConstantOffset */

		const DataLayout &DL = gep->getModule()->getDataLayout();
		APInt Offset(128, 0); // 128 because we don't want overflow

		auto AccumulateOffset = [&](APInt Index, uint64_t Size) -> bool {
			Index = Index.sextOrTrunc(Offset.getBitWidth());
			APInt IndexedSize = APInt(Offset.getBitWidth(), Size);
			Offset += Index * IndexedSize;
			return true;
		};

		const GEPOperator *gepo = cast<GEPOperator>(gep);
		for (gep_type_iterator GTI = gep_type_begin(gepo), GTE = gep_type_end(gepo); GTI != GTE; ++GTI) {
			Value *V = GTI.getOperand();
			StructType *STy = GTI.getStructTypeOrNull();
			if (auto ConstOffset = dyn_cast<ConstantInt>(V)) {
				if (ConstOffset->isZero())
					continue;
				if (STy) {
					unsigned ElementIdx = ConstOffset->getZExtValue();
					const StructLayout *SL = DL.getStructLayout(STy);
					if (!AccumulateOffset(APInt(Offset.getBitWidth(), SL->getElementOffset(ElementIdx)), 1))
						return false;
					continue;
				}

				if (!AccumulateOffset(ConstOffset->getValue(), DL.getTypeAllocSize(GTI.getIndexedType())))
					return false;
				continue;
			}

#ifdef FORGET_SPECTRE
			// **** FIXME ***
			// SCEV is not Spectre-safe
			// **** FIXME ***
			if (!SE.isSCEVable(V->getType()))
				return false;
			// TODO: we should use getSCEVAtScope
			// FIXME: think about signed
			const SCEV *SCEV_V = SE.getSCEV(V);
			APInt range = SE.getUnsignedRangeMax(SCEV_V);
			if (!SE.isKnownPositive(SCEV_V)) {
				// FIXME: think
				if (!SE.isKnownNegative(SCEV_V)) {
					if (valueBoundedTo32Bit(V))
						range = APInt(64, 4*1024*1024*1024ull) - 1;
					else
						return false;
				} else
					range = SE.getSignedRangeMin(SCEV_V);
			}
			//dbgs() << " SCEV thinks " << *V << " has range " << range << "\n";
			if (!AccumulateOffset(range, DL.getTypeAllocSize(GTI.getIndexedType())))
				return false;
#else
			uint64_t maxBound = 0;
			std::set<const Value *> seen;
			if (maxBoundForValue(V, maxBound, seen)) {
				if (!AccumulateOffset(APInt(64, maxBound), DL.getTypeAllocSize(GTI.getIndexedType())))
					return false;
			} else {
				//dbgs() << *gep << " has non-bounded index, got as far as " << Offset << "\n";
				return false;
			}
#endif
		}

		//dbgs() << *gep << " has max offset " << Offset << "\n";
		if (Offset.isNegative())
			Offset.negate();
		if (Offset.ult(8*4*1024*1024*1024ull)) {
			//dbgs() << "that is bounded!\n";
			return true;
		}

		return false;

#if 0
		uint64_t ts = baseSizeOfGEP(gep);
		if (ts > 8)
			return false;

		uint64_t multiplier = 1;
		for (auto &index : gep->indices()) {
			multiplier = multiplier * ts;
			const Value *gepidx = index.get();
			if (const Constant *C = dyn_cast<const Constant>(gepidx))
				if (C->isZeroValue())
					continue;
			if (multiplier > ts) return false;
			// TODO: copy SCEV code in here
			if (!valueBoundedTo32Bit(gepidx)) return false;
		}
		return true;
#endif
	}

	bool tryToMaskGEPIndex(GetElementPtrInst *gep) {
		const DataLayout &DL = gep->getModule()->getDataLayout();

		const GEPOperator *gepo = cast<GEPOperator>(gep);
		uint32_t numRealIndices = 0;
		for (gep_type_iterator GTI = gep_type_begin(gepo), GTE = gep_type_end(gepo); GTI != GTE; ++GTI) {
			Value *V = GTI.getOperand();
			if (auto ConstOffset = dyn_cast<ConstantInt>(V)) {
				if (numRealIndices == 0 && ConstOffset->isZero())
					continue;
			}

			uint64_t ts = DL.getTypeAllocSize(GTI.getIndexedType());
			if (ts > 8)
				return false;

			numRealIndices++;
			if (numRealIndices > 1)
				return false;
		}	

		return true;
	}

	bool isTriviallyValid(const Value *current) {
		/* known valid pointers */
		if (dyn_cast<AllocaInst>(current) ||
		    dyn_cast<Argument>(current) ||
		    dyn_cast<GlobalValue>(current) ||
		    dyn_cast<LoadInst>(current) ||
		    dyn_cast<ConstantPointerNull>(current))
			return true;

		/* null ptr */
		auto constantInt = dyn_cast<ConstantInt>(current);
		if (constantInt && constantInt->isZero()) return true;

		/* malloc and new result is valid */
		auto callInst = dyn_cast<CallInst>(current);
		if (callInst && isMallocCall(callInst)) return true;

		return false;
	}

	uint64_t baseSizeOfGEP(const GetElementPtrInst *gep) {
		const DataLayout &DL = gep->getModule()->getDataLayout();

		Type *gept = gep->getType();
		//dbgs() << *gept << "\n";
		uint64_t ts;
		if (gept->isPointerTy())
			ts = DL.getTypeStoreSize(gept->getPointerElementType());
		else if (gept->isStructTy())
			ts = DL.getTypeStoreSize(gept); // TODO: ok?
		else if (gept->isVectorTy()) { // TODO: we ignore scalable
			VectorType *vt = cast<VectorType>(gept);
			uint64_t ne = vt->getNumElements();
			Type *et = vt->getElementType();
			assert(et->isPointerTy());
			ts = 1;
			for (int j = 0; j < ne; ++j)
				ts = ts * DL.getTypeStoreSize(et->getPointerElementType());
		} else
			assert(false);
#if 0
		if (ts > 8)
			dbgs() << "size " << ts << " for non-constant gep " << *gep << "\n";
#endif
		return ts;
	}

	/*
	 * Check whether there is a nearby dominating load of this GEP.
	 */
	bool simpleGEPCheck(const GetElementPtrInst *gep, bool *usesAreZeroOffset) {
		/*if (gep->getParent()->getParent()->getName() != "_ZN11xercesc_2_510ValueStore8containsEPKNS_13FieldValueMapE")
			return false;*/

		if (!DominatorOpts)
			return false;

#ifdef SIMPLE_DEBUG
		dbgs() << "simple gep check: " << *gep << "\n";
#endif

		const DataLayout &DL = gep->getModule()->getDataLayout();

		int64_t OrigOffset = 0;
		const Value *OrigBase = GetPointerBaseWithConstantOffset(gep, OrigOffset, DL);
		if (!OrigBase)
			return false;

		bool offsetIsZero = (OrigOffset == 0);

#ifdef SIMPLE_DEBUG
		dbgs() << "base: " << OrigOffset << " from " << *OrigBase << "\n";
#endif

		/*
		  %149 = load i16*, i16** %148, align 8 // dominating load
		  [...]
                 150:
                  %151 = phi i16* [ %149, %145 ], [ null, %127 ] // dominatng load
		  [...]
		 203:
		  %204 = phi i16* [ %205, %203 ], [ %151, %200 ] // phi
		  %205 = getelementptr inbounds i16, i16* %204, i64 1 // GEP close to phi
		  %206 = load i16, i16* %205, align 2 // dominating load
		  %207 = icmp eq i16 %206, 0
		  br i1 %207, label %208, label %203
		 */

		/*
		 * newer example [from gobmk]:
		 * %15 = load %struct.change_stack_entry*, %struct.change_stack_entry** @change_stack_pointer, align 8
		 * loop:
		 *   %17 = phi %struct.change_stack_entry* [ %15, %2 ], [ %23, %16 ]
		 *   %21 = getelementptr inbounds %struct.change_stack_entry, %struct.change_stack_entry* %17, i64 0, i32 0
		 *   store i32 [something], i32** %21, align 8
		 *   %23 = getelementptr inbounds %struct.change_stack_entry, %struct.change_stack_entry* %17, i64 1
		 *   %24 = getelementptr inbounds %struct.change_stack_entry, %struct.change_stack_entry* %17, i64 0, i32 1
		 *   store i32 [something], i32* %24, align 8
		 *   if (...) jmp loop
		 */

		// check for trivial cases, @_ZN11xercesc_2_510ValueStore8containsEPKNS_13FieldValueMapE had one
		// idea: if ALL of the sources are trivially valid, except the GEP itself (via a phi node)
		//   then we can check for loads/stores of the GEP which also happen in the BB
		//   if we find one, then we know that the GEP itself is also valid :)
		SmallVector<const Value *, 32> queue;
		SmallPtrSet<const Value *, 32> Seen;
		queue.push_back(OrigBase);

		while (!queue.empty()) {
			const Value *current = queue.back();
			queue.pop_back();
		
			if (!Seen.insert(current).second)
				continue;

			if (isTriviallyValid(current))
				continue;

			/* values based on other values */
			auto castInst = dyn_cast<CastInst>(current);
			if (castInst) {
				for (auto &use : castInst->operands()) queue.push_back(use.get());
				continue;
			}

			auto phiNode = dyn_cast<PHINode>(current);
			if (phiNode) {
				for (auto &use : phiNode->incoming_values()) {
					Value *inval = use.get();
					// this is the GEP we're checking! is there a trivial load/store which always happens in our BB?
					// actually: *any* GEP in this BB might be OK, as long as the base is shared..
					// (since we're searching backwards, we know that we have the loop style we're looking for..)
					const GetElementPtrInst *incoming_gep = dyn_cast<GetElementPtrInst>(inval);
					if (!incoming_gep || (incoming_gep->getParent() != gep->getParent())) {
						queue.push_back(inval);
						continue;
					}
					int64_t IncomingGEPOffset = 0;
					const Value *IncomingGEPBase = GetPointerBaseWithConstantOffset(incoming_gep, IncomingGEPOffset, DL);
					if (IncomingGEPBase != OrigBase) {
						queue.push_back(inval);
						continue;
					}
					bool haveDomLoad = false;
#if 1
					// we check all instructions:
					//   - there might be a nearby load from the same base, gobmk has some examples
					//   - any store/load in our BB will happen before our result hits the phi node, so don't need dominance
					for (auto &i : *gep->getParent()) {
						const Instruction *LSI = &i;
						const Value *ptrSrc = nullptr;
						if (const LoadInst *LI = dyn_cast<const LoadInst>(LSI)) {
							ptrSrc = LI->getPointerOperand();
						} else if (const StoreInst *SI = dyn_cast<const StoreInst>(LSI)) {
							ptrSrc = SI->getPointerOperand();
						} else
							continue;
						int64_t NewOffset = 0;
						const Value *NewBase = GetPointerBaseWithConstantOffset(ptrSrc, NewOffset, DL);
						// must be the same source
						if (NewBase != OrigBase)
							continue;
						NewOffset += IncomingGEPOffset; // FIXME: think about this, are we adding it to the right one (is adding even right?)
						// must be nearby
						if ((OrigOffset + NewOffset) > ARBITRARY_SIZE || (OrigOffset + NewOffset) < -ARBITRARY_SIZE)
							continue;
						haveDomLoad = true;
						// offsetIsZero means BOTH are at zero
						// TODO: Think
						if (!offsetIsZero)
							offsetIsZero = (OrigOffset == 0) && (NewOffset == 0);
						break;
					}
#else
					// FIXME alyssa is tired, check
					// FIXME: this is broken since we might be STORING this value
					// TODO: this would be better if we also check for nearby GEPs, gobmk has some nice examples
					for (auto &use : gep->uses()) {
						const Value *U = use.getUser();
						const Instruction *LSI = dyn_cast<const Instruction>(U);
						// TODO: really silly to use DT here when we could just do a walk
						if (LSI && (isa<const LoadInst>(LSI) || isa<const StoreInst>(LSI)) && (LSI->getParent() == gep->getParent())
							&& GDT->dominates(gep, LSI)) {
							// yay :)
							haveDomLoad = true;
						}
					}
#endif
					if (!haveDomLoad) {
#ifdef SIMPLE_DEBUG
						dbgs() << "fail due to uses check\n";
#endif
						return false;
					}
				}
				continue;
			}

			auto selectInst = dyn_cast<SelectInst>(current);
			if (selectInst) {
				queue.push_back(selectInst->getFalseValue());
				queue.push_back(selectInst->getTrueValue());
				continue;
			}

#ifdef SIMPLE_DEBUG
			dbgs() << "fail due to " << *current << "\n";
#endif
			return false;
		}

#ifdef SIMPLE_DEBUG
		dbgs() << "simple all accesses ok\n";
#endif
		*usesAreZeroOffset = offsetIsZero;
		return true;
	}

	/*
	 * Is there a load/store which is 'close enough' to our GEP and dominates userInst?
	 * If so, our GEP is safe in the context of userInst.
	 * That's because such a load/store is equivalent to having a 'valid' base pointer there!
	 * (Our GEP is even valid in the context of userInst if the offset from the load/store to our GEP is zero.)
	 *
	 * Note that this is only useful if *all* users of a GEP pass this check.
	 */
	bool hasDominatingPointerAccess(
		DominatorTree &dt,
		ScalarEvolution &SE,
		const Instruction *userInst,
		//const Value *pointer,
		const Instruction *baseInst,
		bool *usesAreZeroOffset) {

		if (!DominatorOpts)
			return false;

		// TODO: why not pointer->getType()
		const Type *ptrtype = baseInst->getType();
		if (!ptrtype->isPtrOrPtrVectorTy())
			return false;

#ifdef EXTRA_DEBUG
		dbgs() << "hasDominatingPointerAccess for " << *baseInst << " in context of " << *userInst << "\n";
#endif

		const DataLayout &DL = userInst->getModule()->getDataLayout();

		/* check for dominating accesses in this block and previous blocks */
		SmallVector<const BasicBlock *, 32> BlocksToCheck;
		const BasicBlock *baseBB = dyn_cast<Instruction>(userInst)->getParent();
		for (auto it = pred_begin(baseBB), et = pred_end(baseBB); it != et; ++it) {
			BlocksToCheck.push_back(*it);
		}
		BlocksToCheck.push_back(baseBB);

		SmallPtrSet<const BasicBlock *, 32> Visited;

		int64_t OrigOffset = 0;
		const Value *OrigBase = GetPointerBaseWithConstantOffset(baseInst, OrigOffset, DL);
		if (!OrigBase)
			return false;

		std::queue<const Instruction*> candidates;

		while (!BlocksToCheck.empty()) {
			const BasicBlock *BB = BlocksToCheck.back();
			BlocksToCheck.pop_back();

			if (!Visited.insert(BB).second)
				continue;

			BasicBlock::const_iterator ScanPos = BB->end();
			while (ScanPos != BB->begin()) {
				const Instruction *inst = &*--ScanPos;
				if (!dt.dominates(inst, userInst))
					continue;

				if (dyn_cast<const LoadInst>(inst) || dyn_cast<const StoreInst>(inst))
					candidates.push(inst);
			}
		}

		for (auto &use : OrigBase->uses()) {
			const Value *V = use.getUser();
			if (const Instruction *inst = dyn_cast<const Instruction>(V)) {
				if (!dt.dominates(inst, userInst))
					continue;
				if (dyn_cast<const LoadInst>(inst) || dyn_cast<const StoreInst>(inst))
					candidates.push(inst);
			}
		}

		bool foundCloseEnough = false;
		while (!candidates.empty()) {
			const Instruction *inst = candidates.front();
			candidates.pop();

			const LoadInst *loadInst = dyn_cast<const LoadInst>(inst);
			const StoreInst *storeInst = dyn_cast<const StoreInst>(inst);
			int64_t NewOffset = 0;
			const Value *NewBase = NULL;
			if (loadInst) {
				NewBase = GetPointerBaseWithConstantOffset(loadInst->getPointerOperand(), NewOffset, DL);
			} else if (storeInst) {
				NewBase = GetPointerBaseWithConstantOffset(storeInst->getPointerOperand(), NewOffset, DL);
			}
			if (!NewBase)
				continue;
			if (NewBase == OrigBase) {
#ifdef EXTRA_DEBUG
				dbgs() << *userInst << " dominated by " << *inst << "\n";
				dbgs() << "shared pointer base '" << *OrigBase << "', distance " << OrigOffset << " vs " << NewOffset << "\n";
#endif

				// TODO arbitrary size
				if (OrigOffset > ARBITRARY_SIZE || NewOffset > ARBITRARY_SIZE)
				       continue;
				if (OrigOffset < -ARBITRARY_SIZE || NewOffset < -ARBITRARY_SIZE)
				       continue;
				if (OrigOffset == NewOffset)
					return true;
				else
					foundCloseEnough = true;

			}

			// FIXME: this may not be provably Spectre-safe because SCEV can in theory be too trusting
			// (this does not seem to be a problem with the cases we manually checked, but still)
#if 0		
#ifdef EXTRA_EXTRA_DEBUG
			dbgs() << OrigOffset << "," << NewOffset << "," << *NewBase << "," << *OrigBase << "\n";
#endif
			// not sure what's going on here, I think GEPs with vectors end badly?
			if (!SE.isSCEVable(OrigBase->getType()) || !SE.isSCEVable(NewBase->getType()))
				continue;

			const SCEV *SCEVOrig = SE.getSCEV(const_cast<Value *>(OrigBase));
			const SCEV *SCEVNew = SE.getSCEV(const_cast<Value *>(NewBase));

			// llvm issue workaround
			// using https://reviews.llvm.org/D75628 which is upstreamed
			SmallVector<const SCEV *, 8> BoundExprs;
			BoundExprs.push_back(SCEVOrig);
			BoundExprs.push_back(SCEVNew);
			if (!SE.satisfiesTotalOrder(BoundExprs))
				continue;

			const SCEV *distance = SE.getMinusSCEV(SCEVOrig, SCEVNew);
			if (const SCEVConstant *distconst = dyn_cast<const SCEVConstant>(distance)) {
				APInt distval = distconst->getAPInt();
				if (distval.isNegative())
					distval.negate();
				// *********** FIXME ****************
				// FIXME: fold OrigOffset/NewOffset into below
				// *********** FIXME ****************
#ifdef EXTRA_DEBUG
				dbgs() << *userInst << " dominated by " << *inst << "\n";
				dbgs() << "distance from '" << *OrigBase << "' to '" << *NewBase << "' is " << *distance << "\n";
#endif
				if (distval.ule(ARBITRARY_SIZE)) {
					if (distval.isNullValue())
						return true;
					foundCloseEnough = true;
				} else {
					dbgs() << "too far :o\n";
				}
			}
#endif
		}

		if (foundCloseEnough) {
			*usesAreZeroOffset = false;
			return true;
		}

		return false;
	}

	/*
         * is ptr still usable as a pointer after inst? cases:
         *  - truncation to 32 bits
         *  - left/right shifts
         *  - binary operators which are only used by one of the above or:
         *    - a compare operation
         */
	bool transformedBeyondUse(const Value *inst, const Value *ptr) {
		if (isa<LoadInst>(inst) || isa<StoreInst>(inst) || isa<CallBase>(inst) || isa<AtomicCmpXchgInst>(inst) || isa<AtomicRMWInst>(inst) || isa<IndirectBrInst>(inst))
			return false;

		const Type *instType = inst->getType();
		// is the size of the result <= 32 bits?
		if (instType->getScalarSizeInBits() && instType->getScalarSizeInBits() <= 32)
			return true;

		if (dyn_cast<BinaryOperator>(inst)) {
			// we assume that shifts and divides/multiplies destroy pointers
			if (match(inst, m_LShr(m_Specific(ptr), m_Value())))
				return true;
			if (match(inst, m_AShr(m_Specific(ptr), m_Value())))
				return true;
			if (match(inst, m_Shl(m_Specific(ptr), m_Value())))
				return true;
			if (match(inst, m_UDiv(m_Specific(ptr), m_Value())))
				return true;
			if (match(inst, m_SDiv(m_Specific(ptr), m_Value())))
				return true;
			if (match(inst, m_Mul(m_Specific(ptr), m_Value())))
				return true;
			ConstantInt *someCI;
			if (match(inst, m_c_And(m_Specific(ptr), m_ConstantInt(someCI)))) {
				uint64_t andv = someCI->getZExtValue();
				if (andv <= 0xffffffff)
					return true;
			}

			// check the uses: they may all still be fine
			for (auto &use : inst->uses()) {
				ICmpInst::Predicate Pred;
				if (match(inst, m_Cmp(Pred, m_Value(), m_Value())))
					continue;
				if (!transformedBeyondUse(use.getUser(), inst))
					return false;
			}
			return true;
		}

		return false;
	}

	void instTrackUses(
		DominatorTree &dt,
		const Instruction *baseInst,
		bool *onlyStores,
		bool *allUsesVerified,
		bool *usesAreZeroOffset,
		bool *escapes,
		ScalarEvolution &SE) {
		// <pointer, instruction using the pointer, is this the first ptr arith?>
		// We use the last to work out whether to mark pointers as escaped,
		// because we only need to mask the pointer arithmetic closest to the escape.
		std::queue<std::tuple<const Value *, const Value *, bool>> todo;
		std::set<std::tuple<const Value *, const Value *, bool>> seen;

		*onlyStores = true;
		*allUsesVerified = true;
		*escapes = false;
		*usesAreZeroOffset = true;

		for (auto &use : baseInst->uses()) {
			Value *user = use.getUser();
			bool isArith = isa<Instruction>(user) && pointerArithInsts.count(cast<Instruction>(user));
			todo.push(std::tuple<const Value *, const Value *, bool>(baseInst, user, isArith));
		}

		while (!todo.empty()) {
			std::tuple<const Value *, const Value *, bool> tuple = todo.front();
			const Value *pointer = std::get<0>(tuple);
			const Value *instruction = std::get<1>(tuple);
			bool wentThroughArith = std::get<2>(tuple);
			todo.pop();

#ifdef DEBUG_USES
			dbgs() << "considering use: " << *instruction << " (" << *pointer << ")\n";
#endif

			if (seen.count(tuple)) continue;
			seen.insert(tuple);

			if (transformedBeyondUse(instruction, pointer)) {
#ifdef DEBUG_USES
				dbgs() << "transformed beyond use: " << *instruction << "(" << *pointer << ")\n";
#endif
				continue;
			}

			if (isa<InsertValueInst>(instruction)) {
				// We treat insertion into aggregates as escapes.
				if (!wentThroughArith) *escapes = true;
			}

			if (dyn_cast<CastInst>(instruction) ||
				dyn_cast<SelectInst>(instruction) ||
				dyn_cast<PHINode>(instruction)) {
				for (auto &use : instruction->uses()) {
					todo.push(std::tuple<const Value *, const Value *, bool>(instruction, use.getUser(), wentThroughArith));
				}
				continue;
			}

			if (const GetElementPtrInst *gepi = dyn_cast<GetElementPtrInst>(instruction)) {
				bool thisIsArith = pointerArithInsts.count(const_cast<GetElementPtrInst *>(gepi));
				if (thisIsArith && (pointer == gepi->getPointerOperand())) {
					for (auto &use : instruction->uses()) {
						todo.push(std::tuple<const Value *, const Value *, bool>(instruction, use.getUser(), true));
					}
				} else {
					// TODO: make sure these are ok?
					// (in theory: it shouldn't matter since they get masked. and they look ok. but still, to be sure, best to think on it more..)
					//dbgs() << "ignoring non-ptr user " << *instruction << "of ptr " << *pointer << " (when tracing " << *gep << ")\n";
				}
				continue;
			}

			if (const BinaryOperator *bo = dyn_cast<BinaryOperator>(instruction)) {
				bool thisIsArith = pointerArithInsts.count(const_cast<BinaryOperator *>(bo));
				if (thisIsArith) {
					for (auto &use : instruction->uses()) {
						todo.push(std::tuple<const Value *, const Value *, bool>(instruction, use.getUser(), true));
					}
				}
				continue;
			}

#if 0
			if (dyn_cast<BinaryOperator>(instruction)) {
				// FIXME: this *should* be ok..? but think about it
				bool makesSense = false;
				ConstantInt *someCI;
				if (match(instruction, m_c_And(m_Specific(pointer), m_ConstantInt(someCI)))) {
					int64_t andv = someCI->getSExtValue();
					if (andv == -8 || andv == -16)
						makesSense = true;
				}
				for (auto &use : instruction->uses()) {
					todo.push(std::tuple<const Value *, const Value *, bool>(instruction, use.getUser(), wentThroughArith));
				}

				if (makesSense) {
					continue;
				} else if (!wentThroughArith) { // warn only on the 'last' GEP
					if (pointerArithInsts.find(const_cast<Instruction *>(cast<Instruction>(instruction))) == pointerArithInsts.end()) {
#if 0
						dbgs() << "meh " << *instruction << "\n";
						if (probablyNotPointer(instruction))
							dbgs() << ".. probably not pointer\n";
						dbgs() << "<- " << *pointer << "\n";
						for (auto &use : instruction->uses()) {
							dbgs() << "-> " << *use.getUser() << "\n";
						}
#endif
					}
					// fallthrough
				}
			}
#endif

			// we don't want to mask values used in comparisons
			// (some weird pointer arithmetic will get masked otherwise, see perlbench)
			// TODO: this is not ideal, also maybe unnecessary now
			if (dyn_cast<ICmpInst>(instruction)) {
				continue;
			}

			// does it escape *as a pointer*?
			// TODO: maybe also check ProtectStores here
			if (const IntrinsicInst *intrinsicInst = dyn_cast<IntrinsicInst>(instruction)) {
				switch (intrinsicInst->getIntrinsicID()) {
				case Intrinsic::memset:
					if (pointer == intrinsicInst->getOperand(0)) {
						if (!ProtectStores) continue;
					} else {
						//dbgs() << *intrinsicInst << "only uses " << *pointer << " as non-ptr\n";
						continue;
					}
					break;
				case Intrinsic::memcpy:
				case Intrinsic::memmove:
					if (pointer == intrinsicInst->getOperand(0)) {
						// dest
						if (!ProtectStores) continue;
					} else if (pointer == intrinsicInst->getOperand(1)) {
						// src
					} else {
						//dbgs() << *intrinsicInst << "only uses " << *pointer << " as non-ptr\n";
						continue;
					}
					break;
				default:
					if (!wentThroughArith) *escapes = true;
					break;
				}
			} else {
				// TODO: think about other possibilities, maybe also check if it's used as argument?
				if (dyn_cast<ReturnInst>(instruction) && !wentThroughArith) *escapes = true;
				if (dyn_cast<CallBase>(instruction) && !wentThroughArith) *escapes = true; /* call, invoke */
			}

			const Instruction *userInst = dyn_cast<const Instruction>(instruction);

			const StoreInst *storeInst = dyn_cast<StoreInst>(instruction);
			if (storeInst) {
				if (pointer == storeInst->getValueOperand() && !wentThroughArith)
					*escapes = true;
				if (!ProtectStores && pointer != storeInst->getValueOperand()) {
					// don't ignore stores, otherwise pointer arithmetic causes problems
					// TODO: think about this, prbly not the right fix
					if (!*allUsesVerified || !userInst || !hasDominatingPointerAccess(dt, SE, userInst, baseInst, usesAreZeroOffset)) *allUsesVerified = false;
					continue;
				}
			}

#ifdef EXTRA_DEBUG
			dbgs() << "GEP used for protected instruction " << valueTypeToStr(instruction) << "\n";
#endif
			*onlyStores = false;

			if (!*allUsesVerified || !userInst || !hasDominatingPointerAccess(dt, SE, userInst, baseInst, usesAreZeroOffset)) *allUsesVerified = false;
		}
	}

	int compareBBPosition(const BasicBlock *left, const BasicBlock *right) {
		// Which block comes first in function?
		// Better option: see if one block dominates the other first,
		// but the impact should be tiny either way
		if (left == right) return 0;
		assert(left->getParent() == right->getParent());

		// left dominates right, or vice-versa?
		if (GDT->dominates(left->getFirstNonPHI(), right->getFirstNonPHI()))
			return -1;
		if (GDT->dominates(right->getFirstNonPHI(), left->getFirstNonPHI()))
			return 1;

		for (const BasicBlock &bb : *left->getParent()) {
			if (left == &bb) return -1;
			if (right == &bb) return 1;
		}
		assert(false);
		return 0;
	}

	int compareInstPosition(
		const Instruction *left, const Instruction *right) {
		if (left == right) return 0;
		if (left->getParent() != right->getParent()) {
			return compareBBPosition(left->getParent(), right->getParent());
		}

		// Which instruction comes first in basic block?
		for (const Instruction &inst : *left->getParent()) {
			if (left == &inst) return -1;
			if (right == &inst) return 1;
		}
		assert(false);
		return 0;
	}

	Instruction *getFirstInst(const std::set<Instruction *> &insts) {
		Instruction *chosen = nullptr;
		for (Instruction *inst : insts) {
			if (!chosen || compareInstPosition(chosen, inst) < 0) {
				chosen = inst;
			}
		}
		return chosen;
	}

	// see below
	bool probablyNotPointer(const Value *V) {
		const ConstantInt *CI = dyn_cast<ConstantInt>(V);
		if (CI)
			return true;

		// this is to try and deal with some edge cases
		// TODO: we should rethink this whole mess before trying to upstream :-)
		const Instruction *I = dyn_cast<Instruction>(V);
		if (!I)
			return false;
		AAMDNodes AAMD;
		I->getAAMetadata(AAMD);
		if (AAMD.TBAA) {
			MDNode *accessTypeNode = dyn_cast<MDNode>(AAMD.TBAA->getOperand(1));
			if (accessTypeNode) {
				MDString *typeStr = dyn_cast<MDString>(accessTypeNode->getOperand(0));
				// FIXME: trying to keep this minimal for now, it can't hurt to be conservative
				if (typeStr && typeStr->getString() == "long") {
					return true;
				}
			}
		}
		return false;
	}

#if 0
	/*
	 * is this GEP likely to only be used for pointer arithmetic?
	 * this is currently a HACK to cope with problematic LLVM transformations
	 * but if we have problems it may need to be extended further
	 * [cpu2017's xalancbmk says: it needs to be extended further]
	 *
	 * we're looking for this pattern:
	 * %gep = GEP i8 *%ptr, [sub 0, <something>]
	 * %gepi = ptrtoint %gep
	 */
	bool isPointerArithmeticGEP(GetElementPtrInst *gep) {
		if (gep->getNumOperands() != 2)
			return false;

		// TODO: make sure it's i8*?

		/* must have only one index, which must be [sub 0, <something>] */
		const Value *V = gep->getOperand(1);
		const Operator *Op = dyn_cast<const Operator>(V);
		if (!Op)
			return false;
		ConstantInt *CI;
		switch (Op->getOpcode()) {
		case Instruction::Sub:
			// if we think we're subtractng something from a non-pointer..
			if (!probablyNotPointer(Op->getOperand(0)))
				return false;
			// unless we're subtracting a constant, in which case NOPE, for perf reasons
			// (this is an atempt to deal with cpu2006's gcc)
			// (this does not appear to be required for correctness, but TODO think about it)
			CI = dyn_cast<ConstantInt>(Op->getOperand(1));
			if (CI)
				return false;
			break;
		case Instruction::Xor:
			// this is to catch 'xor ptr, -1', might have some in perlbench?
			// FIXME: check the other operand too
			CI = dyn_cast<ConstantInt>(Op->getOperand(1));
			if (!CI || !CI->isMinusOne())
				return false;
			break;
		default:
			return false;
		}

		/* must only have uses which are ptrtoint? */
		for (auto &use : gep->uses()) {
			User *U = use.getUser();
			auto *I = dyn_cast<PtrToIntInst>(U);
			if (!I) {
#if 0
				dbgs() << "rejecting ptr arith GEP " << *gep << " because it has user " << *U << "\n";
#endif
				return false;
			}
		}

#if 1
		/* truncate to 32 bits */
		IRBuilder<> IRB(gep->getParent(), ++gep->getIterator());
		Type *typeI32 = Type::getInt32Ty(gep->getContext());
		Type *typeI64 = Type::getInt64Ty(gep->getContext());
		Type *gepType = gep->getType();
		Type *typeDummy = getElementType(gepType);

		Instruction *dummy = IRB.CreateAlloca(typeDummy);
		Value *ptrNew64 = IRB.CreatePtrToInt(dummy, typeI64);
		Value *truncGEP = IRB.CreateTrunc(ptrNew64, typeI32);
		Value *sextGEP = IRB.CreateZExt(truncGEP, typeI64); // FIXME: check sign of original pls?
		Value *newGEP = IRB.CreateIntToPtr(sextGEP, gepType);
		gep->replaceAllUsesWith(newGEP);
		dummy->replaceAllUsesWith(gep);
		dummy->eraseFromParent();
#endif

#if 0
		dbgs() << "GEP seems likely to be only used in ptr arith: " << *gep << "\n";
#endif

		return true;
	}
#endif

	enum pointer_class classifyPtrArith(
		DominatorTree &dt,
		Instruction *inst,
		ScalarEvolution &SE) {
		bool allUsesVerified, onlyStores, escapes, usesAreZeroOffset;

		if (inst->getMetadata("typeisolation.ignore"))
			return ptr_cls_valid;

#if 0
		if (isPointerArithmeticGEP(gep)) {
			if (pointerArithInsts.find(const_cast<Instruction *>(cast<Instruction>(gep))) != pointerArithInsts.end()) {
				dbgs() << "oh-oh: we conluded that " << *gep << " is not a pointer\n";
				gep->getParent()->getParent()->dump();
			}
			return ptr_cls_valid; // TODO: valid is not quite right here, but should be harmless (since we truncate them)
		}
#endif

		// No need to instrument GEPs only used as the pointer in
		// a store, as we don't protect writers
		instTrackUses(dt, inst, &onlyStores, &allUsesVerified, &usesAreZeroOffset, &escapes, SE);
		// TODO: rename onlyStores, because it also means 'not used'
		if (onlyStores && !escapes) {
			if (debugPointerClasses)
				dbgs() << "GEP class F=" << inst->getParent()->getParent()->getName() << " safe onlystores\n";
			return ptr_cls_safe;
		}

		if (!pointerArithInsts.count(inst)) {
			dbgs() << "oh-oh: we conluded that " << *inst << " is a pointer\n";
			inst->getParent()->getParent()->dump();
		}

		if (!allUsesVerified)
			usesAreZeroOffset = false;

		// If ALL of our users are dominated by loads at the same address as our GEP, we're done.
		// Note that this includes any escaping pointers, so we don't have to check escapes.
		if (allUsesVerified && usesAreZeroOffset) {
			if (debugPointerClasses)
				dbgs() << "GEP class F=" << inst->getParent()->getParent()->getName() << " valid all users dominated\n";
#ifdef DUMP_OPTIMISM
			dbgs() << *inst << "\n"; // this is probably suspicious..
#endif
			return ptr_cls_valid;
		}

		enum pointer_class pointerClass;
		Value *ptr = pointerBases[inst];
		if (!ptr) {
			/* We cannot handle this case. */
			dbgs() << "Error: Cannot classify " << *inst << "\n";

			/* for the paper: to ensure safety, we always give a fatal error unless we checked the function and list it here */
			// .. more recent refactoring gave us a bunch of weird cases, just hard-code them :(
			StringRef funcName = inst->getParent()->getParent()->getName();
			if (funcName.startswith("BN_nist_mod_")) { // openssl, sometimes (sigh)
				// This is (ptr & mask) | (ptr & ~mask).
				// TODO: We need to be able to detect this and only mask the result with ptr.
				// For now, we force everything as valid (code is not on any hotpath).
				assert(isa<BinaryOperator>(inst));
				pointerClasses[inst->getOperand(0)] = ptr_cls_valid;
				pointerClasses[inst->getOperand(1)] = ptr_cls_valid;
				return ptr_cls_valid;
			}
			if (funcName == "S_unpack_rec") { // cpu2017 perlbench
				// TODO: this is delta, so someday we should try fixing the logic to mark it as a delta
				assert(isa<BinaryOperator>(inst));
				return ptr_cls_valid;
			}
			if (funcName == "_ZN12MCOwnerTable7get_MCOEv") { // cpu2017 leela
				// this is a pointer on both sides :x
				// one side is new, and the other side is loaded with pointer type
				// TODO: work out why
				// %227 = sub i64 %226, %220
				// where: %220 = ptrtoint i32* %219 to i64
				//        %219 = load i32*, i32** %11
				// and: %226 = ptrtoint i8* %12 to i64
				//      %12 = invoke i8* @tc_typed_new(i64 1764, i64 371748734273408)
				// --> clearly the new is the valid half
				assert(isa<BinaryOperator>(inst));
				ptr = inst->getOperand(0);
				pointerBases[inst] = ptr;
				return ptr_cls_unsafe;
			}
			if (funcName == "_cpp_preprocess_dir_only") { // cpu2017 gcc
				// %359 = sub i64 %358, %19
				// where %19 is an offset
				// TODO: why does this not get marked correctly
				assert(isa<BinaryOperator>(inst));
				assert(isa<LoadInst>(inst->getOperand(1)));
				ptr = inst->getOperand(0);
				pointerBases[inst] = ptr;
				return ptr_cls_unsafe;
			}
			inst->getParent()->getParent()->dump();
			report_fatal_error("TypeIsolation fail");
		}

		GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(inst);
		if (!gep) {
			dbgs() << "forcing non-GEP " << *inst << " as unsafe\n";
			// TODO: non-GEPs deserve analysis too
			return ptr_cls_unsafe;
		}

		if (allUsesVerified) {
			// TODO: double-check maybe?

			if (debugPointerClasses)
				dbgs() << "GEP class F=" << gep->getParent()->getParent()->getName() << " safe all users dominated\n";
#ifdef DUMP_OPTIMISM
			dbgs() << *gep << "\n"; // this is probably suspicious..
#endif

			// we verified that our GEP is safe in the context of *all* users
			// TODO: think about below too
			// NOTE: these users may be other GEPs, so you CANNOT return valid here, you must check the index!
			pointerClass = ptr_cls_valid;
		} else if (simpleGEPCheck(gep, &usesAreZeroOffset)) {
			// now we looked backwards. is there a dominating access close enough to our GEP..?

			// if so: there is a dominating access within 4GB of our GEP
			// and that dominating access is valid

			if (debugPointerClasses)
				dbgs() << "GEP class F=" << gep->getParent()->getParent()->getName() << " nearby dom\n";

			// we have a 'nearby' pointer which is valid
			pointerClass = ptr_cls_valid;

			// we don't want to run the 'else' block below,
			// even though it could potentially mark usesAreZeroOffset == 0
			//   because it will mark base GEPs as unsafe when they don't need to be
			//   -> the second parameter prevents it from doing that
			// TODO: this kind of problem is likely a candidate for further optimizations?
			if (!usesAreZeroOffset && gep->hasAllZeroIndices()) {
				enum pointer_class classifyPointerClass = classifyPointerOperand(ptr, false);
				if (classifyPointerClass == ptr_cls_valid) {
					usesAreZeroOffset = true;
				}
			}
		} else {
			// if not: we check the GEP's pointer operand

			// [insert complicated check here]
			pointerClass = lowDistanceToPointer(SE, ptr, gep);
			if (pointerClass == ptr_cls_valid) {
				if (!escapes) {
					if (debugPointerClasses)
						dbgs() << "GEP class F=" << gep->getParent()->getParent()->getName() << " valid low distance\n";
					return ptr_cls_safe;
				}
			}

			// is it known-valid? if so:
			//   any access within 4GB of our GEP is safe -> we have a 'nearby' valid pointer
			// is it known-safe? if so:
			//   is it at the same offset? if so -> we are also safe
			//   either convert it to unsafe -> it gets masked -> we have a 'nearby' valid pointer
			pointerClass = classifyPointerOperand(ptr, !gep->hasAllZeroIndices());

			if (gep->hasAllZeroIndices())
				usesAreZeroOffset = true;
		}

		// 'nearby' pointer which is valid *and* also at the same place? if so: we are valid
		if (pointerClass == ptr_cls_valid && usesAreZeroOffset) {
			if (debugPointerClasses)
				dbgs() << "GEP class F=" << gep->getParent()->getParent()->getName() << " valid zero-offset\n";
#ifdef DUMP_OPTIMISM
			dbgs() << *gep << "\n";
#endif
			return ptr_cls_valid;
		}

		if (pointerClass == ptr_cls_unknown)
			return ptr_cls_unknown;

		// we aren't valid -> if the pointer escapes, we are unsafe
		if (escapes) {
			if (debugPointerClasses)
				dbgs() << "GEP class F=" << gep->getParent()->getParent()->getName() << " unsafe escapes\n";
			return ptr_cls_unsafe;
		}

		// If the offset is arbitrary 64-bit, all bets are off
		if (!gepIndexBoundedByGuardZone(SE, gep)) {
			if (tryToMaskGEPIndex(gep)) {
				if (debugPointerClasses)
					dbgs() << "GEP class F=" << gep->getParent()->getParent()->getName() << " safe unbounded masked\n";
				return ptr_cls_safe_base;
			}
			if (debugPointerClasses)
				dbgs() << "GEP class F=" << gep->getParent()->getParent()->getName() << " unsafe unbounded\n";
			return ptr_cls_unsafe;
		}

		// If the GEP offset is zero, safety class of result is same as
		// the input pointer
		if (usesAreZeroOffset) {
			switch (pointerClass) {
			case ptr_cls_valid:
				if (debugPointerClasses)
					dbgs() << "GEP class F=" << gep->getParent()->getParent()->getName() << " valid validptr-zero\n";
				break;
			case ptr_cls_safe:
				if (debugPointerClasses)
					dbgs() << "GEP class F=" << gep->getParent()->getParent()->getName() << " safe safeptr-zero\n";
				break;
			case ptr_cls_unsafe:
				if (debugPointerClasses)
					dbgs() << "GEP class F=" << gep->getParent()->getParent()->getName() << " unsafe unsafeptr-zero\n";
				break;
			default:
				return ptr_cls_unknown;
			}
			return pointerClass;
		}

		// If the GEP offset is potentially nonzero (but fits in 32
		// bits), result is only safe if we know we had a valid pointer
		// before
		switch (pointerClass) {
		case ptr_cls_valid:
			if (debugPointerClasses)
				dbgs() << "GEP class F=" << gep->getParent()->getParent()->getName() << " safe validptr\n";
			return ptr_cls_safe;
		case ptr_cls_safe:
			if (debugPointerClasses)
				dbgs() << "GEP class F=" << gep->getParent()->getParent()->getName() << " unsafe safeptr\n";
			return ptr_cls_unsafe;
		case ptr_cls_unsafe:
			if (debugPointerClasses)
				dbgs() << "GEP class F=" << gep->getParent()->getParent()->getName() << " unsafe unsafeptr\n";
			return ptr_cls_unsafe;
		default:
			return ptr_cls_unknown;
		}
	}
	
	void findPtrArithInFunction(
		Function &F,
		ScalarEvolution &SE) {
		std::set<Instruction *> instUnclassified;
		enum pointer_class instClass;
		Instruction *victim;
		DominatorTree dt(F);

		for (auto ari : pointerArithInsts) {
			GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(ari);
			if (gep) {
				pointerBases[gep] = gep->getPointerOperand();
			} else {
				BinaryOperator *bo = cast<BinaryOperator>(ari);
				assert(bo);
				Value *op0 = bo->getOperand(0);
				Value *op1 = bo->getOperand(1);
				value_category cat0 = valueCategories[op0];
				value_category cat1 = valueCategories[op1];
				if (cat0 == vc_pointer && cat1 == vc_pointer) {
					// This happens sometimes when calculating hashes.
					// We special-case it later (by dying if we try to mask this).
					pointerBases[bo] = nullptr;
				} else if (cat0 == vc_pointer) {
					pointerBases[bo] = op0;
				} else if (cat1 == vc_pointer) {
					pointerBases[bo] = op1;
				} else {
					// We have pointer arithmetic where neither side is a pointer?
					// We special-case it later (by dying if we try to mask this),
					// but complain anyway because it's weird.
					dbgs() << "Warning: neither side is a pointer for pointer arith " << *bo << "\n";
					pointerBases[bo] = nullptr;
				}
			}
			instUnclassified.insert(ari);
		}

		std::vector<Instruction *> allInsts(instUnclassified.begin(), instUnclassified.end());
		std::sort(allInsts.begin(), allInsts.end(), [this](Instruction *a, Instruction *b) { return compareInstPosition(a, b) < 0; });

		bool firstRound = true;
		for (;;) {
			// Classify all GEPs we can this round

			// doing std::sort every time is REALLY slow, we should maybe think about data types here
			std::vector<Instruction *> instTodo;
			for (Instruction *gep : allInsts) {
				if (firstRound || instUnclassified.find(gep) != instUnclassified.end())
					instTodo.push_back(gep);
			}
			firstRound = false;

			for (Instruction *inst : instTodo) {
				instClass = classifyPtrArith(dt, inst, SE);
				if (instClass == ptr_cls_unknown)
					continue;
				pointerClasses[inst] = instClass;
				instUnclassified.erase(inst);
			}
			
			// Did we manage to classify all?
			if (instUnclassified.empty()) break;
			
			// If no progress, mark the first GEP as unsafe
			// to avoid an infinite loop
			if (instUnclassified.size() == instTodo.size()) {
				dbgs() << "GEP class F=" << F.getName() << " unsafe victim\n";
				victim = getFirstInst(instUnclassified);
				pointerClasses[victim] = ptr_cls_unsafe;
				instUnclassified.erase(victim);
			}
		}

		// FIXME: return of the statistics
		/*dbgs() << "GEP classification F=" << F.getName() << 
			" valid=" << gepValid.size() <<
			" safe=" << gepSafe.size() <<
			" unsafe=" << gepUnsafe.size() <<
			"\n";*/
	}

	bool blacklistFunction(const std::string &name) {
		// typesafestack support functions
		if (name == "__typesafestack_init") return true;
		if (name == "thread_start") return true;
		if (name == "thread_cleanup_handler") return true;
		if (name == "unsafe_stack_alloc_and_setup") return true;
		if (name == "unsafe_stack_setup") return true;
		if (name == "unsafe_stack_alloc") return true;
		if (name == "__interceptor_pthread_create") return true;

		// libunwind ELF header manipulation
		if (name == "_ZZN9libunwind17LocalAddressSpace18findUnwindSectionsEmRNS_18UnwindInfoSectionsEENUlP12dl_phdr_infomPvE_8__invokeES4_mS5_") return true;
		if (name == "_ZN9libunwind17LocalAddressSpace11getEncodedPERmmhm") return true;
		if (name == "_ZN9libunwind17DwarfInstructionsINS_17LocalAddressSpaceENS_16Registers_x86_64EE16getSavedRegisterERS1_RKS2_mRKNS_10CFI_ParserIS1_E16RegisterLocationE") return true;
		if (name == "_ZN9libunwind12UnwindCursorINS_17LocalAddressSpaceENS_16Registers_x86_64EE24setInfoBasedOnIPRegisterEb") return true;
		if (name == "_ZZN9libunwind17LocalAddressSpace18findUnwindSectionsEmRNS_18UnwindInfoSectionsEENKUlP12dl_phdr_infomPvE_clES4_mS5_") return true;
		if (name == "__init_libc") return true;
		if (name == "cgt_init") return true;
		if (name == "_ZN10__cxxabiv1L18readEncodedPointerEPPKhh") return true;

		// TODO bench-specific hacks:

#if 0
		// SPEC CPU2006 400.perlbench
		// because it potentially sorts pointers from different arenas
		if (name == "S_mergesortsv") return true;
		// because the [inlined] xiv stuff does pointer arith we don't handle
		//if (name == "Perl_sv_upgrade") return true;
#endif

		// SPEC CPU2017 400.perlbench
		// mergesort gets inlined into this
		// some pretty bad ptr arith going on here, meh..
		/*
params: -I./lib checkspam.pl 2500 5 25 11 150 1 1 1 1

#0  0x000000000050fb32 in S_mergesortsv (base=0x480020ad10, nmemb=46991237185496, cmp=0x59ab40 <Perl_sv_cmp>, flags=<optimized out>) at pp_sort.c:454              
#1  Perl_sortsv_flags (array=<optimized out>, nmemb=46991237185496, cmp=<optimized out>, flags=0) at pp_sort.c:1468                                               
#2  0x0000000000513208 in Perl_pp_sort () at pp_sort.c:1723       

nmemb = 0x2abcffffffd8, almost certainly a cross-arena delta..?
TODO: check whether we need to blacklist Perl_pp_sort instead
*/
		if (name == "Perl_sortsv_flags") return true;

		return false;
	}
	
	bool debugFunction(const std::string &name) {
		return false;
	}

	Value *getConstPtr(IRBuilder<> &IRB, Type *type, long value) {
		Value *constInt = ConstantInt::getSigned(type, value);
		Value *constPtr = IRB.CreateIntToPtr(constInt, type->getPointerTo());
		return constPtr;
	}

	void addArithInstCheck(IRBuilder<> &IRB, Instruction *gep,
		Value *xor64, Value *ptrNew64) {
		Instruction *cond = dyn_cast<Instruction>(IRB.CreateICmpNE(xor64, ptrNew64));
		Instruction *branch = SplitBlockAndInsertIfThen(cond, cond->getNextNode(), false);
		BasicBlock *bbIf = branch->getParent();
		IRBuilder<> IRBIf(bbIf->getTerminator());
		Type *typeI64 = Type::getInt64Ty(gep->getContext());
		IRBIf.CreateStore(xor64, getConstPtr(IRBIf, typeI64, -1), true);
		IRBIf.CreateStore(ptrNew64, getConstPtr(IRBIf, typeI64, -2), true);
		DILocation *debugLoc = gep->getDebugLoc().get();
		if (debugLoc) {
			IRBIf.CreateStore(ConstantInt::get(typeI64, debugLoc->getLine()), getConstPtr(IRBIf, typeI64, -3), true);
			DILocation *inlineLoc = gep->getDebugLoc().getInlinedAt();
			if (inlineLoc) {
				IRBIf.CreateStore(ConstantInt::get(typeI64, inlineLoc->getLine()), getConstPtr(IRBIf, typeI64, -4), true);
			}
		}
	}
	
	void instrumentArithInstruction(Instruction *gep) {
		Type *typeI32 = Type::getInt32Ty(gep->getContext());
		Type *typeI64 = Type::getInt64Ty(gep->getContext());
		IRBuilder<> IRB(gep->getParent(), ++gep->getIterator());
		Type *typeGep = gep->getType();
		Type *typeDummy = getElementType(typeGep);
		if (!typeDummy && (typeGep != typeI64)) {
			errs() << "TypeIsolation: warning: GEP does not yield a pointer nor I64: ";
			gep->dump();
			errs() << "TypeIsolation: GEP type is ";
			typeGep->dump();
			gep->getParent()->getParent()->dump();
			report_fatal_error("TypeIsolation fail");
			return;
		}
		Value *ptrOperand = pointerBases[gep];
		if (isa<ConstantPointerNull>(ptrOperand)) {
			errs() << "TypeIsolation: warning: GEP has null base: ";
			gep->dump();
			gep->getParent()->getParent()->dump();
			// This happens in dealII, with an inbounds(!) GEP. Thanks, clang and/or CPU2006 and/or libstdc++.
			/*report_fatal_error("TypeIsolation fail");
			return;*/
		}
		// Base value must be a 64-bit integer, pointer, or vector of pointers.
		if (((!ptrOperand->getType()->isPtrOrPtrVectorTy()) && ptrOperand->getType() != typeI64) ||
			(isa<CompositeType>(gep->getType()) && !isa<VectorType>(gep->getType()))) {
			errs() << "TypeIsolation: warning: GEP does not use a proper pointer: ";
			gep->dump();
			errs() << "TypeIsolation: GEP type is ";
			typeGep->dump();
			errs() << "TypeIsolation: pointer type is ";
			ptrOperand->getType()->dump();
			gep->getParent()->getParent()->dump();
			report_fatal_error("TypeIsolation fail");
			return;
		}
		if (DummyOperation) {
			// FIXME: This doesn't work with I64s or vectors.
			Value *ptrOrig64 = IRB.CreatePtrToInt(ptrOperand, typeI64);
			Value *ptrMasked = IRB.CreateIntToPtr(ptrOrig64, gep->getType());
			gep->replaceAllUsesWith(ptrMasked);
			return;
		}
		// TODO can we use AND masking with aligned arenas if we know
		// the offset is 32-bit positive?
		/* replace gep with ptrMasked = ptr ^ ((uint32_t)ptr ^ (uint32_t)gep) */

		Instruction *dummy = gep->clone();
		Type *type64 = typeI64;
		Type *type32 = typeI32;
		unsigned vectorSize = 0;
		if (typeGep->isVectorTy()) {
			vectorSize = cast<VectorType>(typeGep)->getNumElements();
			type64 = VectorType::get(type64, vectorSize);
			type32 = VectorType::get(type32, vectorSize);
		}
		if (typeDummy) {
			// GEP based on a pointer (or vector of pointers)
			Value *ptrOrig64;
			// the base pointer might be a vector, we need to splat if it's a raw pointer
			bool needsSplat = (typeGep->isVectorTy() && !ptrOperand->getType()->isVectorTy());
			if (needsSplat) {
				ptrOrig64 = IRB.CreatePtrToInt(ptrOperand, typeI64);
				ptrOrig64 = IRB.CreateVectorSplat(vectorSize, ptrOrig64);
			} else {
				ptrOrig64 = IRB.CreatePtrToInt(ptrOperand, type64);
			}
			Value *ptrOrig32 = IRB.CreateTrunc(ptrOrig64, type32);
			Value *ptrNew64 = IRB.CreatePtrToInt(dummy, type64);
			Value *ptrNew32 = IRB.CreateTrunc(ptrNew64, type32);
			Value *xor32 = IRB.CreateBinOp(Instruction::Xor, ptrOrig32, ptrNew32);
			Value *xor32ext = IRB.CreateZExt(xor32, type64);
			Value *xor64 = IRB.CreateBinOp(Instruction::Xor, ptrOrig64, xor32ext);
			Value *ptrMasked = IRB.CreateIntToPtr(xor64, gep->getType());
			gep->replaceAllUsesWith(ptrMasked);
		} else {
			Value *ptrOrig64 = ptrOperand;
			Value *ptrOrig32 = IRB.CreateTrunc(ptrOrig64, type32);
			Value *ptrNew64 = dummy;
			Value *ptrNew32 = IRB.CreateTrunc(ptrNew64, type32);
			Value *xor32 = IRB.CreateBinOp(Instruction::Xor, ptrOrig32, ptrNew32);
			Value *xor32ext = IRB.CreateZExt(xor32, type64);
			Value *xor64 = IRB.CreateBinOp(Instruction::Xor, ptrOrig64, xor32ext);
			gep->replaceAllUsesWith(xor64);
		}
		dummy->replaceAllUsesWith(gep);
		dummy->deleteValue();
		// FIXME: fix this
		//if (CheckResult) addArithInstCheck(IRB, gep, xor64, ptrNew64);
	}

	void maskGEPIndex(GetElementPtrInst *gep) {
		unsigned idx;
		for (idx = 1; idx < gep->getNumOperands(); ++idx) {
			Value *V = gep->getOperand(idx);
			if (auto ConstOffset = dyn_cast<ConstantInt>(V)) {
				if (ConstOffset->isZero())
					continue;
			}
			break;
		}

		assert(idx < gep->getNumOperands());

		Value *oldIndex = gep->getOperand(idx);

		/* truncate to 32 bits */
		IRBuilder<> IRB(gep->getParent(), gep->getIterator());
		Type *type32 = Type::getInt32Ty(gep->getContext());
		Type *type64 = Type::getInt64Ty(gep->getContext());

		if (gep->getType()->isVectorTy()) {
			unsigned vectorSize = cast<VectorType>(gep->getType())->getNumElements();
			type64 = VectorType::get(type64, vectorSize);
			type32 = VectorType::get(type32, vectorSize);
		}

		Value *truncVal = IRB.CreateTrunc(oldIndex, type32);
		Value *sextVal = IRB.CreateSExt(truncVal, type64); // FIXME: check sign of original pls
		gep->setOperand(idx, sextVal);
	}
	
	bool instrumentUnsafeArithmetic(Function &F) {
		bool changed = false;
		unsigned numUnsafeMasked = 0;
		unsigned numUnsafeBaseMasked = 0;
		// RAUW invalidates pointers, so we take a copy
		std::vector<Instruction *> unsafeArith;
		std::vector<GetElementPtrInst *> unsafeBases;
		for (auto cl : pointerClasses) {
			Value *val = cl->first;
			pointer_class ptrclass = cl->second;
			if (ptrclass == ptr_cls_unsafe) {
				changed = true;
				assert(isa<Instruction>(val));
				unsafeArith.push_back(cast<Instruction>(val));
			} else if (ptrclass == ptr_cls_safe_base) {
				changed = true;
				assert(isa<GetElementPtrInst>(val));
				unsafeBases.push_back(cast<GetElementPtrInst>(val));
			}
		}
		for (auto inst : unsafeArith) {
			if (isa<BinaryOperator>(inst)) {
				dbgs() << "YAY INST " << inst->getParent()->getParent()->getName() << " / " << *inst << "\n";
			}
			instrumentArithInstruction(inst);
			numUnsafeMasked++;
		}
		for (auto gep : unsafeBases) {
			maskGEPIndex(gep);
			numUnsafeBaseMasked++;
		}
		dbgs() << "TypeIsolation: " << F.getName() << " - masked " << numUnsafeMasked << " arith insts (of " << pointerArithInsts.size() << "), " << numUnsafeBaseMasked << " GEP indexes\n";
		return changed;
	}

	bool runOnFunction(Function &F) override {
		if (blacklistFunction(F.getName())) {
			dbgs() << "TypeIsolation blacklisted " << F.getName() << "\n";
			return false;
		}
		if (debugFunction(F.getName())) {
		       dbgs() << "DEBUG TypeIsolation before\n";
		       F.dump();
	       	}

		TargetLibraryInfoImpl TLII(Triple(F.getParent()->getTargetTriple()));
		TargetLibraryInfo TLI(TLII);
		AssumptionCache AC(F);
		DominatorTree DT(F);
		LoopInfo LI(DT);
		GLI = &LI;
		GDT = &DT;
		ScalarEvolution SE(F, TLI, AC, DT, LI);

		pointerClasses.clear();
		pointerBases.clear();

		categorizePointerTypes(F);

		findPtrArithInFunction(F, SE);
		bool r = instrumentUnsafeArithmetic(F);
		if (debugFunction(F.getName())) {
			dbgs() << "DEBUG TypeIsolation after\n";
			F.dump();
		}
		return r;
	}

	// TODO implement correctly using getAnalysisUsage()
	// http://llvm.org/doxygen/classllvm_1_1AnalysisUsage.html
	/*
	int getOptimizationSchedule(void) const override {
		return (RunBeforeOpt ? -1 : 0) + (RunAfterOpt ? 1 : 0);
	}
	*/
};

char TypeIsolation::ID = 0;
static RegisterPass<TypeIsolation> X("typeisolation", "Type Isolation Pass", true, false);
