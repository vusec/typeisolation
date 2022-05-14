#include <llvm/ADT/SmallSet.h>
#include <llvm/Pass.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/CodeGen/TargetLowering.h>
#include <llvm/CodeGen/TargetSubtargetInfo.h>
#include <llvm/Transforms/Utils/Local.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include <map>
#include <list>
#include <set>
#include <string>
#include <vector>

#include "Utils.h"
#include "SizeofTypes.h"
//#include "metadata.h"

#define DEBUG_TYPE "TypedMalloc"

using namespace llvm;

cl::opt<bool> OptOneType ("typedmalloc-onetype", cl::desc("Use the same type hash everywhere"), cl::init(false));
cl::opt<bool> OptPerCallsite ("typedmalloc-percallsite", cl::desc("Use a separate hash for every callsite"), cl::init(false));

struct TypedMalloc : public ModulePass {
	static char ID;

	TypedMalloc() : ModulePass(ID) { }

private:
	SizeofTypes *sizeofTypes;

public:
	struct statistics {
#		define STAT(name) long name; double name##_weighed;
#		include "TypedMallocStats.h"
#		undef STAT
		std::set<Type *> types;
	};

#define STATINC_W(name) do { stats.name++; stats.name##_weighed += weight; } while (0)

	bool isConstant(Value *value) {
		return dyn_cast<Constant>(value) != nullptr;
	}

	static bool isLLVMFunction(Function *f) {
		std::string name = f->getName();
		return name.substr(0, 5) == "llvm.";
	}

	static void getStoredTypesCallInst(TodoList<Value> &todo, CallBase *ci, Value *value) {
		Function *calledFunction = ci->getCalledFunction();
		if (!calledFunction) return;
		if (calledFunction->empty()) return;
		if (isLLVMFunction(calledFunction)) return;


		for (unsigned i = 0; i < ci->getNumArgOperands(); i++) {
			Value *argCall = ci->getArgOperand(i);
			if (argCall != value) continue;

			unsigned index = 0;
			for (Argument &argFunc : calledFunction->args()) {
				if (index == 0) {
					todo.add(&argFunc);
					break;
				}
				index++;
			}
		}
	}

	static void getStoredTypesInvokeInst(TodoList<Value> &todo, InvokeInst *ii, Value *value) {
		Function *calledFunction = ii->getCalledFunction();
		if (!calledFunction) return;
		if (calledFunction->empty()) return;

		for (unsigned i = 0; i < ii->getNumArgOperands(); i++) {
			Value *argCall = ii->getArgOperand(i);
			if (argCall != value) continue;

			unsigned index = 0;
			for (Argument &argFunc : calledFunction->args()) {
				if (index == 0) {
					todo.add(&argFunc);
					break;
				}
				index++;
			}
		}
	}

	static void getStoredTypesReturnInst(TodoList<Value> &todo, ReturnInst *ri) {
		Function *calledFunction = ri->getParent()->getParent();
		for (Use &use : calledFunction->uses()) {
			User *user = use.getUser();
			CallInst *ci = dyn_cast<CallInst>(user);
			if (ci && ci->getCalledFunction() == calledFunction) todo.add(ci);
			InvokeInst *ii = dyn_cast<InvokeInst>(user);
			if (ii && ii->getCalledFunction() == calledFunction) todo.add(ii);
		}
	}

	static void getStoredOrBitcastTypes(std::set<Type *> &types, Instruction *i, bool bitcast) {
		TodoList<Value> todo;
		Value *value;

		todo.add(i);
		while ((value = todo.get())) {
			for (const Use &use : value->uses()) {
				User *user = use.getUser();

				CastInst *casti = dyn_cast<CastInst>(user);
				if (casti) {
					if (bitcast) types.insert(casti->getType());
					todo.add(user);
					continue;
				}

				CallInst *ci = dyn_cast<CallInst>(user);
				if (ci) {
					getStoredTypesCallInst(todo, ci, value);
					continue;
				}

				InvokeInst *ii = dyn_cast<InvokeInst>(user);
				if (ii) {
					getStoredTypesInvokeInst(todo, ii, value);
					continue;
				}

				PHINode *phi = dyn_cast<PHINode>(user);
				if (phi) {
					todo.add(user);
					continue;
				}

				ReturnInst *ri = dyn_cast<ReturnInst>(user);
				if (ri) {
					getStoredTypesReturnInst(todo, ri);
					continue;
				}

				SelectInst *seli = dyn_cast<SelectInst>(user);
				if (seli) {
					if (seli->getTrueValue() == user ||
						seli->getFalseValue() == user) {
						todo.add(user);
					}
					continue;
				}

				StoreInst *si = dyn_cast<StoreInst>(user);
				if (si) {
					if (!bitcast) types.insert(si->getType());
					continue;
				}
			}
		}
	}

	static bool isFuncPointer(Type *type) {
		PointerType *pt;
		while ((pt = dyn_cast<PointerType>(type))) {
			type = pt->getElementType();
			if (type->isFunctionTy()) return true;
		}
		return false;
	}

	static bool isIntPointer(Type *type) {
		PointerType *pt;
		while ((pt = dyn_cast<PointerType>(type))) {
			type = pt->getElementType();
			if (type->isIntegerTy()) return true;
		}
		return false;
	}

	static int isInterestingType(Type *type) {
		/* non-pointer: not interesting at all */
		PointerType *pt = dyn_cast<PointerType>(type);
		if (!pt) return 0;
		Type *et = pt->getElementType();

		/* void pointer: not interesting at all */
		if (et->isVoidTy()) return 0;

		IntegerType *it = dyn_cast<IntegerType>(et);
		if (it) {
			/* LLVM represents void* as char* */
			if (it->getBitWidth() == 8) return 0;
			/* int pointer: often used as an intermediate, probably not interesting */
			return 1;
		}

		/* double/triple pointers to int are also sometimes used as intermediate */
		if (isIntPointer(et)) return 2;

		/* classes are often cast to triple pointers to functions */
		if (isFuncPointer(et)) return 2;

		/* other pointers: probably interesting */
		return 3;
	}

	static bool isFirstElementType(Type *typeFirst, StructType *typeContainer) {
		for (;;) {
			if (typeContainer->getNumElements() < 1) return false;
			Type *typeElement = *typeContainer->element_begin();
			if (typeElement == typeFirst) return true;
			if (!typeElement->isStructTy()) return false;
			typeContainer = dyn_cast<StructType>(typeElement);
		}
	}
	
	static void listFields(Type *type, std::vector<Type *> &fields) {
		if (!type->isStructTy()) {
			fields.push_back(type);
			return;
		}
		StructType *st = dyn_cast<StructType>(type);
		for (Type *element : st->elements()) {
			listFields(element, fields);
		}
	}

	static bool isBaseClass(Type *typeBase, Type *typeDerived) {
		unsigned i;
		std::vector<Type *> fieldsBase;
		std::vector<Type *> fieldsDerived;

		/* strip wrappers on both sides */
		while ((typeBase->isArrayTy() && typeDerived->isArrayTy()) ||
			(typeBase->isPointerTy() && typeDerived->isPointerTy()) ||
			(typeBase->isVectorTy() && typeDerived->isVectorTy())) {
			typeBase = getElementType(typeBase);
			typeDerived = getElementType(typeDerived);
		}

		/* compare common fields */
		listFields(typeDerived, fieldsDerived);
		if (fieldsDerived.size() < 2) return false;
		listFields(typeBase, fieldsBase);
		if (fieldsBase.size() < 1) return false;
		if (fieldsBase.size() >= fieldsDerived.size()) return false;
		for (i = 0; i < fieldsBase.size(); i++) {
			if (fieldsBase[i] != fieldsDerived[i]) return false;
		}
		return true;
	}

	static void removeBaseClasses(std::set<Type *> &types) {
		unsigned i, j;
		std::vector<Type *> typeList;
		for (Type *type : types) typeList.push_back(type);
		for (i = 0; i < typeList.size(); i++) {
			for (j = 0; j < typeList.size(); j++) {
				if (i == j) continue;
				if (isBaseClass(typeList[i], typeList[j])) {
					types.erase(typeList[i]);
				}
			}
		}
	}

	static void extractMostInterestingTypes(
		const std::set<Type *> &typesAll,
		std::set<Type *> &typesInteresting) {
		int interestingLevel;

		typesInteresting.clear();
		for (interestingLevel = 3; interestingLevel > 0; interestingLevel--) {
			for (Type *type : typesAll) {
				if (isInterestingType(type) >= interestingLevel) typesInteresting.insert(type);
			}
			if (typesInteresting.size() > 0) break;
		}

		removeBaseClasses(typesInteresting);
	}

	static void printTypes(const std::set<Type *> &types) {
		for (Type *type : types) {
			dbgs() << " ";
			type->print(dbgs(), false);
		}
	}

	static Type *getStoredType(Instruction *i,
		struct statistics &stats,
		double weight) {
		Function *f = i->getParent()->getParent();
		std::set<Type *> typesAll;
		std::set<Type *> typesInteresting;

		getStoredOrBitcastTypes(typesAll, i, false);
		extractMostInterestingTypes(typesAll, typesInteresting);

		if (typesInteresting.size() == 1) {
			STATINC_W(storedtype_found);

			Type *type = *typesInteresting.begin();
			dbgs() << "TypedMalloc: found stored type ";
			type->print(dbgs(), false);
			dbgs() << " in " << f->getName() << "\n";
			return type;
		}

		if (typesInteresting.size() > 1) {
			STATINC_W(storedtype_conflict);
			dbgs() << "TypedMalloc: type clash on stored types in " << f->getName() << ":";
			printTypes(typesInteresting);
			dbgs() << "\n";
			return nullptr;
		}

		STATINC_W(storedtype_notfound);
		dbgs() << "TypedMalloc: missing stored type in " << f->getName() << "\n";
		return nullptr;
	}

	static Type *getBitcastType(Instruction *i,
		struct statistics &stats,
		double weight) {
		Function *f = i->getParent()->getParent();
		std::set<Type *> typesAll;
		std::set<Type *> typesInteresting;

		getStoredOrBitcastTypes(typesAll, i, true);
		extractMostInterestingTypes(typesAll, typesInteresting);

		if (typesInteresting.size() == 1) {
			STATINC_W(bitcasttype_found);

			Type *type = *typesInteresting.begin();
			dbgs() << "TypedMalloc: found bitcast type ";
			type->print(dbgs(), false);
			dbgs() << " in " << f->getName() << "\n";
			return type;
		}

		if (typesInteresting.size() > 1) {
			STATINC_W(bitcasttype_conflict);
			dbgs() << "TypedMalloc: type clash on bitcast types in " << f->getName() << ":";
			printTypes(typesInteresting);
			dbgs() << "\n";
			return nullptr;
		}

		STATINC_W(bitcasttype_notfound);
		dbgs() << "TypedMalloc: missing bitcast type in " << f->getName() << "\n";
		return nullptr;
	}

	static void getNewTypes(std::set<Type *> &types, Instruction *i) {
		for (const Use &use : i->uses()) {
			User *user = use.getUser();

			BitCastInst *bci = dyn_cast<BitCastInst>(user);
			if (bci) {
				Type *type = bci->getType();
				types.insert(type);
			}
		}
	}

	static Type *getNewType(Instruction *i,
		struct statistics &stats,
		double weight) {
		Function *f = i->getParent()->getParent();
		std::set<Type *> typesAll;
		std::set<Type *> typesInteresting;

		/* for new, we look for a direct bitcast without any propagation */
		getNewTypes(typesAll, i);
		extractMostInterestingTypes(typesAll, typesInteresting);

		if (typesInteresting.size() < 1) {
			STATINC_W(newtype_notfound);
			dbgs() << "TypedMalloc: missing bitcast type in " << f->getName() << "\n";
			return nullptr;
		}

		if (typesInteresting.size() == 1) {
			STATINC_W(newtype_found);
			Type *type = *typesInteresting.begin();
			dbgs() << "TypedMalloc: found bitcast type ";
			type->print(dbgs(), false);
			dbgs() << " in " << f->getName() << "\n";
			return type;
		}

		STATINC_W(newtype_conflict);
		dbgs() << "TypedMalloc: type clash on bitcast type in " << f->getName() << ":";
		printTypes(typesInteresting);
		dbgs() << "\n";
		return nullptr;
	}

	static uint64_t getStringHash(const char *s) {
		uint64_t hash = 0;
		while (*s) {
			hash = 129 * hash + *s;
			s++;
		}
		return hash;
	}

	static uint64_t getTypeHash(Type *type) {
		std::string typeStr = typeToStr(type);
		uint64_t hash = getStringHash(typeStr.c_str());
		dbgs() << "TypedMalloc: hash " << hash << " maps to type " << typeStr << "\n";
		return hash;
	}

	static uint64_t getCallSiteHash(Instruction *i, unsigned callindex) {
		Function *f = i->getParent()->getParent();
		std::string name = f->getName();
		uint64_t hash = getStringHash(name.c_str()) + callindex;
		dbgs() << "TypedMalloc: hash " << hash << " maps to call site " << name << ":" << callindex << "\n";
		return hash;
	}

	GlobalValue *getTypedTcmallocFunction(Module *m,
		std::string name, const std::vector<Value*> &argsnew) {

		GlobalValue *gv = m->getNamedValue(name);
		if (gv) return gv;

		Type *voidType = IntegerType::get(m->getContext(), 8);
		Type *resultType = PointerType::get(voidType, 0);
		if (name == "tc_typed_posix_memalign") {
			// returns int
			resultType = IntegerType::get(m->getContext(), 32);
		}
		std::vector<Type*> argTypes;
		for (Value *arg : argsnew) {
			argTypes.push_back(arg->getType());
		}
		FunctionType *type = FunctionType::get(resultType, argTypes, false);
		return Function::Create(type, Function::ExternalLinkage, name, m);
	}

	Type *getSizeofType(CallBase *ci,
		struct statistics &stats,
		double weight) {
		if (!sizeofTypes) {
			STATINC_W(sizeoftype_notpresent);
			return nullptr;
		}

		Type *type = sizeofTypes->getSizeofType(ci);

		if (type) {
			STATINC_W(sizeoftype_found);
			Function *f = ci->getParent()->getParent();
			dbgs() << "TypedMalloc: found sizeof type " << *type <<
				" in " << f->getName() << "\n";

			/* get a pointer to the type, as this is what bitcast and store find */
			type = PointerType::get(type, 0);
		} else {
			STATINC_W(sizeoftype_notfound);
		}

		return type;
	}
	
	static Type *selectType(Type **types,
		int count,
		struct statistics &stats,
		double weight) {
		int bestInteresting = -1;
		Type *bestType = NULL;
		bool conflict = false;
		int i;
		int interesting;
		Type *type;

		/* select the most interesting and most derived type,
		 * using the order of the array for priority in case
		 * of conflicts
		 */
		for (i = 0; i < count; i++) {
			type = types[i];
			if (!type) continue;
			interesting = isInterestingType(type);
			if (bestInteresting > interesting) continue;
			if (bestInteresting == interesting && !isBaseClass(bestType, type)) continue;
			bestType = type;
			bestInteresting = interesting;
		}
		for (i = 0; i < count; i++) {
			type = types[i];
			if (!type) continue;
			if (type == bestType) continue;
			interesting = isInterestingType(type);
			if (bestInteresting > interesting) continue;
			if (isBaseClass(type, bestType)) continue;
			conflict = true;
		}
		if (conflict) {
			dbgs() << "TypedMalloc: different approaches lead to different types:";
			for (i = 0; i < count; i++) {
				type = types[i];
				if (type) {
					dbgs() << " ";
					type->print(dbgs(), false);
				} else {
					dbgs() << " (notfound)";
				}
			}
			dbgs() << "\n";
			STATINC_W(type_conflict);
		}

		return bestType;
	}

	bool runOnMallocCall(
		CallBase *ci,
		unsigned callindex,
		std::string calleename,
		bool isNew,
		struct statistics &stats,
		double weight) {
		Type *type = NULL;
		uint64_t hash;
		Function *F = ci->getParent()->getParent();
		Module *M = F->getParent();

		if (OptPerCallsite) {
			type = NULL;
		} else if (OptOneType) {
			type = Type::getVoidTy(M->getContext());
		} else {
			if (isNew) {
				type = getNewType(ci, stats, weight);
				STATINC_W(type_need_new);
				if (type) STATINC_W(type_have_new);

				dbgs() << "TypedMalloc: calling hook " << calleename <<
					" typeBitcast=" << (type != NULL);
				dbgs() << "\n";
			} else {
				Type *typeSizeof = getSizeofType(ci, stats, weight);
				Type *typeStore = getStoredType(ci, stats, weight);
				Type *typeBitcast = getBitcastType(ci, stats, weight);
				Type *types[] = { typeSizeof, typeStore, typeBitcast };
				type = selectType(types, 3, stats, weight);
				
				STATINC_W(type_need_nonnew);
				if (type) STATINC_W(type_have_nonnew);
				if (typeSizeof) STATINC_W(type_have_sizeof);
				if (typeStore) STATINC_W(type_have_store);
				if (typeBitcast) STATINC_W(type_have_bitcast);

				dbgs() << "TypedMalloc: calling hook " << calleename <<
					" typeSizeof=" << (typeSizeof != NULL) <<
					" typeStore=" << (typeStore != NULL) <<
					" typeBitcast=" << (typeBitcast != NULL) <<
					"\n";
			}
		}

		if (type) {
			stats.types.insert(type);
			STATINC_W(hash_type);
			hash = getTypeHash(type);
		} else {
			STATINC_W(hash_callsite);
			hash = getCallSiteHash(ci, callindex);
		}

		Function *callee = ci->getCalledFunction();
		size_t argcount = callee->arg_size();
		Module *m = ci->getParent()->getParent()->getParent();
		IntegerType *hashType = IntegerType::get(m->getContext(), 64);
		std::vector<Value*> argsnew;
		for (size_t i = 0; i < argcount; i++) {
			argsnew.push_back(ci->getArgOperand(i));
		}
		argsnew.push_back(ConstantInt::get(hashType, hash));

		GlobalValue *calleenew = getTypedTcmallocFunction(m, calleename, argsnew);
		CallBase *callnew;
		if (isa<CallInst>(ci)) {
			callnew = CallInst::Create(calleenew, argsnew, "", ci);
		} else if (isa<InvokeInst>(ci)) {
			InvokeInst *ii = dyn_cast<InvokeInst>(ci);
			callnew = InvokeInst::Create(calleenew, ii->getNormalDest(), ii->getUnwindDest(), argsnew, "", ci);
		} else {
			dbgs() << "don't know how to replace " << *ci << "\n";
			report_fatal_error("TypedMalloc failed to replace function");
		}
		ci->replaceAllUsesWith(callnew);
		ci->eraseFromParent();
		return true;
	}

	typedef enum {
		None,
		Calloc,
		Malloc,
		NewArray,
		NewObject,
		Realloc,
		Memalign,
		PosixMemalign,
	} CallSiteType;

	CallSiteType getCallSiteType(CallBase *ci) {
		Function *callee = ci->getCalledFunction();
		if (!callee) return CallSiteType::None;

		if ((callee->getName() == "_Znwj" ||
			callee->getName() == "_ZnwjRKSt9nothrow_t" ||
			callee->getName() == "_Znwm" ||
			callee->getName() == "_ZnwmRKSt9nothrow_t") &&
			callee->arg_size() == 1) {
			return CallSiteType::NewObject;
		}
		if ((callee->getName() == "_Znaj" ||
			callee->getName() == "_ZnajRKSt9nothrow_t" ||
			callee->getName() == "_Znam" ||
			callee->getName() == "_ZnamRKSt9nothrow_t") &&
			callee->arg_size() == 1) {
			return CallSiteType::NewArray;
		}
		if (callee->getName() == "calloc" &&
			callee->arg_size() == 2) {
			return CallSiteType::Calloc;
		}
		if (callee->getName() == "malloc" &&
			callee->arg_size() == 1) {
			return CallSiteType::Malloc;
		}
		if ((callee->getName() == "realloc" ||
			callee->getName() == "reallocf") &&
			callee->arg_size() == 2) {
			return CallSiteType::Realloc;
		}
		if (callee->getName() == "memalign" &&
			callee->arg_size() == 2) {
			return CallSiteType::Memalign;
		}
		if (callee->getName() == "posix_memalign" &&
			callee->arg_size() == 3) {
			return CallSiteType::PosixMemalign;
		}
	
		return CallSiteType::None;
	}

	uint64_t getDebugLocHash(CallBase *ci, unsigned callIndex, bool log) {
		const DebugLoc &dl = ci->getDebugLoc();
		DIScope *scope = dl.get() ? dyn_cast<DIScope>(dl.getScope()) : nullptr;

		Function *f = ci->getParent()->getParent();
		if (log) {
			dbgs() << "TypedMalloc: found call site " <<
				f->getName() << ":" << callIndex;
			Function *callee = ci->getCalledFunction();
			if (callee) dbgs() << " to " << callee->getName();
		}

		uint64_t hash;
		if (scope) {
			std::string filename = scope->getFilename();
			hash = getStringHash(filename.c_str()) +
				dl.getLine() * 256 + dl.getCol();
			if (log) {
				dbgs() << " debug location is " << filename <<
					":" << dl.getLine() << ":" <<
					dl.getCol() << " hash=" << hash;
			}
		} else {
			hash = 0;
			if (log) dbgs() << " debug location not found";
		}
		if (log) dbgs() << "\n";
		return hash;
	}

	double getWeight(CallBase *ci,
		unsigned callIndex,
		const std::map<uint64_t, long> &callsiteInlineCounts) {
		uint64_t hash = getDebugLocHash(ci, callIndex, false);
		if (!hash) return 1;
		auto it = callsiteInlineCounts.find(hash);
		if (it == callsiteInlineCounts.end()) {
			dbgs() << "TypedMalloc: error: call site with hash " << hash << " not found\n";
			getDebugLocHash(ci, callIndex, true);
			return 1;
		}
		long inlineCount = it->second;
		return 1.0 / inlineCount;
	}

	bool runOnCallInstruction(CallBase *ci,
		unsigned &callindex,
		struct statistics &stats,
		const std::map<uint64_t, long> &callsiteInlineCounts) {
		CallSiteType type = getCallSiteType(ci);
		double weight = (type == CallSiteType::None) ? 1 :
			getWeight(ci, callindex, callsiteInlineCounts);

		STATINC_W(count_call);

		switch (type) {
		case CallSiteType::Calloc:
			STATINC_W(callsite_calloc);
			return runOnMallocCall(ci, callindex++, "tc_typed_calloc", false, stats, weight);
		case CallSiteType::Malloc:
			STATINC_W(callsite_malloc);
			return runOnMallocCall(ci, callindex++, "tc_typed_malloc", false, stats, weight);
		case CallSiteType::NewArray:
		case CallSiteType::NewObject:
			STATINC_W(callsite_new);
			return runOnMallocCall(ci, callindex++, "tc_typed_new", true, stats, weight);
		case CallSiteType::Realloc:
			STATINC_W(callsite_realloc);
			return runOnMallocCall(ci, callindex++, "tc_typed_realloc", false, stats, weight);
		case CallSiteType::Memalign:
			STATINC_W(callsite_memalign);
			return runOnMallocCall(ci, callindex++, "tc_typed_memalign", false, stats, weight);
		case CallSiteType::PosixMemalign:
			STATINC_W(callsite_memalign);
			return runOnMallocCall(ci, callindex++, "tc_typed_posix_memalign", false, stats, weight);
		default:
			return false;
		}
	}

	bool runOnInstruction(Instruction *i,
		unsigned &callindex,
		struct statistics &stats,
		const std::map<uint64_t, long> &callsiteInlineCounts) {
		stats.count_ins++;

		CallBase *ci = dyn_cast<CallBase>(i);
		if (ci) return runOnCallInstruction(ci, callindex, stats, callsiteInlineCounts);

		return false;
	}

	bool runOnBasicBlock(BasicBlock &bb,
		unsigned &callindex,
		struct statistics &stats,
		const std::map<uint64_t, long> &callsiteInlineCounts) {
		bool changed = false;
		std::list<Instruction *> instructions;
		stats.count_bb++;
		for (auto &i : bb) {
			instructions.push_back(&i);
		}
		for (auto *i : instructions) {
			if (runOnInstruction(i, callindex, stats, callsiteInlineCounts)) changed = true;
		}
		return changed;
	}

	bool runOnFunction(Function &f,
		struct statistics &stats,
		const std::map<uint64_t, long> &callsiteInlineCounts) {
		bool changed = false;
		unsigned callindex = 0;
		stats.count_func++;
		for (auto &bb : f) {
			if (runOnBasicBlock(bb, callindex, stats, callsiteInlineCounts)) changed = true;
		}
		return changed;
	}

	void printStatistics(const struct statistics &stats, const std::string &modname) {
		dbgs() << "TypedMallocStats:\tmodname\ttypes.size";
#		define STAT(name) dbgs() << "\t" #name "\t" #name "_weighed";
#		include "TypedMallocStats.h"
#		undef STAT
		dbgs() << "\n";
		dbgs() << "TypedMallocStats:\t" << modname << "\t" << stats.types.size();
#		define STAT(name) dbgs() << "\t" << stats.name << "\t" << stats.name##_weighed;
#		include "TypedMallocStats.h"
#		undef STAT
		dbgs() << "\n";
	}

	void addCallsiteInlineCount(
		CallBase *ci,
		unsigned callIndex,
		std::map<uint64_t, long> &callsiteInlineCounts) {
		uint64_t hash = getDebugLocHash(ci, callIndex, true);
		auto it = callsiteInlineCounts.find(hash);
		long value = (it == callsiteInlineCounts.end()) ? 0 : it->second;
		callsiteInlineCounts[hash] = value + 1;
	}

	void getCallsiteInlineCounts(
		Module &m,
		std::map<uint64_t, long> &callsiteInlineCounts) {
		for (auto &f : m) {
			unsigned callIndex = 0;
			for (auto &bb : f) {
				for (auto &i : bb) {
					CallBase *ci = dyn_cast<CallBase>(&i);
					if (!ci) continue;
					if (getCallSiteType(ci) == CallSiteType::None) continue;
					addCallsiteInlineCount(ci, callIndex, callsiteInlineCounts);
					callIndex++;
				}
			}
		}
	}

	bool runOnModule(Module &m) override {
		struct statistics stats = { };
		sizeofTypes = getAnalysisIfAvailable<SizeofTypes>();
		bool changed = false;

		std::map<uint64_t, long> callsiteInlineCounts;
		getCallsiteInlineCounts(m, callsiteInlineCounts);

		for (auto &f : m) {
			if (runOnFunction(f, stats, callsiteInlineCounts)) changed = true;
		}
		std::string modname = m.getName();
		printStatistics(stats, modname);
		return changed;
	}

	void getAnalysisUsage(AnalysisUsage &AU) const override {
		AU.addPreserved<SizeofTypes>();
		AU.addUsedIfAvailable<SizeofTypes>();
	}
};

char TypedMalloc::ID = 0;
static RegisterPass<TypedMalloc> X("typedmalloc", "Typed Malloc Pass");
