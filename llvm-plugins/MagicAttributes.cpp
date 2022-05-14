#include <llvm/ADT/SmallSet.h>
#include <llvm/Pass.h>
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

#include <string>
#include <list>
#include <set>
#include <vector>

#include <Utils.h>
//#include <metadata.h>

#include "SizeofTypes.h"

#define DEBUG_TYPE "MagicAttributes"

using namespace llvm;

struct MagicAttributes : public ModulePass {
	static char ID;
	const DataLayout *DL;

	MagicAttributes() : ModulePass(ID) {
	}

	void getAnalysisUsage(AnalysisUsage &AU) const override {
	}

	bool doFinalization(Module &M) override {
		return false;
	}

	bool checkMemoryAccessInBounds(Value *ptr, uint64_t size, uint64_t offset) {
		// FIXME: make sure this makes sense in the morning
		uint64_t ptrSize = DL->getTypeAllocSize(ptr->getType());
		// should be no overflow because only constant offsets
		if (offset + ptrSize > size)
			return false;
		return true;
	}

	SmallPtrSet<Argument *, 32> seenArgs;
	std::vector<Argument *> retryList;

	bool runOnArgument(Function &F, Argument &arg) {
		// based on PointerMayBeCaptured
		if (!arg.getType()->isPointerTy())
			return false;
		uint64_t argSize = DL->getTypeAllocSize(arg.getType());

		SmallVector<std::pair<Use *, uint64_t>, 20> Worklist;
		SmallSet<std::pair<Use *, uint64_t>, 20> Visited;

		auto AddUses = [&](Value *V, uint64_t offset) {
			for (Use &U : V->uses()) {
				Worklist.push_back(std::pair<Use *, uint64_t>(&U, offset));
			}
		};

		AddUses(&arg, 0);

		bool safe = true;
		while (safe && !Worklist.empty()) {
			std::pair<Use *, uint64_t> nextup = Worklist.pop_back_val();
			if (!Visited.insert(nextup).second)
				continue;
			Use *U = nextup.first;
			uint64_t curoffset = nextup.second;
			if (curoffset >= argSize)
				return false;
			Instruction *I = cast<Instruction>(U->getUser());
			Value *V = U->get();

			switch (I->getOpcode()) {
			case Instruction::ICmp:
				// this is fine (e.g. comparisons with null)
				break;
			case Instruction::Call:
				{
				// it annoyed me that Perl_sv_2pv_flags didn't get marked, so this deals with that situation:
				CallInst *CI = dyn_cast<CallInst>(I);
				Function *CF = CI->getCalledFunction();
				// is the called function the one we're checking?
				if (CF && (CF == &F) && CI->isTailCall()) {
					// are we just passing this onward to a tail call, unmodified, and is it in the same spot?
					if (curoffset == 0 && arg.getArgNo() == U->getOperandNo())
						break;
				}

				if (MemIntrinsic *MI = dyn_cast<MemIntrinsic>(CI)) {
					// check bounds on the memory intrinsic
					const auto *Len = dyn_cast<ConstantInt>(MI->getLength());
					if (!Len) return false;
					uint64_t len = Len->getZExtValue();
					if (curoffset + len > argSize)
						return false;
					break;
				}
				}
			case Instruction::Invoke:
				{
				Function *CF = dyn_cast<CallBase>(I)->getCalledFunction();
				if (CF && CF->getAttributes().hasAttribute(U->getOperandNo() + 1, "boringptr")) { // +1 is FirstArgIndex, llvm9 has no StringRef variant of hasParamAttribute :(
					break;
				} else {
					// we didn't check this yet, queue it for the second round
					if (seenArgs.insert(&arg).second)
						retryList.push_back(&arg);
					return false;
				}
				}
				safe = false;
				break;
			case Instruction::Store:
				if (V == I->getOperand(0)) {
					// pointer escapes
					safe = false;
					break;
				}
				safe = checkMemoryAccessInBounds(I->getOperand(1), argSize, curoffset);
				break;
			case Instruction::Load:
				safe = checkMemoryAccessInBounds(I->getOperand(0), argSize, curoffset);
				break;
			case Instruction::PHI:
				// phi is safe because constant GEPs will eventually go out of bounds
			case Instruction::BitCast:
			case Instruction::Select:
				AddUses(I, curoffset);
				break;
			case Instruction::GetElementPtr:
				{
				GEPOperator *GEPOp = cast<GEPOperator>(I);
				if (GEPOp->hasAllConstantIndices()) {
					APInt newoffset(64, curoffset);
					if (GEPOp->accumulateConstantOffset(*DL, newoffset)) {
						AddUses(I, newoffset.getLimitedValue());
					} else
						safe = false;
				} else
					safe = false;
				}
				break;
			default:
				// who knows?
				safe = false;
				break;
			}
		}

		if (safe) {
			//dbgs() << "annotating arg " << arg << " of " << F.getName() << " as boringptr\n";
			arg.addAttr(Attribute::get(F.getContext(), "boringptr"));
			return true;
		}

		return false;
	}

	bool runOnFunction(Function &F) {
		bool changed = false;

		if (F.empty())
			return false;

		// annotate arguments
		for (Argument &arg : F.args()) {
			changed |= runOnArgument(F, arg);
		}

		return changed;
	}

	bool runOnModule(Module &M) override {
		DL = &M.getDataLayout();
		bool changed = false;
		for (Function &F : M)
			changed |= runOnFunction(F);
		for (Argument *arg : retryList)
			changed |= runOnArgument(*arg->getParent(), *arg);
		return changed;
	}
};

char MagicAttributes::ID = 0;
static RegisterPass<MagicAttributes> X("magicattributes", "Add the attributes that Alyssa needs", true, false);
