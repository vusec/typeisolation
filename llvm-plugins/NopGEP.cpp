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

#define DEBUG_TYPE "NopGEP"

using namespace llvm;

struct NopGEP : public FunctionPass {
	static char ID;

	NopGEP() : FunctionPass(ID) {
	}

	void getAnalysisUsage(AnalysisUsage &AU) const override {
		AU.addPreserved<SizeofTypes>();
	}
	
	Instruction *nextInst(Instruction *i) {
		bool found = false;
		BasicBlock *bb = i->getParent();
		for (auto &inst : *bb) {
			if (found) return &inst;
			if (&inst == i) found = true;
		}
		dbgs() << "NopGEP: error: next instruction not found\n";
		return nullptr;
	}

	bool runOnGEP(GetElementPtrInst *gep) {
		std::vector<Type *> asmParamTypes;
		FunctionType *asmFuncType = FunctionType::get(
			Type::getVoidTy(gep->getContext()),
			asmParamTypes,
			false);
		InlineAsm *asmFunc = InlineAsm::get(
			asmFuncType,
			"nop\n",
			"" /* Constraints */,
			true /* hasSideEffects */);
		std::vector<Value *> asmParams;
		Instruction *insertBefore = nextInst(gep);
		CallInst::Create(asmFunc, asmParams, "", insertBefore);

		return false;
	}

	bool runOnInstruction(Instruction &i) {
		GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(&i);
		if (gep) return runOnGEP(gep);

		return false;
	}

	bool runOnBasicBlock(BasicBlock &bb) {
		bool changed = false;
		for (auto &i : bb) {
			if (runOnInstruction(i)) changed = true;
		}
		return changed;
	}

	bool runOnFunction(Function &f) override {
		bool changed = false;
		for (auto &bb : f) {
			if (runOnBasicBlock(bb)) changed = true;
		}
		return changed;
	}

};

char NopGEP::ID = 0;
static RegisterPass<NopGEP> X("nopgep", "Insert Nop after GEP Pass", true, false);
