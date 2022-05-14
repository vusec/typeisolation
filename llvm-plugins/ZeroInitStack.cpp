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

#define DEBUG_TYPE "ZeroInitStack"

using namespace llvm;

struct ZeroInitStack : public FunctionPass {
	static char ID;

	struct statistics {
#	define STAT(name) long name;
#	include "ZeroInitStackStats.h"
#	undef STAT
	} stats;

	void printStatistics(const std::string &modname) {
	dbgs() << "ZeroInitStackStats:\tmodname";
#	define STAT(name) dbgs() << "\t" #name;
#	include "ZeroInitStackStats.h"
#	undef STAT
	dbgs() << "\n";
	dbgs() << "ZeroInitStackStats:\t" << modname;
#	define STAT(name) dbgs() << "\t" << stats.name;
#	include "ZeroInitStackStats.h"
#	undef STAT
	dbgs() << "\n";
	}

	ZeroInitStack() : FunctionPass(ID) {
		memset(&stats, 0, sizeof(stats));
	}

	void getAnalysisUsage(AnalysisUsage &AU) const override {
		AU.addPreserved<SizeofTypes>();
	}

	bool doFinalization(Module &M) override {
		printStatistics(M.getName());
		return false;
	}

	bool runOnAllocaInstruction(AllocaInst *ai) {
		stats.count_alloca++;

		if (!ai->isStaticAlloca()) {
			stats.count_alloca_dynamic++;
			errs() << "warning: ZeroInitStack: dynamic variable " <<
				ai->getName() << " in function " <<
				ai->getParent()->getParent()->getName() <<
				" not moved to unsafe stack\n";
			return false;
		}
		if (ai->isArrayAllocation()) {
			stats.count_alloca_array++;
			errs() << "warning: ZeroInitStack: array allocation " <<
				ai->getName() << " in function " <<
				ai->getParent()->getParent()->getName() <<
				" not moved to unsafe stack\n";
			return false;
		}
		stats.count_alloca_zeroed++;
		Type *type = ai->getAllocatedType();
		Constant *zero = Constant::getNullValue(type);
		StoreInst *si = new StoreInst(zero, ai);
		si->insertAfter(ai);
		return true;
	}

	bool runOnInstruction(Instruction &i) {
		stats.count_ins++;

		AllocaInst *ai = dyn_cast<AllocaInst>(&i);
		if (ai) return runOnAllocaInstruction(ai);

		return false;
	}

	bool runOnBasicBlock(BasicBlock &bb) {
		bool changed = false;
		stats.count_bb++;
		for (auto &i : bb) {
			if (runOnInstruction(i)) changed = true;
		}
		return changed;
	}

	bool runOnFunction(Function &f) override {
		bool changed = false;
		stats.count_func++;
		for (auto &bb : f) {
			if (runOnBasicBlock(bb)) changed = true;
		}
		return changed;
	}

};

char ZeroInitStack::ID = 0;
static RegisterPass<ZeroInitStack> X("zeroinitstack", "Zero Init Stack Pass", true, false);
