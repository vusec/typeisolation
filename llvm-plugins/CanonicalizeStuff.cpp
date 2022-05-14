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

#define DEBUG_TYPE "CanonicalizeStuff"

using namespace llvm;

struct CanonicalizeStuff : public FunctionPass {
	static char ID;
	const DataLayout *DL;

	CanonicalizeStuff() : FunctionPass(ID) {
	}

	void getAnalysisUsage(AnalysisUsage &AU) const override {
	}

	bool doFinalization(Module &M) override {
		return false;
	}

	bool runOnGEP(GetElementPtrInst *gep) {
		Type *GEPEltType = gep->getSourceElementType();
		uint64_t TyAllocSize = DL->getTypeAllocSize(GEPEltType);

		/*
		 * Find known-NULL pointers that InstCombine doesn't find:
		 * (see obstack_free macro used in cpu2017's gcc bitmap.c)
		 *
		 * %y = sub i64 0, %x
		 * %x2 = inttoptr %x
		 * %gep = gep %x2, %y <- this is %x - %x = NULL
		 */
		if (gep->getNumIndices() != 1 || TyAllocSize != 1)
			return false;
		// TODO: check types/widths?
		// do we have 'sub i64 0, %x' as our sole index?
		Operator *Index = dyn_cast<Operator>(gep->getOperand(1));
		if (!Index || Index->getOpcode() != Instruction::Sub)
			return false;
		auto *CI = dyn_cast<ConstantInt>(Index->getOperand(0));
		if (!CI || !CI->isZero())
			return false;

		/* do we have 'inttoptr %y' as our base pointer? */
		IntToPtrInst *ipi = dyn_cast<IntToPtrInst>(gep->getPointerOperand());
		if (!ipi)
			return false;

		/* is %x == %y? */
		if (Index->getOperand(1) != ipi->getOperand(0))
			return false;

		/* if so: it's NULL */
		Constant *zero = Constant::getNullValue(gep->getType());
		dbgs() << "canonicalized " << *gep << " to NULL\n";
		gep->replaceAllUsesWith(zero);
		gep->eraseFromParent();
		return true;
	}

	bool runOnPHI(PHINode *phi) {
		/*
		 * If a phi node is used by multiple GEPs, then make a new zero-offset GEP,
		 * and convert those GEPs to use the new GEP.
		 *
		 * This (hopefully) makes it easier to minimize the number of GEPs to mask,
		 * because hopefully the new GEP will be marked as valid in many situations.
		 *
		 * We actually only do this if we have at least two GEPs with constant
		 * offsets, in an attempt to remove unnecessary masking which doesn't help us.
		 * (This is actually fighting with TypeIsolation opts which don't look though GEPs :x)
		 *
		 * TODO: It might make more sense to look for *all* the GEPs in a block
		 * for the loop induction variable case, even though we special-case that one?
		 */
		unsigned gepcnt = 0;
		for (User *U : phi->users()) {
			GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(U);
			
			if (gep && gep->getPointerOperand() == phi) {
				if (gep->hasAllConstantIndices())
					gepcnt++;
			}
		}
		if (gepcnt < 2)
			return false;

		Instruction *dummy = phi->clone();
		dummy->insertBefore(phi);

		IRBuilder<> IRB(phi->getParent(), phi->getParent()->getFirstInsertionPt());
		Type *IntPtrTy = DL->getIntPtrType(phi->getType());
		Value *NewGEP = IRB.CreateGEP(nullptr, dummy,
			ConstantInt::get(IntPtrTy, 0),
			phi->getName() + ".newgep");

		phi->replaceAllUsesWith(NewGEP);
		dummy->replaceAllUsesWith(phi);
		dummy->eraseFromParent();
		return true;
	}

	bool runOnICmp(ICmpInst *icmp) {
		/*
		 * if we encounter the following, transform to an integer comparison:
		 * icmp ult i8* %scevgep543, inttoptr (i64 112 to i8*)
		 * [from perlbench's mergesort]
		 * 
		 * to keep things simple, we're just going to transform everything with an inttoptr operand
 		 */
		ConstantExpr *castOperand;
		uint otherOperand;
		//if (dyn_cast<IntToPtrInst>(icmp->getOperand(0))) {
		if (ConstantExpr *c = dyn_cast<ConstantExpr>(icmp->getOperand(0))) {
			if (c->getOpcode() != Instruction::IntToPtr)
				return false;
			//castOperand = dyn_cast<IntToPtrInst>(icmp->getOperand(0));
			castOperand = c;
			otherOperand = 1;
		} else if (ConstantExpr *c = dyn_cast<ConstantExpr>(icmp->getOperand(1))) {
			if (c->getOpcode() != Instruction::IntToPtr)
				return false;
			//castOperand = dyn_cast<IntToPtrInst>(icmp->getOperand(0));
			castOperand = c;
			otherOperand = 0;
		} else
			return false;

		dbgs() << "canonicalized " << *icmp << " to ";
		icmp->setOperand(1-otherOperand, castOperand->getOperand(0));
		icmp->setOperand(otherOperand, new PtrToIntInst(icmp->getOperand(otherOperand), castOperand->getOperand(0)->getType(), "icmprecast", icmp));
		/*if (castOperand->use_empty()) {
			if (Instruction *inst = dyn_cast<Instruction>(castOperand)) {
				if (inst->isSafeToRemove())
					inst->eraseFromParent();
			} else
				castOperand->deleteValue();
		}*/
		dbgs() << *icmp << "\n";
		return true;
	}

	bool runOnInstruction(Instruction &i) {
		ICmpInst *icmp = dyn_cast<ICmpInst>(&i);
		if (icmp) return runOnICmp(icmp);

		GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(&i);
		if (gep) return runOnGEP(gep);
	
		PHINode *phi = dyn_cast<PHINode>(&i);
		if (phi) return runOnPHI(phi);

		return false;
	}

	bool runOnBasicBlock(BasicBlock &bb) {
		bool changed = false;
		std::list<Instruction *> instructions;
		for (auto &i : bb) {
			instructions.push_back(&i);
		}
		for (auto *i : instructions) {
			if (runOnInstruction(*i)) changed = true;
		}
		return changed;
	}

	bool runOnFunction(Function &f) override {
		DL = &f.getParent()->getDataLayout();
		bool changed = false;
		for (auto &bb : f) {
			if (runOnBasicBlock(bb)) changed = true;
		}
		return changed;
	}

};

char CanonicalizeStuff::ID = 0;
static RegisterPass<CanonicalizeStuff> X("canonicalizestuff", "Canonicalize the things InstCombine refuses to", true, false);
