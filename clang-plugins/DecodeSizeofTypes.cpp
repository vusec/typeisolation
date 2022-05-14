#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/PassRegistry.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Support/CommandLine.h"

#define ifcast(ty, var, val) if (ty *var = dyn_cast<ty>(val))
#define ifncast(ty, var, val) ty *var = dyn_cast<ty>(val); if (var == nullptr)

#define DEBUG_TYPE "decode-sizeof-types"
#define DBG_LINE(line) { llvm::dbgs() << DEBUG_TYPE << ": " << line << '\n'; }

using namespace llvm;

template<typename T>
static inline T *getSingleUser(Instruction *I) {
    assert(I->hasOneUse());
    return cast<T>(*I->user_begin());
}

struct DecodeSizeofTypes : public ModulePass {
    static char ID;
    DecodeSizeofTypes() : ModulePass(ID) {}

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
        AU.addRequired<DominatorTreeWrapperPass>();
        AU.setPreservesCFG();
    }

    virtual bool runOnModule(Module &M) {
        Function *F = M.getFunction("__sizeof_arg");

        // If sizeof was never used, no transformations are needed
        if (!F)
            return false;

        assert(F->isDeclaration());
        SmallPtrSet<Instruction*, 4> Visited;
        SetVector<Instruction*> Dummies, DelInst, Pointers;

        for (User *U : F->users()) {
            //dbgs() << "checking " << *U << "\n";
            CallInst *CI = cast<CallInst>(U);
            DT = &getAnalysis<DominatorTreeWrapperPass>(*CI->getParent()->getParent()).getDomTree();

            AllocaInst *Dummy = cast<AllocaInst>(CI->getArgOperand(0)->stripPointerCasts());
            Type *AllocTy = Dummy->getAllocatedType()->getPointerElementType();
            Visited.clear();
            annotateCallUsers(CI, AllocTy, Visited);

            // debug for when sizeof breaks
            if (!isa<ConstantInt>(CI->getArgOperand(1))) {
                M.dump();
                dbgs() << "fail loc: " << *U << "\n";
                dbgs() << "fail func: " << CI->getParent()->getParent()->getName() << "\n";
            }

            Constant *AllocSize = cast<ConstantInt>(CI->getArgOperand(1));
            CI->replaceAllUsesWith(AllocSize);
            DelInst.insert(CI);
            Dummies.insert(Dummy);
        }

        // Delete calls and helper function declaration
        for (Instruction *I : DelInst)
            I->eraseFromParent();
        DelInst.clear();
        F->eraseFromParent();

        // Find runaway dummies that have no uses (these exist sometimes after
        // optimizations remove the call)
        // FIXME: not sure if we still need this in clang pass
        for (Function &F : M) {
            if (F.empty())
                continue;

            for (Instruction &I : F.getEntryBlock()) {
                ifncast(AllocaInst, AI, &I)
                    continue;
                if (AI->hasName() && AI->getName().startswith("__sizeof_dummy"))
                    Dummies.insert(AI);
            }
        }

        // Delete leftover dummy variables
        for (Instruction *Dummy : Dummies)
            recursivelyFindUsers(Dummy, DelInst);

        for (Instruction *I : DelInst)
            I->eraseFromParent();

        return true;
    }

private:
    DominatorTree *DT;

    void annotateCallUsers(Instruction *I, Type *Ty,
            SmallPtrSetImpl<Instruction*> &Visited) {
        if (Visited.count(I))
            return;
        Visited.insert(I);

        //dbgs() << "annotating " << *I << "\n";

        for (User *U : I->users()) {
            ifcast(CallInst, CI, U) {
                Constant *C = ConstantAggregateZero::get(StructType::create(Ty));
                MDNode *MD = MDNode::get(CI->getContext(), ConstantAsMetadata::get(C));
                CI->setMetadata("sizeofglob", MD);
            }
            // Sometimes the call result is stored in a temporary variable
            else ifcast(StoreInst, SI, U) {
                ifcast(AllocaInst, TmpAI, SI->getPointerOperand()) {
                    for (User *TmpU : TmpAI->users()) {
                        ifcast(LoadInst, LI, TmpU) {
                            //assert(DT->dominates(SI, LI));
                            if (DT->dominates(SI, LI)) {
                                annotateCallUsers(LI, Ty, Visited);
                            } else {
                                // TODO: alyssa doesn't understand what this is meant to do
                                /*dbgs() << "store " << *SI << " not dominating " << *LI << " when checking " << *TmpAI << "for top-level " << *I << "\n";
                                I->getParent()->getParent()->dump();
				report_fatal_error("sad");*/
                            }
                        }
                    }
                }
            }
            else {
                annotateCallUsers(cast<Instruction>(U), Ty, Visited);
            }
        }
        //dbgs() << "done annotating " << *I << "\n";
    }

    void recursivelyFindUsers(Instruction *I,
            SetVector<Instruction*> &DeleteList) {
        for (User *U : I->users()) {
            assert(!isa<PHINode>(U)); // Avoid cycles, should not happen
            recursivelyFindUsers(cast<Instruction>(U), DeleteList);
        }
        DeleteList.insert(I);
    }
};

static const char *descr = "Undo source transformations made by sizeof-types plugin and add metadata";

char DecodeSizeofTypes::ID = 0;
//static RegisterPass<DecodeSizeofTypes> X("decode-sizeof-types", descr);
static cl::opt<bool> OptEnableCompileTimePass("decode-sizeof-types", cl::desc(descr), cl::init(false));
static void loadPass(const PassManagerBuilder &Builder, legacy::PassManagerBase &PM) {
    if (OptEnableCompileTimePass)
        PM.add(new DecodeSizeofTypes());
}
static RegisterStandardPasses Y(PassManagerBuilder::EP_ModuleOptimizerEarly, loadPass);
static RegisterStandardPasses Z(PassManagerBuilder::EP_EnabledOnOptLevel0, loadPass);
