/*
 * WriteTracker.cpp
 *
 *  Created on: Nov 13, 2015
 *      Author: haller
 */

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
//#include <llvm/CodeGen/TargetLowering.h>
//#include <llvm/CodeGen/TargetSubtargetInfo.h>
#include <llvm/Transforms/Utils/Local.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>
#include <llvm/Analysis/ScalarEvolutionExpressions.h>

#include <string>
#include <list>
#include <set>
#include <vector>

#include <Utils.h>
//#include <metadata.h>

#define DEBUG_TYPE "WriteTracker"

using namespace llvm;

cl::opt<bool> OnlyPointerWrites ("METALLOC_ONLYPOINTERWRITES", cl::desc("Track only pointer writes"), cl::init(false));

struct WriteTracker : public FunctionPass {
    static char ID;
    Module *M;
    const DataLayout *DL;
    SafetyManager *SM;
    Type* VoidTy;
    IntegerType *Int1Ty;
    IntegerType *Int8Ty;
    IntegerType *Int32Ty;
    IntegerType *IntPtrTy;
    IntegerType *IntMetaTy;
    PointerType *PtrVoidTy;

    int tracked = 0;
    int untracked = 0;
    int optimized = 0;
    int unoptimized = 0;

    //declare i64 @metabaseget(i64)
    Constant *MetabasegetFunc;
    //declare iM @metaget(_deep)(i64)
    Constant *MetagetFunc;
    //declare iM @metaget_base(_deep)(i64, i64)
    Constant *MetagetWithBaseFunc;
    //declare void @meta_check(iM, iM)
    Constant *MetacheckFunc;
    //declare i64 @metaset_alignment(i64, i64, iM, i64)
    Constant *MetasetFunc;

    std::set<const Instruction*> ignoredGlobalStores;

    std::map<const Value*, Value*> rootInfo;
    std::list<const Value*> globalsUsed;
    std::map<const Value*, int> rootUseCount;
    std::map<const Value*, const Value*> sideEffectRoot;
    std::map<const GlobalValue*, std::set<std::pair<const llvm::Instruction*, const llvm::Value*> > > unsafeGlobalSideEffects;
    std::map<const Value*, std::set<std::pair<const llvm::Instruction*, const llvm::Value*> > > unsafeSideEffects;

    WriteTracker() : FunctionPass(ID) { }

    void AccumulateErrnoStores(const CallInst *CI, std::set<const Instruction*> SafeStores)
    {
        Function *Callee = CI->getCalledFunction();

        if (!Callee || Callee->getName() != "__errno_location")
            return;

        // Go through all uses of this value.

        SmallPtrSet<const Value *, 16> Visited;
        SmallVector<const Instruction *, 8> WorkList;
        WorkList.push_back(CI);

        // A DFS search through all uses of the value in bitcasts/PHI/GEPs/etc.
        while (!WorkList.empty()) {
            const Instruction *V = WorkList.pop_back_val();
            for (const Use &UI : V->uses()) {
                auto I = cast<const Instruction>(UI.getUser());
                assert(V == UI.get());

                switch (I->getOpcode()) {
                    case Instruction::Load:
                        // Loading from a pointer is safe.
                        break;
                    case Instruction::Store:
                        // Storing to the pointee is safe.
                        SafeStores.insert(I);
                        break;
                    case Instruction::BitCast:
                    case Instruction::IntToPtr:
                    case Instruction::PtrToInt:
                    case Instruction::PHI:
                        if (Visited.insert(I).second)
                            WorkList.push_back(cast<const Instruction>(I));
                        break;
                    default:
                        errs() << "INVALID USE OF ERRNO\n";
                        I->dump();
                        errs() << "Function " << I->getParent()->getParent()->getName() << "\n";
			I->getParent()->getParent()->dump();
                        exit(-1);
                        break;
                }
            }
        }
    }

    void UpdateSideEffectInfo(const Value* root, const Value* sideEffect) {
        if (sideEffectRoot.count(sideEffect) == 0) {
            sideEffectRoot[sideEffect] = root;
            rootUseCount[root]++;
        } else {
            const Value *oldRoot = sideEffectRoot[sideEffect];
            if (oldRoot != NULL) {
                rootUseCount[oldRoot]--;
                sideEffectRoot[sideEffect] = NULL;
            }
        }
    }

    void RemoveSideEffectInfo(const Value* sideEffect) {
        if (sideEffectRoot.count(sideEffect) == 0) {
            sideEffectRoot[sideEffect] = NULL;
        } else {
            const Value *oldRoot = sideEffectRoot[sideEffect];
            if (oldRoot != NULL) {
                rootUseCount[oldRoot]--;
                sideEffectRoot[sideEffect] = NULL;
            }
        }
    }

    void MapRootUses(Function *F, DominatorTree *DT) {
        for (auto &mapping : unsafeSideEffects) {
            for (auto &pair : mapping.second) {
                if (auto *SI = dyn_cast<StoreInst>(pair.first)) {
                    if (SI->getPointerOperand() == pair.second) {
                        if (!isa<Instruction>(mapping.first) || DT->dominates(dyn_cast<Instruction>(mapping.first), SI)) {
                            UpdateSideEffectInfo(mapping.first, SI);
                        } else {
                            RemoveSideEffectInfo(SI);
                        }
                    }
                }
            }
        }
        for (auto &mapping : unsafeGlobalSideEffects) {
            for (auto &pair : mapping.second) {
                if (auto *SI = dyn_cast<StoreInst>(pair.first)) {
                    if (SI->getParent()->getParent() == F && SI->getPointerOperand() == pair.second) {
                        UpdateSideEffectInfo(mapping.first, SI);
                    }
                }
            }
        }
    }

    void ProcessPointerArgs(Function *F) {
        // Start building instructions after first alloca
        BasicBlock::iterator It(F->getEntryBlock().getFirstInsertionPt());
        while (isa<AllocaInst>(*It) || isa<DbgInfoIntrinsic>(*It))
            ++It;
        Instruction *firstNonAlloca = &*It;
        IRBuilder<> B(firstNonAlloca);

        // Find all pointer args
        std::list<Argument*> pointerArgs;
        for (auto &a : F->args()) {
            Argument *Arg = dyn_cast<Argument>(&a);
            if (rootUseCount[Arg] > 0) {
                pointerArgs.push_back(Arg);
            }
        }

        // Insert metabase retrieval for pointer args
        for (Argument *Arg : pointerArgs) {
            Value *ptrInt = B.CreatePtrToInt(Arg, IntPtrTy);
            std::vector<Value *> callParams;
            callParams.push_back(ptrInt);
            rootInfo[Arg] = B.CreateCall(MetabasegetFunc, callParams);
        }
    }

    void ProcessUnsafeStack(Function *F) {
        // Traverse function looking for explicit metaset
        // Use arguments to metaset to reverse metabase for unsafe Alloca-s
        for (auto &i : instructions(F)) {
            CallInst *CI = dyn_cast<CallInst>(&i);
            if (CI && CI->getCalledFunction() && CI->getCalledFunction() == MetasetFunc) {
                Instruction *ptrToInt = dyn_cast<Instruction>(CI->getArgOperand(0));
                AllocaInst *AI = dyn_cast<AllocaInst>(ptrToInt->getOperand(0));
                if (rootUseCount[AI] > 0) {
                    rootInfo[AI] = CI;
                }
            }
        }
    }

    void ProcessGlobals(Function *F) {
        // Start building instructions after first alloca
        BasicBlock::iterator It(F->getEntryBlock().getFirstInsertionPt());
        while (isa<AllocaInst>(*It) || isa<DbgInfoIntrinsic>(*It))
            ++It;
        Instruction *firstNonAlloca = &*It;
        IRBuilder<> B(firstNonAlloca);

        // Find globals used by this function and get their metabase
        for (auto& global: M->globals()) {
            GlobalValue *G = &global;
            if (rootUseCount[G] > 0) {
                // Insert metabase retrieval for global
                Value *ptrInt = B.CreatePtrToInt(G, IntPtrTy);
                std::vector<Value *> callParams;
                callParams.push_back(ptrInt);
                rootInfo[G] = B.CreateCall(MetabasegetFunc, callParams);
            }
        }
    }

    void ProcessPointerReads(Function *F) {

        // Start building instructions after first alloca
        BasicBlock::iterator It(F->getEntryBlock().getFirstInsertionPt());
        while (isa<AllocaInst>(*It) || isa<DbgInfoIntrinsic>(*It))
            ++It;
        Instruction *firstNonAlloca = &*It;
        IRBuilder<> B(firstNonAlloca);

        // Find all pointer reads
        std::list<Instruction*> pointerReads;
        for (auto &i : instructions(F)) {
            LoadInst *LI = dyn_cast<LoadInst>(&i);
            if (LI && rootUseCount[LI]) {
                pointerReads.push_back(LI);
            }
            CallInst *CI = dyn_cast<CallInst>(&i);
            if (CI && rootUseCount[CI]) {
                pointerReads.push_back(CI);
            }
        }
                
        // Insert metabase retrieval for pointer reads
        for (Instruction *I : pointerReads) {
            // Get next instruction after load
            Instruction *insertBeforeInstruction = I;
            BasicBlock::iterator nextIt(I);
            ++nextIt;
            insertBeforeInstruction = &*nextIt;
            // Insert call to retrieval function
            IRBuilder<> B(insertBeforeInstruction);
            Value *ptrInt = B.CreatePtrToInt(I, IntPtrTy);
            std::vector<Value *> callParams;
            callParams.push_back(ptrInt);
            rootInfo[I] = B.CreateCall(MetabasegetFunc, callParams);
        }
    }

    bool runOnFunction(Function &F) override {
        if (ISMETADATAFUNC(F.getName().str().c_str()))
            return false;
    
        SM = new SafetyManager(DL, &getAnalysis<ScalarEvolutionWrapperPass>().getSE());
        /*DominatorTree *DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();

        unsafeSideEffects.clear();
        SM->AccumulateUnsafeSideEffects(&F, unsafeSideEffects);
        rootUseCount.clear();
        sideEffectRoot.clear();
        MapRootUses(&F, DT);
        ProcessPointerArgs(&F);
        ProcessUnsafeStack(&F);
        ProcessGlobals(&F);
        ProcessPointerReads(&F);*/

        std::set<const Instruction*> ignoredStores;
        SM->AccumulateSafeSideEffects(&F, ignoredStores);

        for (auto &bb : F) {
            for (auto &i : bb) {
                Instruction *ins = &i;
                CallInst *CI = dyn_cast<CallInst>(ins);
                if (CI)
                    AccumulateErrnoStores(CI, ignoredStores);
            }
            for (auto &i : bb) {
                Instruction *ins = &i;
                StoreInst *SI = dyn_cast<StoreInst>(ins);
                if (SI && ignoredStores.count(SI) == 0 && ignoredGlobalStores.count(SI) == 0) {tracked++;
                    BasicBlock::iterator nextIt(ins);
                    ++nextIt;
                    Instruction *nextIns = NULL;
                    if (nextIt != bb.end())
                        nextIns = &*nextIt;
                    IRBuilder<> B(nextIns);

                    Value *ptr = NULL;
                    if (OnlyPointerWrites && SI->getValueOperand()->getType()->isPointerTy()) {
                        ptr = SI->getPointerOperand();
                    }
                    if (!OnlyPointerWrites) {
                        ptr = SI->getPointerOperand();
                    }
                    if (ptr) {
                        /*if (sideEffectRoot[SI] != NULL) {
                            Value *ptrInt = B.CreatePtrToInt(ptr, IntPtrTy);
                            Value *oldPtrInt = B.CreatePtrToInt(const_cast<Value*>(sideEffectRoot[SI]), IntPtrTy);
                            std::vector<Value *> callParams;
                            callParams.push_back(ptrInt);
                            callParams.push_back(rootInfo[sideEffectRoot[SI]]);
                            callParams.push_back(oldPtrInt);
                            Value *meta = B.CreateCall(MetagetWithBaseFunc, callParams);
                            callParams.clear();
                            callParams.push_back(meta);
                            callParams.push_back(ConstantInt::get(IntMetaTy, 0));
                            B.CreateCall(MetacheckFunc, callParams);
                            optimized++;                   
                        } else */{
                            Value *ptrInt = B.CreatePtrToInt(ptr, IntPtrTy);
                            std::vector<Value *> callParams;
                            callParams.push_back(ptrInt);
                            Value *meta = B.CreateCall(MetagetFunc, callParams);
                            callParams.clear();
                            callParams.push_back(meta);
                            callParams.push_back(ConstantInt::get(IntMetaTy, 0));
                            B.CreateCall(MetacheckFunc, callParams);
                            unoptimized++;
                        }
                    }
                } else if (SI) untracked++;
            }
        }

        DEBUG(errs() << "Tracked: " << tracked << "  Untracked: " << untracked << "\n");
        /*errs() << "Optimized: " << optimized << "  Unoptimized: " << unoptimized << "\n";*/

        delete SM;

        return false;
    }

    bool doInitialization(Module &Mod) override {
        M = &Mod;

        DL = &(M->getDataLayout());
        if (!DL)
            report_fatal_error("Data layout required");

	    // Type definitions
        VoidTy = Type::getVoidTy(M->getContext());
        Int1Ty = Type::getInt1Ty(M->getContext());
        Int8Ty = Type::getInt8Ty(M->getContext());
        Int32Ty = Type::getInt32Ty(M->getContext());
        IntPtrTy = DL->getIntPtrType(M->getContext(), 0);
        if (DeepMetadata)
            IntMetaTy = Type::getIntNTy(M->getContext(), 8 * DeepMetadataBytes);
        else
            IntMetaTy = Type::getIntNTy(M->getContext(), 8 * MetadataBytes);
        PtrVoidTy = PointerType::getUnqual(Type::getInt8Ty(M->getContext()));

        std::string functionName;
        //declare i64 @metabaseget(i64)
        functionName = "metabaseget";
        MetabasegetFunc = M->getOrInsertFunction(functionName, IntPtrTy, IntPtrTy, NULL);
        //declare iM @metaget_deep(i64)
        if (DeepMetadata)
            functionName = "metaget_deep_" + std::to_string(DeepMetadataBytes);
        else if (FixedCompression)
            functionName = "metaget_fixed_" + std::to_string(MetadataBytes);
        else
            functionName = "metaget_" + std::to_string(MetadataBytes);
        MetagetFunc = M->getOrInsertFunction(functionName, IntMetaTy, IntPtrTy, NULL);
        //declare iM @metaget_base_deep(i64, i64. i64)
        if (DeepMetadata)
            functionName = "metaget_base_deep_" + std::to_string(DeepMetadataBytes);
        else
            functionName = "metaget_base_" + std::to_string(MetadataBytes);
        MetagetWithBaseFunc = M->getOrInsertFunction(functionName, IntMetaTy, IntPtrTy, IntPtrTy, IntPtrTy, NULL);
        //declare void @metacheck(iM, iM)
        if (DeepMetadata)
            functionName = "metacheck_" + std::to_string(DeepMetadataBytes);
        else
            functionName = "metacheck_" + std::to_string(MetadataBytes);
        MetacheckFunc = M->getOrInsertFunction(functionName, VoidTy, IntMetaTy, IntMetaTy, NULL);
        //declare i64 @metaset_alignment(i64, i64, iM, i64)
        if (!FixedCompression) {
            functionName = "metaset_alignment_" + std::to_string(MetadataBytes);
            MetasetFunc = M->getOrInsertFunction(functionName, IntPtrTy,
                IntPtrTy, IntPtrTy, IntMetaTy, IntPtrTy, NULL);
        } else {
            functionName = "metaset_fixed_" + std::to_string(MetadataBytes);
            MetasetFunc = M->getOrInsertFunction(functionName, IntPtrTy,
                IntPtrTy, IntPtrTy, IntMetaTy, NULL);
        }

        SM = new SafetyManager(DL, &getAnalysis<ScalarEvolutionWrapperPass>().getSE());

        SM->AccumulateSafeSideEffects(M, ignoredGlobalStores);
        SM->AccumulateUnsafeSideEffects(M, unsafeGlobalSideEffects);

        delete SM;

        return false;
    }
    
    void getAnalysisUsage(AnalysisUsage &AU) const override {
        AU.addRequired<ScalarEvolutionWrapperPass>();
        AU.addRequired<DominatorTreeWrapperPass>();
    }

};

char WriteTracker::ID = 0;
static RegisterPass<WriteTracker> X("writetracker", "Write Tracker Pass", true, false);




