#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/InlineCost.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include <llvm/Transforms/IPO/Inliner.h>

#define DEBUG_TYPE "inline-malloc-wrappers"

#include <set>

#include "utils/Common.h"
#include "SizeofTypes.h"

#undef DEBUG
#define DEBUG(x) x

using namespace llvm;

// The default is fairly arbitrary:
// Perl_safesysmalloc = 300
// Perl_safesysrealloc = 390
static cl::opt<int>
CustomInlineThreshold("inline-malloc-wrapper-threshold",
        cl::init(700), cl::ZeroOrMore,
        cl::desc("Custom inlining threshold (default = 700)"));

class InlineMallocWrappers : public LegacyInlinerBase {
    SmallPtrSet<CallInst*, 16> PossibleWrapperCalls, ActuallyInlined;
    Function *MarkEntry, *MarkExit;
    SizeofTypes *ST;
    unsigned NPropagated;
    DenseMap<ConstantInt*, unsigned> NumInlines;
    Module *module;

public:
    static char ID;
    InlineMallocWrappers() : LegacyInlinerBase(ID) {}

    void getAnalysisUsage(AnalysisUsage &AU) const override {
        AU.addUsedIfAvailable<SizeofTypes>();
        AU.addPreserved<SizeofTypes>();
        LegacyInlinerBase::getAnalysisUsage(AU);
    }

    bool runOnSCC(CallGraphSCC &SCC) override {
        return LegacyInlinerBase::runOnSCC(SCC);
    }

    struct statistics {
#       define STAT(name) long name;
#       include "InlineMallocWrappersStats.h"
#       undef STAT
    };

    void printStatistics(const struct statistics &stats, const std::string &modname) {
        dbgs() << "InlineMallocWrappersStats:\tmodname";
#       define STAT(name) dbgs() << "\t" #name;
#       include "InlineMallocWrappersStats.h"
#       undef STAT
        dbgs() << "\n";
        dbgs() << "InlineMallocWrappersStats:\t" << modname;
#       define STAT(name) dbgs() << "\t" << stats.name;
#       include "InlineMallocWrappersStats.h"
#       undef STAT
        dbgs() << "\n";
    }

    bool doInitialization(CallGraph &CG) override {
        struct statistics stats = { };

        Module &M = CG.getModule();
	module = &M;
        ST = getAnalysisIfAvailable<SizeofTypes>();

        // Find direct calls to known allocation functions
        SetVector<CallInst*> WorkList;

        for (Function &F : M) {
            stats.count_func++;
            if (isAllocFn(F)) {
                stats.count_allocfunc++;
                for (User *U : F.users()) {
                    stats.count_allocuser++;
                    ifcast(CallInst, CI, U)
                        WorkList.insert(CI);
                }
            }
        }
        stats.callsites_before = WorkList.size();

        // Walk back over users to find potential wrapper calls
        PossibleWrapperCalls.clear();
        SmallVector<CallInst*, 16> SizeofWrappers;
	std::set<std::string> wrapperNames;

        for (unsigned i = 0; i != WorkList.size(); ++i) {
            CallInst *CI = WorkList[i];
            Function *Parent = CI->getParent()->getParent();

            // Can't inline indirect calls.
            // (This happens when an alloc function is passed to such a call.)
            if (!CI->getCalledFunction()) {
                 continue;
            }

            // Can not inline direct malloc calls
            if (!CI->getCalledFunction()->isDeclaration()) {
                stats.callsites_stdlib++;
                PossibleWrapperCalls.insert(CI);

                // If the call uses sizeof in an argument, treat it as a wrapper
                // call, so don't inline its parent
                if (ST && ST->getSizeofType(CI)) {
                    stats.callsites_stdlib_sizeof++;
                    SizeofWrappers.push_back(CI);
                    continue;
                }
            }

            // If the parent function signature matches a potential
            // wrapper, inline calls to it and recurse
            stats.callsites_nonstdlib++;
            if (couldBeWrapper(Parent)) {
                stats.callsites_wrapper++;
                for (User *U : Parent->users()) {
                    stats.count_wrapperuser++;
                    ifcast(CallInst, UCI, U) {
                        stats.count_wrappercall++;
                        WorkList.insert(UCI);
                        wrapperNames.insert(Parent->getName());
                    }
                }
            }
        }
        stats.wrapper_funcs = wrapperNames.size();
        stats.callsites_after = WorkList.size();
        stats.callsites_inline = PossibleWrapperCalls.size();

	dbgs() << DEBUG_TYPE ": wrapper names:";
	for (auto wrapperName : wrapperNames) {
		dbgs() << " " << wrapperName;
	}
	dbgs() << "\n";

        dbgs() << DEBUG_TYPE ": Will try to inline " << PossibleWrapperCalls.size() << " callsites";
        if (ST)
            dbgs() << " of which " << SizeofWrappers.size() << " have a sizeof argument";
        dbgs() << "\n";

        // Insert markers to be able to find inlined locations after inlining
        // for sizeof type propagation
        IRBuilder<> B(M.getContext());
        Type *i32 = B.getInt32Ty();

        MarkEntry = Function::Create(
                FunctionType::get(B.getVoidTy(), {i32, i32, B.getInt8PtrTy()}, false),
                GlobalValue::ExternalLinkage, "__mark_entry", &M);
        CallGraphNode *EntryNode = CG.getOrInsertFunction(MarkEntry);

        MarkExit = Function::Create(
                FunctionType::get(B.getVoidTy(), {i32, i32}, false),
                GlobalValue::ExternalLinkage, "__mark_exit", &M);
        CallGraphNode *ExitNode = CG.getOrInsertFunction(MarkExit);

        Constant *Nil = ConstantPointerNull::get(B.getInt8PtrTy());
        unsigned NMarkers = 0;
        DenseMap<Function*, ConstantInt*> WrapperIDs;

        for (CallInst *CI : PossibleWrapperCalls) {
            ConstantInt *MarkerID = B.getInt32(NMarkers++);
            CallGraphNode *Node = CG[CI->getParent()->getParent()];

            // Assign a unique ID to each callee
            ConstantInt *FnID;
            Function *Wrapper = CI->getCalledFunction();
            auto it = WrapperIDs.find(Wrapper);
            if (it == WrapperIDs.end())
                FnID = WrapperIDs[Wrapper] = B.getInt32(WrapperIDs.size());
            else
                FnID = it->second;

            // Entry marker
            B.SetInsertPoint(CI);
            Value *Val = Nil;
            if (Type *Ty = ST->getSizeofType(CI)) {
                AllocaInst *Dummy = B.CreateAlloca(Ty, nullptr, "__sizeof_dummy");
                Val = B.CreateBitCast(Dummy, B.getInt8PtrTy());
            }
            CallInst *Marker = B.CreateCall(MarkEntry, {MarkerID, FnID, Val});
            Node->addCalledFunction(Marker, EntryNode);

            // Exit marker
            B.SetInsertPoint(CI->getParent(), std::next(BasicBlock::iterator(CI)));
            Marker = B.CreateCall(MarkExit, {MarkerID, FnID});
            Node->addCalledFunction(Marker, ExitNode);
        }

/*        for (CallInst *CI : PossibleWrapperCalls)
            DEBUG(LOG_LINE("try to inline:" << *CI));

        for (CallInst *CI : SizeofWrappers)
            DEBUG(LOG_LINE("sizeof wrapper call in " << CI->getParent()->getParent()->getName() << ":" << *CI)); */

        ActuallyInlined.clear();

        printStatistics(stats, M.getName());
        return ST != nullptr;
    }

    bool doFinalization(CallGraph &CG) override {
        dbgs() << DEBUG_TYPE ": Actually inlined " << ActuallyInlined.size() << " callsites\n";

        // Add weight metadata annotations to malloc calls based on the number
        // of inlines: every nested malloc call gets weight 1/ninlines to be
        // able to compare with the number of callsites when no inlining is
        // done
        for (User *U : MarkEntry->users()) {
            ConstantInt *ID = getMarkerCalleeID(cast<Instruction>(U));
            NumInlines[ID] = NumInlines.lookup(ID) + 1;
        }

        SmallPtrSet<BasicBlock*, 16> Visited;
        for (Function &F : CG.getModule()) {
            if (!F.empty()) {
                Visited.clear();
                propagateWeight(&F.getEntryBlock(), 1, Visited);
            }
        }

        // Propagate sizeof types metadata from wrapper call to malloc call,
        // and delete markers, while updating the call graph accordingly
        SetVector<Instruction*> DeleteList;
        SmallPtrSet<Function*, 16> PrunedFuncs;
        CallGraphNode *EntryNode = CG[MarkEntry];
        CallGraphNode *ExitNode = CG[MarkExit];

        NPropagated = 0;

        for (User *U : MarkExit->users())
            DeleteList.insert(cast<CallInst>(U));

        for (User *U : MarkEntry->users()) {
            CallInst *Marker = cast<CallInst>(U);
            ConstantInt *ID = getMarkerID(Marker);

            Function *F = Marker->getParent()->getParent();
            if (!PrunedFuncs.count(F)) {
                CG[F]->removeAnyCallEdgeTo(EntryNode);
                CG[F]->removeAnyCallEdgeTo(ExitNode);
                PrunedFuncs.insert(F);
            }

            ifcast(AllocaInst, Dummy, Marker->getArgOperand(2)->stripPointerCasts()) {
                if (ST)
                    propagateTypeToMallocs(Marker, ID, Dummy->getAllocatedType());
                recursivelyFindUsers(Dummy, DeleteList);
            } else {
                DeleteList.insert(Marker);
            }
        }

        for (Instruction *I : DeleteList)
            I->eraseFromParent();

        delete CG.removeFunctionFromModule(EntryNode);
        delete CG.removeFunctionFromModule(ExitNode);

        LOG_LINE("Propagated " << NPropagated << " sizeof types from wrapper calls to direct malloc calls");
        DEBUG(LOG_LINE("Deleted " << DeleteList.size() << " marker insts"));

        return true;
    }

    static AssumptionCache &getAssumptionCache(Function &F) {
	static std::map<Function*, AssumptionCache*> assumptionCaches;
	auto pair = assumptionCaches.find(&F);
	if (pair == assumptionCaches.end()) {
		AssumptionCache *AC = new AssumptionCache(F); /* TODO memory leak */
		assumptionCaches[&F] = AC;
		return *AC;
	} else {
		return *pair->second;
	}
    }

    InlineCost getInlineCost(CallSite CS) override {
        ifcast(CallInst, CI, CS.getInstruction()) {
            if (PossibleWrapperCalls.count(CI)) {
		InlineParams IP = getInlineParams(CustomInlineThreshold);
		TargetTransformInfo TTI(module->getDataLayout());
		std::function<AssumptionCache &(Function &)> gac(getAssumptionCache);
                InlineCost Cost = llvm::getInlineCost(*CI, IP, TTI, gac,
			Optional<function_ref<llvm::BlockFrequencyInfo &(llvm::Function &)> >(), nullptr);
                if (Cost)
                    ActuallyInlined.insert(CI);
                //errs() << "callsite:"; CI->dump();
                //if (Cost.isAlways()) {
                //    errs() << "cost:      always\n";
                //} else if (Cost.isVariable()) {
                //    errs() << "cost:      " << Cost.getCost() << " (delta " << Cost.getCostDelta() << ")\n";
                //} else {
                //    assert(Cost.isNever());
                //    errs() << "cost:      never\n";
                //}
                return Cost;
            }
        }
        return InlineCost::getNever("TODO Reason");
    }

private:
    bool couldBeWrapper(Function *F) {
        // TODO: also functions that take a pointer-pointer argument?
        return F->getReturnType()->isPointerTy();
    }

    bool isEntryMarker(Instruction *I) {
        ifcast(CallInst, CI, I) {
            if (Function *F = CI->getCalledFunction())
                return F == MarkEntry;
        }
        return false;
    }

    bool isExitMarker(Instruction *I, ConstantInt *ID = nullptr) {
        ifcast(CallInst, CI, I) {
            Function *F = CI->getCalledFunction();
            if (F && F == MarkExit)
                return !ID || getMarkerID(CI) == ID;
        }
        return false;
    }

    ConstantInt *getMarkerID(Instruction *Marker) {
        return cast<ConstantInt>(cast<CallInst>(Marker)->getArgOperand(0));
    }

    ConstantInt *getMarkerCalleeID(Instruction *Marker) {
        return cast<ConstantInt>(cast<CallInst>(Marker)->getArgOperand(1));
    }

    void setWeight(CallInst *CI, unsigned Inlines) {
        LLVMContext &C = CI->getContext();
        Constant *Weight = ConstantFP::get(Type::getDoubleTy(C), 1.0 / Inlines);
        MDNode *MD = MDNode::get(C, ConstantAsMetadata::get(Weight));
        CI->setMetadata("inline_weight", MD);
    }

    void propagateWeight(BasicBlock *BB, unsigned Inlines, SmallPtrSetImpl<BasicBlock*> &Visited) {
        Visited.insert(BB);

        for (Instruction &I : *BB) {
            ifncast(CallInst, CI, &I)
                continue;

            if (isEntryMarker(CI)) {
                Inlines *= NumInlines[getMarkerCalleeID(CI)];
            } else if (isExitMarker(CI)) {
                Inlines /= NumInlines[getMarkerCalleeID(CI)];
            } else {
                Function *F = CI->getCalledFunction();
                if (F && isAllocFn(*F))
                    setWeight(CI, Inlines);
            }
        }

        for (succ_iterator BI = succ_begin(BB), E = succ_end(BB); BI != E; ++BI) {
            BasicBlock *Succ = *BI;
            if (!Visited.count(Succ))
                propagateWeight(Succ, Inlines, Visited);
        }
    }

    void propagateTypeToMallocs(Instruction *Marker, ConstantInt *ID, Type *Ty) {
        SmallPtrSet<BasicBlock*, 8> Visited;
        propagateTypeToMallocs(Marker->getParent(), BasicBlock::iterator(Marker), ID, Ty, Visited);
    }

    void propagateTypeToMallocs(BasicBlock *BB, BasicBlock::iterator It,
            ConstantInt *ID, Type *Ty, SmallPtrSetImpl<BasicBlock*> &Visited) {
        Visited.insert(BB);

        for (BasicBlock::iterator E = BB->end(); It != E; ++It) {
            Instruction *I = &*It;

            if (isExitMarker(I, ID))
                return;

            ifcast(CallInst, CI, I) {
                Function *F = CI->getCalledFunction();
                if (F && isAllocFn(*F)) {
                    DEBUG(LOG_LINE("Force set type " << *Ty << " on call to " <<
                            CI->getCalledFunction()->getName() << " in " <<
                            BB->getParent()->getName() << ":"));
                    ST->setSizeofType(CI, Ty);
                    NPropagated++;
                }
            }
        }

        for (succ_iterator BI = succ_begin(BB), E = succ_end(BB); BI != E; ++BI) {
            BasicBlock *Succ = *BI;
            if (!Visited.count(Succ))
                propagateTypeToMallocs(Succ, Succ->begin(), ID, Ty, Visited);
        }
    }

    bool isAllocFn(Function &F) {
        if (!F.isDeclaration())
            return false;

        if ((F.getName() == "_Znwj" ||
             F.getName() == "_ZnwjRKSt9nothrow_t" ||
             F.getName() == "_Znwm" ||
             F.getName() == "_ZnwmRKSt9nothrow_t" ||
             F.getName() == "_Znaj" ||
             F.getName() == "_ZnajRKSt9nothrow_t" ||
             F.getName() == "_Znam" ||
             F.getName() == "_ZnamRKSt9nothrow_t" ||
             F.getName() == "malloc") &&
            F.arg_size() == 1) {
            return true;
        }

        if ((F.getName() == "calloc" ||
             F.getName() == "realloc" ||
             F.getName() == "reallocf" ||
             F.getName() == "memalign") &&
            F.arg_size() == 2) {
            return true;
        }

        if (F.getName() == "posix_memalign" && F.arg_size() == 3) return true;

        return false;
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

char InlineMallocWrappers::ID = 0;
static RegisterPass<InlineMallocWrappers>
    X("inline-malloc-wrappers", "Inline potential malloc wrappers");
