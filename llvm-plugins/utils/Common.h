#ifndef COMMON_UTILS_H
#define COMMON_UTILS_H

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
#include <llvm/IR/CallSite.h>
#include <llvm/IR/CFG.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/Analysis/ScalarEvolutionExpressions.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/Debug.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/MapVector.h>
#include <llvm/ADT/SetVector.h>

#include <string>
#include <list>
#include <set>
#include <vector>
#include <cassert>

//#include <metadata.h>  /* defines ISMETADATAFUNC */
#define ISMETADATAFUNC(x) 0

#define NOINSTRUMENT_PREFIX "__noinstrument_"

#define ifcast(ty, var, val) if (ty *var = dyn_cast<ty>(val))
#define ifncast(ty, var, val) ty *var = dyn_cast<ty>(val); if (var == nullptr)
#define foreach(ty, var, arr) for (auto *_I : (arr)) if (ty *var = cast<ty>(_I))
#define foreach_func_inst(fn, var) \
    for (inst_iterator _II = inst_begin(fn), _E = inst_end(fn); _II != _E; ++_II) \
        if (Instruction *var = &*_II)

#define LOG_LINE(line) { llvm::dbgs() << DEBUG_TYPE << ": " << line << '\n'; }

enum Possibility { No, Yes, Maybe };

llvm::Instruction *getInsertPointAfter(llvm::Instruction *I);
llvm::Instruction *getInsertPointAfter(llvm::Argument *I);

void collectPHIOrigins(llvm::PHINode *PN, std::vector<llvm::Value*> &Origins);

inline llvm::Value* otherOperand(llvm::Instruction *I, llvm::Value *Op) {
    assert(I->getNumOperands() == 2);
    return I->getOperand(I->getOperand(0) == Op ? 1 : 0);
}

inline bool isNoInstrument(llvm::Value *V) {
    assert(V);
    return V->hasName() && V->getName().startswith(NOINSTRUMENT_PREFIX);
}

inline bool shouldInstrument(llvm::Function *F) {
    return !isNoInstrument(F) && !ISMETADATAFUNC(F->getName().str().c_str());
}

llvm::Function* createNoInstrumentFunction(llvm::Module &M,
        llvm::FunctionType *FnTy, llvm::StringRef Name, bool AlwaysInline=true);
llvm::Function* getNoInstrumentFunction(llvm::Module &M, llvm::StringRef Name, bool AllowMissing=false);

inline bool isUnionType(llvm::Type *Ty) {
    return Ty->isStructTy() && Ty->getStructName().startswith("union.");
}

#endif /* !COMMON_UTILS_H */
