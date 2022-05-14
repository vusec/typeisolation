#ifndef SIZE_OF_TYPES_H
#define SIZE_OF_TYPES_H

#include "llvm/Pass.h"
#include "llvm/ADT/DenseMap.h"

using namespace llvm;

struct SizeofTypes : public ModulePass {
    static char ID;
    SizeofTypes() : ModulePass(ID) {}

    void getAnalysisUsage(AnalysisUsage &AU) const override;
    bool runOnModule(Module &M) override;

    Type *getSizeofType(CallBase *CI);
    void setSizeofType(CallBase *CI, Type *Ty);

private:
    DenseMap<CallBase*, Type*> mallocTypes;
};

#endif  /* SIZE_OF_TYPES_H */
