#ifndef REGISTER_PASS_H
#define REGISTER_PASS_H

#include "llvm/PassRegistry.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Support/CommandLine.h"

#define REGISTER_STANDARD_CLANG_PASS(cls, name, descr)                                                  \
    char cls::ID = 0;                                                                                   \
    static cl::opt<bool> OptEnable##cls(name, cl::desc(descr), cl::init(false));                        \
    static void loadPass##cls(const PassManagerBuilder &Builder, legacy::PassManagerBase &PM) {         \
        if (OptEnable##cls)                                                                             \
            PM.add(new cls());                                                                          \
    }                                                                                                   \
    static RegisterPass<cls> X##cls(name, descr);                                               \
    static RegisterStandardPasses Y##cls(PassManagerBuilder::EP_ModuleOptimizerEarly, loadPass##cls);   \
    static RegisterStandardPasses Z##cls(PassManagerBuilder::EP_EnabledOnOptLevel0, loadPass##cls);

#endif /* !REGISTER_PASS_H */
