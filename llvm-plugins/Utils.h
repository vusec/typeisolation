#ifndef UTILS_H
#define UTILS_H

#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/Analysis/ScalarEvolutionExpressions.h>

#include <map>
#include <set>
#include <utility>

extern llvm::cl::opt<bool> FixedCompression;
extern llvm::cl::opt<unsigned long> MetadataBytes;
extern llvm::cl::opt<bool> DeepMetadata;
extern llvm::cl::opt<unsigned long> DeepMetadataBytes;

class SafetyManager {
public:
    const llvm::DataLayout *DL;
    llvm::ScalarEvolution *SE;

    SafetyManager(const llvm::DataLayout *DL, llvm::ScalarEvolution *SE) : DL(DL), SE(SE) {}    

    unsigned long GetStaticAllocaAllocationSize(const llvm::AllocaInst *AI);
    unsigned long GetByvalArgumentSize(const llvm::Argument *Arg);
    bool IsSafeStackAlloca(const llvm::Value *AllocaPtr, unsigned long AllocaSize);
    void AccumulateSafeSideEffects(const llvm::Module *M, std::set<const llvm::Instruction*> &SafeSideEffects);
    void AccumulateSafeSideEffects(const llvm::Function *F, std::set<const llvm::Instruction*> &SafeSideEffects);
    void AccumulateUnsafeSideEffects(const llvm::Value *AllocaPtr, unsigned long AllocaSize,  std::set<std::pair<const llvm::Instruction*, const llvm::Value*> > &UnsafeSideEffects);
    void AccumulateUnsafeSideEffects(const llvm::Function *F, std::map<const llvm::Value*, std::set<std::pair<const llvm::Instruction*, const llvm::Value*> > > &UnsafeSideEffects);
    void AccumulateUnsafeSideEffects(const llvm::Module *M, std::map<const llvm::GlobalValue *, std::set<std::pair<const llvm::Instruction*, const llvm::Value*> > > &UnsafeSideEffects);
private:
    bool DoesContainPointer(const llvm::Type *T);
    bool IsAccessSafe(llvm::Value *Addr, unsigned long AccessSize,
                        const llvm::Value *AllocaPtr, unsigned long AllocaSize);
    bool IsMemIntrinsicSafe(const llvm::MemIntrinsic *MI, const llvm::Use &U,
                        const llvm::Value *AllocaPtr,
                        unsigned long AllocaSize);
    void AccumulateSideEffects(const llvm::Value *AllocaPtr, std::set<const llvm::Instruction*> &SideEffects);
};

template<class T>
class TodoList {

private:
	llvm::SmallPtrSet<const T *, 16> seen;
	llvm::SmallVector<T *, 8> todo;

public:
	void add(T *obj) {
		if (!seen.count(obj)) {
			todo.push_back(obj);
			seen.insert(obj);
		}
	}
	
	T *get(void) {
		return todo.empty() ? nullptr : todo.pop_back_val();
	}
	
};

llvm::Type *getElementType(llvm::Type *type);

std::string typeToStr(llvm::Type *type);

unsigned long RoundUpToAlignment(unsigned long value, unsigned long align);

#endif /* !UTILS_H */
