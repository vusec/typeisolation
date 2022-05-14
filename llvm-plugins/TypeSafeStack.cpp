//===- SafeStack.cpp - Safe Stack Insertion -------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This pass splits the stack into the safe stack (kept as-is for LLVM backend)
// and the unsafe stack (explicitly allocated and managed through the runtime
// support library).
//
// http://clang.llvm.org/docs/SafeStack.html
//
//===----------------------------------------------------------------------===//

#include "TypeSafeStackColoring.h"
#include "TypeSafeStackLayout.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/BranchProbabilityInfo.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/InlineCost.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include <llvm/Analysis/ValueTracking.h>
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/ConstantRange.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Use.h"
#include "llvm/IR/User.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <string>
#include <utility>

#include <Utils.h>

#include <iostream>
#include <sstream>

#include "SizeofTypes.h"

#undef DEBUG
#define DEBUG(x) x

using namespace llvm;
using namespace llvm::safestack;

#define DEBUG_TYPE "type-safe-stack"

static std::string kUnsafeStackPtrCountVar = "__typesafestack_unsafe_stack_ptr_count";
static std::string kUnsafeStackPtrVar = "__typesafestack_unsafe_stack_ptrs";
static std::string kUnsafeStackPtrVarFinal = "__typesafestack_unsafe_stack_ptrs_final";
static std::string kUnsafeStackPtrVarTemp = "__typesafestack_unsafe_stack_ptrs_temp";

cl::opt<bool> OptOneStack ("typesafe-stack-onestack", cl::desc("Use a single unsafe stack, dropping type safety"), cl::init(false));
cl::opt<bool> SpecSafeStack ("typesafe-stack-specsafe", cl::desc("Require speculative safety"), cl::init(true));
cl::opt<bool> OptKeepArrays ("typesafe-stack-keeparrays", cl::desc("Do not strip wrapper arrays in determining a type ID"), cl::init(false));

namespace llvm {

STATISTIC(NumFunctions, "Total number of functions");
STATISTIC(NumUnsafeStackFunctions, "Number of functions with unsafe stack");
STATISTIC(NumUnsafeStackRestorePointsFunctions,
          "Number of functions that use setjmp or exceptions");

STATISTIC(NumAllocas, "Total number of allocas");
STATISTIC(NumUnsafeStaticAllocas, "Number of unsafe static allocas");
STATISTIC(NumUnsafeDynamicAllocas, "Number of unsafe dynamic allocas");
STATISTIC(NumUnsafeByValArguments, "Number of unsafe byval arguments");
STATISTIC(NumUnsafeStackRestorePoints, "Number of setjmps and landingpads");

} // namespace llvm

namespace {

struct statistics {
#	define STAT(name) long name;
#	include "TypeSafeStackStats.h"
#	undef STAT
};

/// Rewrite an SCEV expression for a memory access address to an expression that
/// represents offset from the given alloca.
///
/// The implementation simply replaces all mentions of the alloca with zero.
class AllocaOffsetRewriter : public SCEVRewriteVisitor<AllocaOffsetRewriter> {
  const Value *AllocaPtr;

public:
  AllocaOffsetRewriter(ScalarEvolution &SE, const Value *AllocaPtr)
      : SCEVRewriteVisitor(SE), AllocaPtr(AllocaPtr) {}

  const SCEV *visitUnknown(const SCEVUnknown *Expr) {
    if (Expr->getValue() == AllocaPtr)
      return SE.getZero(Expr->getType());
    return Expr;
  }
};

/// The TypeSafeStack pass splits the stack of each function into the safe
/// stack, which is only accessed through memory safe dereferences (as
/// determined statically), and per-type unsafe stacks, which contain all
/// local variables that are accessed in ways that we can't prove to
/// be safe.
class TypeSafeStack {
  Function &F;
  const DataLayout &DL;
  ScalarEvolution &SE;

  PointerType *StackPtrTy;
  Type *IntPtrTy;
  Type *Int32Ty;
  Type *Int8Ty;

  GlobalVariable *stackPointerArray;
  std::map<std::string, size_t> &typeIndexByTypeId;
  size_t &typeIndexNext;
  struct statistics &stats;

  /// Unsafe stack alignment. Each stack frame must ensure that the stack is
  /// aligned to this value. We need to re-align the unsafe stack if the
  /// alignment of any object on the stack exceeds this value.
  ///
  /// 16 seems like a reasonable upper bound on the alignment of objects that we
  /// might expect to appear on the stack on most common targets.
  enum { StackAlignment = 16 };

  /// \brief Build a constant representing a pointer to the unsafe stack
  /// pointer.
  size_t getTypeIndex(const std::string &typeId);
  Value *getUnsafeStackPtr(IRBuilder<> &IRB, Function &F, const std::string &typeId);

  /// Find all static allocas, dynamic allocas, return instructions and
  /// stack restore points (exception unwind blocks and setjmp calls) in the
  /// given function and append them to the respective vectors.
  void findInsts(Function &F, SmallVectorImpl<AllocaInst *> &StaticAllocas,
                 SmallVectorImpl<AllocaInst *> &DynamicAllocas,
                 SmallVectorImpl<Argument *> &ByValArguments,
                 SmallVectorImpl<ReturnInst *> &Returns,
                 SmallVectorImpl<Instruction *> &StackRestorePoints);

  /// Calculate the allocation size of a given alloca. Returns 0 if the
  /// size can not be statically determined.
  uint64_t getStaticAllocaAllocationSize(const AllocaInst* AI);

  /// Allocate space for all static allocas in \p StaticAllocas,
  /// replace allocas with pointers into the unsafe stack and generate code to
  /// restore the stack pointer before all return instructions in \p Returns.
  ///
  /// \returns A pointer to the top of the unsafe stack after all unsafe static
  /// allocas are allocated.
  void moveStaticAllocasToUnsafeStack(IRBuilder<> &IRB, Function &F,
                                        MutableArrayRef<AllocaInst *> StaticAllocas,
					ArrayRef<Argument *> ByValArguments,
                                        ArrayRef<ReturnInst *> Returns,
					std::map<std::string, Instruction*> &BasePointers,
                                        std::map<std::string, Value *> &StaticTops,
                                        const std::set<std::string> &typeIds);
  Value *moveStaticAllocasToUnsafeStack(IRBuilder<> &IRB, Function &F,
                                        MutableArrayRef<AllocaInst *> StaticAllocas,
					ArrayRef<Argument *> ByValArguments,
                                        ArrayRef<ReturnInst *> Returns,
					Instruction *BasePointer,
					TypeSafeStackColoring &SSC,
                                        const std::string &typeId);

  /// Generate code to restore the stack after all stack restore points
  /// in \p StackRestorePoints.
  ///
  /// \returns A local variable in which to maintain the dynamic top of the
  /// unsafe stack if needed.
  void
  createStackRestorePoints(IRBuilder<> &IRB, Function &F,
                           ArrayRef<Instruction *> StackRestorePoints,
                           std::map<std::string, Value *> &StaticTops,
                           std::map<std::string, AllocaInst *> &DynamicTops,
                           bool NeedDynamicTop,
                           const std::set<std::string> &typeIds);
  AllocaInst *
  createStackRestorePoints(IRBuilder<> &IRB, Function &F,
                           ArrayRef<Instruction *> StackRestorePoints,
                           Value *StaticTop, bool NeedDynamicTop,
                           const std::string &typeId);

  /// Replace all allocas in \p DynamicAllocas with code to allocate
  /// space dynamically on the unsafe stack and store the dynamic unsafe stack
  /// top to \p DynamicTop if non-null.
  void moveDynamicAllocasToUnsafeStack(IRBuilder<> &IRB,
                                       Function &F,
                                       std::map<std::string, AllocaInst *> &DynamicTops,
                                       MutableArrayRef<AllocaInst *> DynamicAllocas,
                                       const std::set<std::string> &typeIds);
  void moveDynamicAllocasToUnsafeStack(IRBuilder<> &IRB,
                                       Function &F,
                                       AllocaInst *DynamicTop,
                                       MutableArrayRef<AllocaInst *> DynamicAllocas,
                                       const std::string &typeId);

  bool IsSafeStackAlloca(const Value *AllocaPtr, uint64_t AllocaSize);

  bool IsMemIntrinsicSafe(const MemIntrinsic *MI, const Use &U,
                          const Value *AllocaPtr, uint64_t AllocaSize);
  bool IsAccessSafe(Value *Addr, uint64_t Size, const Value *AllocaPtr,
                    uint64_t AllocaSize);

public:
  TypeSafeStack(Function &F, const DataLayout &DL,
            ScalarEvolution &SE,
	    GlobalVariable *stackPointerArray,
            std::map<std::string, size_t> &typeIndexByTypeId,
            size_t &typeIndexNext,
	    struct statistics &stats)
      : F(F), DL(DL), SE(SE),
        StackPtrTy(Type::getInt8PtrTy(F.getContext())),
        IntPtrTy(DL.getIntPtrType(F.getContext())),
        Int32Ty(Type::getInt32Ty(F.getContext())),
        Int8Ty(Type::getInt8Ty(F.getContext())),
	stackPointerArray(stackPointerArray),
	typeIndexByTypeId(typeIndexByTypeId),
	typeIndexNext(typeIndexNext),
	stats(stats) {
	  memset(&stats, 0, sizeof(stats));
	}

  // Run the transformation on the associated function.
  // Returns whether the function was changed.
  bool run();
};

uint64_t TypeSafeStack::getStaticAllocaAllocationSize(const AllocaInst* AI) {
  uint64_t Size = DL.getTypeAllocSize(AI->getAllocatedType());
  if (AI->isArrayAllocation()) {
    auto C = dyn_cast<ConstantInt>(AI->getArraySize());
    if (!C)
      return 0;
    Size *= C->getZExtValue();
  }
  return Size;
}

bool TypeSafeStack::IsAccessSafe(Value *Addr, uint64_t AccessSize,
                             const Value *AllocaPtr, uint64_t AllocaSize) {
  AllocaOffsetRewriter Rewriter(SE, AllocaPtr);
  const SCEV *Expr = Rewriter.visit(SE.getSCEV(Addr));

  uint64_t BitWidth = SE.getTypeSizeInBits(Expr->getType());
  ConstantRange AccessStartRange = SE.getUnsignedRange(Expr);
  ConstantRange SizeRange =
      ConstantRange(APInt(BitWidth, 0), APInt(BitWidth, AccessSize));
  ConstantRange AccessRange = AccessStartRange.add(SizeRange);
  ConstantRange AllocaRange =
      ConstantRange(APInt(BitWidth, 0), APInt(BitWidth, AllocaSize));
  bool Safe = AllocaRange.contains(AccessRange);

  if (SpecSafeStack) {
    int64_t AddrOffset = 0;
    const Value *AddrBase = GetPointerBaseWithConstantOffset(Addr, AddrOffset, DL);
		
    // we can't trust AccessStartRange here (so we can't trust AccessRange)
    /*if (auto C = dyn_cast<ConstantInt>(Addr)) {
      ConstantRange NewAccessRange = ConstantRange(APInt(BitWidth, C->getZExtValue()), APInt(BitWidth, C->getZExtValue() + AccessSize));
      Safe = AllocaRange.contains(NewAccessRange);
    } else*/ if (AddrBase == AllocaPtr) {
      // FIXME: I was obviously not making much sense when I wrote the above, so please check me for sanity, love alyssa at 3am
      ConstantRange NewAccessRange = ConstantRange(APInt(BitWidth, AddrOffset), APInt(BitWidth, AddrOffset + AccessSize));
      Safe = AllocaRange.contains(NewAccessRange);
    } else {
      // FIXME: check some other basic stuff
      Safe = false;
    }
  }

  LLVM_DEBUG(
      dbgs() << "[TypeSafeStack] "
             << (isa<AllocaInst>(AllocaPtr) ? "Alloca " : "ByValArgument ")
             << *AllocaPtr << "\n"
             << "            Access " << *Addr << "\n"
             << "            SCEV " << *Expr
             << " U: " << SE.getUnsignedRange(Expr)
             << ", S: " << SE.getSignedRange(Expr) << "\n"
             << "            Range " << AccessRange << "\n"
             << "            AllocaRange " << AllocaRange << "\n"
             << "            " << (Safe ? "safe" : "unsafe") << "\n");

  return Safe;
}

bool TypeSafeStack::IsMemIntrinsicSafe(const MemIntrinsic *MI, const Use &U,
                                   const Value *AllocaPtr,
                                   uint64_t AllocaSize) {
  if (auto MTI = dyn_cast<MemTransferInst>(MI)) {
    if (MTI->getRawSource() != U && MTI->getRawDest() != U)
      return true;
  } else {
    if (MI->getRawDest() != U)
      return true;
  }

  const auto *Len = dyn_cast<ConstantInt>(MI->getLength());
  // Non-constant size => unsafe. FIXME: try SCEV getRange.
  if (!Len) return false;
  return IsAccessSafe(U, Len->getZExtValue(), AllocaPtr, AllocaSize);
}

/// Check whether a given allocation must be put on the safe
/// stack or not. The function analyzes all uses of AI and checks whether it is
/// only accessed in a memory safe way (as decided statically).
bool TypeSafeStack::IsSafeStackAlloca(const Value *AllocaPtr, uint64_t AllocaSize) {
  // Go through all uses of this alloca and check whether all accesses to the
  // allocated object are statically known to be memory safe and, hence, the
  // object can be placed on the safe stack.
  SmallPtrSet<const Value *, 16> Visited;
  SmallVector<const Value *, 8> WorkList;
  WorkList.push_back(AllocaPtr);

  // A DFS search through all uses of the alloca in bitcasts/PHI/GEPs/etc.
  while (!WorkList.empty()) {
    const Value *V = WorkList.pop_back_val();
    for (const Use &UI : V->uses()) {
      auto I = cast<const Instruction>(UI.getUser());
      assert(V == UI.get());

      switch (I->getOpcode()) {
      case Instruction::Load:
        if (!IsAccessSafe(UI, DL.getTypeStoreSize(I->getType()), AllocaPtr,
                          AllocaSize))
          return false;
        break;

      case Instruction::VAArg:
        // "va-arg" from a pointer is safe.
        break;
      case Instruction::Store:
        if (V == I->getOperand(0)) {
          // Stored the pointer - conservatively assume it may be unsafe.
          LLVM_DEBUG(dbgs()
                     << "[TypeSafeStack] Unsafe alloca: " << *AllocaPtr
                     << "\n            store of address: " << *I << "\n");
          return false;
        }

        if (!IsAccessSafe(UI, DL.getTypeStoreSize(I->getOperand(0)->getType()),
                          AllocaPtr, AllocaSize))
          return false;
        break;

      case Instruction::Ret:
        // Information leak.
        return false;

      case Instruction::Call:
      case Instruction::Invoke: {
        ImmutableCallSite CS(I);

        if (I->isLifetimeStartOrEnd())
          continue;

        if (const MemIntrinsic *MI = dyn_cast<MemIntrinsic>(I)) {
          if (!IsMemIntrinsicSafe(MI, UI, AllocaPtr, AllocaSize)) {
            LLVM_DEBUG(dbgs()
                       << "[TypeSafeStack] Unsafe alloca: " << *AllocaPtr
                       << "\n            unsafe memintrinsic: " << *I << "\n");
            return false;
          }
          continue;
        }

        // LLVM 'nocapture' attribute is only set for arguments whose address
        // is not stored, passed around, or used in any other non-trivial way.
        // We assume that passing a pointer to an object as a 'nocapture
        // readnone' argument is safe.
        // FIXME: a more precise solution would require an interprocedural
        // analysis here, which would look at all uses of an argument inside
        // the function being called.
        ImmutableCallSite::arg_iterator B = CS.arg_begin(), E = CS.arg_end();
        for (ImmutableCallSite::arg_iterator A = B; A != E; ++A)
          if (A->get() == V) {
            if (!(CS.doesNotCapture(A - B) && (CS.doesNotAccessMemory(A - B) ||
                                               CS.doesNotAccessMemory()))) {
              // are we passing an unmodified copy of the pointer as a boringptr attribute?
              // if so, that is still safe; it will only be accessed in-bounds
              // FIXME: we actually need to make sure the size doesn't exceed allowed size here, but this doens't happen in our benchmarks for paper
              const Function *F = nullptr;
              if (V->getType()->isPointerTy()) {
                F = CS.getCalledFunction();
                int64_t AddrOffset = 0;
                const Value *AddrBase = GetPointerBaseWithConstantOffset(V, AddrOffset, DL);
                if (AddrBase != AllocaPtr || AddrOffset != 0)
                  F = nullptr;
              }
              if (!F || !F->getAttributes().hasAttribute((A - B) + 1, "boringptr")) { // +1 is FirstArgIndex, llvm9 has no StringRef variant of hasParamAttribute :(
                LLVM_DEBUG(dbgs() << "[TypeSafeStack] Unsafe alloca: " << *AllocaPtr
                                  << "\n            unsafe call: " << *I << "\n");
                return false;
              }
            }
          }
        continue;
      }

      default:
        if (Visited.insert(I).second)
          WorkList.push_back(cast<const Instruction>(I));
      }
    }
  }

  // All uses of the alloca are safe, we can place it on the safe stack.
  return true;
}

size_t TypeSafeStack::getTypeIndex(const std::string &typeId) {
  /* TODO need to lock access to typeIndexByTypeId in case LLVM causes concurrent access? */

  auto typeIndexIt = typeIndexByTypeId.find(typeId);
  size_t typeIndex;
  if (typeIndexIt == typeIndexByTypeId.end()) {
    typeIndex = typeIndexNext++;
    dbgs() << "TypeSafeStack getTypeIndex typeId=" << typeId << " typeIndex=" << typeIndex << "\n";
    typeIndexByTypeId[typeId] = typeIndex;
  } else {
    typeIndex = typeIndexIt->second;
  }

  return typeIndex;  
}

Value *TypeSafeStack::getUnsafeStackPtr(IRBuilder<> &IRB, Function &F,
  const std::string &typeId) {
  size_t typeIndex = getTypeIndex(typeId);
  IntegerType *gepIndexType = IntegerType::get(IRB.getContext(), 32);
  std::vector<Value *> gepIndices;
  gepIndices.push_back(ConstantInt::get(gepIndexType, 0));
  gepIndices.push_back(ConstantInt::get(gepIndexType, typeIndex));
  Value *gep = IRB.CreateGEP(stackPointerArray, gepIndices);
  /* constant-folded so fine */
  //dyn_cast<Instruction>(gep)->setMetadata("typeisolation.ignore", MDNode::get(IRB.getContext(), {}));
  return gep;
}

void TypeSafeStack::findInsts(Function &F,
                          SmallVectorImpl<AllocaInst *> &StaticAllocas,
                          SmallVectorImpl<AllocaInst *> &DynamicAllocas,
                          SmallVectorImpl<Argument *> &ByValArguments,
                          SmallVectorImpl<ReturnInst *> &Returns,
                          SmallVectorImpl<Instruction *> &StackRestorePoints) {
  for (Instruction &I : instructions(&F)) {
    if (auto AI = dyn_cast<AllocaInst>(&I)) {
      ++NumAllocas;

      uint64_t Size = getStaticAllocaAllocationSize(AI);
      if (IsSafeStackAlloca(AI, Size))
        continue;

      if (AI->isStaticAlloca()) {
        ++NumUnsafeStaticAllocas;
        StaticAllocas.push_back(AI);
      } else {
        ++NumUnsafeDynamicAllocas;
        DynamicAllocas.push_back(AI);
      }
    } else if (auto RI = dyn_cast<ReturnInst>(&I)) {
      Returns.push_back(RI);
    } else if (auto CI = dyn_cast<CallInst>(&I)) {
      // setjmps require stack restore.
      if (CI->getCalledFunction() && CI->canReturnTwice())
        StackRestorePoints.push_back(CI);
    } else if (auto LP = dyn_cast<LandingPadInst>(&I)) {
      // Exception landing pads require stack restore.
      StackRestorePoints.push_back(LP);
    } else if (auto II = dyn_cast<IntrinsicInst>(&I)) {
      if (II->getIntrinsicID() == Intrinsic::gcroot)
        report_fatal_error(
            "gcroot intrinsic not compatible with safestack attribute");
    }
  }
  for (Argument &Arg : F.args()) {
    if (!Arg.hasByValAttr())
      continue;
    uint64_t Size =
        DL.getTypeStoreSize(Arg.getType()->getPointerElementType());
    if (IsSafeStackAlloca(&Arg, Size))
      continue;

    ++NumUnsafeByValArguments;
    ByValArguments.push_back(&Arg);
  }
  stats.count_static += StaticAllocas.size();
  stats.count_dynamic += DynamicAllocas.size();
  stats.count_return += Returns.size();
  stats.count_restore += StackRestorePoints.size();
}

static Type *typeStripArrays(Type *type) {
  ArrayType *at;

  while ((at = dyn_cast<ArrayType>(type))) {
    type = at->getElementType();
  }

  return type;
}

static std::string getTypeId(Type *type) {
  if (OptOneStack) return "onestack";
  if (!OptKeepArrays) type = typeStripArrays(type);
  return typeToStr(type);
}

static std::string getTypeId(AllocaInst *ai) {
  return ai ? getTypeId(ai->getAllocatedType()) : "";
}

static std::string getTypeId(Argument *arg) {
  return arg ? getTypeId(arg->getType()) : "";
}

template<typename T>
static void getTypeIds(
  std::set<std::string> &typeIds, ArrayRef<T *> instructions) {
  for (T *instruction : instructions) {
    std::string typeId = getTypeId(instruction);
    typeIds.insert(typeId);
  }
}

static size_t countAllocaForTypeId(
  ArrayRef<AllocaInst *> allocas, const std::string &typeId) {
  size_t count = 0;
  for (AllocaInst *ai : allocas) {
    if (getTypeId(ai) == typeId) count++;
  }
  return count;
}

void
TypeSafeStack::createStackRestorePoints(IRBuilder<> &IRB, Function &F,
                                    ArrayRef<Instruction *> StackRestorePoints,
                                    std::map<std::string, Value *> &StaticTops,
                                    std::map<std::string, AllocaInst *> &DynamicTops,
                                    bool NeedDynamicTop,
                                    const std::set<std::string> &typeIds) {
  for (auto &typeId : typeIds) {
    BasicBlock *insertBlock = IRB.GetInsertBlock();
    BasicBlock::iterator insertPt = IRB.GetInsertPoint();
    DynamicTops[typeId] =
      createStackRestorePoints(IRB, F, StackRestorePoints, StaticTops[typeId], NeedDynamicTop, typeId);
    IRB.SetInsertPoint(insertBlock, insertPt);
  }
}

AllocaInst *
TypeSafeStack::createStackRestorePoints(IRBuilder<> &IRB, Function &F,
                                    ArrayRef<Instruction *> StackRestorePoints,
                                    Value *StaticTop, bool NeedDynamicTop,
                                    const std::string &typeId) {
  if (StackRestorePoints.empty())
    return nullptr;

  // We need the current value of the shadow stack pointer to restore
  // after longjmp or exception catching.

  // FIXME: On some platforms this could be handled by the longjmp/exception
  // runtime itself.

  AllocaInst *DynamicTop = nullptr;
  if (NeedDynamicTop) {
    // If we also have dynamic alloca's, the stack pointer value changes
    // throughout the function. For now we store it in an alloca.
    DynamicTop = IRB.CreateAlloca(StackPtrTy, /*ArraySize=*/nullptr,
                                  "unsafe_stack_dynamic_ptr_" + typeId);
    IRB.CreateStore(StaticTop, DynamicTop);
  }

  // Restore current stack pointer after longjmp/exception catch.
  for (Instruction *I : StackRestorePoints) {
    ++NumUnsafeStackRestorePoints;

    IRB.SetInsertPoint(I->getNextNode());
    Value *CurrentTop =
        DynamicTop ? IRB.CreateLoad(StackPtrTy, DynamicTop) : StaticTop;
    Value *UnsafeStackPtr = getUnsafeStackPtr(IRB, F, typeId);
    IRB.CreateStore(CurrentTop, UnsafeStackPtr);
  }

  return DynamicTop;
}

void
TypeSafeStack::moveStaticAllocasToUnsafeStack(IRBuilder<> &IRB, Function &F,
                                          MutableArrayRef<AllocaInst *> StaticAllocas,
					  ArrayRef<Argument *> ByValArguments,
                                          ArrayRef<ReturnInst *> Returns,
					  std::map<std::string, Instruction*> &BasePointers,
                                          std::map<std::string, Value *> &StaticTops,
					  const std::set<std::string> &typeIds) {
  TypeSafeStackColoring SSC(F, StaticAllocas);
  SSC.run();
  SSC.removeAllMarkers();

  for (auto &typeId : typeIds) {
    StaticTops[typeId] =
      moveStaticAllocasToUnsafeStack(IRB, F, StaticAllocas, ByValArguments, Returns, BasePointers[typeId], SSC, typeId);
  }
}

/// We explicitly compute and set the unsafe stack layout for all unsafe
/// static alloca instructions. We save the unsafe "base pointer" in the
/// prologue into a local variable and restore it in the epilogue.
Value *TypeSafeStack::moveStaticAllocasToUnsafeStack(
    IRBuilder<> &IRB, Function &F, MutableArrayRef<AllocaInst *> StaticAllocas,
    ArrayRef<Argument *> ByValArguments, ArrayRef<ReturnInst *> Returns,
    Instruction *BasePointer, TypeSafeStackColoring &SSC,
    const std::string &typeId) {
  if (StaticAllocas.empty() && ByValArguments.empty())
    return BasePointer;

  DIBuilder DIB(*F.getParent());

  // Unsafe stack always grows down.
  TypeSafeStackLayout SSL(StackAlignment);
  for (Argument *Arg : ByValArguments) {
    if (getTypeId(Arg) != typeId) continue;
    Type *Ty = Arg->getType()->getPointerElementType();
    uint64_t Size = DL.getTypeStoreSize(Ty);
    if (Size == 0)
      Size = 1; // Don't create zero-sized stack objects.

    // Ensure the object is properly aligned.
    unsigned Align = std::max((unsigned)DL.getPrefTypeAlignment(Ty),
                              Arg->getParamAlignment());
    SSL.addObject(Arg, Size, Align, SSC.getFullLiveRange());
  }

  for (AllocaInst *AI : StaticAllocas) {
    if (getTypeId(AI) != typeId) continue;
    Type *Ty = AI->getAllocatedType();
    uint64_t Size = getStaticAllocaAllocationSize(AI);
    if (Size == 0)
      Size = 1; // Don't create zero-sized stack objects.

    // Ensure the object is properly aligned.
    unsigned Align =
        std::max((unsigned)DL.getPrefTypeAlignment(Ty), AI->getAlignment());

    SSL.addObject(AI, Size, Align, SSC.getLiveRange(AI));
  }

  SSL.computeLayout();
  unsigned FrameAlignment = SSL.getFrameAlignment();

  // FIXME: tell SSL that we start at a less-then-MaxAlignment aligned location
  // (AlignmentSkew).
  if (FrameAlignment > StackAlignment) {
    // Re-align the base pointer according to the max requested alignment.
    assert(isPowerOf2_32(FrameAlignment));
    IRB.SetInsertPoint(BasePointer->getNextNode());
    BasePointer = cast<Instruction>(IRB.CreateIntToPtr(
        IRB.CreateAnd(IRB.CreatePtrToInt(BasePointer, IntPtrTy),
                      ConstantInt::get(IntPtrTy, ~uint64_t(FrameAlignment - 1))),
        StackPtrTy));
  }

  IRB.SetInsertPoint(BasePointer->getNextNode());

  for (Argument *Arg : ByValArguments) {
    if (getTypeId(Arg) != typeId) continue;
    unsigned Offset = SSL.getObjectOffset(Arg);
    unsigned Align = SSL.getObjectAlignment(Arg);
    Type *Ty = Arg->getType()->getPointerElementType();

    uint64_t Size = DL.getTypeStoreSize(Ty);
    if (Size == 0)
      Size = 1; // Don't create zero-sized stack objects.

    Value *Off = IRB.CreateGEP(Int8Ty, BasePointer, // BasePointer is i8*
                               ConstantInt::get(Int32Ty, -Offset));
    // FIXME: do we want to ignore these for typeisolation XXX
    //dyn_cast<Instruction>(Off)->setMetadata("typeisolation.ignore", MDNode::get(IRB.getContext(), {}));
    Value *NewArg = IRB.CreateBitCast(Off, Arg->getType(),
                                     Arg->getName() + ".unsafe-byval");

    // Replace alloc with the new location.
    replaceDbgDeclare(Arg, BasePointer, BasePointer->getNextNode(), DIB,
                      DIExpression::ApplyOffset, -Offset);
    Arg->replaceAllUsesWith(NewArg);
    IRB.SetInsertPoint(cast<Instruction>(NewArg)->getNextNode());
    IRB.CreateMemCpy(Off, Align, Arg, Arg->getParamAlignment(), Size);
  }

  // Allocate space for every unsafe static AllocaInst on the unsafe stack.
  for (size_t i = 0; i < StaticAllocas.size(); i++) {
    AllocaInst *AI = StaticAllocas[i];
    if (getTypeId(AI) != typeId) continue;
    IRB.SetInsertPoint(AI);
    unsigned Offset = SSL.getObjectOffset(AI);

    replaceDbgDeclareForAlloca(AI, BasePointer, DIB, DIExpression::ApplyOffset,
                               -Offset);
    replaceDbgValueForAlloca(AI, BasePointer, DIB, -Offset);

    // Replace uses of the alloca with the new location.
    // Insert address calculation close to each use to work around PR27844.
    std::string Name = std::string(AI->getName()) + ".unsafe";
    while (!AI->use_empty()) {
      Use &U = *AI->use_begin();
      Instruction *User = cast<Instruction>(U.getUser());

      Instruction *InsertBefore;
      if (auto *PHI = dyn_cast<PHINode>(User))
        InsertBefore = PHI->getIncomingBlock(U)->getTerminator();
      else
        InsertBefore = User;

      IRBuilder<> IRBUser(InsertBefore);
      Value *Off = IRBUser.CreateGEP(Int8Ty, BasePointer, // BasePointer is i8*
                                     ConstantInt::get(Int32Ty, -Offset));
      dyn_cast<Instruction>(Off)->setMetadata("typeisolation.ignore", MDNode::get(IRB.getContext(), {}));
      Value *Replacement = IRBUser.CreateBitCast(Off, AI->getType(), Name);

      if (auto *PHI = dyn_cast<PHINode>(User))
        // PHI nodes may have multiple incoming edges from the same BB (why??),
        // all must be updated at once with the same incoming value.
        PHI->setIncomingValueForBlock(PHI->getIncomingBlock(U), Replacement);
      else
        U.set(Replacement);
    }

    AI->eraseFromParent();
    StaticAllocas[i] = nullptr; // clean up after erase
  }

  // Re-align BasePointer so that our callees would see it aligned as
  // expected.
  // FIXME: no need to update BasePointer in leaf functions.
  unsigned FrameSize = alignTo(SSL.getFrameSize(), StackAlignment);

  // Update shadow stack pointer in the function epilogue.
  IRB.SetInsertPoint(BasePointer->getNextNode());

  Value *StaticTop =
      IRB.CreateGEP(Int8Ty, BasePointer, ConstantInt::get(Int32Ty, -FrameSize),
                    "unsafe_stack_static_top_" + typeId);
  //dyn_cast<Instruction>(StaticTop)->setMetadata("typeisolation.ignore", MDNode::get(IRB.getContext(), {}));
  Value *UnsafeStackPtr = getUnsafeStackPtr(IRB, F, typeId);
  IRB.CreateStore(StaticTop, UnsafeStackPtr);
  return StaticTop;
}

void TypeSafeStack::moveDynamicAllocasToUnsafeStack(
    IRBuilder<> &IRB, Function &F,
    std::map<std::string, AllocaInst *> &DynamicTops,
    MutableArrayRef<AllocaInst *> DynamicAllocas,
    const std::set<std::string> &typeIds) {
  for (auto &typeId : typeIds) {
    moveDynamicAllocasToUnsafeStack(IRB, F, DynamicTops[typeId], DynamicAllocas, typeId);
  }
}

void TypeSafeStack::moveDynamicAllocasToUnsafeStack(
    IRBuilder<> &IRB,
    Function &F, AllocaInst *DynamicTop,
    MutableArrayRef<AllocaInst *> DynamicAllocas,
    const std::string &typeId) {
  if (countAllocaForTypeId(DynamicAllocas, typeId) == 0)
    return;

  DIBuilder DIB(*F.getParent());

  for (size_t i = 0; i < DynamicAllocas.size(); i++) {
    AllocaInst *AI = DynamicAllocas[i];
    if (getTypeId(AI) != typeId) continue;

    // Compute the new SP value (after AI).
    Value *ArraySize = AI->getArraySize();
    if (ArraySize->getType() != IntPtrTy)
      ArraySize = IRB.CreateIntCast(ArraySize, IntPtrTy, false);

    IRB.SetInsertPoint(AI);

    Type *Ty = AI->getAllocatedType();
    uint64_t TySize = DL.getTypeAllocSize(Ty);
    Value *Size = IRB.CreateMul(ArraySize, ConstantInt::get(IntPtrTy, TySize));

    Value *UnsafeStackPtr = getUnsafeStackPtr(IRB, F, typeId);
    Value *SP = IRB.CreatePtrToInt(IRB.CreateLoad(StackPtrTy, UnsafeStackPtr),
                                   IntPtrTy);
    SP = IRB.CreateSub(SP, Size);

    // Align the SP value to satisfy the AllocaInst, type and stack alignments.
    unsigned Align = std::max(
        std::max((unsigned)DL.getPrefTypeAlignment(Ty), AI->getAlignment()),
        (unsigned)StackAlignment);

    assert(isPowerOf2_32(Align));
    Value *NewTop = IRB.CreateIntToPtr(
      IRB.CreateAnd(SP, ConstantInt::get(IntPtrTy, ~uint64_t(Align - 1))),
      StackPtrTy);

    // Save the stack pointer.
    IRB.CreateStore(NewTop, UnsafeStackPtr);
    if (DynamicTop)
      IRB.CreateStore(NewTop, DynamicTop);

    Value *NewAI = IRB.CreatePointerCast(NewTop, AI->getType());
    if (AI->hasName() && isa<Instruction>(NewAI))
      NewAI->takeName(AI);

    replaceDbgDeclareForAlloca(AI, NewAI, DIB, DIExpression::ApplyOffset, 0);
    AI->replaceAllUsesWith(NewAI);
    AI->eraseFromParent();
    DynamicAllocas[i] = nullptr; // clean up after erase
  }

  if (!DynamicAllocas.empty()) {
    // Now go through the instructions again, replacing stacksave/stackrestore.
    for (inst_iterator It = inst_begin(&F), Ie = inst_end(&F); It != Ie;) {
      Instruction *I = &*(It++);
      auto II = dyn_cast<IntrinsicInst>(I);
      if (!II)
        continue;

      if (II->getIntrinsicID() == Intrinsic::stacksave) {
        IRBuilder<> IRB(II);
        Value *UnsafeStackPtr = getUnsafeStackPtr(IRB, F, typeId);
        Instruction *LI = IRB.CreateLoad(StackPtrTy, UnsafeStackPtr);
        LI->takeName(II);
        II->replaceAllUsesWith(LI);
        II->eraseFromParent();
      } else if (II->getIntrinsicID() == Intrinsic::stackrestore) {
        IRBuilder<> IRB(II);
        Value *UnsafeStackPtr = getUnsafeStackPtr(IRB, F, typeId);
        Instruction *SI = IRB.CreateStore(II->getArgOperand(0), UnsafeStackPtr);
        SI->takeName(II);
        assert(II->use_empty());
        II->eraseFromParent();
      }
    }
  }
}

bool ignoreFunction(Function &F) {
  if (F.getName() == "__typesafestack_init") return true;
  if (F.getName() == "unsafe_stack_alloc") return true;
  if (F.getName() == "unsafe_stack_setup") return true;
  return false;
}

static void replaceUsesInConstGEP(User *replaceWhat, User *replaceWith, ConstantExpr *ceOld) {
  if (ceOld->getOperand(0) != replaceWhat) {
    errs() << "TypeSafeStack: error: unexpected use of temporary stack pointer array in constant GEP index: ";
    ceOld->dump();
    return;
  }

  std::vector<Constant *> gepIndices;
  for (unsigned i = 1; i < ceOld->getNumOperands(); i++) {
    gepIndices.push_back(cast<Constant>(ceOld->getOperand(i)));
  }
  Constant *replaceWithConst = cast<Constant>(replaceWith);
  Constant *ceNew = ConstantExpr::getInBoundsGetElementPtr(nullptr, replaceWithConst, gepIndices);
  ceOld->replaceAllUsesWith(ceNew);
}

static void replaceUsesInConstBitcast(User *replaceWith, ConstantExpr *ceOld) {
  Constant *replaceWithConst = cast<Constant>(replaceWith);
  Constant *ceNew = ConstantExpr::getBitCast(replaceWithConst, ceOld->getType());
  ceOld->replaceAllUsesWith(ceNew);
}

static void replaceUsesInConstIntToPtr(User *replaceWith, ConstantExpr *ceOld) {
  Constant *replaceWithConst = cast<Constant>(replaceWith);
  Constant *ceNew = ConstantExpr::getIntToPtr(replaceWithConst, ceOld->getType());
  ceOld->replaceAllUsesWith(ceNew);
}

static void replaceUsesInConstPtrToInt(User *replaceWith, ConstantExpr *ceOld) {
  Constant *replaceWithConst = cast<Constant>(replaceWith);
  Constant *ceNew = ConstantExpr::getPtrToInt(replaceWithConst, ceOld->getType());
  ceOld->replaceAllUsesWith(ceNew);
}

static void replaceUsesInConst(User *replaceWhat, User *replaceWith, ConstantExpr *ceOld) {
  switch (ceOld->getOpcode()) {
  case Instruction::BitCast:
    replaceUsesInConstBitcast(replaceWith, ceOld); break;
  case Instruction::GetElementPtr:
    replaceUsesInConstGEP(replaceWhat, replaceWith, ceOld); break;
  case Instruction::IntToPtr:
    replaceUsesInConstIntToPtr(replaceWith, ceOld); break;
  case Instruction::PtrToInt:
    replaceUsesInConstPtrToInt(replaceWith, ceOld); break;
  default:
    errs() << "TypeSafeStack: error: unexpected constant using temporary stack pointer array: ";
    ceOld->dump();
  }
}

static void replaceUsesInGEP(User *replaceWhat, User *replaceWith, GetElementPtrInst *gepOld) {
  if (gepOld->getPointerOperand() != replaceWhat) {
    errs() << "TypeSafeStack: error: unexpected use of temporary stack pointer array in GEP index: ";
    gepOld->dump();
    return;
  }

  std::vector<Value *> gepIndices;
  for (auto gepIndex = gepOld->idx_begin(); gepIndex != gepOld->idx_end(); ++gepIndex) {
    gepIndices.push_back(*gepIndex);
  }

  std::vector<Use *> uses;
  for (auto &use : gepOld->uses()) uses.push_back(&use);
  for (auto *use : uses) {
    User *userUser = use->getUser();
    Instruction *insertBefore = dyn_cast<Instruction>(userUser);
    if (!insertBefore) {
      errs() << "TypeSafeStack: error: temporary stack pointer array GEP used by non-instruction: ";
      userUser->dump();
      continue;
    }
    GetElementPtrInst *gepNew = GetElementPtrInst::CreateInBounds(replaceWith, gepIndices, "", insertBefore);
    use->set(gepNew);
  }
  gepOld->eraseFromParent();
}

bool TypeSafeStack::run() {
  stats.func++;

  //assert(F.hasFnAttribute(Attribute::TypeSafeStack) &&
  //       "Can't run TypeSafeStack on a function without the attribute");
  assert(!F.isDeclaration() && "Can't run TypeSafeStack on a function declaration");

  if (ignoreFunction(F)) {
    DEBUG(dbgs() << "[TypeSafeStack]     typesafestack is not requested"
                    " for function " << F.getName() << "\n");
    return false;
  }

  ++NumFunctions;

  SmallVector<AllocaInst *, 16> StaticAllocas;
  SmallVector<AllocaInst *, 4> DynamicAllocas;
  SmallVector<Argument *, 4> ByValArguments;
  SmallVector<ReturnInst *, 4> Returns;

  // Collect all points where stack gets unwound and needs to be restored
  // This is only necessary because the runtime (setjmp and unwind code) is
  // not aware of the unsafe stack and won't unwind/restore it properly.
  // To work around this problem without changing the runtime, we insert
  // instrumentation to restore the unsafe stack pointer when necessary.
  SmallVector<Instruction *, 4> StackRestorePoints;

  // Find all static and dynamic alloca instructions that must be moved to the
  // unsafe stack, all return instructions and stack restore points.
  findInsts(F, StaticAllocas, DynamicAllocas, ByValArguments, Returns,
            StackRestorePoints);

  if (StaticAllocas.empty() && DynamicAllocas.empty() &&
      ByValArguments.empty() && StackRestorePoints.empty()) {
    stats.func_nothing++;
    return false; // Nothing to do in this function.
  }

  if (!StaticAllocas.empty() || !DynamicAllocas.empty() ||
      !ByValArguments.empty())
    ++NumUnsafeStackFunctions; // This function has the unsafe stack.

  if (!StackRestorePoints.empty())
    ++NumUnsafeStackRestorePointsFunctions;

  std::set<std::string> typeIds;
  getTypeIds<AllocaInst>(typeIds, StaticAllocas);
  getTypeIds<AllocaInst>(typeIds, DynamicAllocas);
  getTypeIds<Argument>(typeIds, ByValArguments);

  IRBuilder<> IRB(&F.front(), F.begin()->getFirstInsertionPt());
  // Calls must always have a debug location, or else inlining breaks. So
  // we explicitly set a artificial debug location here.
  if (DISubprogram *SP = F.getSubprogram())
    IRB.SetCurrentDebugLocation(DebugLoc::get(SP->getScopeLine(), 0, SP));

  // Load the current stack pointer (we'll also use it as a base pointer).
  // FIXME: use a dedicated register for it ?
  std::map<std::string, Instruction *> BasePointers;
  for (std::string typeId : typeIds) {
    Value *UnsafeStackPtr = getUnsafeStackPtr(IRB, F, typeId);
    BasePointers[typeId] =
      IRB.CreateLoad(StackPtrTy, UnsafeStackPtr, false, "unsafe_stack_ptr_" + typeId);
    assert(BasePointers[typeId]->getType() == StackPtrTy);
  }

  // The top of the unsafe stack after all unsafe static allocas are
  // allocated.
  std::map<std::string, Value *> StaticTops;
  moveStaticAllocasToUnsafeStack(IRB, F, StaticAllocas, ByValArguments,
                                     Returns, BasePointers, StaticTops, typeIds);

  // Safe stack object that stores the current unsafe stack top. It is updated
  // as unsafe dynamic (non-constant-sized) allocas are allocated and freed.
  // This is only needed if we need to restore stack pointer after longjmp
  // or exceptions, and we have dynamic allocations.
  // FIXME: a better alternative might be to store the unsafe stack pointer
  // before setjmp / invoke instructions.
  std::map<std::string, AllocaInst *> DynamicTops;
  createStackRestorePoints(
      IRB, F, StackRestorePoints, StaticTops, DynamicTops, !DynamicAllocas.empty(), typeIds);

  // Handle dynamic allocas.
  moveDynamicAllocasToUnsafeStack(IRB, F, DynamicTops,
                                  DynamicAllocas, typeIds);

  // Restore the unsafe stack pointer before each return.
  for (ReturnInst *RI : Returns) {
    IRB.SetInsertPoint(RI);
    for (auto &typeId : typeIds) {
      Value *UnsafeStackPtr = getUnsafeStackPtr(IRB, F, typeId);
      IRB.CreateStore(BasePointers[typeId], UnsafeStackPtr);
    }
  }

  LLVM_DEBUG(dbgs() << "[TypeSafeStack]     safestack applied\n");
  return true;
}

class TypeSafeStackLegacyPass : public FunctionPass {
  GlobalVariable *stackPointerArray = nullptr;
  std::map<std::string, size_t> typeIndexByTypeId;
  size_t typeIndexNext = 0;

  struct statistics stats;

  void printStatistics(const std::string &modname) {
	dbgs() << "TypeSafeStackStats:\tmodname";
#	define STAT(name) dbgs() << "\t" #name;
#	include "TypeSafeStackStats.h"
#	undef STAT
	dbgs() << "\n";
	dbgs() << "TypeSafeStackStats:\t" << modname;
#	define STAT(name) dbgs() << "\t" << stats.name;
#	include "TypeSafeStackStats.h"
#	undef STAT
	dbgs() << "\n";
  }

public:
  static char ID; // Pass identification, replacement for typeid..

  TypeSafeStackLegacyPass() : FunctionPass(ID) {
    //initializeTypeSafeStackLegacyPassPass(*PassRegistry::getPassRegistry());
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addPreserved<SizeofTypes>();
    AU.addRequired<TargetLibraryInfoWrapperPass>();
    AU.addRequired<AssumptionCacheTracker>();
  }

  GlobalVariable *createStackPtrArray(
    Module &M,
    const std::string &varName,
    size_t count) {

    /* we need an array of count stack pointers */
    PointerType *StackPtrTy = Type::getInt8PtrTy(M.getContext());
    ArrayType *type = ArrayType::get(StackPtrTy, count);

    /* initialize every stack pointer to NULL */
    Constant *initElement = ConstantPointerNull::get(StackPtrTy);
    std::vector<Constant *> initElements;
    for (size_t i = 0; i < count; i++) initElements.push_back(initElement);
    Constant *init = ConstantArray::get(type, initElements);

    /* create the global var */
    GlobalVariable *gv = new GlobalVariable(
        /*Module=*/M,
        /*Type=*/type,
        /*isConstant=*/false,
        /*Linkage=*/GlobalValue::ExternalLinkage,
        /*Initializer=*/init,
        /*Name=*/varName,
        /*InsertBefore=*/nullptr,
        /*ThreadLocalMode=*/GlobalValue::InitialExecTLSModel);
    return gv;
  }

  GlobalVariable *createStackPtrCount(
    Module &M,
    const std::string &varName,
    size_t count) {

    IntegerType *type = IntegerType::get(M.getContext(), 64);
    Constant *init = ConstantInt::get(type, count);
    GlobalVariable *gv = dyn_cast_or_null<GlobalVariable>(M.getNamedValue(varName));
    if (!gv) {
      errs() << "TypeSafeStack: error: no reference to variable " << varName << " found, is the static library linked in?\n";
      return nullptr;
    }
    gv->setInitializer(init);
    gv->setConstant(true);

    return gv;
  }

  void replaceUses(User *replaceWhat, User *replaceWith) {
    /* replaceAllUsesWith does not work in this case because the type of an
     * array changes when its size changes
     */
    std::vector<User *> users;
    for (auto user : replaceWhat->users()) users.push_back(user);
    for (auto user : users) {
      ConstantExpr *ceOld = dyn_cast<ConstantExpr>(user);
      if (ceOld) {
        replaceUsesInConst(replaceWhat, replaceWith, ceOld);
        continue;
      }

      GetElementPtrInst *gepOld = dyn_cast<GetElementPtrInst>(user);
      if (gepOld) {
        replaceUsesInGEP(replaceWhat, replaceWith, gepOld);
        continue;
      }

      errs() << "TypeSafeStack: error: unexpected instruction using temporary stack pointer array: ";
      user->dump();
    }
  }

  bool doInitialization(Module &M) override {
    /* create a temporary stack pointer array as we do not know the size yet */
    stackPointerArray = createStackPtrArray(M, kUnsafeStackPtrVarTemp, 0);

    return false;
  }

  bool doFinalization(Module &M) override {
    /* replace temporary stack pointer array with a properly sized one */
    GlobalVariable *stackPointerArrayFinal = createStackPtrArray(M, kUnsafeStackPtrVarFinal, typeIndexNext);
    replaceUses(stackPointerArray, stackPointerArrayFinal);
    stackPointerArray->eraseFromParent();
    stackPointerArray = nullptr;

    /* replace static library stack pointer array with the newly allocated one */
    GlobalVariable *stackPointerArrayStatic = dyn_cast_or_null<GlobalVariable>(M.getNamedValue(kUnsafeStackPtrVar));
    if (stackPointerArrayStatic) {
      replaceUses(stackPointerArrayStatic, stackPointerArrayFinal);
      stackPointerArrayStatic->eraseFromParent();
    } else {
      errs() << "TypeSafeStack: error: no reference to variable " << kUnsafeStackPtrVar << " found, is the static library linked in?\n";
    }

    /* provide stack pointer count to static library */
    createStackPtrCount(M, kUnsafeStackPtrCountVar, typeIndexNext);

    printStatistics(M.getName());
    return false;
  }

  bool runOnFunction(Function &F) override {
    LLVM_DEBUG(dbgs() << "[TypeSafeStack] Function: " << F.getName() << "\n");

    //if (!F.hasFnAttribute(Attribute::TypeSafeStack)) {
    //  LLVM_DEBUG(dbgs() << "[TypeSafeStack]     safestack is not requested"
    //                       " for this function\n");
    //  return false;
    //}

    if (F.isDeclaration()) {
      LLVM_DEBUG(dbgs() << "[TypeSafeStack]     function definition"
                           " is not available\n");
      return false;
    }

    auto *DL = &F.getParent()->getDataLayout();
    auto &TLI = getAnalysis<TargetLibraryInfoWrapperPass>().getTLI();
    auto &ACT = getAnalysis<AssumptionCacheTracker>().getAssumptionCache(F);

    // Compute DT and LI only for functions that have the attribute.
    // This is only useful because the legacy pass manager doesn't let us
    // compute analyzes lazily.
    // In the backend pipeline, nothing preserves DT before TypeSafeStack, so we
    // would otherwise always compute it wastefully, even if there is no
    // function with the safestack attribute.
    DominatorTree DT(F);
    LoopInfo LI(DT);

    ScalarEvolution SE(F, TLI, ACT, DT, LI);

    return TypeSafeStack(F, *DL, SE, stackPointerArray, typeIndexByTypeId, typeIndexNext, stats).run();
  }
};

} // end anonymous namespace

char TypeSafeStackLegacyPass::ID = 0;
static RegisterPass<TypeSafeStackLegacyPass> X("typesafe-stack",
		                         "TypeSafe Stack instrumentation pass", false, false);

