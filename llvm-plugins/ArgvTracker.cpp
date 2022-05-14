/*
 * ArgvTracker.cpp
 *
 *  Created on: Jun 4, 2016
 *      Author: Erik van der Kouwe
 */

#include <llvm/Pass.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
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

#include <string>
#include <list>
#include <set>
#include <vector>

#include <Utils.h>

#define DEBUG_TYPE "ArgvTracker"

#undef DEBUG
#define DEBUG(x) x

using namespace llvm;

struct ArgvTracker : public FunctionPass {
    static char ID;

    ArgvTracker() : FunctionPass(ID) { }

    bool runOnFunction(Function &F) override {

	/* we only do main */
	if (F.getName() != "main") return false;
	DEBUG(errs() << "ArgvTracker: found main");

	/* prepare some types and constants */
	Module *mod = F.getParent();
	PointerType* charPtrType = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
	PointerType* charPtrPtrType = PointerType::get(charPtrType, 0);
	PointerType* charPtrPtrPtrType = PointerType::get(charPtrPtrType, 0);
	ConstantPointerNull* charPtrPtrPtrNull = ConstantPointerNull::get(charPtrPtrPtrType);

	/* create a reference to argvcopy */
	Function* func_argvcopy = mod->getFunction("argvcopy");
	if (!func_argvcopy) {
		std::vector<Type*> func_argvcopy_type_args;
		func_argvcopy_type_args.push_back(charPtrPtrPtrType);
		func_argvcopy_type_args.push_back(charPtrPtrPtrType);
		FunctionType* func_argvcopy_type = FunctionType::get(
			/*Result=*/Type::getVoidTy(mod->getContext()),
			/*Params=*/func_argvcopy_type_args,
			/*isVarArg=*/false);
		func_argvcopy = Function::Create(
			/*Type=*/func_argvcopy_type,
			/*Linkage=*/GlobalValue::ExternalLinkage,
			/*Name=*/"argvcopy",
			mod);
	}

	/* fetch the argc  */
	Function::arg_iterator args = F.arg_begin();
	Value* arg_argc = (args == F.arg_end()) ? (Value *) NULL : &*(args++);
	Value* arg_argv = (args == F.arg_end()) ? (Value *) NULL : &*(args++);
	Value* arg_envp = (args == F.arg_end()) ? (Value *) NULL : &*(args++);
	DEBUG(errs() << "ArgvTracker: " << (arg_argc ? "found" : "did not find") << " argc");
	DEBUG(errs() << "ArgvTracker: " << (arg_argv ? "found" : "did not find") << " argv");
	DEBUG(errs() << "ArgvTracker: " << (arg_envp ? "found" : "did not find") << " envp");
	if (arg_argc && !arg_argc->getType()->isIntegerTy()) {
		errs() << "warning: ArgvTracker: incorrect argc type for function main\n";
	}
	if (arg_argv && !arg_argc->getType()->isIntegerTy()) {
		errs() << "warning: ArgvTracker: incorrect argv type for function main\n";
	}
	if (arg_envp && !arg_argc->getType()->isIntegerTy()) {
		errs() << "warning: ArgvTracker: incorrect envp type for function main\n";
	}

	/* insert at the start of the function */
	BasicBlock &firstBB = F.getEntryBlock();
	IRBuilder<> Builder(&firstBB, firstBB.getFirstInsertionPt());

	/* create a local for argv to be able to pass a pointer to it */
	AllocaInst* arg_argv_addr = NULL;
	Value *arg_argv_value;
	if (arg_argv) {
		arg_argv_addr = Builder.CreateAlloca(charPtrPtrType);
		arg_argv_value = arg_argv_addr;
	} else {
		arg_argv_value = charPtrPtrPtrNull;
	}

	/* create a local for enpv to be able to pass a pointer to it */
	AllocaInst* arg_envp_addr = NULL;
	Value *arg_envp_value;
	if (arg_envp) {
		arg_envp_addr = Builder.CreateAlloca(charPtrPtrType);
		arg_envp_value = arg_envp_addr;
	} else {
		arg_envp_value = charPtrPtrPtrNull;
	}

	/* call argvcopy */
	std::vector<Value*> func_argvcopy_params;
	func_argvcopy_params.push_back(arg_argv_value);
	func_argvcopy_params.push_back(arg_envp_value);
	CallInst *call_inst = Builder.CreateCall(func_argvcopy, func_argvcopy_params);

	/* replace uses of argv and envp with the new local */
	if (arg_argv_addr) {
		LoadInst *arg_argv_load = Builder.CreateLoad(arg_argv_addr);
		arg_argv->replaceAllUsesWith(arg_argv_load);
		new StoreInst(arg_argv, arg_argv_addr, call_inst);
	}
	if (arg_envp_addr) {
		LoadInst *arg_envp_load = Builder.CreateLoad(arg_envp_addr);
		arg_envp->replaceAllUsesWith(arg_envp_load);
		new StoreInst(arg_envp, arg_envp_addr, call_inst);
	}

        return false;
    }

};

char ArgvTracker::ID = 0;
static RegisterPass<ArgvTracker> X("argvtracker", "Argv Tracker Pass", true, false);




