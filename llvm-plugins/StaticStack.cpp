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
#include <llvm/IR/GlobalValue.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/CodeGen/TargetLowering.h>
#include <llvm/CodeGen/TargetSubtargetInfo.h>
#include <llvm/Transforms/Utils/Local.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>
#include <llvm/Analysis/ScalarEvolutionExpressions.h>

#include <list>
#include <map>
#include <set>
#include <stack>
#include <string>
#include <vector>

#include <sys/time.h>

#include <Utils.h>
//#include <metadata.h>

#include "SizeofTypes.h"

#define DEBUG_TYPE "StaticStack"

using namespace llvm;

cl::opt<bool> OptSingleThread ("staticstack-singlethread", cl::desc("Use global vars instead of TLS"), cl::init(false));
cl::opt<bool> OptAlign ("staticstack-align", cl::desc("Align on 64-byte boundaries"), cl::init(false));
cl::opt<bool> OptTrapRecurse ("staticstack-traprecurse", cl::desc("Trap on unexpected recursion"), cl::init(false));

struct StaticStack : public ModulePass {
	static char ID;

	StaticStack() : ModulePass(ID) { }

	void getAnalysisUsage(AnalysisUsage &AU) const override {
		AU.addPreserved<SizeofTypes>();
	}

	struct FunctionInfo {
		std::string name;
		bool calledIndirect = false;
		bool calledLibrary = false;
		bool calledSignal = false;
		bool callsIndirect = false;
		bool callsLibrary = false;
		bool isLibrary = false;
		std::set<std::string> callees;
		std::set<std::string> callers;
		
		bool isRecursive(void) {
			return callees.count(name) > 0;
		}

		bool canReplaceVars(void) {
			if ((calledIndirect || calledLibrary) &&
				(callsIndirect || callsLibrary)) {
				return false;
			}
			if (isRecursive()) return false;
			return true;
		}
	};
	
	bool functionIsSafe(Function *f) {
		if (f->doesNotReturn()) return true;

		std::string name = f->getName();
		
		/* support functions */
		if (name == "__dynamic_cast") return true;
		if (name == "__errno_location") return true;
		if (name == "__mulsc3") return true;
		if (name == "llvm.bswap.i16") return true;
		if (name == "llvm.bswap.i32") return true;
		if (name == "llvm.bswap.v4i32") return true;
		if (name == "llvm.bswap.v8i16") return true;
		if (name == "llvm.ctlz.i64") return true;
		if (name == "llvm.eh.typeid.for") return true;
		if (name == "llvm.invariant.start") return true;
		/* these are NOT safe, listing them as such crashes perlbench
		if (name == "llvm.lifetime.end") return true;
		if (name == "llvm.lifetime.start") return true;
		*/
		if (name == "llvm.uadd.with.overflow.i64") return true;
		if (name == "llvm.umul.with.overflow.i64") return true;
		if (name == "llvm.va_copy") return true;
		if (name == "llvm.va_end") return true;
		if (name == "llvm.va_start") return true;

		/* string functions */
		if (name == "__ctype_b_loc") return true;
		if (name == "__ctype_get_mb_cur_max") return true;
		if (name == "__ctype_tolower_loc") return true;
		if (name == "__ctype_toupper_loc") return true;
		if (name == "atoi") return true;
		if (name == "isalpha") return true;
		if (name == "isalnum") return true;
		if (name == "isspace") return true;
		if (name == "isswpace") return true;
		if (name == "llvm.memcpy.p0i8.p0i8.i64") return true;
		if (name == "llvm.memmove.p0i8.p0i8.i64") return true;
		if (name == "llvm.memset.p0i8.i64") return true;
		if (name == "memchr") return true;
		if (name == "memcmp") return true;
		if (name == "memcpy") return true;
		if (name == "memmove") return true;
		if (name == "memrchr") return true;
		if (name == "memset") return true;
		if (name == "strcasestr") return true;
		if (name == "strcat") return true;
		if (name == "strchr") return true;
		if (name == "strcmp") return true;
		if (name == "strcpy") return true;
		if (name == "strcspn") return true;
		if (name == "strlen") return true;
		if (name == "strncat") return true;
		if (name == "strncmp") return true;
		if (name == "strncpy") return true;
		if (name == "strpbrk") return true;
		if (name == "strrchr") return true;
		if (name == "strspn") return true;
		if (name == "strstr") return true;
		if (name == "strtof") return true;
		if (name == "strtod") return true;
		if (name == "strtok") return true;
		if (name == "strtol") return true;
		if (name == "strtold") return true;
		if (name == "strtoll") return true;
		if (name == "strtoul") return true;
		if (name == "strtoull") return true;
		if (name == "tolower") return true;
		if (name == "toupper") return true;
		if (name == "towlower") return true;
		if (name == "towupper") return true;

		/* math functions */
		if (name == "acos") return true;
		if (name == "acosh") return true;
		if (name == "asin") return true;
		if (name == "asinh") return true;
		if (name == "atan") return true;
		if (name == "atan2") return true;
		if (name == "atanh") return true;
		if (name == "ceil") return true;
		if (name == "copysign") return true;
		if (name == "cos") return true;
		if (name == "cosh") return true;
		if (name == "div") return true;
		if (name == "erfc") return true;
		if (name == "exp") return true;
		if (name == "exp2") return true;
		if (name == "fabs") return true;
		if (name == "fabsf") return true;
		if (name == "floor") return true;
		if (name == "floorf") return true;
		if (name == "fmod") return true;
		if (name == "frexp") return true;
		if (name == "ldexp") return true;
		if (name == "llvm.fabs.f32") return true;
		if (name == "llvm.fabs.v2f32") return true;
		if (name == "llvm.fabs.v2f64") return true;
		if (name == "llvm.fabs.v4f32") return true;
		if (name == "llvm.powi.f64") return true;
		if (name == "log") return true;
		if (name == "log10") return true;
		if (name == "logb") return true;
		if (name == "modf") return true;
		if (name == "pow") return true;
		if (name == "rand") return true;
		if (name == "sin") return true;
		if (name == "sinh") return true;
		if (name == "sqrt") return true;
		if (name == "sqrtf") return true;
		if (name == "srand") return true;
		if (name == "tan") return true;
		if (name == "tanh") return true;
		
		/* time functions */
		if (name == "asctime") return true;
		if (name == "asctime_r") return true;
		if (name == "clock") return true;
		if (name == "ctime") return true;
		if (name == "ctime_r") return true;
		if (name == "difftime") return true;
		if (name == "gmtime") return true;
		if (name == "gmtime_r") return true;
		if (name == "localtime") return true;
		if (name == "localtime_r") return true;
		if (name == "mktime") return true;
		if (name == "strftime") return true;
		if (name == "time") return true;

		return false;
	}

	std::string valueTypeName(Value *value) {
#define T(t) if (isa<t>(value)) return #t;
		/* children of GlobalObject */
		T(Function)
		T(GlobalVariable)
		/* children of GlobalValue */
		T(GlobalAlias)
		T(GlobalObject)
		/* children of Constant */
		T(BlockAddress)
		T(ConstantAggregateZero)
		T(ConstantArray)
		T(ConstantDataSequential)
		T(ConstantExpr)
		T(ConstantFP)
		T(ConstantInt)
		T(ConstantPointerNull)
		T(ConstantStruct)
		T(ConstantVector)
		T(GlobalValue)
		T(UndefValue)
		/* children of CallBase */
		T(CallBrInst)
		T(CallInst)
		T(InvokeInst)
		/* children of UnaryInstruction */
		T(AllocaInst)
		T(CastInst)
		T(ExtractValueInst)
		T(LoadInst)
		T(VAArgInst)
		/* children of Instruction */
		T(AtomicCmpXchgInst)
		T(AtomicRMWInst)
		T(BinaryOperator)
		T(BranchInst)
		T(CallBase)
		T(CatchReturnInst)
		T(CatchSwitchInst)
		T(CleanupReturnInst)
		T(CmpInst)
		T(ExtractElementInst)
		T(FenceInst)
		T(GetElementPtrInst)
		T(IndirectBrInst)
		T(InsertElementInst)
		T(InsertValueInst)
		T(LandingPadInst)
		T(PHINode)
		T(ResumeInst)
		T(ReturnInst)
		T(SelectInst)
		T(ShuffleVectorInst)
		T(StoreInst)
		T(SwitchInst)
		T(UnaryInstruction)
		T(UnreachableInst)
		/* children of User */
		T(Constant)
		T(Instruction)
		T(Operator)
		/* children of Value */
		T(Argument)
		T(BasicBlock)
		T(InlineAsm)
		T(MetadataAsValue)
		T(User)
#undef T
		return "Value";
	}

	FunctionInfo *buildFunctionInfo(Function &f) {
		FunctionInfo *fi = new FunctionInfo();
		fi->name = f.getName();
		fi->callsLibrary = fi->isLibrary = f.empty();

		for (auto &bb : f) {
			for (auto &i : bb) {
				CallInst *ci = dyn_cast<CallInst>(&i);
				InvokeInst *ii = dyn_cast<InvokeInst>(&i);
				Function *cf;
				if (ci) {
					cf = ci->getCalledFunction();
				} else if (ii) {
					cf = ii->getCalledFunction();
				} else {
					continue;
				}

				if (cf) {
					if (functionIsSafe(cf)) continue;
					fi->callees.insert(cf->getName());
				} else {
					fi->callsIndirect = true;
				}
			}
		}

		return fi;
	}
	
	void insertAll(std::set<std::string> &into, const std::set<std::string> &from) {
		if (&into == &from) return;
		for (auto value : from) {
			into.insert(value);
		}
	}
	
	void transitiveClosure(std::map<std::string, FunctionInfo *> &functions) {
		/* loop through calling functions */
		for (auto &pair : functions) {
			FunctionInfo *caller = pair.second;
			std::set<std::string> seen;
			std::stack<std::string> todo;

			/* for this caller, perform a depth-first search of callees) */
			seen.insert(pair.first);
			todo.push(pair.first);
			while (!todo.empty()) {
				/* add callee's callees to caller's callees */
				std::string calleeName = todo.top();
				todo.pop();
				FunctionInfo *callee = functions[calleeName];
				insertAll(caller->callees, callee->callees);

				/* we'll need to search the callee's callees' callees too */
				for (auto &calleeCalleeName : callee->callees) {
					if (!seen.count(calleeCalleeName)) {
						seen.insert(calleeCalleeName);
						todo.push(calleeCalleeName);
					}
				}
			}
		}
	}

	void initializeCallers(std::map<std::string, FunctionInfo *> &functions) {
		FunctionInfo *fi;

		for (auto &pair : functions) {
			fi = pair.second;
			for (auto &name : fi->callees) {
				FunctionInfo *callee = functions[name];
				callee->callers.insert(pair.first);
			}
		}
	}

	void propagateIndirect(std::map<std::string, FunctionInfo *> &functions) {
		FunctionInfo *fi;

		/* note: we assume transitive closure has already been determined */
		for (auto &pair : functions) {
			fi = pair.second;
			for (auto &name : fi->callees) {

				FunctionInfo *callee = functions[name];
				if (fi->calledIndirect) callee->calledIndirect = true;
				if (fi->calledLibrary) callee->calledLibrary = true;
				if (fi->calledSignal) callee->calledSignal = true;
				if (callee->callsIndirect) fi->callsIndirect = true;
				if (callee->callsLibrary) fi->callsLibrary = true;
			}
		}
	}
	
	void dumpCFG(std::map<std::string, FunctionInfo *> &functions, const char *desc) {
		FunctionInfo *fi;
		int countAll = 0;
		int countCalledIndirect = 0;
		int countCalledLibrary = 0;
		int countCalledSignal = 0;
		int countCallsIndirect = 0;
		int countCallsLibrary = 0;
		int countIsLibrary = 0;
		int countIsRecursive = 0;
		int countAcceptable = 0;
		bool isRecursive;

		for (auto &pair : functions) {
			fi = pair.second;
			isRecursive = fi->isRecursive();
			dbgs() << "StaticStack: " << desc << ":"
				<< " name=" << pair.first
				<< " calledIndirect=" << fi->calledIndirect
				<< " calledLibrary=" << fi->calledLibrary
				<< " calledSignal=" << fi->calledSignal
				<< " callsIndirect=" << fi->callsIndirect
				<< " callsLibrary=" << fi->callsLibrary
				<< " isLibrary=" << fi->isLibrary
				<< " isRecursive=" << isRecursive
				<< " callees=" << fi->callees.size()
				<< " callers=" << fi->callers.size()
				<< "\n";
			countAll++;
			if (fi->calledIndirect) countCalledIndirect++;
			if (fi->calledLibrary) countCalledLibrary++;
			if (fi->calledSignal) countCalledSignal++;
			if (fi->callsIndirect) countCallsIndirect++;
			if (fi->callsLibrary) countCallsLibrary++;
			if (fi->isLibrary) countIsLibrary++;
			if (isRecursive) countIsRecursive++;
			if (fi->canReplaceVars()) countAcceptable++;
		}
		dbgs() << "StaticStack: " << desc << ":"
			<< " totals "
			<< " countAll=" << countAll
			<< " countCalledIndirect=" << countCalledIndirect
			<< " countCalledLibrary=" << countCalledLibrary
			<< " countCalledSignal=" << countCalledSignal
			<< " countCallsIndirect=" << countCallsIndirect
			<< " countCallsLibrary=" << countCallsLibrary
			<< " countRecursive=" << countIsRecursive
			<< " countAcceptable=" << countAcceptable
			<< "\n";
	}

	char *itoa(int value, char *buffer, int radix) {
		int digits = 0;
		char *p = buffer;
		int valuerem = value;

		if (value < 0) {
			value = -value;
			*(p++) = '-';
		}

		do {
			digits++;
			valuerem /= radix;
		} while (valuerem > 0);

		valuerem = value;
		p[digits] = 0;
		do {
			p[--digits] = "0123456789abcdefghijklmnopqrstuvwxyz"[valuerem % radix];
			valuerem /= radix;
		} while (digits > 0);

		return buffer;
	}
	
	GlobalVariable *createGlobalVar(
		Module *m,
		std::string &name,
		Type *type) {
		GlobalVariable::ThreadLocalMode tls = OptSingleThread ?
			GlobalVariable::ThreadLocalMode::NotThreadLocal :
			GlobalVariable::ThreadLocalMode::LocalExecTLSModel;
		GlobalVariable *gv = new GlobalVariable(
			*m,
			type,
			false, /* isConstant */
			GlobalVariable::LinkageTypes::InternalLinkage,
			Constant::getNullValue(type),
			name,
			nullptr, /* InsertBefore */
			tls);
		return gv;
	}
	
	bool runOnAllocaInst(AllocaInst *ai, int index) {
		if (!ai->isStaticAlloca()) return false;
		if (ai->isArrayAllocation()) return false; /* TODO we can probably handle this case, likely speeding up perlbench and milc with typesafestack */

		char indexstr[16];
		Function *f = ai->getParent()->getParent();
		std::string name = "localvar_";
		name += f->getName();
		name += "_";
		name += ai->getName();
		name += "_";
		name += itoa(index, indexstr, 10);

		Module *m = f->getParent();
		GlobalVariable *gv = createGlobalVar(m, name, ai->getAllocatedType());
		if (index == 0 && OptAlign) gv->setAlignment(64);
		dbgs() << "StaticStack: module " << f->getParent()->getName()
			<< " function " << f->getName()
			<< " var " << ai->getName()
			<< " replaced with TLS " << gv->getName()
			<< "\n";
		ai->replaceAllUsesWith(gv);
		return true;
	}
	
	bool runOnCallInst(CallInst *ci, int index) {
		
		if (!OptTrapRecurse) return false;
		
		/* build recursion check variable */
		char indexstr[16];
		Function *f = ci->getParent()->getParent();
		std::string name = "recursecheck_";
		name += f->getName();
		name += "_";
		name += itoa(index, indexstr, 10);
		Module *m = f->getParent();
		Type *type = Type::getInt1Ty(m->getContext());
		GlobalVariable *gv = createGlobalVar(m, name, type);

		/* split the basic block and add an error case */
		BasicBlock *bbStart = ci->getParent();
		BasicBlock *bbCall = bbStart->splitBasicBlock(ci, bbStart->getName() + "_recurseCheck");
		BasicBlock *bbError = BasicBlock::Create(m->getContext(), bbStart->getName() + "_recurseError", f);
		bbStart->getTerminator()->eraseFromParent();

		/* add a check in bbStart to detect recursion */
		Constant *valueZero = ConstantInt::getFalse(type);
		Constant *valueOne = ConstantInt::getTrue(type);
		LoadInst *load = new LoadInst(gv, "", bbStart);
		CmpInst *cmp = CmpInst::Create(
			Instruction::OtherOps::ICmp,
			ICmpInst::ICMP_EQ,
			load,
			valueZero,
			"",
			bbStart);
		BranchInst::Create(
			bbCall,
			bbError,
			cmp,
			bbStart);

		/* set and unset the variable in bbCall to know when a call is recursive */
		(new StoreInst(valueOne, gv))->insertBefore(ci);
		(new StoreInst(valueZero, gv))->insertAfter(ci);

		/* add a trap in bbError to let the user find out about unexpected recursion */
		new StoreInst(valueZero, gv, "", bbError);
		Function *functionTrap = Intrinsic::getDeclaration(m, Intrinsic::trap);
		CallInst *callTrap = CallInst::Create(functionTrap, "", bbError);
		callTrap->setDoesNotReturn();
		new UnreachableInst(m->getContext(), bbError);
		
		return true;
	}

	bool runOnFunction(Function &f, FunctionInfo *fi) {
		int indexAlloca = 0;
		int indexCall = 0;

		/* ignore possibly recursive functions */
		if (!fi) {
			errs() << "StaticStack: no FunctionInfo for function " << f.getName() << "\n";
			return false;
		}
		if (!fi->canReplaceVars()) return false;

		bool changed = false;
		SmallVector<Instruction *, 16> instructions;
		for (auto &bb : f) {
			for (auto &i : bb) {
				instructions.push_back(&i);
			}
		}
		for (auto i : instructions) {
			/* move stack allocations to TLS */
			AllocaInst *ai = dyn_cast<AllocaInst>(i);
			if (ai && runOnAllocaInst(ai, indexAlloca++)) {
				changed = true;
			}

			/* debug option: check non-recursivity of calls */
			CallInst *ci = dyn_cast<CallInst>(i);
			if (ci && runOnCallInst(ci, indexCall++)) {
				changed = true;
			}
		}
		return changed;
	}
	
	bool traceFunctionPointerStore(
		FunctionInfo *fi,
		std::map<std::string, FunctionInfo *> &functions,
		StoreInst *si,
		TodoList<Value> &todouses) {
		TodoList<Value> todo;
		Value *value;

		/* XXX we should only consider uses that may come after
		 * the store, the current approach is overly conservative
		 */

		/* unwrap the pointer stored to */
		todo.add(si->getPointerOperand());
		while ((value = todo.get())) {
			/* we're looking for variables; once we find the
			 * original value we need to consider its uses to find
			 * out how the stored function pointer is used
			 */
			AllocaInst *ai = dyn_cast<AllocaInst>(value);
			if (ai) {
				dbgs() << "CfgDbg store AllocaInst\n";
				todouses.add(ai);
				continue;
			}

			GlobalVariable *gv = dyn_cast<GlobalVariable>(value);
			if (gv && gv->hasInitializer()) {
				dbgs() << "CfgDbg store GlobalVariable\n";
				todouses.add(gv);
				continue;
			}

			/* we're not worried about any NULL pointers or integer
			 * constants that may be in the address computation
			 */
			if (isa<ConstantInt>(value)) continue;
			if (isa<ConstantPointerNull>(value)) continue;

			/* these instructions can be used to compute a pointer;
			 * we want to find out where the pointer comes
			 * from originally
			 */
			BinaryOperator *bo = dyn_cast<BinaryOperator>(value);
			if (bo) {
				todo.add(bo->getOperand(0));
				todo.add(bo->getOperand(1));
				continue;
			}

			CastInst *csti = dyn_cast<CastInst>(value);
			if (csti) {
				todo.add(csti->getOperand(0));
				continue;
			}

			ConstantExpr *ce = dyn_cast<ConstantExpr>(value);
			if (ce) {
				for (Value *operand : ce->operands()) {
					todo.add(operand);
				}
				continue;
			}

			GetElementPtrInst *gepi = dyn_cast<GetElementPtrInst>(value);
			if (gepi) {
				todo.add(gepi->getPointerOperand());
				continue;
			}

			PHINode *phi = dyn_cast<PHINode>(value);
			if (phi) {
				for (Value *invalue : phi->incoming_values()) {
					todo.add(invalue);
				}
				continue;
			}
			
			SelectInst *seli = dyn_cast<SelectInst>(value);
			if (seli) {
				todo.add(seli->getTrueValue());
				todo.add(seli->getFalseValue());
				continue;
			}

			/* in other cases we give up, we cannot trace back
			 * the store location with certainty
			 */
			dbgs() << "CfgDbg store other:" << valueTypeName(value) << "\n";
			return false;
		}

		return true;
	}
	
	Argument *getArgumentByIndex(Function *function, int index) {
		for (Argument &arg : function->args()) {
			if (index-- <= 0) return &arg;
		}
		return nullptr;
	}

	void traceFunctionPointerUse(
		FunctionInfo *fi,
		std::map<std::string, FunctionInfo *> &functions,
		Value *value,
		TodoList<Value> &todo,
		User *user) {

		/* BinaryOperator must be tracked */
		if (isa<BinaryOperator>(user)) {
			todo.add(user);
			return;
		}

		/* calling the function pointer creates an edge in the CFG;
		 * specifying the function pointer as a call argument requires
		 * interprocedural analysis
		 */
		CallInst *ci = dyn_cast<CallInst>(user);
		InvokeInst *ii = dyn_cast<InvokeInst>(user);
		if (ci || ii) {
			Function *caller = (ci ? ci->getParent() : ii->getParent())->getParent();
			Function *calledFunction = ci ? ci->getCalledFunction() : ii->getCalledFunction();
			Value *calledValue = ci ? ci->getCalledValue() : ii->getCalledValue();
			FunctionInfo *ficaller = functions[caller->getName()];
			if (!calledFunction &&
				calledValue == value) {
				ficaller->callees.insert(fi->name);
				dbgs() << "CfgDbg use indircall\n";
			}
			int argIndex = 0;
			for (Value *arg : (ci ? ci->arg_operands() : ii->arg_operands())) {
				if (arg == value) {
					if (calledFunction && !calledFunction->empty()) {
						Argument *arg = getArgumentByIndex(calledFunction, argIndex);
						if (arg) {
							todo.add(arg);
							continue;
						}
					}

					dbgs() << "CfgDbg use callarg\n";

					fi->calledIndirect = true;
					fi->calledLibrary = true;

					/* XXX this is not conservative, we assume sgnal handler registrations are obvious */
					if (calledFunction && (calledFunction->getName() == "signal" || calledFunction->getName() == "sigaction")) {
						fi->calledSignal = true;
					}
				}
				argIndex++;
			}
			return;
		}

		/* compare is safe */
		if (isa<CmpInst>(user)) return;

		/* CastInst must be tracked */
		if (isa<CastInst>(user)) {
			todo.add(user);
			return;
		}

		/* constants must be tracked */
		if (isa<ConstantArray>(user) ||
			isa<ConstantStruct>(user) ||
			isa<ConstantVector>(user)) {
			todo.add(user);
			return;
		}

		/* ConstantExpr may need to be tracked */
		ConstantExpr *ce = dyn_cast<ConstantExpr>(user);
		if (ce) {
			if (!ce->isCompare()) todo.add(user);
			return;
		}

		/* element extraction must be tracked */
		if (isa<ExtractElementInst>(user)) {
			todo.add(user);
			return;
		}

		/* GEP must be tracked */
		if (isa<GetElementPtrInst>(user)) {
			todo.add(user);
			return;
		}

		/* global variables initialized to this must be tracked */
		if (isa<GlobalVariable>(user)) {
			todo.add(user);
			return;
		}

		/* load must be tracked */
		if (isa<LoadInst>(user)) {
			todo.add(user);
			return;
		}

		/* PHINode must be tracked */
		if (isa<PHINode>(user)) {
			todo.add(user);
			return;
		}

		/* returning function pointer from a function requires
		 * interprocedural analysis and is considered unsafe (for now)
		 */
		if (isa<ReturnInst>(user)) {
			dbgs() << "CfgDbg use return\n";
			fi->calledIndirect = true;
			fi->calledLibrary = true;
			/* XXX to be conservative we need to check whether the
			 * return value is registered as a signal handler
			 */
			return;
		}

		/* SelectInst must be tracked */
		SelectInst *seli = dyn_cast<SelectInst>(user);
		if (seli && (seli->getTrueValue() == value || seli->getFalseValue() == value)) {
			todo.add(seli);
			return;
		}

		/* StoreInst requires tracking of the value stored to */
		StoreInst *si = dyn_cast<StoreInst>(user);
		if (si) {
			if (!traceFunctionPointerStore(fi, functions, si, todo)) {
				dbgs() << "CfgDbg use store\n";
				/* if we cannot trace the store location,
				 * we must consider it unsafe
				 */
				fi->calledIndirect = true;
				fi->calledLibrary = true;
				/* XXX to be conservative we need to check
				 * whether the stored value is registered as
				 * a signal handler
				 */
			}
			return;
		}

		/* switch is safe */
		if (isa<SwitchInst>(user)) return;

		/* any other/unexpected uses are considered unsafe */
		std::string parentName = "";
		Instruction *instr = dyn_cast<Instruction>(user);
		if (instr) parentName = instr->getParent()->getParent()->getName();

		dbgs() << "CfgDbg use other:" << valueTypeName(user) << "\n";
		fi->calledIndirect = true;
		fi->calledLibrary = true;
		/* XXX to be conservative we need to check whether the value
		 * is registered as a signal handler
		 */
	}

	void traceFunctionPointerUses(
		FunctionInfo *fi,
		std::map<std::string, FunctionInfo *> &functions,
		Value *value,
		TodoList<Value> &todo) {
		for (const Use &use : value->uses()) {
			traceFunctionPointerUse(fi, functions, value, todo, use.getUser());
		}
	}

	void traceFunctionPointers(
		Function &f,
		std::map<std::string, FunctionInfo *> &functions) {
		FunctionInfo *fi = functions[f.getName()];
		TodoList<Value> todo;
		Value *value;

		todo.add(&f);
		while ((value = todo.get())) {
			traceFunctionPointerUses(fi, functions, value, todo);
		}
	}

	long timeToLong(struct timeval time) {
		return time.tv_sec * 1000000L + time.tv_usec;
	}

	void dumpTimeDiffs(struct timeval *times, const char **timenames, int count) {
		long diff;
		int i;

		dbgs() << "times:";
		for (i = 1; i < count; i++) {
			diff = timeToLong(times[i]) - timeToLong(times[i - 1]);
			dbgs() << " " << timenames[i - 1] << "=" << diff;
		}
		dbgs() << "\n";
	}

	bool runOnModule(Module &m) override {
		bool changed = false;
		std::map<std::string, FunctionInfo *> functions;
		struct timeval times[16];
		const char *timenames[16];
		int timeindex = 0;

#define MEASURETIME(name) { gettimeofday(&times[timeindex], NULL); timenames[timeindex] = name; timeindex++; }

		MEASURETIME("buildFunctionInfo")
		for (auto &f : m) {
			functions[f.getName()] = buildFunctionInfo(f);
		}
		MEASURETIME("traceFunctionPointers")
		for (auto &f : m) {
			traceFunctionPointers(f, functions);
		}
		MEASURETIME("initializeCallers")
		initializeCallers(functions);
		MEASURETIME("dumpCFG-pre-closure")
		dumpCFG(functions, "pre-closure");

		MEASURETIME("transitiveClosure")
		transitiveClosure(functions);
		MEASURETIME("initializeCallers")
		initializeCallers(functions);
		MEASURETIME("propagateIndirect")
		propagateIndirect(functions);
		MEASURETIME("dumpCFG-post-closure")
		dumpCFG(functions, "post-closure");

		MEASURETIME("runOnFunction")
		for (auto &f : m) {
			if (runOnFunction(f, functions[f.getName()])) {
				changed = true;
			}
		}

		MEASURETIME("cleanup")
		for (auto pair : functions) {
			delete pair.second;
		}

		MEASURETIME("")
		dumpTimeDiffs(times, timenames, timeindex);

		return changed;
	}

};

char StaticStack::ID = 0;
static RegisterPass<StaticStack> X("staticstack", "Static Stack Pass", true, false);
