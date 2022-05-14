#include "clang/Driver/Options.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "llvm/ADT/StringMap.h"

#include <algorithm>
#include <sstream>

using namespace clang;
using std::vector;
using std::string;

struct SizeofFinder : public ConstStmtVisitor<SizeofFinder> {
    vector<const UnaryExprOrTypeTraitExpr *> Sizeofs;

    void VisitBinMul(const BinaryOperator *E) {
        Visit(E->getLHS());
        Visit(E->getRHS());
    }

    void VisitBinAdd(const BinaryOperator *E) {
        Visit(E->getLHS());
        Visit(E->getRHS());
    }

    void VisitImplicitCastExpr(const ImplicitCastExpr *E) {
        return Visit(E->getSubExpr());
    }

    void VisitParenExpr(const ParenExpr *E) {
        return Visit(E->getSubExpr());
    }

    void VisitUnaryExprOrTypeTraitExpr(const UnaryExprOrTypeTraitExpr *E) {
        if (E->getKind() != UETT_SizeOf)
            return;
        if (E->getTypeOfArgument()->isVoidType())
            return;
        if (E->getTypeOfArgument()->isPointerType())
            if (E->getTypeOfArgument()->getAs<PointerType>()->getPointeeType()->isVoidType())
                return;

        Sizeofs.push_back(E);
    }
};

class SizeofVisitor : public RecursiveASTVisitor<SizeofVisitor> {
    CompilerInstance &CI;
    Rewriter Rewrite;
    FrontendAction &Action;
    std::shared_ptr<PCHContainerOperations> PCHContainerOps;
    PrintingPolicy PP;

public:
    explicit SizeofVisitor(CompilerInstance &CI, FrontendAction &Action)
      : CI(CI),
        Rewrite(CI.getSourceManager(), CI.getLangOpts()),
        Action(Action),
        PCHContainerOps(CI.getPCHContainerOperations()),
        PP(CI.getLangOpts())
    {
        // Stringify 'bool' instead of '_Bool' (fixes omnetpp)
        PP.Bool = 1;
    }

    virtual ~SizeofVisitor() {}

    void commitChanges(ASTContext &Ctx) {
        FileID FID = Ctx.getSourceManager().getMainFileID();
        assert(FID.isValid());

        // Forward declaration of sizeof dummy function
        llvm::outs() << "#ifdef __cplusplus\n";
        llvm::outs() << "extern \"C\" {\n";
        llvm::outs() << "__attribute__((nothrow))\n";
        llvm::outs() << "#endif\n";
        llvm::outs() << "extern unsigned long __sizeof_arg(void*, unsigned long);\n";
        llvm::outs() << "#ifdef __cplusplus\n";
        llvm::outs() << "}\n";
        llvm::outs() << "#else\n";
        llvm::outs() << "#include <stdbool.h>\n";
        llvm::outs() << "#endif\n";

        // Original (preprocessed) source
        Rewrite.getEditBuffer(FID).write(llvm::outs());
    }

    virtual bool VisitStmt(Stmt *S) {
        // Check if the statement is a function call
        CallExpr *Call = dyn_cast<CallExpr>(S);
        if (!Call || !Call->getDirectCallee())
            return true;

	// TODO: Find a better way to deal with built-ins.
	auto *FDecl = dyn_cast_or_null<FunctionDecl>(Call->getCallee()->getReferencedDeclOfCallee());
	if (FDecl) {
		IdentifierInfo *idinfo = FDecl->getIdentifier();
		if (idinfo) {
			if (idinfo->getName() == "__atomic_is_lock_free" || idinfo->getName() == "__c11_atomic_is_lock_free") {
				llvm::dbgs() << "ignoring built-in\n";
				return true;
			}
		}
	}

        // Look for sizeof in call arguments
        for (unsigned i = 0, n = Call->getNumArgs(); i < n; i++) {
            Expr *Arg = Call->getArg(i);

            // Check if arg is sizeof expression
            if (!Arg->getType()->isIntegralOrUnscopedEnumerationType())
                continue;

            SizeofFinder SFinder;
            SFinder.Visit(Arg);
            if (SFinder.Sizeofs.size() != 1)
                continue;

            // Replace sizeof with call to helper function that will be
            // inserted later
            const UnaryExprOrTypeTraitExpr *Sizeof = SFinder.Sizeofs[0];
            const string fullty = Sizeof->getTypeOfArgument().getAsString(PP);

            // Some C types are anonymous, don't support those for now
            if (fullty.find("(anonymous") != string::npos)
                continue;

            // soplex uses weird types that look like this
            if (fullty.find("<dependent type>") != string::npos)
                continue;

            // php does memcpy of function pointers
            if (fullty.find("(*)") != string::npos)
                continue;

	    // dealII
	    if (fullty == "__gnu_cxx::char_traits::char_type")
		continue;

	    // leela
	    if (fullty == "std::_Base_bitset::_WordT")
		continue;

            // libunwind (thx reviewers): C++ template issues with TAT, alyssa HACK blacklist
            if (fullty == "libunwind::DwarfFDECache::entry")
		continue;
            if (fullty == "libunwind::CFI_Parser::PrologInfoStackEntry")
		continue;

            // same, but with libcxx, alyssa HACK blacklist
            // (these are actually probably relevant for type safety, so this is really a TODO)
            if (fullty == "std::__1::num_put::char_type")
		continue;
            if (fullty == "std::__1::money_put::char_type")
		continue;
            if (fullty == "std::__1::__val_expr::result_type")
		continue;
            if (fullty == "std::__1::valarray::value_type")
		continue;

            size_t bracket = fullty.find('[');
            if (bracket == string::npos)
                bracket = fullty.length();
            const string elty = fullty.substr(0, bracket);

            std::stringstream ss;
            ss << "({ ";
            ss << elty << " *__sizeof_dummy; ";
            ss << "__sizeof_arg((void*)&__sizeof_dummy, ";
            ss << "sizeof (" << fullty << "));";
            ss << " })";
            Rewrite.ReplaceText(Sizeof->getSourceRange(), ss.str());
        }

        return true;
    }
};

class SizeofASTConsumer : public ASTConsumer {
    SizeofVisitor Vis;

public:
    // override the constructor in order to pass CI
    explicit SizeofASTConsumer(CompilerInstance &CI, StringRef File, FrontendAction &Action)
        : Vis(CI, Action) {}

    virtual void HandleTranslationUnit(ASTContext &Ctx) {
        Vis.TraverseDecl(Ctx.getTranslationUnitDecl());
        Vis.commitChanges(Ctx);
    }
};

class SizeofASTAction : public PluginASTAction {
protected:
    std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, StringRef File) {
        return std::unique_ptr<ASTConsumer>(new SizeofASTConsumer(CI, File, *this));
    }

    // Get the instrumented functions from the cmd-line args
    bool ParseArgs(const CompilerInstance &CI, const vector<string> &args) {
        return true;
    }
};

static FrontendPluginRegistry::Add<SizeofASTAction> X("sizeof-types",
        "Replace sizeof in callinst args with equivalent type-preserving snippet");
