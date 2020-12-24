//
// Created by matthew on 22/12/2020.
//

#include "../../include/compiler/Compiler.h"

CompileContext::CompileContext()
        : ctxContext(std::make_unique<llvm::LLVMContext>()),
          ctxBuilder(std::make_unique<llvm::IRBuilder<>>(*ctxContext)) {

}

Compiler::Compiler(std::unique_ptr<ParseTree> &&tree, const std::string &moduleName)
        : tree(std::move(tree)) {

}

void Compiler::compile() {
    for (const std::pair<const std::string, std::unique_ptr<TypeDeclASTNode>> &type : tree->types()) {

    }

    std::vector<std::unique_ptr<llvm::Function>> compiledFunctions;

    for (const std::pair<const std::string, std::unique_ptr<FunctionDefinitionASTNode>> &func : tree->functions()) {
        // Skip over polymorphic functions - they will be instantiated on demand from concrete functions
        // Note that it is impossible to ever "add" polymorphism in a function, i.e. a concrete function
        // can never become polymorphic.
        if (func.second->isPolymorphic()) {
            continue;
        }
        std::vector<std::unique_ptr<llvm::Function>> quota = func.second->generate(context);
        compiledFunctions.reserve(compiledFunctions.size() + quota.size());
        std::move(std::begin(quota), std::end(quota), std::back_inserter(compiledFunctions));
    }
}
