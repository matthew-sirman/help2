//
// Created by matthew on 22/12/2020.
//

#ifndef HELP2_COMPILER_H
#define HELP2_COMPILER_H

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>

#include "../parser/ParseTree.h"

class CompileContext {
public:
    CompileContext();

    constexpr const std::unique_ptr<llvm::LLVMContext> &context() const { return ctxContext; }

    constexpr const std::unique_ptr<llvm::IRBuilder<>> &builder() const { return ctxBuilder; }

    const std::unique_ptr<llvm::Module> &module(const std::string &name) const { return ctxModules.at(name); }

private:
    std::unique_ptr<llvm::LLVMContext> ctxContext;
    std::unique_ptr<llvm::IRBuilder<>> ctxBuilder;
    std::unordered_map<std::string, std::unique_ptr<llvm::Module>> ctxModules;
};

/*
 * Compiler Class for converting an AST into LLVM IR Code
 * Here, we assume that the tree has already been type checked so
 * we just have to generate code for the various functions
 */
class Compiler {
public:
    explicit Compiler(std::unique_ptr<ParseTree> &&tree, const std::string &moduleName);

    void compile();

private:
    CompileContext context;

    std::unique_ptr<ParseTree> tree;
};

#endif //HELP2_COMPILER_H
