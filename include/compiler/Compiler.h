//
// Created by matthew on 22/12/2020.
//

#ifndef HELP2_COMPILER_H
#define HELP2_COMPILER_H

#include <optional>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>

#include "../parser/ParseTree.h"
#include "CodeGenerator.h"

class Compiler;

struct TypeInfo {
    llvm::Type *llvmType;
    bool tagged;
};

class CompileContext {
    friend class Compiler;
public:
    CompileContext(ParseTree &tree);

    constexpr const std::unique_ptr<llvm::LLVMContext> &context() const { return ctxContext; }

    constexpr const std::unique_ptr<llvm::IRBuilder<>> &builder() const { return ctxBuilder; }

    const std::unique_ptr<llvm::Module> &module(const std::string &name) const { return ctxModules.at(name); }

    void setCurrentModule(const std::string &name);

    constexpr const std::unique_ptr<llvm::Module> &currentModule() const { return *ctxCurrentModule; }

    constexpr const ParseTree &parseTree() const { return tree; }

    void addInstantiatedType(const std::string &name, llvm::Type *type);

    void addConstructorType(const std::string &name, llvm::Type *type, bool tagged);

    llvm::Type *lookupType(const std::string &name) const;

    std::optional<TypeInfo> lookupConstructor(const std::string &name) const;

    // Helpers
    llvm::IntegerType *int8Type() const;

    llvm::IntegerType *int16Type() const;

    llvm::IntegerType *int32Type() const;

    llvm::IntegerType *int64Type() const;

    llvm::FunctionType *intBinaryOpType() const;

    llvm::ConstantInt *intValue(long long val) const;

private:
    std::unique_ptr<llvm::LLVMContext> ctxContext;
    std::unique_ptr<llvm::IRBuilder<>> ctxBuilder;
    std::unordered_map<std::string, std::unique_ptr<llvm::Module>> ctxModules;
    const std::unique_ptr<llvm::Module> *ctxCurrentModule;

    ParseTree &tree;

    std::unordered_map<std::string, llvm::Type *> instantiatedTypes;
    std::unordered_map<std::string, TypeInfo> instantiatedConstructors;
};

/*
 * Compiler Class for converting an AST into LLVM IR Code
 * Here, we assume that the tree has already been type checked so
 * we just have to generate code for the various functions
 */
class Compiler {
public:
    explicit Compiler(ParseTree &tree);

    void compile();

//private:
    CompileContext context;
};

#endif //HELP2_COMPILER_H
