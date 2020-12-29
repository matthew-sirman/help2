//
// Created by matthew on 24/12/2020.
//

#ifndef HELP2_CODEGENERATOR_H
#define HELP2_CODEGENERATOR_H

#include <iostream>

#include "../parser/ast/Viewable.h"

#include "../../include/parser/ast/ExpressionASTNodes.h"
#include "../../include/parser/ast/FunctionASTNodes.h"
#include "../../include/parser/ast/TypeASTNodes.h"

class CompileContext;
class Compiler;

class TypeCodeGenerator {
    friend class Compiler;
public:
    template<Viewable NodeT>
    llvm::Type *generate(const typename NodeT::View &nodeView);

    static std::string generateUniqueTypeName(const std::string &name, const std::vector<llvm::Type *> &bindings);

protected:
    TypeCodeGenerator(CompileContext &context);

private:
    CompileContext &context;

    static std::hash<std::string> stringHasher;

    void generateConstructorFunction(const DataConstructorASTNode::View &nodeView,
                                     llvm::StructType *consType,
                                     const std::vector<llvm::Type *> &fields);
};

class FunctionCodeGenerator {
    friend class Compiler;

public:
    llvm::BasicBlock *generateImplementationBlock(const FunctionImplASTNode::View &nodeView);

    llvm::Function *generateDefinition(const FunctionDefinitionASTNode::View &nodeView, const BindingMap &bindingMap);

protected:
    FunctionCodeGenerator(CompileContext &context, TypeCodeGenerator &typeCodeGenerator);

private:
    CompileContext &context;
    TypeCodeGenerator &typeCodeGenerator;
};

class ExpressionCodeGenerator {
    friend class Compiler;
    friend class FunctionCodeGenerator;

public:
    template<Viewable NodeT>
    llvm::Value *generate(const typename NodeT::View &nodeView);

protected:
    ExpressionCodeGenerator(CompileContext &context, FunctionCodeGenerator &funcCodeGenerator);

    void addParameterRoot(const std::string &parameterName, llvm::Argument *root);

private:
    CompileContext &context;
    FunctionCodeGenerator &funcCodeGenerator;

    std::unordered_map<std::string, llvm::Value *> instantiatedParameters;
    std::unordered_map<std::string, llvm::Argument *> parameterRootMap;
};

template<Viewable NodeT>
llvm::Type *TypeCodeGenerator::generate(const typename NodeT::View &nodeView) {
    std::cerr << "DEVELOPER: No code generator for viewable node type found (" << typeid(NodeT).name() << ")." << std::endl;
    throw;
}

template<Viewable NodeT>
llvm::Value *ExpressionCodeGenerator::generate(const typename NodeT::View &nodeView) {
    std::cerr << "DEVELOPER: No code generator for viewable node type found (" << typeid(NodeT).name() << ")." << std::endl;
    throw;
}

#endif //HELP2_CODEGENERATOR_H
