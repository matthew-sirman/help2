//
// Created by matthew on 24/12/2020.
//

#include "../../include/compiler/CodeGenerator.h"
#include "../../include/compiler/Compiler.h"

#include <filesystem>

#include <llvm/IR/Module.h>

llvm::BasicBlock *
FunctionCodeGenerator::generateImplementationBlock(const FunctionImplASTNode::View &nodeView) {
    llvm::BasicBlock *implBlock = llvm::BasicBlock::Create(
            *context.context(),
            "impl_variant" + std::to_string(nodeView.variant),
            nodeView.parentFunction
    );

    context.builder()->SetInsertPoint(implBlock);

    // Create an expression generator
    ExpressionCodeGenerator expressionGenerator(context, *this);

    // Create a root map for the expression generator so it can identify which function arguments point
    // to each available binder
    for (size_t i = 0; i < nodeView.patterns.size(); i++) {
        BinderMap patternVariables;
        nodeView.patterns[i]->addAllBinders(patternVariables);
        for (const std::pair<const std::string, const VariablePatternASTNode *> &var : patternVariables) {
            expressionGenerator.addParameterRoot(var.first, nodeView.parentFunction->getArg(i));
        }
    }

    context.builder()->CreateRet(nodeView.body->generate(expressionGenerator));

    return implBlock;
}

llvm::Function *
FunctionCodeGenerator::generateDefinition(const FunctionDefinitionASTNode::View &nodeView, const BindingMap &bindingMap) {
    std::string moduleName = context.parseTree()->getModuleName(nodeView.declaration->fileIndex());
    context.setCurrentModule(moduleName);

    llvm::Function *function = context.currentModule()->getFunction(nodeView.declaration->name());

    if (function) {
        // Function already exists, so just return it
        return function;
    }

    // Otherwise generate any function type dependencies. We know that we are not generating a polymorphic function,
    // so the type is concrete
    llvm::Type *type = nodeView.declaration->functionType()->instantiate(typeCodeGenerator, bindingMap);
    llvm::FunctionType *fType;

    if (type->isFunctionTy()) {
        fType = reinterpret_cast<llvm::FunctionType *>(type);
    } else {
        fType = llvm::FunctionType::get(type, false);
    }

    // Next create the actual function object
    function = llvm::Function::Create(
            fType,
            llvm::Function::ExternalLinkage,
            nodeView.declaration->name(),
            context.module(moduleName).get()
    );

    // If there are no implementations, just return the function declaration
    if (nodeView.implementations.empty()) {
        return function;
    }

    llvm::IRBuilderBase::InsertPoint startIP = context.builder()->saveIP();

    // If the function has multiple implementations (overloads) then create an entry block, otherwise
    // just the standard entry block is correct
    if (nodeView.hasOverloads) {
        llvm::BasicBlock *functionEntry = llvm::BasicBlock::Create(*context.context(), "entry", function);
        context.builder()->SetInsertPoint(functionEntry);
    }

    std::vector<llvm::BasicBlock *> implBlocks(nodeView.implementations.size());

    unsigned variant = 0;
    std::transform(nodeView.implementations.begin(), nodeView.implementations.end(), implBlocks.begin(),
                   [this, &variant, function](const std::unique_ptr<FunctionImplASTNode> &impl) {
                       return impl->generate(*this, variant++, function);
                   });

    context.builder()->restoreIP(startIP);

    return function;
}
