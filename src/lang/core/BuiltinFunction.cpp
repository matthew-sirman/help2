//
// Created by matthew on 27/12/2020.
//

#include "../../../include/lang/core/BuiltinFunction.h"

#include "../../../include/compiler/Compiler.h"

#include <iostream>

BuiltinFunction::BuiltinFunction(std::string funcName, std::string symbolicName)
        : funcName(std::move(funcName)), symbolicName(std::move(symbolicName)) {

}

llvm::Function *BuiltinFunction::generate(CompileContext &context) const {
    // If the function already exists, just return it
    llvm::Function *function = context.currentModule()->getFunction(symbolicName);
    if (function) {
        return function;
    }

    // Otherwise declare the function
    function = llvm::Function::Create(
            opFunctionType(context),
            llvm::Function::InternalLinkage,
            symbolicName,
            context.currentModule().get()
    );

    // Save the builder point
    llvm::IRBuilderBase::InsertPoint startIP = context.builder()->saveIP();

    // Create the entry point
    llvm::BasicBlock *addEntry = llvm::BasicBlock::Create(*context.context(), "entry", function);
    context.builder()->SetInsertPoint(addEntry);
    // Return the value computed by the function (outsourced to virtual method)
    context.builder()->CreateRet(opBody(context, function));

    // Restore the builder
    context.builder()->restoreIP(startIP);

    // Return the created function
    return function;
}

IntBinOpFunction::IntBinOpFunction(const std::string &name, const std::string &symbolicName,
                                   int precedence, Associativity assoc)
        : BuiltinFunction(name, symbolicName), precedence(precedence), assoc(assoc) {

}

llvm::FunctionType *IntBinOpFunction::opFunctionType(CompileContext &context) const {
    return context.intBinaryOpType();
}

AddIntFunction::AddIntFunction(const std::string &name, const std::string &symbolicName,
                               int precedence, Associativity assoc)
        : IntBinOpFunction(name, symbolicName, precedence, assoc) {

}

llvm::Value *AddIntFunction::opBody(CompileContext &context, llvm::Function *addFunc) const {
    return context.builder()->CreateAdd(
            addFunc->getArg(0),
            addFunc->getArg(1),
            "addtmp"
    );
}

SubIntFunction::SubIntFunction(const std::string &name, const std::string &symbolicName,
                               int precedence, Associativity assoc)
        : IntBinOpFunction(name, symbolicName, precedence, assoc) {

}

llvm::Value *SubIntFunction::opBody(CompileContext &context, llvm::Function *subFunc) const {
    return context.builder()->CreateSub(
            subFunc->getArg(0),
            subFunc->getArg(1),
            "subtmp"
    );
}

MulIntFunction::MulIntFunction(const std::string &name, const std::string &symbolicName,
                               int precedence, Associativity assoc)
        : IntBinOpFunction(name, symbolicName, precedence, assoc) {

}

llvm::Value *MulIntFunction::opBody(CompileContext &context, llvm::Function *mulFunc) const {
    return context.builder()->CreateMul(
            mulFunc->getArg(0),
            mulFunc->getArg(1),
            "multmp"
    );
}
