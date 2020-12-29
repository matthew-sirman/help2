//
// Created by matthew on 27/12/2020.
//

#ifndef HELP2_BUILTINFUNCTION_H
#define HELP2_BUILTINFUNCTION_H

#include <string>

#include <llvm/IR/Function.h>

class CoreBuilder;
class CompileContext;

// TODO: Maybe move this
enum class Associativity {
    Left = 1,
    Right = -1
};

class BuiltinFunction {
    friend class CoreBuilder;
public:
    constexpr const std::string &name() const { return funcName; }

    constexpr const std::string &symbolName() const { return symbolicName; }

    llvm::Function *generate(CompileContext &context) const;

    virtual constexpr int operatorPrecedence() const { return 0; }

    virtual constexpr int associativity() const { return 0; }

protected:
    virtual llvm::FunctionType *opFunctionType(CompileContext &context) const = 0;

    virtual llvm::Value *opBody(CompileContext &context, llvm::Function *func) const = 0;

    BuiltinFunction(std::string funcName, std::string symbolicName);

    std::string funcName;
    std::string symbolicName;
};

class IntBinOpFunction : public BuiltinFunction {
    constexpr int operatorPrecedence() const override { return precedence; }

    constexpr int associativity() const override { return (int) assoc; }

protected:
    IntBinOpFunction(const std::string &name, const std::string &symbolicName,
                     int precendence, Associativity assoc);

    llvm::FunctionType *opFunctionType(CompileContext &context) const override;

    int precedence;
    Associativity assoc;
};

class AddIntFunction : public IntBinOpFunction {
    friend class CoreBuilder;

protected:
    llvm::Value * opBody(CompileContext &context, llvm::Function *addFunc) const override;

private:
    AddIntFunction(const std::string &name, const std::string &symbolicName,
                   int precedence, Associativity assoc);
};

class SubIntFunction : public IntBinOpFunction {
    friend class CoreBuilder;

protected:
    SubIntFunction(const std::string &name, const std::string &symbolicName,
                   int precedence, Associativity assoc);

    llvm::Value *opBody(CompileContext &context, llvm::Function *subFunc) const override;
};

class MulIntFunction : public IntBinOpFunction {
    friend class CoreBuilder;

protected:
    MulIntFunction(const std::string &name, const std::string &symbolicName,
                   int precedence, Associativity assoc);

    llvm::Value *opBody(CompileContext &context, llvm::Function *mulFunc) const override;
};

#endif //HELP2_BUILTINFUNCTION_H
