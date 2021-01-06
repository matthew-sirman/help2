//
// Created by matthew on 28/11/2020.
//

#ifndef HELP2_EXPRESSIONASTNODES_H
#define HELP2_EXPRESSIONASTNODES_H

#include <string>
#include <memory>

#include <llvm/IR/Value.h>

#include "PatternASTNodes.h"

#include "../../lang/core/BuiltinFunction.h"

class ExpressionCodeGenerator;

enum class ExpressionType {
    Lambda,
    Application,
    LetBinding,
    Function,
    Variable,
    PrimitiveConstructor
};

class ExpressionASTNode : public ASTNode {
public:
    ExpressionASTNode(size_t lineNum, size_t fileIndex);

    virtual constexpr ExpressionType type() const = 0;

    virtual llvm::Value *generate(ExpressionCodeGenerator &generator) const = 0;
};

/*
 * Lambda Function syntax:
 * $var => expression
 */
class LambdaExpressionASTNode : public ExpressionASTNode {
public:
    struct View {
        const std::unique_ptr<PatternASTNode> &binder;
        const std::unique_ptr<ExpressionASTNode> &expression;
    };

    LambdaExpressionASTNode(size_t lineNum, size_t fileIndex, std::unique_ptr<PatternASTNode> &&binder,
                            std::unique_ptr<ExpressionASTNode> &&expression);

    llvm::Value *generate(ExpressionCodeGenerator &generator) const override;

    constexpr ExpressionType type() const override { return ExpressionType::Lambda; }

    const PatternASTNode &binderPattern() const { return *binder; }

    const ExpressionASTNode &bodyExpression() const { return *expression; }

private:
    std::unique_ptr<PatternASTNode> binder;
    std::unique_ptr<ExpressionASTNode> expression;
};

/*
 * Application syntax:
 * e1 e2
 */
class ApplicationASTNode : public ExpressionASTNode {
public:
    struct View {
        const std::unique_ptr<ExpressionASTNode> &function, &argument;
    };

    ApplicationASTNode(size_t lineNum, size_t fileIndex, std::unique_ptr<ExpressionASTNode> &&func,
                       std::unique_ptr<ExpressionASTNode> &&arg);

    constexpr const std::unique_ptr<ExpressionASTNode> &appFunction() const { return function; }

    constexpr const std::unique_ptr<ExpressionASTNode> &appArgument() const { return argument; }

    llvm::Value *generate(ExpressionCodeGenerator &generator) const override;

    constexpr ExpressionType type() const override { return ExpressionType::Application; }

private:
    std::unique_ptr<ExpressionASTNode> function, argument;
};

/*
 * Let Binding syntax:
 * let var = e1 in e2
 */
class LetBindingASTNode : public ExpressionASTNode {
public:
    struct View {
        const std::unique_ptr<PatternASTNode> &binder;
        const std::unique_ptr<ExpressionASTNode> &boundExpression, &usage;
    };

    LetBindingASTNode(size_t lineNum, size_t fileIndex, std::unique_ptr<PatternASTNode> &&binder,
                      std::unique_ptr<ExpressionASTNode> &&body,
                      std::unique_ptr<ExpressionASTNode> &&usage);

    llvm::Value *generate(ExpressionCodeGenerator &generator) const override;

    constexpr ExpressionType type() const override { return ExpressionType::LetBinding; }

    const ExpressionASTNode &usageExpression() const { return *usage; }

private:
    // Bound variable(s) (var)
    std::unique_ptr<PatternASTNode> binder;
    // Bound expression (e1) and usage (e2)
    std::unique_ptr<ExpressionASTNode> boundExpression, usage;
};

class FunctionASTNode : public ExpressionASTNode {
public:
    struct View {
        const std::string &name;
        bool nullary;
    };

    FunctionASTNode(size_t lineNum, size_t fileIndex, std::string name, bool nullary = false);

    llvm::Value *generate(ExpressionCodeGenerator &generator) const override;

    constexpr ExpressionType type() const override { return ExpressionType::Function; }

    constexpr const std::string &functionName() const { return name; }

    constexpr bool isNullary() const { return nullary; }

private:
    std::string name;
    bool nullary;
};

/*
class BuiltinFunctionASTNode : public ExpressionASTNode {
public:
    struct View {
        const std::unique_ptr<BuiltinFunction> &func;
    };

    BuiltinFunctionASTNode(size_t lineNum, size_t fileIndex, const std::unique_ptr<BuiltinFunction> &func);

    llvm::Value *generate(ExpressionCodeGenerator &generator) const override;

    constexpr ExpressionType type() const override { return ExpressionType::InlineFunction; }

private:
    const std::unique_ptr<BuiltinFunction> &func;
};
 */

class VariableASTNode : public ExpressionASTNode {
public:
    struct View {
        const VariablePatternASTNode &variableRef;
    };

    VariableASTNode(size_t lineNum, size_t fileIndex, const VariablePatternASTNode &variableRef);

    llvm::Value *generate(ExpressionCodeGenerator &generator) const override;

    constexpr ExpressionType type() const override { return ExpressionType::Variable; }

private:
    const VariablePatternASTNode &variableRef;
};

/*
class ConstructorASTNode : public ExpressionASTNode {
public:
    struct View {
        const std::string &name;
    };

    ConstructorASTNode(size_t lineNum, size_t fileIndex, std::string name);

    llvm::Value *generate(ExpressionCodeGenerator &generator) const override;

    constexpr ExpressionType type() const override { return ExpressionType::Constructor; }

private:
    std::string name;
};
 */

class PrimitiveConstructorASTNode : public ExpressionASTNode {
public:
    PrimitiveConstructorASTNode(size_t lineNum, size_t fileIndex);

    constexpr ExpressionType type() const override { return ExpressionType::PrimitiveConstructor; }
};

class IntegralConstructorASTNode : public PrimitiveConstructorASTNode {
public:
    struct View {
        long long value;
    };

    IntegralConstructorASTNode(size_t lineNum, size_t fileIndex, long long value);

    llvm::Value *generate(ExpressionCodeGenerator &generator) const override;

private:
    long long value;
};

class DecimalConstructorASTNode : public PrimitiveConstructorASTNode {
public:
    struct View {
        double value;
    };

    DecimalConstructorASTNode(size_t lineNum, size_t fileIndex, double value);

    llvm::Value *generate(ExpressionCodeGenerator &generator) const override;

private:
    double value;
};

class CharConstructorASTNode : public PrimitiveConstructorASTNode {
public:
    struct View {
        char value;
    };

    CharConstructorASTNode(size_t lineNum, size_t fileIndex, char value);

    llvm::Value *generate(ExpressionCodeGenerator &generator) const override;

private:
    char value;
};

#endif //HELP2_EXPRESSIONASTNODES_H
