//
// Created by matthew on 28/11/2020.
//

#ifndef HELP2_FUNCTIONASTNODES_H
#define HELP2_FUNCTIONASTNODES_H

#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>

#include "TypeASTNodes.h"
#include "ExpressionASTNodes.h"

class CodeGenerator;

enum class FunctionUsage {
    Prefix,
    Infix,
    Value
};

class FunctionDeclASTNode : public ASTNode {
public:
    FunctionDeclASTNode(size_t lineNum, size_t fileIndex, std::string name,
                        std::unique_ptr<TypeInstanceASTNode> &&funcType);

    const std::string &name() const { return funcName; }

    virtual constexpr FunctionUsage funcUsage() const = 0;

    virtual size_t maxArgs() const = 0;

    constexpr const std::unique_ptr<TypeInstanceASTNode> &functionType() const { return funcType; }

protected:
    std::string funcName;
    std::unique_ptr<TypeInstanceASTNode> funcType;
};

/*
 * Prefix Function declaration syntax:
 * func name: a -> b;
 */
class PrefixFunctionDeclASTNode : public FunctionDeclASTNode {
public:
    PrefixFunctionDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                              std::unique_ptr<TypeInstanceASTNode> &&funcType);

    constexpr FunctionUsage funcUsage() const override { return FunctionUsage::Prefix; }

    size_t maxArgs() const override;
};

enum class Associativity {
    Left = 1,
    Right = -1
};

/*
 * Infix Function declaration syntax:
 * infix func name: a -> b -> c;
 */
class InfixFunctionDeclASTNode : public FunctionDeclASTNode {
public:
    InfixFunctionDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                             std::unique_ptr<TypeInstanceASTNode> &&funcType,
                             int precedence, Associativity assoc);

    constexpr FunctionUsage funcUsage() const override { return FunctionUsage::Infix; }

    constexpr size_t maxArgs() const override { return 2; }

    constexpr int operatorPrecedence() const { return precedence; }

    constexpr Associativity associativity() const { return assoc; }

private:
    int precedence;

    Associativity assoc;
};

/*
 * Value Function declaration syntax:
 * value name: a;
 */
class ValueFunctionDeclASTNode : public FunctionDeclASTNode {
public:
    ValueFunctionDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                             std::unique_ptr<TypeInstanceASTNode> &&valueType);

    constexpr FunctionUsage funcUsage() const override { return FunctionUsage::Value; }

    constexpr size_t maxArgs() const override { return 0; }
};

enum class PatternUsage {
    Variable,
    Constructor
};

class PatternASTNode : public ASTNode {
public:
    PatternASTNode(size_t lineNum, size_t fileIndex);

    virtual constexpr PatternUsage patternUsage() const = 0;
};

class VariablePatternASTNode : public PatternASTNode {
public:
    VariablePatternASTNode(size_t lineNum, size_t fileIndex, std::string binder);

    constexpr PatternUsage patternUsage() const override { return PatternUsage::Variable; }

    constexpr const std::string &name() const { return binderName; }

private:
    std::string binderName;
};

class ConstructorPatternASTNode : public PatternASTNode {
public:
    ConstructorPatternASTNode(size_t lineNum, size_t fileIndex, std::string dataConstructor);

    ConstructorPatternASTNode(size_t lineNum, size_t fileIndex, std::string dataConstructor,
                              std::vector<std::unique_ptr<PatternASTNode>> &&subPatterns);

    constexpr PatternUsage patternUsage() const override { return PatternUsage::Constructor; }

    constexpr const std::string &constructorName() const { return dataConstructorName; }

private:
    std::string dataConstructorName;
    std::vector<std::unique_ptr<PatternASTNode>> subPatterns;
};

class FunctionImplASTNode : public ASTNode {
public:
    FunctionImplASTNode(size_t lineNum, size_t fileIndex, std::unique_ptr<ExpressionASTNode> &&body);

    virtual constexpr FunctionUsage functionUsage() const = 0;

    constexpr const std::unique_ptr<ExpressionASTNode> &functionBody() const { return body; }

    virtual void generate(const CodeGenerator &generator) const = 0;

protected:
    std::unique_ptr<ExpressionASTNode> body;
};

/*
 * Prefix Function implementation syntax:
 * name [pattern0 [pattern1 ...]] => expression;
 */
class PrefixFunctionImplASTNode : public FunctionImplASTNode {
public:
    struct View {
        const std::vector<std::unique_ptr<PatternASTNode>> &patterns;
        const std::unique_ptr<ExpressionASTNode> &body;
    };

    PrefixFunctionImplASTNode(size_t lineNum, size_t fileIndex, std::unique_ptr<ExpressionASTNode> &&body);

    PrefixFunctionImplASTNode(size_t lineNum, size_t fileIndex, std::unique_ptr<ExpressionASTNode> &&body,
                              std::vector<std::unique_ptr<PatternASTNode>> &&patterns);

    constexpr FunctionUsage functionUsage() const override { return FunctionUsage::Prefix; }

    constexpr const std::vector<std::unique_ptr<PatternASTNode>> &parameterPatterns() const { return patterns; }

    void generate(const CodeGenerator &generator) const override;

private:
    std::vector<std::unique_ptr<PatternASTNode>> patterns;
};

/*
 * Infix Function implementation syntax:
 * lPattern name rPattern => expression;
 */
class InfixFunctionImplASTNode : public FunctionImplASTNode {
public:
    struct View {
        const std::unique_ptr<PatternASTNode> &lhs, &rhs;
        const std::unique_ptr<ExpressionASTNode> &body;
    };

    InfixFunctionImplASTNode(size_t lineNum, size_t fileIndex, std::unique_ptr<ExpressionASTNode> &&body,
                             std::unique_ptr<PatternASTNode> &&left, std::unique_ptr<PatternASTNode> &&right);

    constexpr FunctionUsage functionUsage() const override { return FunctionUsage::Infix; }

    constexpr const std::unique_ptr<PatternASTNode> &leftPattern() const { return lhs; }

    constexpr const std::unique_ptr<PatternASTNode> &rightPattern() const { return rhs; }

    void generate(const CodeGenerator &generator) const override;

private:
    std::unique_ptr<PatternASTNode> lhs, rhs;
};


class FunctionDefinitionASTNode {
public:
    struct View {
        const std::unique_ptr<const FunctionDeclASTNode> &declaration;
        const std::vector<const std::unique_ptr<const FunctionImplASTNode>> &implementations;
    };

    FunctionDefinitionASTNode(std::unique_ptr<FunctionDeclASTNode> &&declaration);

    void addImplementation(std::unique_ptr<FunctionImplASTNode> &&implementation);

    const std::string &name() const { return declaration->name(); }

    constexpr std::unique_ptr<FunctionDeclASTNode> &decl() { return declaration; }

    constexpr const std::vector<std::unique_ptr<FunctionImplASTNode>> &
    implementationVariants() const { return implementations; }

    void generate(const CodeGenerator &generator) const;

    bool isPolymorphic() const { return declaration->functionType()->isPolymorphic(); }

private:
    std::unique_ptr<FunctionDeclASTNode> declaration;
    std::vector<std::unique_ptr<FunctionImplASTNode>> implementations;
};

#endif //HELP2_FUNCTIONASTNODES_H
