//
// Created by matthew on 28/11/2020.
//

#ifndef HELP2_FUNCTIONASTNODES_H
#define HELP2_FUNCTIONASTNODES_H

#include "TypeASTNodes.h"
#include "ExpressionASTNodes.h"

enum class FunctionUsage {
    Prefix,
    Infix,
    Value
};

class FunctionDeclASTNode {
public:
    FunctionDeclASTNode(std::string name, std::unique_ptr<TypeInstanceASTNode> &&funcType);

    const std::string &name() const { return funcName; }

    virtual constexpr FunctionUsage funcUsage() const = 0;

    virtual size_t maxArgs() const = 0;

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
    PrefixFunctionDeclASTNode(const std::string &name, std::unique_ptr<TypeInstanceASTNode> &&funcType);

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
    InfixFunctionDeclASTNode(const std::string &name, std::unique_ptr<TypeInstanceASTNode> &&funcType,
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
    ValueFunctionDeclASTNode(const std::string &name, std::unique_ptr<TypeInstanceASTNode> &&valueType);

    constexpr FunctionUsage funcUsage() const override { return FunctionUsage::Value; }

    constexpr size_t maxArgs() const override { return 0; }
};

enum class PatternUsage {
    Variable,
    Constructor
};

class PatternASTNode {
public:
    virtual constexpr PatternUsage patternUsage() const = 0;
};

class VariablePatternASTNode : public PatternASTNode {
public:
    VariablePatternASTNode(std::string binder);

    constexpr PatternUsage patternUsage() const override { return PatternUsage::Variable; }

    constexpr const std::string &name() const { return binderName; }

private:
    std::string binderName;
};

class ConstructorPatternASTNode : public PatternASTNode {
public:
    ConstructorPatternASTNode(std::string dataConstructor);

    ConstructorPatternASTNode(std::string dataConstructor, std::vector<std::unique_ptr<PatternASTNode>> &&subPatterns);

    constexpr PatternUsage patternUsage() const override { return PatternUsage::Constructor; }

    constexpr const std::string &constructorName() const { return dataConstructorName; }

private:
    std::string dataConstructorName;
    std::vector<std::unique_ptr<PatternASTNode>> subPatterns;
};

class FunctionImplASTNode {
public:
    FunctionImplASTNode(std::unique_ptr<ExpressionASTNode> &&body);

    virtual constexpr FunctionUsage functionUsage() const = 0;

private:
    std::unique_ptr<ExpressionASTNode> body;
};

/*
 * Prefix Function implementation syntax:
 * name [pattern0 [pattern1 ...]] => expression;
 */
class PrefixFunctionImplASTNode : public FunctionImplASTNode {
public:
    PrefixFunctionImplASTNode(std::unique_ptr<ExpressionASTNode> &&body);

    PrefixFunctionImplASTNode(std::unique_ptr<ExpressionASTNode> &&body, std::vector<std::unique_ptr<PatternASTNode>> &&patterns);

    constexpr FunctionUsage functionUsage() const override { return FunctionUsage::Prefix; }

private:
    std::vector<std::unique_ptr<PatternASTNode>> patterns;
};

/*
 * Infix Function implementation syntax:
 * lPattern name rPattern => expression;
 */
class InfixFunctionImplASTNode : public FunctionImplASTNode {
public:
    InfixFunctionImplASTNode(std::unique_ptr<ExpressionASTNode> &&body, std::unique_ptr<PatternASTNode> &&left, std::unique_ptr<PatternASTNode> &&right);

    constexpr FunctionUsage functionUsage() const override { return FunctionUsage::Infix; }

private:
    std::unique_ptr<PatternASTNode> lhs, rhs;
};


class FunctionDefinitionASTNode {
public:
    FunctionDefinitionASTNode(std::unique_ptr<FunctionDeclASTNode> &&declaration);

    void addImplementation(std::unique_ptr<FunctionImplASTNode> &&implementation);

    const std::string &name() const { return declaration->name(); }

    constexpr std::unique_ptr<FunctionDeclASTNode> &decl() { return declaration; }

private:
    std::unique_ptr<FunctionDeclASTNode> declaration;
    std::vector<std::unique_ptr<FunctionImplASTNode>> implementations;
};

#endif //HELP2_FUNCTIONASTNODES_H
