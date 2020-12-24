//
// Created by matthew on 28/11/2020.
//

#ifndef HELP2_EXPRESSIONASTNODES_H
#define HELP2_EXPRESSIONASTNODES_H

#include <string>
#include <memory>

#include "ASTNode.h"

class ExpressionASTNode : public ASTNode {
public:
    ExpressionASTNode(size_t lineNum, size_t fileIndex);
};

/*
 * Lambda Function syntax:
 * $var => expression
 */
class LambdaExpressionASTNode : public ExpressionASTNode {
public:
    LambdaExpressionASTNode(size_t lineNum, size_t fileIndex, std::string binder, std::unique_ptr<ExpressionASTNode> &&expression);

private:
    std::string binder;
    std::unique_ptr<ExpressionASTNode> expression;
};

/*
 * Application syntax:
 * e1 e2
 */
class ApplicationASTNode : public ExpressionASTNode {
public:
    ApplicationASTNode(size_t lineNum, size_t fileIndex, std::unique_ptr<ExpressionASTNode> &&func, std::unique_ptr<ExpressionASTNode> &&arg);

private:
    std::unique_ptr<ExpressionASTNode> function, argument;
};

/*
 * Infix Application syntax:
 * l e r
 */
class InfixApplicationASTNode : public ExpressionASTNode {

private:
    std::unique_ptr<ExpressionASTNode> lhs, function, rhs;
};

/*
 * Let Binding syntax:
 * let var = e1 in e2
 */
class LetBindingASTNode : public ExpressionASTNode {
public:
    LetBindingASTNode(size_t lineNum, size_t fileIndex, std::string binder, std::unique_ptr<ExpressionASTNode> &&body,
                      std::unique_ptr<ExpressionASTNode> &&usage);

private:
    // Bound variable (var)
    std::string binder;
    // Bound expression (e1) and usage (e2)
    std::unique_ptr<ExpressionASTNode> boundExpression, usage;
};

class FunctionASTNode : public ExpressionASTNode {
public:
    FunctionASTNode(size_t lineNum, size_t fileIndex, std::string name);

private:
    std::string name;
};

class VariableASTNode : public ExpressionASTNode {
public:
    VariableASTNode(size_t lineNum, size_t fileIndex, std::string name);

private:
    // Name of the variable
    std::string name;
};

class ConstructorASTNode : public ExpressionASTNode {
public:
    ConstructorASTNode(size_t lineNum, size_t fileIndex, std::string name);

private:
    std::string name;
};

class IntegralConstructorASTNode : public ExpressionASTNode {
public:
    IntegralConstructorASTNode(size_t lineNum, size_t fileIndex, long long value);

private:
    long long value;
};

class DecimalConstructorASTNode : public ExpressionASTNode {
public:
    DecimalConstructorASTNode(size_t lineNum, size_t fileIndex, double value);

private:
    double value;
};

class CharConstructorASTNode : public ExpressionASTNode {
public:
    CharConstructorASTNode(size_t lineNum, size_t fileIndex, char value);

private:
    char value;
};

#endif //HELP2_EXPRESSIONASTNODES_H
