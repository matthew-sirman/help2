//
// Created by matthew on 28/11/2020.
//

#ifndef HELP2_EXPRESSIONASTNODES_H
#define HELP2_EXPRESSIONASTNODES_H

#include <string>
#include <memory>

class ExpressionASTNode {

};

/*
 * Lambda Function syntax:
 * $var => expression
 */
class LambdaExpressionASTNode : public ExpressionASTNode {
public:
    LambdaExpressionASTNode(std::string binder, std::unique_ptr<ExpressionASTNode> &&expression);

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
    ApplicationASTNode(std::unique_ptr<ExpressionASTNode> &&func, std::unique_ptr<ExpressionASTNode> &&arg);

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
    LetBindingASTNode(std::string binder, std::unique_ptr<ExpressionASTNode> &&body,
                      std::unique_ptr<ExpressionASTNode> &&usage);

private:
    // Bound variable (var)
    std::string binder;
    // Bound expression (e1) and usage (e2)
    std::unique_ptr<ExpressionASTNode> boundExpression, usage;
};

class FunctionASTNode : public ExpressionASTNode {
public:
    FunctionASTNode(std::string name);

private:
    std::string name;
};

class VariableASTNode : public ExpressionASTNode {
public:
    VariableASTNode(std::string name);

private:
    // Name of the variable
    std::string name;
};

class ConstructorASTNode : public ExpressionASTNode {
public:
    ConstructorASTNode(std::string name);

private:
    std::string name;
};

#endif //HELP2_EXPRESSIONASTNODES_H
