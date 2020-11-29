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

private:
    std::string binder;
    std::unique_ptr<ExpressionASTNode> expression;
};

/*
 * Application syntax:
 * e1 e2
 */
class ApplicationASTNode : public ExpressionASTNode {

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

private:
    // Bound variable (var)
    std::string binder;
    // Bound expression (e1) and usage (e2)
    std::unique_ptr<ExpressionASTNode> boundExpression, usage;
};

#endif //HELP2_EXPRESSIONASTNODES_H
