//
// Created by matthew on 28/11/2020.
//

#include "../../../include/parser/ast/ExpressionASTNodes.h"

LambdaExpressionASTNode::LambdaExpressionASTNode(std::string binder, std::unique_ptr<ExpressionASTNode> &&expression)
        : binder(std::move(binder)), expression(std::move(expression)) {

}

ApplicationASTNode::ApplicationASTNode(std::unique_ptr<ExpressionASTNode> &&func,
                                       std::unique_ptr<ExpressionASTNode> &&arg)
        : function(std::move(func)), argument(std::move(arg)) {

}

LetBindingASTNode::LetBindingASTNode(std::string binder, std::unique_ptr<ExpressionASTNode> &&body,
                                     std::unique_ptr<ExpressionASTNode> &&usage)
        : binder(std::move(binder)), boundExpression(std::move(body)), usage(std::move(usage)) {

}

FunctionASTNode::FunctionASTNode(std::string name)
        : name(std::move(name)) {

}

VariableASTNode::VariableASTNode(std::string name)
        : name(std::move(name)) {

}

ConstructorASTNode::ConstructorASTNode(std::string name)
        : name(std::move(name)) {

}
