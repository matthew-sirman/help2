//
// Created by matthew on 28/11/2020.
//

#include "../../../include/parser/ast/ExpressionASTNodes.h"

ExpressionASTNode::ExpressionASTNode(size_t lineNum, size_t fileIndex)
        : ASTNode(lineNum, fileIndex) {

}

LambdaExpressionASTNode::LambdaExpressionASTNode(size_t lineNum, size_t fileIndex, std::string binder, std::unique_ptr<ExpressionASTNode> &&expression)
        : ExpressionASTNode(lineNum, fileIndex), binder(std::move(binder)), expression(std::move(expression)) {

}

ApplicationASTNode::ApplicationASTNode(size_t lineNum, size_t fileIndex,
                                       std::unique_ptr<ExpressionASTNode> &&func,
                                       std::unique_ptr<ExpressionASTNode> &&arg)
        : ExpressionASTNode(lineNum, fileIndex), function(std::move(func)), argument(std::move(arg)) {

}

LetBindingASTNode::LetBindingASTNode(size_t lineNum, size_t fileIndex,
                                     std::string binder, std::unique_ptr<ExpressionASTNode> &&body,
                                     std::unique_ptr<ExpressionASTNode> &&usage)
        : ExpressionASTNode(lineNum, fileIndex), binder(std::move(binder)), boundExpression(std::move(body)), usage(std::move(usage)) {

}

FunctionASTNode::FunctionASTNode(size_t lineNum, size_t fileIndex, std::string name)
        : ExpressionASTNode(lineNum, fileIndex), name(std::move(name)) {

}

VariableASTNode::VariableASTNode(size_t lineNum, size_t fileIndex, std::string name)
        : ExpressionASTNode(lineNum, fileIndex), name(std::move(name)) {

}

ConstructorASTNode::ConstructorASTNode(size_t lineNum, size_t fileIndex, std::string name)
        : ExpressionASTNode(lineNum, fileIndex), name(std::move(name)) {

}

IntegralConstructorASTNode::IntegralConstructorASTNode(size_t lineNum, size_t fileIndex, long long int value)
        : ExpressionASTNode(lineNum, fileIndex), value(value) {

}

DecimalConstructorASTNode::DecimalConstructorASTNode(size_t lineNum, size_t fileIndex, double value)
        : ExpressionASTNode(lineNum, fileIndex), value(value) {

}

CharConstructorASTNode::CharConstructorASTNode(size_t lineNum, size_t fileIndex, char value)
        : ExpressionASTNode(lineNum, fileIndex), value(value) {

}
