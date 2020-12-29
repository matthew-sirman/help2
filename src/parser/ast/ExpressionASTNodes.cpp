//
// Created by matthew on 28/11/2020.
//

#include "../../../include/parser/ast/ExpressionASTNodes.h"
#include "../../../include/compiler/CodeGenerator.h"

ExpressionASTNode::ExpressionASTNode(size_t lineNum, size_t fileIndex)
        : ASTNode(lineNum, fileIndex) {

}

LambdaExpressionASTNode::LambdaExpressionASTNode(size_t lineNum, size_t fileIndex, std::unique_ptr<PatternASTNode> &&binder,
                                                 std::unique_ptr<ExpressionASTNode> &&expression)
        : ExpressionASTNode(lineNum, fileIndex), binder(std::move(binder)), expression(std::move(expression)) {

}

llvm::Value *LambdaExpressionASTNode::generate(ExpressionCodeGenerator &generator) const {
    return generator.generate<LambdaExpressionASTNode>(View{
            .binder = binder,
            .expression = expression
    });
}

ApplicationASTNode::ApplicationASTNode(size_t lineNum, size_t fileIndex,
                                       std::unique_ptr<ExpressionASTNode> &&func,
                                       std::unique_ptr<ExpressionASTNode> &&arg)
        : ExpressionASTNode(lineNum, fileIndex), function(std::move(func)), argument(std::move(arg)) {

}

llvm::Value *ApplicationASTNode::generate(ExpressionCodeGenerator &generator) const {
    return generator.generate<ApplicationASTNode>(View{
            .function = function,
            .argument = argument
    });
}

LetBindingASTNode::LetBindingASTNode(size_t lineNum, size_t fileIndex,
                                     std::unique_ptr<PatternASTNode> &&binder, std::unique_ptr<ExpressionASTNode> &&body,
                                     std::unique_ptr<ExpressionASTNode> &&usage)
        : ExpressionASTNode(lineNum, fileIndex), binder(std::move(binder)), boundExpression(std::move(body)),
          usage(std::move(usage)) {

}

llvm::Value *LetBindingASTNode::generate(ExpressionCodeGenerator &generator) const {
    return generator.generate<LetBindingASTNode>(View{
            .binder = binder,
            .boundExpression = boundExpression,
            .usage = usage
    });
}

FunctionASTNode::FunctionASTNode(size_t lineNum, size_t fileIndex, std::string name, bool nullary)
        : ExpressionASTNode(lineNum, fileIndex), name(std::move(name)), nullary(nullary) {

}

llvm::Value *FunctionASTNode::generate(ExpressionCodeGenerator &generator) const {
    return generator.generate<FunctionASTNode>(View{
            .name = name,
            .nullary = nullary
    });
}

BuiltinFunctionASTNode::BuiltinFunctionASTNode(size_t lineNum, size_t fileIndex, const std::unique_ptr<BuiltinFunction> &func)
        : ExpressionASTNode(lineNum, fileIndex), func(func) {

}

llvm::Value *BuiltinFunctionASTNode::generate(ExpressionCodeGenerator &generator) const {
    return generator.generate<BuiltinFunctionASTNode>(View{
            .func = func
    });
}

VariableASTNode::VariableASTNode(size_t lineNum, size_t fileIndex, const VariablePatternASTNode *variableRef)
        : ExpressionASTNode(lineNum, fileIndex), variableRef(variableRef) {

}

llvm::Value *VariableASTNode::generate(ExpressionCodeGenerator &generator) const {
    return generator.generate<VariableASTNode>(View{
            .variableRef = variableRef
    });
}

ConstructorASTNode::ConstructorASTNode(size_t lineNum, size_t fileIndex, std::string name)
        : ExpressionASTNode(lineNum, fileIndex), name(std::move(name)) {

}

llvm::Value *ConstructorASTNode::generate(ExpressionCodeGenerator &generator) const {
    return generator.generate<ConstructorASTNode>(View{
            .name = name
    });
}

PrimitiveConstructorASTNode::PrimitiveConstructorASTNode(size_t lineNum, size_t fileIndex)
        : ExpressionASTNode(lineNum, fileIndex) {

}

IntegralConstructorASTNode::IntegralConstructorASTNode(size_t lineNum, size_t fileIndex, long long int value)
        : PrimitiveConstructorASTNode(lineNum, fileIndex), value(value) {

}

llvm::Value *IntegralConstructorASTNode::generate(ExpressionCodeGenerator &generator) const {
    return generator.generate<IntegralConstructorASTNode>(View{
            .value = value
    });
}

DecimalConstructorASTNode::DecimalConstructorASTNode(size_t lineNum, size_t fileIndex, double value)
        : PrimitiveConstructorASTNode(lineNum, fileIndex), value(value) {

}

llvm::Value *DecimalConstructorASTNode::generate(ExpressionCodeGenerator &generator) const {
    return generator.generate<DecimalConstructorASTNode>(View{
            .value = value
    });

}

CharConstructorASTNode::CharConstructorASTNode(size_t lineNum, size_t fileIndex, char value)
        : PrimitiveConstructorASTNode(lineNum, fileIndex), value(value) {

}

llvm::Value *CharConstructorASTNode::generate(ExpressionCodeGenerator &generator) const {
    return generator.generate<CharConstructorASTNode>(View{
            .value = value
    });
}
