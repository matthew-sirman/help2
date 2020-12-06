//
// Created by matthew on 28/11/2020.
//

#include "../../../include/parser/ast/FunctionASTNodes.h"

#include <utility>


FunctionDeclASTNode::FunctionDeclASTNode(std::string name, std::unique_ptr<TypeInstanceASTNode> &&funcType)
        : funcName(std::move(name)), funcType(std::move(funcType)) {

}

PrefixFunctionDeclASTNode::PrefixFunctionDeclASTNode(const std::string &name,
                                                     std::unique_ptr<TypeInstanceASTNode> &&funcType)
        : FunctionDeclASTNode(name, std::move(funcType)) {

}

size_t PrefixFunctionDeclASTNode::maxArgs() const {
    // We know there is at least one function type
    size_t m = 1;
    const FunctionTypeInstanceASTNode *curType = dynamic_cast<const FunctionTypeInstanceASTNode *>(funcType.get());
    // For as long as the right type of the function type is a function type, increment the counter and recurse
    while (curType->right()->typeUsage() == TypeUsage::Function) {
        m++;
        curType = dynamic_cast<const FunctionTypeInstanceASTNode *>(curType->right());
    }
    return m;
}

InfixFunctionDeclASTNode::InfixFunctionDeclASTNode(const std::string &name,
                                                   std::unique_ptr<TypeInstanceASTNode> &&funcType,
                                                   int precedence, Associativity assoc)
        : FunctionDeclASTNode(name, std::move(funcType)), precedence(precedence), assoc(assoc) {

}

ValueFunctionDeclASTNode::ValueFunctionDeclASTNode(const std::string &name,
                                                   std::unique_ptr<TypeInstanceASTNode> &&valueType)
        : FunctionDeclASTNode(name, std::move(valueType)) {

}

VariablePatternASTNode::VariablePatternASTNode(std::string binder)
        : binderName(std::move(binder)) {

}

ConstructorPatternASTNode::ConstructorPatternASTNode(std::string dataConstructor)
        : dataConstructorName(std::move(dataConstructor)) {

}

ConstructorPatternASTNode::ConstructorPatternASTNode(std::string dataConstructor,
                                                     std::vector<std::unique_ptr<PatternASTNode>> &&subPatterns)
        : dataConstructorName(std::move(dataConstructor)), subPatterns(std::move(subPatterns)) {

}

FunctionImplASTNode::FunctionImplASTNode(std::unique_ptr<ExpressionASTNode> &&body)
        : body(std::move(body)) {

}

PrefixFunctionImplASTNode::PrefixFunctionImplASTNode(std::unique_ptr<ExpressionASTNode> &&body)
        : FunctionImplASTNode(std::move(body)) {

}

PrefixFunctionImplASTNode::PrefixFunctionImplASTNode(std::unique_ptr<ExpressionASTNode> &&body,
                                                     std::vector<std::unique_ptr<PatternASTNode>> &&patterns)
        : FunctionImplASTNode(std::move(body)), patterns(std::move(patterns)) {

}

InfixFunctionImplASTNode::InfixFunctionImplASTNode(std::unique_ptr<ExpressionASTNode> &&body,
                                                   std::unique_ptr<PatternASTNode> &&left,
                                                   std::unique_ptr<PatternASTNode> &&right)
        : FunctionImplASTNode(std::move(body)), lhs(std::move(left)), rhs(std::move(right)) {

}

FunctionDefinitionASTNode::FunctionDefinitionASTNode(std::unique_ptr<FunctionDeclASTNode> &&declaration)
        : declaration(std::move(declaration)) {

}

void FunctionDefinitionASTNode::addImplementation(std::unique_ptr<FunctionImplASTNode> &&implementation) {
    implementations.push_back(std::move(implementation));
}

