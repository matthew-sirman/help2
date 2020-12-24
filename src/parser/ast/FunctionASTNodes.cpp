//
// Created by matthew on 28/11/2020.
//

#include "../../../include/parser/ast/FunctionASTNodes.h"
#include "../../../include/compiler/CodeGenerator.h"

#include <utility>
#include <llvm/IR/Type.h>


FunctionDeclASTNode::FunctionDeclASTNode(size_t lineNum, size_t fileIndex, std::string name,
                                         std::unique_ptr<TypeInstanceASTNode> &&funcType)
        : ASTNode(lineNum, fileIndex), funcName(std::move(name)), funcType(std::move(funcType)) {

}

PrefixFunctionDeclASTNode::PrefixFunctionDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                                                     std::unique_ptr<TypeInstanceASTNode> &&funcType)
        : FunctionDeclASTNode(lineNum, fileIndex, name, std::move(funcType)) {

}

size_t PrefixFunctionDeclASTNode::maxArgs() const {
    // We know there is at least one function type
    size_t m = 1;
    const FunctionTypeInstanceASTNode *curType = dynamic_cast<const FunctionTypeInstanceASTNode *>(funcType.get());
    // For as long as the right type of the function type is a function type, increment the counter and recurse
    while (curType->right()->typeUsage() == TypeUsage::Function) {
        m++;
        curType = dynamic_cast<const FunctionTypeInstanceASTNode *>(curType->right().get());
    }
    return m;
}

InfixFunctionDeclASTNode::InfixFunctionDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                                                   std::unique_ptr<TypeInstanceASTNode> &&funcType,
                                                   int precedence, Associativity assoc)
        : FunctionDeclASTNode(lineNum, fileIndex, name, std::move(funcType)), precedence(precedence), assoc(assoc) {

}

ValueFunctionDeclASTNode::ValueFunctionDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                                                   std::unique_ptr<TypeInstanceASTNode> &&valueType)
        : FunctionDeclASTNode(lineNum, fileIndex, name, std::move(valueType)) {

}

PatternASTNode::PatternASTNode(size_t lineNum, size_t fileIndex)
        : ASTNode(lineNum, fileIndex) {

}

VariablePatternASTNode::VariablePatternASTNode(size_t lineNum, size_t fileIndex, std::string binder)
        : PatternASTNode(lineNum, fileIndex), binderName(std::move(binder)) {

}

ConstructorPatternASTNode::ConstructorPatternASTNode(size_t lineNum, size_t fileIndex, std::string dataConstructor)
        : PatternASTNode(lineNum, fileIndex), dataConstructorName(std::move(dataConstructor)) {

}

ConstructorPatternASTNode::ConstructorPatternASTNode(size_t lineNum, size_t fileIndex, std::string dataConstructor,
                                                     std::vector<std::unique_ptr<PatternASTNode>> &&subPatterns)
        : PatternASTNode(lineNum, fileIndex), dataConstructorName(std::move(dataConstructor)),
          subPatterns(std::move(subPatterns)) {

}

FunctionImplASTNode::FunctionImplASTNode(size_t lineNum, size_t fileIndex, std::unique_ptr<ExpressionASTNode> &&body)
        : ASTNode(lineNum, fileIndex), body(std::move(body)) {

}

PrefixFunctionImplASTNode::PrefixFunctionImplASTNode(size_t lineNum, size_t fileIndex,
                                                     std::unique_ptr<ExpressionASTNode> &&body)
        : FunctionImplASTNode(lineNum, fileIndex, std::move(body)) {

}

PrefixFunctionImplASTNode::PrefixFunctionImplASTNode(size_t lineNum, size_t fileIndex,
                                                     std::unique_ptr<ExpressionASTNode> &&body,
                                                     std::vector<std::unique_ptr<PatternASTNode>> &&patterns)
        : FunctionImplASTNode(lineNum, fileIndex, std::move(body)), patterns(std::move(patterns)) {

}

void PrefixFunctionImplASTNode::generate(const CodeGenerator &generator) const {
    generator.generate<PrefixFunctionImplASTNode>(View { .patterns = patterns, .body = body });
}

InfixFunctionImplASTNode::InfixFunctionImplASTNode(size_t lineNum, size_t fileIndex,
                                                   std::unique_ptr<ExpressionASTNode> &&body,
                                                   std::unique_ptr<PatternASTNode> &&left,
                                                   std::unique_ptr<PatternASTNode> &&right)
        : FunctionImplASTNode(lineNum, fileIndex, std::move(body)), lhs(std::move(left)), rhs(std::move(right)) {

}

void InfixFunctionImplASTNode::generate(const CodeGenerator &generator) const {
    generator.generate<InfixFunctionImplASTNode>(View { .lhs = lhs, .rhs = rhs, .body = body });
}

FunctionDefinitionASTNode::FunctionDefinitionASTNode(std::unique_ptr<FunctionDeclASTNode> &&declaration)
        : declaration(std::move(declaration)) {

}

void FunctionDefinitionASTNode::addImplementation(std::unique_ptr<FunctionImplASTNode> &&implementation) {
    implementations.push_back(std::move(implementation));
}

void FunctionDefinitionASTNode::generate(const CodeGenerator &generator) const {
    std::vector<std::unique_ptr<llvm::Function>> impls;
    impls.reserve(implementations.size());

    TypeInstanceASTNode *typeNode = declaration->functionType().get();
    std::vector<llvm::Type *> argTypes;

    /*while (true) {
        if (typeNode->typeUsage() == TypeUsage::Function) {
            FunctionTypeInstanceASTNode *fTypeNode = dynamic_cast<FunctionTypeInstanceASTNode *>(typeNode);
            typeNode = fTypeNode->right().get();
            // argTypes.push_back(fTypeNode->left().)
        } else {

        }
        switch (typeNode->typeUsage()) {
            case TypeUsage::Infix:
                break;
            case TypeUsage::Prefix:
                break;
            case TypeUsage::Function:
                break;
            case TypeUsage::Polymorphic:
                break;
        }
    }*/

    // llvm::FunctionType *ty = llvm::FunctionType::get(llvm::Type::getPo);

    /*std::transform(implementations.begin(), implementations.end(), impls.begin(),
                   [ty, &context](const std::unique_ptr<FunctionImplASTNode> &impl) {
        return impl->generate(context,std::unique_ptr<llvm::Function>(llvm::Function::Create(
                ty, llvm::Function::ExternalLinkage,
                "", context.module("").get()
        )));
    });*/
}

