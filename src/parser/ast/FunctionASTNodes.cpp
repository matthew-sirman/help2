//
// Created by matthew on 28/11/2020.
//

#include "../../../include/parser/ast/FunctionASTNodes.h"
#include "../../../include/compiler/CodeGenerator.h"

#include <utility>
#include <llvm/IR/Type.h>


FunctionDeclASTNode::FunctionDeclASTNode(size_t lineNum, size_t fileIndex, std::string name,
                                         PrerequisiteList &&prerequisites,
                                         std::unique_ptr<TypeInstanceASTNode> &&funcType)
        : ASTNode(lineNum, fileIndex), funcName(std::move(name)), funcType(std::move(funcType)),
          prerequisites(std::move(prerequisites)), isVirtualFunc(this->prerequisites.empty()) {
    // Set to virtual automatically if there are no prerequisites
}

PrefixFunctionDeclASTNode::PrefixFunctionDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                                                     PrerequisiteList &&prerequisites,
                                                     std::unique_ptr<TypeInstanceASTNode> &&funcType)
        : FunctionDeclASTNode(lineNum, fileIndex, name, std::move(prerequisites), std::move(funcType)) {

}

size_t PrefixFunctionDeclASTNode::maxArgs() const {
    // We know there is at least one function type
    size_t m = 1;
    const FunctionTypeInstanceASTNode *curType = dynamic_cast<const FunctionTypeInstanceASTNode *>(funcType.get());
    // For as long as the right type of the function type is a function type, increment the counter and recurse
    while (curType->right().typeUsage() == TypeUsage::Function) {
        m++;
        curType = dynamic_cast<const FunctionTypeInstanceASTNode *>(&curType->right());
    }
    return m;
}

// TODO: Move this
constexpr int defaultPrecedence = 5;

InfixFunctionDeclASTNode::InfixFunctionDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                                                   PrerequisiteList &&prerequisites,
                                                   std::unique_ptr<TypeInstanceASTNode> &&funcType,
                                                   Associativity assoc)
        : FunctionDeclASTNode(lineNum, fileIndex, name, std::move(prerequisites), std::move(funcType)),
          precedence(defaultPrecedence), assoc(assoc) {

}

ValueFunctionDeclASTNode::ValueFunctionDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                                                   PrerequisiteList &&prerequisites,
                                                   std::unique_ptr<TypeInstanceASTNode> &&valueType)
        : FunctionDeclASTNode(lineNum, fileIndex, name, std::move(prerequisites), std::move(valueType)) {

}

FunctionImplASTNode::FunctionImplASTNode(size_t lineNum, size_t fileIndex,
                                         std::vector<std::unique_ptr<PatternASTNode>> &&patterns,
                                         std::unique_ptr<ExpressionASTNode> &&body)
        : ASTNode(lineNum, fileIndex), patterns(std::move(patterns)), body(std::move(body)) {
    for (const std::unique_ptr<PatternASTNode> &pattern : this->patterns) {
        patternList.emplace_back(*pattern);
    }
}

llvm::BasicBlock *
FunctionImplASTNode::generate(FunctionCodeGenerator &generator, unsigned int variant, llvm::Function *parentFunction) {
    return nullptr;
}

/*
FunctionImplASTNode::FunctionImplASTNode(size_t lineNum, size_t fileIndex)
        : ASTNode(lineNum, fileIndex), body(nullptr) {

}

PrefixFunctionImplASTNode::PrefixFunctionImplASTNode(size_t lineNum, size_t fileIndex)
        : FunctionImplASTNode(lineNum, fileIndex) {

}

PrefixFunctionImplASTNode::PrefixFunctionImplASTNode(size_t lineNum, size_t fileIndex,
                                                     std::vector<std::unique_ptr<PatternASTNode>> &&patterns)
        : FunctionImplASTNode(lineNum, fileIndex), patterns(std::move(patterns)) {

}

llvm::BasicBlock *PrefixFunctionImplASTNode::generate(FunctionCodeGenerator &generator, unsigned variant,
                                                      llvm::Function *parentFunction) const {
    std::vector<PatternASTNode *> unboxedPatterns(patterns.size());
    std::transform(
            patterns.begin(), patterns.end(), unboxedPatterns.begin(),
            [](const std::unique_ptr<PatternASTNode> &pattern) {
                return pattern.get();
            }
    );
    return generator.generateImplementationBlock(View{
            .patterns = unboxedPatterns,
            .body = body,
            .variant = variant,
            .parentFunction = parentFunction
    });
}

InfixFunctionImplASTNode::InfixFunctionImplASTNode(size_t lineNum, size_t fileIndex,
                                                   std::unique_ptr<PatternASTNode> &&left,
                                                   std::unique_ptr<PatternASTNode> &&right)
        : FunctionImplASTNode(lineNum, fileIndex), lhs(std::move(left)), rhs(std::move(right)) {

}

llvm::BasicBlock *InfixFunctionImplASTNode::generate(FunctionCodeGenerator &generator, unsigned variant,
                                                     llvm::Function *parentFunction) const {
    return generator.generateImplementationBlock(View{
            .patterns = {lhs.get(), rhs.get()},
            .body = body,
            .variant = variant,
            .parentFunction = parentFunction
    });
}
 */

FunctionDefinitionASTNode::FunctionDefinitionASTNode(std::unique_ptr<FunctionDeclASTNode> &&declaration)
        : declaration(std::move(declaration)) {

}

void FunctionDefinitionASTNode::addImplementation(std::unique_ptr<FunctionImplASTNode> &&implementation) {
    implementationList.emplace_back(*implementation);
    implementations.push_back(std::move(implementation));
}

llvm::Function *
FunctionDefinitionASTNode::generate(FunctionCodeGenerator &generator, const BindingMap &bindingMap) const {
    return generator.generateDefinition(View{
            .declaration = declaration,
            .implementations = implementations,
            .hasOverloads = hasOverloads()
    }, bindingMap);
}

