//
// Created by matthew on 28/11/2020.
//

#include "../../../include/parser/ast/TypeASTNodes.h"
#include "../../../include/compiler/Compiler.h"

#include <iostream>

DataConstructorASTNode::DataConstructorASTNode(size_t lineNum, size_t fileIndex,
                                               const std::unique_ptr<TypeDeclASTNode> &type,
                                               std::string constructorName)
        : ASTNode(lineNum, fileIndex), type(type), constructorName(std::move(constructorName)) {

}

PrefixDataConstructorASTNode::PrefixDataConstructorASTNode(size_t lineNum, size_t fileIndex,
                                                           const std::unique_ptr<TypeDeclASTNode> &type,
                                                           const std::string &constructorName)
        : DataConstructorASTNode(lineNum, fileIndex, type, constructorName) {

}

PrefixDataConstructorASTNode::PrefixDataConstructorASTNode(size_t lineNum, size_t fileIndex,
                                                           const std::unique_ptr<TypeDeclASTNode> &type,
                                                           const std::string &constructorName,
                                                           std::vector<std::unique_ptr<TypeInstanceASTNode>> &&parameters)
        : DataConstructorASTNode(lineNum, fileIndex, type, constructorName), params(std::move(parameters)) {

}

void PrefixDataConstructorASTNode::generate(const CompileContext &context) {

}

InfixDataConstructorASTNode::InfixDataConstructorASTNode(size_t lineNum, size_t fileIndex,
                                                         const std::unique_ptr<TypeDeclASTNode> &type,
                                                         const std::string &constructorName,
                                                         std::unique_ptr<TypeInstanceASTNode> &&leftParam,
                                                         std::unique_ptr<TypeInstanceASTNode> &&rightParam)
        : DataConstructorASTNode(lineNum, fileIndex, type, constructorName), lhs(std::move(leftParam)),
          rhs(std::move(rightParam)) {

}

void InfixDataConstructorASTNode::generate(const CompileContext &context) {

}

TypeDeclASTNode::TypeDeclASTNode(size_t lineNum, size_t fileIndex, std::string name)
        : ASTNode(lineNum, fileIndex), name(std::move(name)) {

}

TypeDeclASTNode::TypeDeclASTNode(size_t lineNum, size_t fileIndex, std::string name,
                                 std::vector<std::unique_ptr<DataConstructorASTNode>> &&constructors)
        : ASTNode(lineNum, fileIndex), name(std::move(name)), constructors(std::move(constructors)) {

}

void TypeDeclASTNode::addDataConstructor(std::unique_ptr<DataConstructorASTNode> &&constructor) {
    constructors.push_back(std::move(constructor));
}

void TypeDeclASTNode::generate(const CompileContext &context) const {
    // If there are 1 or 0 data constructors, we have a special case
    if (constructors.empty()) {
        // The type is not constructable, so simply return
        return;
    }

    if (constructors.size() == 1) {
        // The type only has a single constructor, so we don't need to tag it
        context
    }

    // Otherwise, create a master type and a tagged union for each ctor

    unsigned char tagSize;

    if (constructors.size() < 256) {
        tagSize = 8;
    } else if (constructors.size() < 65536) {
        tagSize = 16;
    }

    std::for_each(constructors.begin(), constructors.end(),
                  [&context](const std::unique_ptr<DataConstructorASTNode> &cons) {
                      cons->generate(context);
                  }
    );
}

PrefixTypeDeclASTNode::PrefixTypeDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                                             std::vector<std::string> &&typeVariables)
        : TypeDeclASTNode(lineNum, fileIndex, name), typeConstructorParameters(std::move(typeVariables)) {

}

PrefixTypeDeclASTNode::PrefixTypeDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                                             std::vector<std::string> &&typeVariables,
                                             std::vector<std::unique_ptr<DataConstructorASTNode>> &&constructors)
        : TypeDeclASTNode(lineNum, fileIndex, name, std::move(constructors)),
          typeConstructorParameters(std::move(typeVariables)) {

}

InfixTypeDeclASTNode::InfixTypeDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name, std::string left,
                                           std::string right)
        : TypeDeclASTNode(lineNum, fileIndex, name), leftParameter(std::move(left)), rightParameter(std::move(right)) {

}

InfixTypeDeclASTNode::InfixTypeDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name, std::string left,
                                           std::string right,
                                           std::vector<std::unique_ptr<DataConstructorASTNode>> &&constructors)
        : TypeDeclASTNode(lineNum, fileIndex, name, std::move(constructors)), leftParameter(std::move(left)),
          rightParameter(std::move(right)) {

}

TypeInstanceASTNode::TypeInstanceASTNode(size_t lineNum, size_t fileIndex, std::string name)
        : ASTNode(lineNum, fileIndex), name(std::move(name)) {

}

PolymorphicTypeInstanceASTNode::PolymorphicTypeInstanceASTNode(size_t lineNum, size_t fileIndex,
                                                               const std::string &name)
        : TypeInstanceASTNode(lineNum, fileIndex, name) {

}

llvm::Type *PolymorphicTypeInstanceASTNode::llvmType() const {
    std::cerr << "DEVELOPER: Tried to extract concrete type from polymorphic type" << std::endl;
    throw;
}

bool PolymorphicTypeInstanceASTNode::isPolymorphic() const {
    return true;
}

InfixTypeInstanceASTNode::InfixTypeInstanceASTNode(size_t lineNum, size_t fileIndex, const std::string &name)
        : TypeInstanceASTNode(lineNum, fileIndex, name) {

}

void InfixTypeInstanceASTNode::bindLeft(std::unique_ptr<TypeInstanceASTNode> &&param) {
    lhs = std::move(param);
}

void InfixTypeInstanceASTNode::bindRight(std::unique_ptr<TypeInstanceASTNode> &&param) {
    rhs = std::move(param);
}

llvm::Type *InfixTypeInstanceASTNode::llvmType() const {
    llvm::PointerType::get;
}

bool InfixTypeInstanceASTNode::isPolymorphic() const {
    return left()->isPolymorphic() || right()->isPolymorphic();
}

FunctionTypeInstanceASTNode::FunctionTypeInstanceASTNode(size_t lineNum, size_t fileIndex, const std::string &name)
        : InfixTypeInstanceASTNode(lineNum, fileIndex, name) {

}

PrefixTypeInstanceASTNode::PrefixTypeInstanceASTNode(size_t lineNum, size_t fileIndex, const std::string &name)
        : TypeInstanceASTNode(lineNum, fileIndex, name) {

}

llvm::Type *FunctionTypeInstanceASTNode::llvmType() const {
    return nullptr;
}

void PrefixTypeInstanceASTNode::bindParameter(std::unique_ptr<TypeInstanceASTNode> &&param) {
    params.push_back(std::move(param));
}

llvm::Type *PrefixTypeInstanceASTNode::llvmType() const {
    return nullptr;
}

bool PrefixTypeInstanceASTNode::isPolymorphic() const {
    return std::any_of(parameters().begin(), parameters().end(),
                       [](const std::unique_ptr<TypeInstanceASTNode> &param) {
                           return param->isPolymorphic();
                       });
}
