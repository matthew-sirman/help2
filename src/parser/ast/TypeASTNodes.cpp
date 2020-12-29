//
// Created by matthew on 28/11/2020.
//

#include "../../../include/parser/ast/TypeASTNodes.h"
#include "../../../include/compiler/CodeGenerator.h"

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

llvm::Type *
PrefixDataConstructorASTNode::generate(TypeCodeGenerator &generator, const std::string &name, unsigned char tagSize,
                                       unsigned short tag, const BindingMap &bindingMap) const {
    std::vector<TypeInstanceASTNode *> unboxedParams(params.size());
    std::transform(params.begin(), params.end(), unboxedParams.begin(),
                   [](const std::unique_ptr<TypeInstanceASTNode> &param) {
                       return param.get();
                   });

    return generator.generate<DataConstructorASTNode>(View{
            .type = type,
            .constructorName = constructorName,
            .params = unboxedParams,
            .uniqueTypeName = name,
            .tagSize = tagSize,
            .tag = tag,
            .bindingMap = bindingMap
    });
}

InfixDataConstructorASTNode::InfixDataConstructorASTNode(size_t lineNum, size_t fileIndex,
                                                         const std::unique_ptr<TypeDeclASTNode> &type,
                                                         const std::string &constructorName,
                                                         std::unique_ptr<TypeInstanceASTNode> &&leftParam,
                                                         std::unique_ptr<TypeInstanceASTNode> &&rightParam)
        : DataConstructorASTNode(lineNum, fileIndex, type, constructorName), lhs(std::move(leftParam)),
          rhs(std::move(rightParam)) {

}

llvm::Type *
InfixDataConstructorASTNode::generate(TypeCodeGenerator &generator, const std::string &name, unsigned char tagSize,
                                      unsigned short tag, const BindingMap &bindingMap) const {
    return generator.generate<DataConstructorASTNode>(View{
            .type = type,
            .constructorName = constructorName,
            .params = {lhs.get(), rhs.get()},
            .uniqueTypeName = name,
            .tagSize = tagSize,
            .tag = tag,
            .bindingMap = bindingMap
    });
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

llvm::Type *PrefixTypeDeclASTNode::generate(TypeCodeGenerator &generator) const {
    std::vector<llvm::Type *> bindings;
    return generate(generator, bindings);
}

llvm::Type *
PrefixTypeDeclASTNode::generate(TypeCodeGenerator &generator, const std::vector<llvm::Type *> &bindings) const {
    return generator.generate<TypeDeclASTNode>(View{
            .fileIndex = fileIndex(),
            .name = name,
            .constructors = constructors,
            .typeConstructorParameters = typeConstructorParameters,
            .bindings = bindings
    });
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

llvm::Type *
InfixTypeDeclASTNode::generate(TypeCodeGenerator &generator, llvm::Type *leftBinding, llvm::Type *rightBinding) const {
    return generator.generate<TypeDeclASTNode>(View{
            .fileIndex = fileIndex(),
            .name = name,
            .constructors = constructors,
            .typeConstructorParameters = {leftParameter, rightParameter},
            .bindings = {leftBinding, rightBinding}
    });
}

TypeInstanceASTNode::TypeInstanceASTNode() = default;

UserTypeInstanceASTNode::UserTypeInstanceASTNode(size_t lineNum, size_t fileIndex, std::string name)
        : ASTNode(lineNum, fileIndex), name(std::move(name)) {

}

PolymorphicTypeInstanceASTNode::PolymorphicTypeInstanceASTNode(size_t lineNum, size_t fileIndex,
                                                               const std::string &name)
        : UserTypeInstanceASTNode(lineNum, fileIndex, name) {

}

bool PolymorphicTypeInstanceASTNode::isPolymorphic() const {
    return true;
}

llvm::Type *
PolymorphicTypeInstanceASTNode::instantiate(TypeCodeGenerator &generator, const BindingMap &bindingMap) const {
    if (bindingMap.contains(name)) {
        return bindingMap.at(name);
    }
    std::cerr << "DEVELOPER: Attempted to generate dependencies on polymorphic type instance." << std::endl;
    throw;
}

InfixTypeInstanceASTNode::InfixTypeInstanceASTNode(size_t lineNum, size_t fileIndex, const std::string &name)
        : UserTypeInstanceASTNode(lineNum, fileIndex, name) {

}

void InfixTypeInstanceASTNode::bindLeft(std::unique_ptr<TypeInstanceASTNode> &&param) {
    lhs = std::move(param);
}

void InfixTypeInstanceASTNode::bindRight(std::unique_ptr<TypeInstanceASTNode> &&param) {
    rhs = std::move(param);
}

bool InfixTypeInstanceASTNode::isPolymorphic() const {
    return left()->isPolymorphic() || right()->isPolymorphic();
}

llvm::Type *InfixTypeInstanceASTNode::instantiate(TypeCodeGenerator &generator, const BindingMap &bindingMap) const {
    return generator.generate<InfixTypeInstanceASTNode>(View{
            .name = name,
            .lhs = lhs,
            .rhs = rhs,
            .bindingMap = bindingMap
    });
}

FunctionTypeInstanceASTNode::FunctionTypeInstanceASTNode(size_t lineNum, size_t fileIndex, const std::string &name)
        : InfixTypeInstanceASTNode(lineNum, fileIndex, name) {

}

llvm::Type *FunctionTypeInstanceASTNode::instantiate(TypeCodeGenerator &generator, const BindingMap &bindingMap) const {
    return generator.generate<FunctionTypeInstanceASTNode>(View{
            .name = name,
            .lhs = lhs,
            .rhs = rhs,
            .bindingMap = bindingMap
    });
}

PrefixTypeInstanceASTNode::PrefixTypeInstanceASTNode(size_t lineNum, size_t fileIndex, const std::string &name)
        : UserTypeInstanceASTNode(lineNum, fileIndex, name) {

}

void PrefixTypeInstanceASTNode::bindParameter(std::unique_ptr<TypeInstanceASTNode> &&param) {
    params.push_back(std::move(param));
}

bool PrefixTypeInstanceASTNode::isPolymorphic() const {
    return std::any_of(parameters().begin(), parameters().end(),
                       [](const std::unique_ptr<TypeInstanceASTNode> &param) {
                           return param->isPolymorphic();
                       });
}

llvm::Type *PrefixTypeInstanceASTNode::instantiate(TypeCodeGenerator &generator, const BindingMap &bindingMap) const {
    return generator.generate<PrefixTypeInstanceASTNode>(View{
            .name = name,
            .params = params,
            .bindingMap = bindingMap
    });
}

PrimitiveTypeInstanceASTNode::PrimitiveTypeInstanceASTNode(const PrimitiveType &type)
        : type(type) {

}

llvm::Type *
PrimitiveTypeInstanceASTNode::instantiate(TypeCodeGenerator &generator, const BindingMap &) const {
    return generator.generate<PrimitiveTypeInstanceASTNode>(View{
        .type = type
    });
}
