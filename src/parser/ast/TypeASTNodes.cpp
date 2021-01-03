//
// Created by matthew on 28/11/2020.
//

#include "../../../include/parser/ast/TypeASTNodes.h"
#include "../../../include/compiler/CodeGenerator.h"

#include <iostream>

DataConstructorASTNode::DataConstructorASTNode(size_t lineNum, size_t fileIndex,
                                               const TypeDeclASTNode &type,
                                               std::string constructorName)
        : ASTNode(lineNum, fileIndex), type(type), constructorName(std::move(constructorName)) {

}

PrefixDataConstructorASTNode::PrefixDataConstructorASTNode(size_t lineNum, size_t fileIndex,
                                                           const TypeDeclASTNode &type,
                                                           const std::string &constructorName)
        : DataConstructorASTNode(lineNum, fileIndex, type, constructorName) {

}

PrefixDataConstructorASTNode::PrefixDataConstructorASTNode(size_t lineNum, size_t fileIndex,
                                                           const TypeDeclASTNode &type,
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
                                                         const TypeDeclASTNode &type,
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

bool operator==(const TypeInstanceASTNode &lhs, const TypeInstanceASTNode &rhs) {
    if (lhs.typeUsage() != rhs.typeUsage()) {
        return false;
    }
    switch (lhs.typeUsage()) {
        case TypeUsage::Infix: {
            const InfixTypeInstanceASTNode
                    &infixL = dynamic_cast<const InfixTypeInstanceASTNode &>(lhs),
                    &infixR = dynamic_cast<const InfixTypeInstanceASTNode &>(rhs);

            return infixL.typeName() == infixR.typeName() &&
                   *infixL.left() == *infixR.left()
                   && *infixL.right() == *infixR.right();
        }
        case TypeUsage::Prefix: {
            const PrefixTypeInstanceASTNode
                    &prefixL = dynamic_cast<const PrefixTypeInstanceASTNode &>(lhs),
                    &prefixR = dynamic_cast<const PrefixTypeInstanceASTNode &>(rhs);

            if (prefixL.typeName() != prefixR.typeName()) {
                return false;
            }

            const std::vector<std::unique_ptr<TypeInstanceASTNode>>
                    &lSubTerms = prefixL.parameters(),
                    &rSubTerms = prefixR.parameters();

            if (lSubTerms.size() != rSubTerms.size()) {
                return false;
            }

            for (size_t i = 0; i < lSubTerms.size(); i++) {
                if (*lSubTerms[i] != *rSubTerms[i]) {
                    return false;
                }
            }
            return true;
        }
        case TypeUsage::Function: {
            const FunctionTypeInstanceASTNode
                    &funcL = dynamic_cast<const FunctionTypeInstanceASTNode &>(lhs),
                    &funcR = dynamic_cast<const FunctionTypeInstanceASTNode &>(rhs);

            return *funcL.left() == *funcR.left() && *funcL.right() == *funcR.right();
        }
        case TypeUsage::Polymorphic:
            // Polymorphic type variables are always considered equal, as they can be anything
            return true;
        case TypeUsage::Primitive: {
            const PrimitiveTypeInstanceASTNode
                    &primL = dynamic_cast<const PrimitiveTypeInstanceASTNode &>(lhs),
                    &primR = dynamic_cast<const PrimitiveTypeInstanceASTNode &>(rhs);

            return primL.primitiveType().name() == primR.primitiveType().name();
        }
    }
}

bool operator!=(const TypeInstanceASTNode &lhs, const TypeInstanceASTNode &rhs) {
    return !(lhs == rhs);
}

UserTypeInstanceASTNode::UserTypeInstanceASTNode(size_t lineNum, size_t fileIndex, std::string name)
        : ASTNode(lineNum, fileIndex), name(std::move(name)) {

}

void PrefixTypeInstanceInterface::bindParameter(std::unique_ptr<TypeInstanceASTNode> &&parameter) {
    params.push_back(std::move(parameter));
}

PolymorphicTypeInstanceASTNode::PolymorphicTypeInstanceASTNode(size_t lineNum, size_t fileIndex,
                                                               const std::string &name)
        : UserTypeInstanceASTNode(lineNum, fileIndex, name) {

}

bool PolymorphicTypeInstanceASTNode::isPolymorphic() const {
    return true;
}

bool PolymorphicTypeInstanceASTNode::containsClosures() const {
    return false;
}

bool PolymorphicTypeInstanceASTNode::containsFunctionType() const {
    return false;
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

bool InfixTypeInstanceASTNode::containsClosures() const {
    return left()->containsClosures() || right()->containsClosures();
}

bool InfixTypeInstanceASTNode::containsFunctionType() const {
    return left()->containsFunctionType() || right()->containsFunctionType();
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

bool FunctionTypeInstanceASTNode::containsClosures() const {

}

bool FunctionTypeInstanceASTNode::containsFunctionType() const {
    return InfixTypeInstanceASTNode::containsFunctionType();
}

std::size_t FunctionTypeInstanceASTNode::functionDepth() const {
    return right()->functionDepth() + 1;
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

bool PrefixTypeInstanceASTNode::isPolymorphic() const {
    return std::any_of(parameters().begin(), parameters().end(),
                       [](const std::unique_ptr<TypeInstanceASTNode> &param) {
                           return param->isPolymorphic();
                       });
}

bool PrefixTypeInstanceASTNode::containsClosures() const {
    return false;
}

bool PrefixTypeInstanceASTNode::containsFunctionType() const {
    return false;
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

TypeclassASTNode::TypeclassASTNode(size_t lineNum, size_t fileIndex, std::string typeclassName, std::string variable,
                                   std::vector<std::unique_ptr<TypeclassInstanceASTNode>> &&prerequisites)
        : ASTNode(lineNum, fileIndex), typeclassName(std::move(typeclassName)),
          variable(std::move(variable)), prerequisites(std::move(prerequisites)) {

}

void TypeclassASTNode::addMethod(std::unique_ptr<FunctionDeclASTNode> &&method) {
    tcMethodNames.insert(method->name());
    typeclassMethods[method->name()] = std::move(method);
}

bool TypeclassASTNode::hasMethod(const std::string &methodName) const {
    return typeclassMethods.contains(methodName);
}

TypeclassInstanceASTNode::TypeclassInstanceASTNode(size_t lineNum, size_t fileIndex, const TypeclassASTNode &typeclass,
                                                   std::unique_ptr<TypeInstanceASTNode> &&typeInstance)
        : ASTNode(lineNum, fileIndex), typeclassReference(typeclass), typeInstance(std::move(typeInstance)) {

}

TypeclassInstanceImplASTNode::TypeclassInstanceImplASTNode(size_t lineNum, size_t fileIndex,
                                                           std::unique_ptr<TypeclassInstanceASTNode> &&typeclass,
                                                           PrerequisiteList &&prerequisites)
        : ASTNode(lineNum, fileIndex), typeclassInstance(std::move(typeclass)),
          prerequisites(std::move(prerequisites)) {

}

TypeclassInstanceImplASTNode &TypeclassASTNode::addInstance(std::unique_ptr<TypeclassInstanceImplASTNode> instance) {
    for (const std::unique_ptr<TypeclassInstanceImplASTNode> &inst : instances) {
        if (*inst->typeclassInstance == *instance->typeclassInstance) {
            return *inst;
        }
    }
    // If the instance wasn't already present, add it and return
    instances.push_back(std::move(instance));
    return *instances.back();
}

bool operator==(const TypeclassInstanceASTNode &lhs, const TypeclassInstanceASTNode &rhs) {
    // Check the typeclasses by pointer (there should only ever be a single instance)
    // Check the type instances by equality
    return &lhs.typeclassReference == &rhs.typeclassReference && *lhs.typeInstance == *rhs.typeInstance;
}

bool operator!=(const TypeclassInstanceASTNode &lhs, const TypeclassInstanceASTNode &rhs) {
    return !(lhs == rhs);
}

bool TypeclassInstanceImplASTNode::fullyImplemented() const {
    if (implementations.size() != typeclassInstance->typeclass().methodCount()) {
        return false;
    }
    return std::all_of(implementations.begin(), implementations.end(),
                       [this](const std::pair<const std::string, std::unique_ptr<FunctionImplASTNode>> &impl) {
                           return typeclassInstance->typeclass().hasMethod(impl.first);
                       });
}

void TypeclassInstanceImplASTNode::addImplementation(const std::string &name,
                                                     std::unique_ptr<FunctionImplASTNode> &&implementation) {
    implementations[name] = std::move(implementation);
}
