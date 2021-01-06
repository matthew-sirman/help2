//
// Created by matthew on 28/11/2020.
//

#include "../../../include/parser/ast/TypeASTNodes.h"
#include "../../../include/compiler/CodeGenerator.h"

#include <iostream>
#include <numeric>
#include <sstream>

DataConstructorASTNode::DataConstructorASTNode(size_t lineNum, size_t fileIndex, const TypeDeclASTNode &type,
                                               std::string constructorName,
                                               std::vector<std::unique_ptr<TypeInstanceASTNode>> &&parameters)
        : ASTNode(lineNum, fileIndex), type(type), constructorName(std::move(constructorName)),
          consUsage(ConstructorUsage::Prefix) {
    // Create the return type of the constructor
    std::unique_ptr<TypeInstanceASTNode> constructorType = type.createInstance(*this);
    // Now, in reverse order, move each constructor parameter type to be a function pointing to the current
    // return type
    for (std::vector<std::unique_ptr<TypeInstanceASTNode>>::reverse_iterator it = parameters.rbegin();
         it != parameters.rend(); ++it) {
        constructorType = std::make_unique<FunctionTypeInstanceASTNode>(
                lineNum, fileIndex, std::move(*it), std::move(constructorType)
        );
    }
    // The prerequisite list is always empty, but we still need to supply one
    PrerequisiteList prerequisites;
    if (parameters.empty()) {
        function = std::make_unique<FunctionDefinitionASTNode>(std::make_unique<ValueFunctionDeclASTNode>(
                lineNum, fileIndex, constructorName, std::move(prerequisites), constructorType->clone()
        ));
    } else {
        function = std::make_unique<FunctionDefinitionASTNode>(std::make_unique<PrefixFunctionDeclASTNode>(
                lineNum, fileIndex, constructorName, std::move(prerequisites), constructorType->clone()
        ));
    }
}

DataConstructorASTNode::DataConstructorASTNode(size_t lineNum, size_t fileIndex, const TypeDeclASTNode &type,
                                               std::string constructorName,
                                               std::unique_ptr<TypeInstanceASTNode> &&leftParam,
                                               std::unique_ptr<TypeInstanceASTNode> &&rightParam)
        : ASTNode(lineNum, fileIndex), type(type), constructorName(std::move(constructorName)),
          consUsage(ConstructorUsage::Infix) {
    std::unique_ptr<TypeInstanceASTNode> constructorType = type.createInstance(*this);
    constructorType = std::make_unique<FunctionTypeInstanceASTNode>(
            lineNum, fileIndex, std::move(rightParam), std::move(constructorType)
    );
    constructorType = std::make_unique<FunctionTypeInstanceASTNode>(
            lineNum, fileIndex, std::move(leftParam), std::move(constructorType)
    );
    // The prerequisite list is always empty, but we still need to supply one
    PrerequisiteList prerequisites;
    function = std::make_unique<FunctionDefinitionASTNode>(std::make_unique<InfixFunctionDeclASTNode>(
            lineNum, fileIndex, constructorName, std::move(prerequisites), constructorType->clone(),
            Associativity::Right
    ));
}

size_t DataConstructorASTNode::args() const {
    return function->decl().maxArgs();
}

llvm::Type *
DataConstructorASTNode::generate(TypeCodeGenerator &generator, const std::string &uniqueTypeName, unsigned char tagSize,
                                 unsigned short tag, const BindingMap &bindingMap) const {
    return nullptr;
}

/*
PrefixDataConstructorASTNode::PrefixDataConstructorASTNode(size_t lineNum, size_t fileIndex,
                                                           const TypeDeclASTNode &type,
                                                           const std::string &constructorName)
        : DataConstructorASTNode(lineNum, fileIndex, type, constructorName) {

}

PrefixDataConstructorASTNode::PrefixDataConstructorASTNode(size_t lineNum, size_t fileIndex,
                                                           const TypeDeclASTNode &type,
                                                           const std::string &constructorName,
                                                           std::vector<std::unique_ptr<TypeInstanceASTNode>> &&parameters)
        : DataConstructorASTNode(lineNum, fileIndex, type, constructorName) {
    for (const std::unique_ptr<TypeInstanceASTNode> &param : params) {
        paramList.emplace_back(*param);
    }
}

std::unique_ptr<FunctionDeclASTNode> PrefixDataConstructorASTNode::createConstructorFunction() const {
    // We just need to make a function declaration, no actual implementations

    // The prerequisite list is always empty, but we still need to supply one
    PrerequisiteList prerequisites;

    // We need to create an instance of the type this constructor will return
    std::unique_ptr<TypeInstanceASTNode> returnType;

    switch (type.typeUsage()) {
        case TypeUsage::Infix: {
            const InfixTypeDeclASTNode &infixType = dynamic_cast<const InfixTypeDeclASTNode &>(type);
            std::unique_ptr<InfixTypeInstanceASTNode> infixReturnType = std::make_unique<InfixTypeInstanceASTNode>(
                    lineNumber(), fileIndex(), infixType.typeName()
            );
            // The arguments are necessarily going to be polymorphic type instances
            infixReturnType->bindLeft(std::make_unique<PolymorphicTypeInstanceASTNode>(
                    lineNumber(), fileIndex(), infixType.leftParameterName()
            ));
            infixReturnType->bindRight(std::make_unique<PolymorphicTypeInstanceASTNode>(
                    lineNumber(), fileIndex(), infixType.rightParameterName()
            ));
            returnType = std::move(infixReturnType);
            break;
        }
        case TypeUsage::Prefix: {
            const PrefixTypeDeclASTNode &prefixType = dynamic_cast<const PrefixTypeDeclASTNode &>(type);
            std::unique_ptr<PrefixTypeInstanceASTNode> prefixReturnType = std::make_unique<PrefixTypeInstanceASTNode>(
                    lineNumber(), fileIndex(), prefixType.typeName()
            );
            for (const std::string &param : prefixType.parameterNames()) {
                prefixReturnType->bindParameter(std::make_unique<PolymorphicTypeInstanceASTNode>(
                        lineNumber(), fileIndex(), param
                ));
            }
            returnType = std::move(prefixReturnType);
            break;
        }
        case TypeUsage::Function:
        case TypeUsage::Polymorphic:
        case TypeUsage::Primitive:
            // We should never have a constructor which thinks its parent type is one of these!
            std::cerr << "DEVELOPER: Attempted to create constructor for function, polymorphic or primitive type!"
                      << std::endl;
            throw;
    }

    // If there are no parameters, generate as a value function
    if (params.empty()) {
        return std::make_unique<ValueFunctionDeclASTNode>(
                lineNumber(), fileIndex(), constructorName, std::move(prerequisites), std::move(returnType)
        );
    }

    // Otherwise, we need to build up the entire function type, which we build up from the back of the param list
    for (RefList<TypeInstanceASTNode>::const_reverse_iterator it = paramList.rbegin(); it != paramList.rend(); ++it) {
        returnType = std::make_unique<FunctionTypeInstanceASTNode>(
                lineNumber(), fileIndex(),
        )
    }
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
 */

TypeDeclASTNode::TypeDeclASTNode(size_t lineNum, size_t fileIndex, std::string name)
        : ASTNode(lineNum, fileIndex), name(std::move(name)) {

}

TypeDeclASTNode::TypeDeclASTNode(size_t lineNum, size_t fileIndex, std::string name,
                                 std::vector<std::unique_ptr<DataConstructorASTNode>> &&constructors)
        : ASTNode(lineNum, fileIndex), name(std::move(name)), constructors(std::move(constructors)) {

}

void TypeDeclASTNode::addDataConstructor(std::unique_ptr<DataConstructorASTNode> &&constructor) {
    constructorList.emplace_back(*constructor);
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

std::unique_ptr<TypeInstanceASTNode> PrefixTypeDeclASTNode::createInstance(const ASTNode &at) const {
    std::unique_ptr<PrefixTypeInstanceASTNode> prefix = std::make_unique<PrefixTypeInstanceASTNode>(
            at.lineNumber(), at.fileIndex(), name
    );
    for (const std::string &name : typeConstructorParameters) {
        prefix->bindParameter(std::make_unique<PolymorphicTypeInstanceASTNode>(
                at.lineNumber(), at.fileIndex(), name
        ));
    }
    return prefix;
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

std::unique_ptr<TypeInstanceASTNode> InfixTypeDeclASTNode::createInstance(const ASTNode &at) const {
    return std::make_unique<InfixTypeInstanceASTNode>(
            at.lineNumber(), at.fileIndex(), name,
            std::make_unique<PolymorphicTypeInstanceASTNode>(at.lineNumber(), at.fileIndex(), leftParameter),
            std::make_unique<PolymorphicTypeInstanceASTNode>(at.lineNumber(), at.fileIndex(), rightParameter)
    );
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
                   infixL.left() == infixR.left() &&
                   infixL.right() == infixR.right();
        }
        case TypeUsage::Prefix: {
            const PrefixTypeInstanceASTNode
                    &prefixL = dynamic_cast<const PrefixTypeInstanceASTNode &>(lhs),
                    &prefixR = dynamic_cast<const PrefixTypeInstanceASTNode &>(rhs);

            if (prefixL.typeName() != prefixR.typeName()) {
                return false;
            }

            const RefList<TypeInstanceASTNode>
                    &lSubTerms = prefixL.parameters(),
                    &rSubTerms = prefixR.parameters();

            if (lSubTerms.size() != rSubTerms.size()) {
                return false;
            }

            for (size_t i = 0; i < lSubTerms.size(); i++) {
                if (lSubTerms[i] != rSubTerms[i]) {
                    return false;
                }
            }
            return true;
        }
        case TypeUsage::Function: {
            const FunctionTypeInstanceASTNode
                    &funcL = dynamic_cast<const FunctionTypeInstanceASTNode &>(lhs),
                    &funcR = dynamic_cast<const FunctionTypeInstanceASTNode &>(rhs);

            return funcL.left() == funcR.left() && funcL.right() == funcR.right();
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
    return false;
}

bool operator!=(const TypeInstanceASTNode &lhs, const TypeInstanceASTNode &rhs) {
    return !(lhs == rhs);
}

UserTypeInstanceASTNode::UserTypeInstanceASTNode(size_t lineNum, size_t fileIndex, std::string name)
        : ASTNode(lineNum, fileIndex), name(std::move(name)) {

}

void PrefixTypeInstanceInterface::bindParameter(std::unique_ptr<TypeInstanceASTNode> &&parameter) {
    paramList.emplace_back(*parameter);
    params.push_back(std::move(parameter));
}

PolymorphicTypeInstanceASTNode::PolymorphicTypeInstanceASTNode(size_t lineNum, size_t fileIndex,
                                                               const std::string &name)
        : UserTypeInstanceASTNode(lineNum, fileIndex, name) {

}

std::unique_ptr<TypeInstanceASTNode> PolymorphicTypeInstanceASTNode::clone() const {
    return std::make_unique<PolymorphicTypeInstanceASTNode>(lineNumber(), fileIndex(), name);
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

std::string PolymorphicTypeInstanceASTNode::typeString(bool) const {
    if (paramList.empty()) {
        return name;
    }
    return std::accumulate(paramList.begin(), paramList.end(), "(" + name,
                           [](const std::string &acc, const TypeInstanceASTNode &param) {
                               return acc + " " + param.typeString();
                           }) + ')';
}

InfixTypeInstanceASTNode::InfixTypeInstanceASTNode(size_t lineNum, size_t fileIndex, const std::string &name)
        : UserTypeInstanceASTNode(lineNum, fileIndex, name) {

}

InfixTypeInstanceASTNode::InfixTypeInstanceASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                                                   std::unique_ptr<TypeInstanceASTNode> &&lhs,
                                                   std::unique_ptr<TypeInstanceASTNode> &&rhs)
        : UserTypeInstanceASTNode(lineNum, fileIndex, name), lhs(std::move(lhs)), rhs(std::move(rhs)) {

}

std::unique_ptr<TypeInstanceASTNode> InfixTypeInstanceASTNode::clone() const {
    return std::make_unique<InfixTypeInstanceASTNode>(
            lineNumber(), fileIndex(), name, lhs->clone(), rhs->clone()
    );
}

void InfixTypeInstanceASTNode::bindLeft(std::unique_ptr<TypeInstanceASTNode> &&param) {
    lhs = std::move(param);
}

void InfixTypeInstanceASTNode::bindRight(std::unique_ptr<TypeInstanceASTNode> &&param) {
    rhs = std::move(param);
}

bool InfixTypeInstanceASTNode::isPolymorphic() const {
    return left().isPolymorphic() || right().isPolymorphic();
}

bool InfixTypeInstanceASTNode::containsClosures() const {
    return left().containsClosures() || right().containsClosures();
}

bool InfixTypeInstanceASTNode::containsFunctionType() const {
    return left().containsFunctionType() || right().containsFunctionType();
}

llvm::Type *InfixTypeInstanceASTNode::instantiate(TypeCodeGenerator &generator, const BindingMap &bindingMap) const {
    return generator.generate<InfixTypeInstanceASTNode>(View{
            .name = name,
            .lhs = lhs,
            .rhs = rhs,
            .bindingMap = bindingMap
    });
}

std::string InfixTypeInstanceASTNode::typeString(bool leftPos) const {
    std::stringstream ss;
    // Print in right associative form
    if (leftPos) {
        ss << "(";
    }

    ss << left().typeString(true);
    ss << " " << name << " ";
    ss << right().typeString(true);

    if (leftPos) {
        ss << ")";
    }

    return ss.str();
}

FunctionTypeInstanceASTNode::FunctionTypeInstanceASTNode(size_t lineNum, size_t fileIndex)
        : InfixTypeInstanceASTNode(lineNum, fileIndex, "->") {

}

FunctionTypeInstanceASTNode::FunctionTypeInstanceASTNode(size_t lineNum, size_t fileIndex,
                                                         std::unique_ptr<TypeInstanceASTNode> &&from,
                                                         std::unique_ptr<TypeInstanceASTNode> &&to)
        : InfixTypeInstanceASTNode(lineNum, fileIndex, "->", std::move(from), std::move(to)) {

}

std::unique_ptr<TypeInstanceASTNode> FunctionTypeInstanceASTNode::clone() const {
    return std::make_unique<FunctionTypeInstanceASTNode>(
            lineNumber(), fileIndex(), lhs->clone(), rhs->clone()
    );
}

bool FunctionTypeInstanceASTNode::containsClosures() const {
    return false;
}

bool FunctionTypeInstanceASTNode::containsFunctionType() const {
    return InfixTypeInstanceASTNode::containsFunctionType();
}

std::size_t FunctionTypeInstanceASTNode::functionDepth() const {
    return right().functionDepth() + 1;
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

std::unique_ptr<TypeInstanceASTNode> PrefixTypeInstanceASTNode::clone() const {
    std::unique_ptr<PrefixTypeInstanceASTNode> prefix = std::make_unique<PrefixTypeInstanceASTNode>(
            lineNumber(), fileIndex(), name
    );
    for (const TypeInstanceASTNode &param : paramList) {
        prefix->bindParameter(param.clone());
    }
    return prefix;
}

bool PrefixTypeInstanceASTNode::isPolymorphic() const {
    return std::any_of(parameters().begin(), parameters().end(),
                       [](const TypeInstanceASTNode &param) {
                           return param.isPolymorphic();
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

std::string PrefixTypeInstanceASTNode::typeString(bool) const {
    if (paramList.empty()) {
        return name;
    }
    return std::accumulate(paramList.begin(), paramList.end(), "(" + name,
                           [](const std::string &acc, const TypeInstanceASTNode &param) {
                               return acc + " " + param.typeString();
                           }) + ')';
}

PrimitiveTypeInstanceASTNode::PrimitiveTypeInstanceASTNode(const PrimitiveType &type)
        : type(type) {

}

std::unique_ptr<TypeInstanceASTNode> PrimitiveTypeInstanceASTNode::clone() const {
    return std::make_unique<PrimitiveTypeInstanceASTNode>(type);
}

llvm::Type *
PrimitiveTypeInstanceASTNode::instantiate(TypeCodeGenerator &generator, const BindingMap &) const {
    return generator.generate<PrimitiveTypeInstanceASTNode>(View{
            .type = type
    });
}

std::string PrimitiveTypeInstanceASTNode::typeString(bool leftPos) const {
    return type.name();
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
