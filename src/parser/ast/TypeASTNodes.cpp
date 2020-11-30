//
// Created by matthew on 28/11/2020.
//

#include "../../../include/parser/ast/TypeASTNodes.h"

DataConstructorASTNode::DataConstructorASTNode(std::string constructorName)
        : constructorName(std::move(constructorName)) {

}

PrefixDataConstructorASTNode::PrefixDataConstructorASTNode(const std::string &constructorName)
        : DataConstructorASTNode(constructorName) {

}

PrefixDataConstructorASTNode::PrefixDataConstructorASTNode(const std::string &constructorName,
                                                           std::vector<std::unique_ptr<TypeInstanceASTNode>> &&parameters)
        : DataConstructorASTNode(constructorName), parameters(std::move(parameters)) {

}

InfixDataConstructorASTNode::InfixDataConstructorASTNode(const std::string &constructorName,
                                                         std::unique_ptr<TypeInstanceASTNode> &&leftParam,
                                                         std::unique_ptr<TypeInstanceASTNode> &&rightParam)
        : DataConstructorASTNode(constructorName), lhs(std::move(leftParam)), rhs(std::move(rightParam)) {

}

TypeDeclASTNode::TypeDeclASTNode(std::string name)
        : name(std::move(name)) {

}

TypeDeclASTNode::TypeDeclASTNode(std::string name, std::vector<std::unique_ptr<DataConstructorASTNode>> &&constructors)
        : name(std::move(name)), constructors(std::move(constructors)) {

}

void TypeDeclASTNode::addDataConstructor(std::unique_ptr<DataConstructorASTNode> &&constructor) {
    constructors.push_back(std::move(constructor));
}

PrefixTypeDeclASTNode::PrefixTypeDeclASTNode(const std::string &name, std::vector<std::string> &&typeVariables)
        : TypeDeclASTNode(name), typeConstructorParameters(std::move(typeVariables)) {

}

PrefixTypeDeclASTNode::PrefixTypeDeclASTNode(const std::string &name, std::vector<std::string> &&typeVariables,
                                             std::vector<std::unique_ptr<DataConstructorASTNode>> &&constructors)
        : TypeDeclASTNode(name, std::move(constructors)), typeConstructorParameters(std::move(typeVariables)) {

}

InfixTypeDeclASTNode::InfixTypeDeclASTNode(const std::string &name, std::string left, std::string right)
        : TypeDeclASTNode(name), leftParameter(std::move(left)), rightParameter(std::move(right)) {

}

InfixTypeDeclASTNode::InfixTypeDeclASTNode(const std::string &name, std::string left, std::string right,
                                           std::vector<std::unique_ptr<DataConstructorASTNode>> &&constructors)
        : TypeDeclASTNode(name, std::move(constructors)), leftParameter(std::move(left)),
          rightParameter(std::move(right)) {

}

TypeInstanceASTNode::TypeInstanceASTNode(std::string name)
        : name(std::move(name)) {

}

PolymorphicTypeInstanceASTNode::PolymorphicTypeInstanceASTNode(const std::string &name)
        : TypeInstanceASTNode(name) {

}

InfixTypeInstanceASTNode::InfixTypeInstanceASTNode(const std::string &name)
        : TypeInstanceASTNode(name) {

}

void InfixTypeInstanceASTNode::bindLeft(std::unique_ptr<TypeInstanceASTNode> &&param) {
    lhs = std::move(param);
}

void InfixTypeInstanceASTNode::bindRight(std::unique_ptr<TypeInstanceASTNode> &&param) {
    rhs = std::move(param);
}

FunctionTypeInstanceASTNode::FunctionTypeInstanceASTNode(const std::string &name)
        : InfixTypeInstanceASTNode(name) {

}

PrefixTypeInstanceASTNode::PrefixTypeInstanceASTNode(const std::string &name)
        : TypeInstanceASTNode(name) {

}

void PrefixTypeInstanceASTNode::bindParameter(std::unique_ptr<TypeInstanceASTNode> &&param) {
    parameters.push_back(std::move(param));
}
