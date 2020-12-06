//
// Created by matthew on 29/11/2020.
//

#include "../../include/parser/Parser.h"
#include "../../include/parser/ParseTree.h"

void ParseTree::addFunctionDeclaration(std::unique_ptr<FunctionDeclASTNode> &&declaration) {
    std::unique_ptr<FunctionDefinitionASTNode> function = std::make_unique<FunctionDefinitionASTNode>(std::move(declaration));

    definedFunctionNodes[function->name()] = std::move(function);
}

void
ParseTree::addFunctionImplementation(const std::string &func, std::unique_ptr<FunctionImplASTNode> &&implementation) {
    definedFunctionNodes[func]->addImplementation(std::move(implementation));
}

void ParseTree::addTypeDeclaration(std::unique_ptr<TypeDeclASTNode> &&declaration) {
    declaredTypeNodes[declaration->typeName()] = std::move(declaration);
}

void ParseTree::addDataConstructor(const std::string &type, std::unique_ptr<DataConstructorASTNode> &&constructor) {
    if (!typeExists(type)) {
        throw;
    }
    declaredDataConstructors[constructor->name()] = constructor.get();
    declaredTypeNodes[type]->addDataConstructor(std::move(constructor));
}

bool ParseTree::functionExists(const std::string &name) const {
    return definedFunctionNodes.find(name) != definedFunctionNodes.end();
}

bool ParseTree::typeExists(const std::string &name) const {
    return declaredTypeNodes.find(name) != declaredTypeNodes.end();
}

bool ParseTree::constructorExists(const std::string &name) const {
    return declaredDataConstructors.find(name) != declaredDataConstructors.end();
}

const std::unique_ptr<FunctionDefinitionASTNode> &ParseTree::getFuncByName(const std::string &name) const {
    return definedFunctionNodes.at(name);
}

const std::unique_ptr<TypeDeclASTNode> &ParseTree::getTypeByName(const std::string &name) const {
    return declaredTypeNodes.at(name);
}

const DataConstructorASTNode *ParseTree::getConstructorByName(const std::string &name) const {
    return declaredDataConstructors.at(name);
}
