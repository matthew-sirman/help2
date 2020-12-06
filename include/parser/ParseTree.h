//
// Created by matthew on 28/11/2020.
//

#ifndef HELP2_PARSETREE_H
#define HELP2_PARSETREE_H

#include <unordered_map>

#include "ast/ExpressionASTNodes.h"
#include "ast/TypeASTNodes.h"
#include "ast/FunctionASTNodes.h"

class ParseTree {
public:
    ParseTree() = default;

    void addFunctionDeclaration(std::unique_ptr<FunctionDeclASTNode> &&declaration);

    void addFunctionImplementation(const std::string &func, std::unique_ptr<FunctionImplASTNode> &&implementation);

    void addTypeDeclaration(std::unique_ptr<TypeDeclASTNode> &&declaration);

    void addDataConstructor(const std::string &type, std::unique_ptr<DataConstructorASTNode> &&constructor);

    bool functionExists(const std::string &name) const;

    bool typeExists(const std::string &name) const;

    bool constructorExists(const std::string &name) const;

    const std::unique_ptr<FunctionDefinitionASTNode> &getFuncByName(const std::string &name) const;

    const std::unique_ptr<TypeDeclASTNode> &getTypeByName(const std::string &name) const;

    const DataConstructorASTNode *getConstructorByName(const std::string &name) const;

private:
    std::unordered_map<std::string, std::unique_ptr<TypeDeclASTNode>> declaredTypeNodes;
    std::unordered_map<std::string, std::unique_ptr<FunctionDefinitionASTNode>> definedFunctionNodes;
    std::unordered_map<std::string, DataConstructorASTNode *> declaredDataConstructors;
};

#endif //HELP2_PARSETREE_H
