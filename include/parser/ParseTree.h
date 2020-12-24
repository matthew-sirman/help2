//
// Created by matthew on 28/11/2020.
//

#ifndef HELP2_PARSETREE_H
#define HELP2_PARSETREE_H

#include <unordered_map>

#include "ast/ExpressionASTNodes.h"
#include "ast/TypeASTNodes.h"
#include "ast/FunctionASTNodes.h"

class TypeChecker;

class ParseTree {
    friend class TypeChecker;
public:
    ParseTree() = default;

    void addFunctionDeclaration(std::unique_ptr<FunctionDeclASTNode> &&declaration);

    void addFunctionImplementation(const std::string &func, std::unique_ptr<FunctionImplASTNode> &&implementation);

    const std::unique_ptr<TypeDeclASTNode> &addTypeDeclaration(std::unique_ptr<TypeDeclASTNode> &&declaration);

    void addDataConstructor(const std::unique_ptr<TypeDeclASTNode> &type, std::unique_ptr<DataConstructorASTNode> &&constructor);

    bool functionExists(const std::string &name) const;

    bool typeExists(const std::string &name) const;

    bool constructorExists(const std::string &name) const;

    const std::unique_ptr<FunctionDefinitionASTNode> &getFuncByName(const std::string &name) const;

    const std::unique_ptr<TypeDeclASTNode> &getTypeByName(const std::string &name) const;

    const DataConstructorASTNode *getConstructorByName(const std::string &name) const;

    const std::unordered_map<std::string, std::unique_ptr<TypeDeclASTNode>> &types() const;

    const std::unordered_map<std::string, std::unique_ptr<FunctionDefinitionASTNode>> &functions() const;

    size_t addFile(const std::string &fileName);

    const std::string &getFileName(size_t fileIdx) const;

    constexpr bool isTypeChecked() const { return typeChecked; }

private:
    std::unordered_map<std::string, std::unique_ptr<TypeDeclASTNode>> declaredTypeNodes;
    std::unordered_map<std::string, std::unique_ptr<FunctionDefinitionASTNode>> definedFunctionNodes;
    std::unordered_map<std::string, DataConstructorASTNode *> declaredDataConstructors;

    size_t fileIndex = 0;
    std::vector<std::string> fileLookup;

    void markAsTypeChecked();

    bool typeChecked;
};

#endif //HELP2_PARSETREE_H
