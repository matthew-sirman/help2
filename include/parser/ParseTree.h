//
// Created by matthew on 28/11/2020.
//

#ifndef HELP2_PARSETREE_H
#define HELP2_PARSETREE_H

#include <unordered_map>
#include <filesystem>

#include "ast/ExpressionASTNodes.h"
#include "ast/TypeASTNodes.h"
#include "ast/FunctionASTNodes.h"

#include "../lang/core/Core.h"

class TypeChecker;

class ParseTree {
    friend class TypeChecker;

public:
    struct Module {
        const std::filesystem::path fileName;
        std::string moduleName;
    };

    ParseTree(Core &&coreModule);

    void addFunctionDeclaration(std::unique_ptr<FunctionDeclASTNode> &&declaration);

    void addFunctionImplementation(const std::string &func, std::unique_ptr<FunctionImplASTNode> &&implementation);

    const TypeDeclASTNode &addTypeDeclaration(std::unique_ptr<TypeDeclASTNode> &&declaration);

    void addTypeclass(std::unique_ptr<TypeclassASTNode> &&typeclass);

    void addDataConstructor(const TypeDeclASTNode &type, std::unique_ptr<DataConstructorASTNode> &&constructor);

    TypeclassInstanceImplASTNode &addTypeclassInstance(const std::string &typeclassName,
                                                       std::unique_ptr<TypeclassInstanceImplASTNode> &&instance);

    bool functionExists(const std::string &name) const;

    bool typeExists(const std::string &name) const;

    bool constructorExists(const std::string &name) const;

    bool typeclassExists(const std::string &name) const;

    const FunctionDefinitionASTNode &getFuncByName(const std::string &name) const;

    const TypeDeclASTNode &getTypeByName(const std::string &name) const;

    const DataConstructorASTNode &getConstructorByName(const std::string &name) const;

    const TypeclassASTNode &getTypeclassByName(const std::string &name) const;

    const std::unordered_map<std::string, std::unique_ptr<TypeDeclASTNode>> &types() const;

    const std::unordered_map<std::string, std::unique_ptr<FunctionDefinitionASTNode>> &functions() const;

    size_t addFile(const std::filesystem::path &fileName);

    const std::filesystem::path &getFilePath(size_t fileIdx) const;

    const std::string &getModuleName(size_t fileIdx) const;

    constexpr const std::vector<Module> &allModules() const { return modules; }

    constexpr bool isTypeChecked() const { return typeChecked; }

    constexpr Core &core() { return langCore; }

    constexpr const Core &core() const { return langCore; }

private:
    std::unordered_map<std::string, std::unique_ptr<TypeDeclASTNode>> declaredTypeNodes;
    std::unordered_map<std::string, std::unique_ptr<TypeclassASTNode>> declaredTypeclasses;
    std::unordered_map<std::string, std::unique_ptr<FunctionDefinitionASTNode>> definedFunctionNodes;
    std::unordered_map<std::string, std::reference_wrapper<const DataConstructorASTNode>> declaredDataConstructors;

    size_t fileIndex = 0;
    std::vector<Module> modules;

    void markAsTypeChecked();

    bool typeChecked = false;

    Core langCore;
};

#endif //HELP2_PARSETREE_H
