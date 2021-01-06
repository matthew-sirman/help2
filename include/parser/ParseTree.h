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
#include "../compiler/Options.h"

class TypeChecker;

class ErrorList {
public:
    using Error = std::string;
    using ErrorStream = std::stringstream;

    friend std::ostream &operator<<(std::ostream &os, const ErrorList &errorList) {
        std::for_each(errorList->begin(), errorList->end(),
                      [&os](const ErrorStream &es) { os << es.str() << std::endl; });
        return os;
    }

    std::vector<ErrorStream> *operator->() {
        return &errors;
    }

    const std::vector<ErrorStream> *operator->() const {
        return &errors;
    }

private:
    std::vector<ErrorStream> errors;
};

class ParseTree {
    friend class TypeChecker;

public:
    struct Module {
        const std::filesystem::path fileName;
        std::string moduleName;
    };

    ParseTree(Core &&coreModule, Options &options);

    void addFunctionDeclaration(std::unique_ptr<FunctionDeclASTNode> &&declaration);

    void addFunctionImplementation(const std::string &func, std::unique_ptr<FunctionImplASTNode> &&implementation);

    const TypeDeclASTNode &addTypeDeclaration(std::unique_ptr<TypeDeclASTNode> &&declaration);

    void addTypeclass(std::unique_ptr<TypeclassASTNode> &&typeclass);

    void addDataConstructor(const TypeDeclASTNode &type, std::unique_ptr<DataConstructorASTNode> &&constructor);

    TypeclassInstanceImplASTNode &addTypeclassInstance(const std::string &typeclassName,
                                                       std::unique_ptr<TypeclassInstanceImplASTNode> &&instance);

    bool functionExists(const std::string &name, bool includeConstructors = false) const;

    bool typeExists(const std::string &name) const;

    bool constructorExists(const std::string &name) const;

    bool typeclassExists(const std::string &name) const;

    const FunctionDefinitionASTNode &getFuncByName(const std::string &name) const;

    const TypeDeclASTNode &getTypeByName(const std::string &name) const;

    const DataConstructorASTNode &getConstructorByName(const std::string &name) const;

    const TypeclassASTNode &getTypeclassByName(const std::string &name) const;

    constexpr const RefList<TypeDeclASTNode> &types() const { return typeList; }

    constexpr const RefList<TypeclassASTNode> &typeclasses() const { return typeclassList; }

    constexpr const RefList<FunctionDefinitionASTNode> &functions() const { return functionList; }

    size_t addFile(const std::filesystem::path &fileName);

    const std::filesystem::path &getFilePath(size_t fileIdx) const;

    const std::string &getModuleName(size_t fileIdx) const;

    constexpr const std::vector<Module> &allModules() const { return modules; }

    constexpr bool isTypeChecked() const { return typeChecked; }

    constexpr Core &core() { return langCore; }

    constexpr const Core &core() const { return langCore; }

private:
    Options &options;

    std::unordered_map<std::string, std::unique_ptr<TypeDeclASTNode>> declaredTypeNodes;
    std::unordered_map<std::string, std::unique_ptr<TypeclassASTNode>> declaredTypeclasses;
    std::unordered_map<std::string, std::unique_ptr<FunctionDefinitionASTNode>> definedFunctionNodes;
    std::unordered_map<std::string, std::reference_wrapper<DataConstructorASTNode>> declaredDataConstructors;

    RefList<TypeDeclASTNode> typeList;
    RefList<FunctionDefinitionASTNode> functionList;
    RefList<TypeclassASTNode> typeclassList;

    size_t fileIndex = 0;
    std::vector<Module> modules;

    void markAsTypeChecked();

    bool typeChecked = false;

    Core langCore;
};

#endif //HELP2_PARSETREE_H
