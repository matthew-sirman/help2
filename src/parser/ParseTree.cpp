//
// Created by matthew on 29/11/2020.
//

#include "../../include/parser/old/Parser.h"
#include "../../include/parser/ParseTree.h"

ParseTree::ParseTree(Core &&coreModule)
        : langCore(std::move(coreModule)) {

}

void ParseTree::addFunctionDeclaration(std::unique_ptr<FunctionDeclASTNode> &&declaration) {
    std::unique_ptr<FunctionDefinitionASTNode> function = std::make_unique<FunctionDefinitionASTNode>(std::move(declaration));

    definedFunctionNodes[function->name()] = std::move(function);
}

void
ParseTree::addFunctionImplementation(const std::string &func, std::unique_ptr<FunctionImplASTNode> &&implementation) {
    definedFunctionNodes[func]->addImplementation(std::move(implementation));
}

const TypeDeclASTNode &ParseTree::addTypeDeclaration(std::unique_ptr<TypeDeclASTNode> &&declaration) {
    std::string typeName = declaration->typeName();
    declaredTypeNodes[typeName] = std::move(declaration);
    return *declaredTypeNodes[typeName];
}

void ParseTree::addTypeclass(std::unique_ptr<TypeclassASTNode> &&typeclass) {
    declaredTypeclasses[typeclass->name()] = std::move(typeclass);
}

void ParseTree::addDataConstructor(const TypeDeclASTNode &type, std::unique_ptr<DataConstructorASTNode> &&constructor) {
    if (!typeExists(type.typeName())) {
        throw;
    }
    declaredDataConstructors[constructor->name()] = *constructor;
    declaredTypeNodes[type.typeName()]->addDataConstructor(std::move(constructor));
}

TypeclassInstanceImplASTNode &ParseTree::addTypeclassInstance(const std::string &typeclassName,
                                                              std::unique_ptr<TypeclassInstanceImplASTNode> &&instance) {
    return declaredTypeclasses[typeclassName]->addInstance(std::move(instance));
}

bool ParseTree::functionExists(const std::string &name) const {
    return definedFunctionNodes.contains(name);
}

bool ParseTree::typeExists(const std::string &name) const {
    return declaredTypeNodes.contains(name);
}

bool ParseTree::constructorExists(const std::string &name) const {
    return declaredDataConstructors.contains(name);
}

bool ParseTree::typeclassExists(const std::string &name) const {
    return declaredTypeclasses.contains(name);
}

const FunctionDefinitionASTNode &ParseTree::getFuncByName(const std::string &name) const {
    return *definedFunctionNodes.at(name);
}

const TypeDeclASTNode &ParseTree::getTypeByName(const std::string &name) const {
    return *declaredTypeNodes.at(name);
}

const DataConstructorASTNode &ParseTree::getConstructorByName(const std::string &name) const {
    return declaredDataConstructors.at(name);
}

const TypeclassASTNode &ParseTree::getTypeclassByName(const std::string &name) const {
    return *declaredTypeclasses.at(name);
}

const std::unordered_map<std::string, std::unique_ptr<TypeDeclASTNode>> &ParseTree::types() const {
    return declaredTypeNodes;
}

const std::unordered_map<std::string, std::unique_ptr<FunctionDefinitionASTNode>> &ParseTree::functions() const {
    return definedFunctionNodes;
}

size_t ParseTree::addFile(const std::filesystem::path &fileName) {
    // Add this module to the back of the vector
    modules.push_back({ .fileName = fileName, .moduleName = fileName.filename().replace_extension() });
    // Return the index of this file and increment the counter
    return fileIndex++;
}

const std::filesystem::path &ParseTree::getFilePath(size_t fileIdx) const {
    return modules[fileIdx].fileName;
}

const std::string &ParseTree::getModuleName(size_t fileIdx) const {
    return modules[fileIdx].moduleName;
}

void ParseTree::markAsTypeChecked() {
    typeChecked = true;
}
