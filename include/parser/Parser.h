//
// Created by matthew on 30/12/2020.
//

#ifndef HELPC_PARSER_H
#define HELPC_PARSER_H

#include <string>
#include <vector>
#include <optional>
#include <unordered_set>
#include <filesystem>
#include <sstream>

#include "Tokeniser.h"
#include "ParseTree.h"
#include "../compiler/Options.h"

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

class Parser {
public:
    Parser(Options &fileStructure, const std::filesystem::path &sourceFile);

    Parser(Options &fileStructure, const std::filesystem::path &sourceFile, const std::string &source);

    void parse(ParseTree &tree);

    bool hasErrors() const { return !errorList->empty(); }

    constexpr const ErrorList &errors() const { return errorList; };

private:
    using TokeniseFunc = std::function<bool(Token &)>;

    static std::string loadSourceFile(const std::filesystem::path &sourcePath);

    // Top level
    void parseImport(Token &token, ParseTree &tree);

    bool parseAnyFunctionDecl(Token &token, const ParseTree &tree, std::unique_ptr<FunctionDeclASTNode> &function);

    std::unique_ptr<FunctionDeclASTNode> parseFunctionDecl(Token &token, const ParseTree &tree, FunctionUsage mode,
                                                           std::optional<Associativity> assoc = std::nullopt);

    TokeniseFunc topLevelTypeDecl(ParseTree &tree, bool infix);

    void parseTypeDecl(Token &token, ParseTree &tree, bool infix = false);

    void parseTypeclass(Token &token, ParseTree &tree);

    void parseTypeclassInstanceDecl(Token &token, ParseTree &tree);

    std::unique_ptr<FunctionImplASTNode> parseImplementation(Token &token, const ParseTree &tree);

    std::unique_ptr<FunctionImplASTNode> parseImplementation(Token &token, const ParseTree &tree, std::string &name);

    // Subparsers
    bool parseTypeInstance(
            Token &token, const ParseTree &tree, std::unique_ptr<TypeInstanceASTNode> &inst,
            std::optional<std::reference_wrapper<const std::unordered_set<std::string>>> typeVars = std::nullopt,
            bool nested = false
    );

    std::unique_ptr<TypeclassInstanceASTNode> parseTypeclassInstance(Token &token, const ParseTree &tree);

    bool parseImplicationList(Token &token, const ParseTree &tree,
                              PrerequisiteList &implList,
                              bool nested = false);

    void parseDataConstructors(Token &token, const TypeDeclASTNode &type, ParseTree &tree,
                               const std::unordered_set<std::string> &typeVars);

    // bool parseFunctionDeclList()

    ErrorList::ErrorStream &appendError(const Token &errorPoint);

    Options &options;
    std::size_t fileIndex;
    Tokeniser tokeniser;
    ErrorList errorList;
};

#endif //HELPC_PARSER_H
