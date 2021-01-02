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
#include "FileStructure.h"

class Parser {
public:
    using Error = std::string;

    Parser(FileStructure &fileStructure, const std::filesystem::path &sourceFile);

    Parser(FileStructure &fileStructure, const std::filesystem::path &sourceFile, const std::string &source);

    void parse(ParseTree &tree);

    bool hasErrors() const { return !errorList.empty(); }

    std::vector<Error> errors() const;

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
            std::optional<std::reference_wrapper<const std::unordered_set<std::string>>> typeVars = std::nullopt
    );

    std::unique_ptr<TypeclassInstanceASTNode> parseTypeclassInstance(Token &token, const ParseTree &tree);

    bool parseImplicationList(Token &token, const ParseTree &tree,
                              std::vector<std::unique_ptr<TypeclassInstanceASTNode>> &implList);

    void parseDataConstructors(Token &token, const TypeDeclASTNode &type, ParseTree &tree,
                               const std::unordered_set<std::string> &typeVars);

    // bool parseFunctionDeclList()

    std::stringstream &appendError(const Token &errorPoint);

    FileStructure &fileStructure;
    std::size_t fileIndex;
    Tokeniser tokeniser;
    std::vector<std::stringstream> errorList;
};

#endif //HELPC_PARSER_H
