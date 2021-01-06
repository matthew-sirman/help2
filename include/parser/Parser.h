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
    bool parseModule(Token &token);

    void parseImport(Token &token, ParseTree &tree);

    bool parseAnyFunctionDecl(Token &token, const ParseTree &tree, std::unique_ptr<FunctionDeclASTNode> &function);

    std::unique_ptr<FunctionDeclASTNode> parseFunctionDecl(Token &token, const ParseTree &tree, FunctionUsage mode,
                                                           std::optional<Associativity> assoc = std::nullopt);

    TokeniseFunc topLevelTypeDecl(ParseTree &tree, bool infix);

    void parseTypeDecl(Token &token, ParseTree &tree, bool infix = false);

    void parseTypeclass(Token &token, ParseTree &tree);

    void parseTypeclassInstanceDecl(Token &token, ParseTree &tree);

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

    bool parsePattern(Token &token, const ParseTree &tree, std::unique_ptr<PatternASTNode> &pattern,
                      BinderMap &binders, bool nested = false);

    bool parseExpression(Token &token, const ParseTree &tree, BinderMap &binders,
                         std::unique_ptr<ExpressionASTNode> &expr, bool nested = false, int precedence = 0);

    std::unique_ptr<LetBindingASTNode> parseLetBinding(Token &token, const ParseTree &tree, BinderMap &binders,
                                                       bool nested);

    std::unique_ptr<LambdaExpressionASTNode> parseLambda(Token &token, const ParseTree &tree, BinderMap &binders,
                                                         bool nested);

    ErrorList::ErrorStream &appendError(const Token &errorPoint);

    Options &options;
    std::size_t fileIndex;
    Tokeniser tokeniser;
    ErrorList errorList;
};

#endif //HELPC_PARSER_H
