//
// Created by matthew on 23/11/2020.
//

#ifndef HELP2_PARSER_H
#define HELP2_PARSER_H

#include <unordered_set>

#include "Tokeniser.h"
#include "ParseTree.h"

class Parser {
public:
    Parser(const Tokeniser &tokeniser);

    std::unique_ptr<ParseTree> parse(std::unique_ptr<ParseTree> &&tree);

private:
    std::unique_ptr<ParseTree> parsePrefixFunction(std::unique_ptr<ParseTree> tree);

    std::unique_ptr<ParseTree> parseInfixFunction(std::unique_ptr<ParseTree> tree);

    std::unique_ptr<ParseTree> parseValue(std::unique_ptr<ParseTree> tree);

    std::unique_ptr<ParseTree> parsePrefixType(std::unique_ptr<ParseTree> tree);

    std::unique_ptr<ParseTree> parseInfixType(std::unique_ptr<ParseTree> tree);

    std::unique_ptr<ParseTree> parseDataConstructors(std::unique_ptr<ParseTree> tree,
                                                     const std::unique_ptr<TypeDeclASTNode> &type,
                                                     const std::unordered_set<std::string> &typeParameters);

    std::unique_ptr<ParseTree> parseDefinition(std::unique_ptr<ParseTree> tree);

    std::unique_ptr<TypeInstanceASTNode> parseTypeInstance(const std::unique_ptr<ParseTree> &tree);

    std::unique_ptr<TypeInstanceASTNode> parseTypeInstance(const std::unique_ptr<ParseTree> &tree,
                                                           const std::unordered_set<std::string> &polyTypes);

    std::unique_ptr<TypeInstanceASTNode> parseTypeInstance(const std::unique_ptr<ParseTree> &tree, bool checkPolyTypes,
                                                           const std::unordered_set<std::string> &polyTypes);

    std::unique_ptr<TypeInstanceASTNode> parseConstructorTypeInstance(const std::unique_ptr<ParseTree> &tree,
                                                                      const std::unordered_set<std::string> &polyTypes);

    std::unique_ptr<PatternASTNode> parsePattern(const std::unique_ptr<ParseTree> &tree,
                                                 std::unordered_set<std::string> &usedBinders);

    std::unique_ptr<ExpressionASTNode> parseExpression(const std::unique_ptr<ParseTree> &tree,
                                                       const std::unordered_set<std::string> &binders,
                                                       int currentPrecedence);

    std::unique_ptr<LetBindingASTNode> parseLetBinding(const std::unique_ptr<ParseTree> &tree,
                                                       const std::unordered_set<std::string> &binders);

    std::unique_ptr<LambdaExpressionASTNode> parseLambda(const std::unique_ptr<ParseTree> &tree,
                                                         const std::unordered_set<std::string> &binders);

    void logError(const std::string &message);

    Tokeniser tokeniser;
    size_t fileIndex = -1;
};

#endif //HELP2_PARSER_H
