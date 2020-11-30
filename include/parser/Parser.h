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

    std::unique_ptr<ParseTree> parse();

private:
    std::unique_ptr<ParseTree> parsePrefixFunction(std::unique_ptr<ParseTree> tree);

    std::unique_ptr<ParseTree> parseInfixFunction(std::unique_ptr<ParseTree> tree);

    std::unique_ptr<ParseTree> parseValue(std::unique_ptr<ParseTree> tree);

    std::unique_ptr<ParseTree> parsePrefixType(std::unique_ptr<ParseTree> tree);

    std::unique_ptr<ParseTree> parseInfixType(std::unique_ptr<ParseTree> tree);

    std::unique_ptr<ParseTree> parseDataConstructors(std::unique_ptr<ParseTree> tree, const std::string &typeName,
                                                     const std::unordered_set<std::string> &typeParameters);

    std::unique_ptr<ParseTree> parseDefinition(std::unique_ptr<ParseTree> tree);

    std::unique_ptr<TypeInstanceASTNode> parseTypeInstance(const std::unique_ptr<ParseTree> &tree);

    std::unique_ptr<TypeInstanceASTNode> parseTypeInstance(const std::unique_ptr<ParseTree> &tree,
                                                           const std::unordered_set<std::string> &polyTypes);

    std::unique_ptr<TypeInstanceASTNode> parseTypeInstance(const std::unique_ptr<ParseTree> &tree, bool checkPolyTypes,
                                                           const std::unordered_set<std::string> &polyTypes);

    std::unique_ptr<TypeInstanceASTNode> parseConstructorTypeInstance(const std::unique_ptr<ParseTree> &tree,
                                                                      const std::unordered_set<std::string> &polyTypes);

    void logError(const std::string &message);

    Tokeniser tokeniser;
};

#endif //HELP2_PARSER_H
