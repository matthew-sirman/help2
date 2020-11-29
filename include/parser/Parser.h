//
// Created by matthew on 23/11/2020.
//

#ifndef HELP2_PARSER_H
#define HELP2_PARSER_H

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

    std::unique_ptr<ParseTree> parseDefinition(std::unique_ptr<ParseTree> tree);

    std::unique_ptr<TypeInstanceASTNode> parseTypeInstance();

    void logError(const std::string &message);

    Tokeniser tokeniser;
};

#endif //HELP2_PARSER_H
