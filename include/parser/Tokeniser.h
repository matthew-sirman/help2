//
// Created by matthew on 23/11/2020.
//

#ifndef HELP2_TOKENISER_H
#define HELP2_TOKENISER_H

#include <string>

/*
 * Language Tokens:
 * Infix: infix
 * FuncDecl: func
 * ValueDecl: value
 * ExprTypeSpecifier: ':'
 * TypeDecl: type
 * TypeConstructorSpecifier: '::='
 * TypeUnionSplitter: '|'
 * EndDecl: ';'
 * FuncBodySpecifier: '=>'
 * ValueBodySpecifier: '='
 * LetSpecifier: 'let'
 * InSpecifier: 'in'
 * LambdaSpecifier: '$'
 * Identifier: [a-zA-Z][a-zA-Z0-9_]*'? (e.g. name_Name123')
 * IntegralLiteral: [0-9]+
 * DecimalLiteral: [0-9]+\.[0-9]+
 * StringLiteral: ".*"
 * OpenList: '['
 * CommaSeparator: ','
 * CloseList: ']'
 * EmptyList: []
 * ConsList: '::'
 * Unit: ()
 * OpPlus: '+'
 * OpMinus: '-'
 * FuncType: '->'
 * OpenParenthesis: '('
 * CloseParenthesis: ')'
 */

enum class Token {
    InvalidToken,
    Infix,
    FuncDecl,
    ValueDecl,
    ExprTypeSpecifier,
    TypeDecl,
    TypeConstructorSpecifier,
    TypeUnionSplitter,
    EndDecl,
    FuncBodySpecifier,
    ValueBodySpecifier,
    LetSpecifier,
    InSpecifier,
    LambdaSpecifier,
    Identifier,
    IntegralLiteral,
    DecimalLiteral,
    StringLiteral,
    OpenList,
    CommaSeparator,
    CloseList,
    EmptyList,
    ConsList,
    Unit,
    OpPlus,
    OpMinus,
    FuncType,
    OpenParenthesis,
    CloseParenthesis,
    EndOfFile
};

class Tokeniser {
public:
    Tokeniser(std::string fileName, std::string source);

    Tokeniser(const Tokeniser &other);

    Token nextToken();

    std::string currentToken() const;

    Token currentTokenType() const;

    size_t lineNumber() const;

    const std::string &sourceFileName() const;

private:
    void logError(const std::string &message);

    const std::string fileName;
    const std::string source;
    std::string::const_iterator position;
    std::string curToken;
    Token curTokenType;
    size_t lineNum;
};


#endif //HELP2_TOKENISER_H
