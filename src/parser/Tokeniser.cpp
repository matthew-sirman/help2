//
// Created by matthew on 23/11/2020.
//

#include <iostream>
#include <regex>

#include "../../include/parser/Tokeniser.h"

Tokeniser::Tokeniser(std::string fileName, std::string source)
        : fileName(std::move(fileName)), source(std::move(source)) {
    position = this->source.cbegin();
    curTokenType = Token::InvalidToken;
    lineNum = 1;
}

Tokeniser::Tokeniser(const Tokeniser &other)
        : fileName(other.fileName), source(other.source),
          curTokenType(other.curTokenType), curToken(other.curToken), lineNum(other.lineNum) {
    position = source.cbegin();
}

Token Tokeniser::nextToken() {
    // Check for EOF
    if (position == source.end()) {
        return curTokenType = Token::EndOfFile;
    }

    // Begin by skipping over any spaces
    while (position != source.end() && isspace(*position)) {
        if (*position == '\n') {
            lineNum++;
        }
        position++;
    }

    // Check again for EOF after skipping spaces
    if (position == source.end()) {
        return curTokenType = Token::EndOfFile;
    }

    // Check for comment lines
    if (std::string(position, std::min(position + 2, source.end())) == "//") {
        lineNum++;
        while (position != source.end() && *position++ != '\n');
        // Recursively call the nextToken function - this will essentially "reset" the next
        // token after seeing a comment
        return nextToken();
    }

    // Check for single character tokens (and any other symbolic tokens which start with
    // a character from the single character token set)
    if (*position == ';') {
        curToken = ";";
        position++;
        return curTokenType = Token::EndDecl;
    }
    if (*position == '=') {
        position++;
        if (position != source.end() && *position == '>') {
            curToken = "=>";
            position++;
            return curTokenType = Token::FuncBodySpecifier;
        } else {
            curToken = "=";
            return curTokenType = Token::ValueBodySpecifier;
        }
    }
    if (*position == '|') {
        curToken = "|";
        position++;
        return curTokenType = Token::TypeUnionSplitter;
    }
    std::cmatch operatorMatch;
    if (std::regex_search(position.base(), operatorMatch, std::regex(R"(^(\+|-(?!>)|\*|/|%|\.))"))) {
        curToken = operatorMatch.str();
        position += operatorMatch.size();
        return curTokenType = Token::SpecialInfixOperator;
    }
    if (std::regex_search(position.base(), operatorMatch, std::regex(R"(^\((\+|-(?!>)|\*|/|%|\.)\))"))) {
        curToken = operatorMatch[1].str();
        position += curToken.size() + 2;
        return curTokenType = Token::SpecialPrefixOperator;
    }
    if (std::regex_search(position.base(), std::regex("^->"))) {
        curToken = "->";
        position += 2;
        return curTokenType = Token::FuncType;
    }
    if (*position == '(') {
        position++;
        if (position != source.end() && *position == ')') {
            curToken = "()";
            position++;
            return curTokenType = Token::Unit;
        } else {
            curToken = "(";
            return curTokenType = Token::OpenParenthesis;
        }
    }
    if (*position == ')') {
        curToken = ")";
        position++;
        return curTokenType = Token::CloseParenthesis;
    }
    if (*position == '[') {
        position++;
        if (position != source.end() && *position == ']') {
            curToken = "[]";
            position++;
            return curTokenType = Token::EmptyList;
        } else {
            curToken = "[";
            return curTokenType = Token::OpenList;
        }
    }
    if (*position == ']') {
        curToken = "]";
        position++;
        return curTokenType = Token::CloseList;
    }
    if (*position == ',') {
        curToken = ",";
        position++;
        return curTokenType = Token::CommaSeparator;
    }
    if (*position == ':') {
        position++;
        if (position != source.end() && *position == ':') {
            position++;
            if (position != source.end() && *position == '=') {
                curToken = "::=";
                position++;
                return curTokenType = Token::TypeConstructorSpecifier;
            } else {
                curToken = "::";
                return curTokenType = Token::ConsList;
            }
        } else {
            curToken = ":";
            return curTokenType = Token::ExprTypeSpecifier;
        }
    }
    if (*position == '$') {
        curToken = "$";
        position++;
        return curTokenType = Token::LambdaSpecifier;
    }

    // Check for keyword tokens
    if (std::regex_search(position.base(), std::regex("^func\\W"))) {
        curToken = "func";
        position += 4;
        return curTokenType = Token::FuncDecl;
    }
    if (std::regex_search(position.base(), std::regex("^value\\W"))) {
        curToken = "value";
        position += 5;
        return curTokenType = Token::ValueDecl;
    }
    if (std::regex_search(position.base(), std::regex("^type\\W"))) {
        curToken = "type";
        position += 4;
        return curTokenType = Token::TypeDecl;
    }
    if (std::regex_search(position.base(), std::regex("^infix\\W"))) {
        curToken = "infix";
        position += 5;
        return curTokenType = Token::Infix;
    }
    if (std::regex_search(position.base(), std::regex("^let\\W"))) {
        curToken = "let";
        position += 3;
        return curTokenType = Token::LetSpecifier;
    }
    if (std::regex_search(position.base(), std::regex("^in\\W"))) {
        curToken = "in";
        position += 2;
        return curTokenType = Token::InSpecifier;
    }

    // Check for literal tokens
    if (*position == '"') {
        std::string::const_iterator literalStart = position;
        do {
            position++;
        } while (position != source.end() && *position != '"');
        curToken = std::string(literalStart, ++position);
        return curTokenType = Token::StringLiteral;
    }
    if (isdigit(*position)) {
        std::string::const_iterator literalStart = position;
        bool seenPoint = false;
        do {
            position++;
            // Allow at most one point
            if (*position == '.') {
                if (seenPoint) {
                    logError("Numeric literal can only contain a single '.' character.");
                    return curTokenType = Token::InvalidToken;
                }
                seenPoint = true;
                position++;
            }
        } while (position != source.end() && isdigit(*position));
        curToken = std::string(literalStart, position);
        return curTokenType = seenPoint ? Token::DecimalLiteral : Token::IntegralLiteral;
    }

    // Finally, check for an identifier
    if (isalpha(*position)) {
        std::string::const_iterator literalStart = position;
        do {
            position++;
        } while (position != source.end() && (isalnum(*position) || *position == '_'));
        // Check if the identifier ends in a ' (prime)
        if (*position == '\'') {
            position++;
        }
        curToken = std::string(literalStart, position);
        return curTokenType = Token::Identifier;
    }

    // If we fall through to this point, the token is invalid.
    logError("Unrecognised token '" + std::string(position, position + 1) + "'.");
    return curTokenType = Token::InvalidToken;
}

std::string Tokeniser::currentToken() const {
    return curToken;
}

Token Tokeniser::currentTokenType() const {
    return curTokenType;
}

size_t Tokeniser::lineNumber() const {
    return lineNum;
}

const std::string &Tokeniser::sourceFileName() const {
    return fileName;
}

void Tokeniser::logError(const std::string &message) {
    std::cerr << "[Tokeniser] " << fileName << " (line " << lineNum << "): " << message << std::endl;
}
