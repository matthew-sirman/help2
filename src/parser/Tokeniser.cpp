//
// Created by matthew on 23/11/2020.
//

#include <iostream>
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
    if (*position == '+') {
        curToken = "+";
        position++;
        return curTokenType = Token::OpPlus;
    }
    if (*position == '-') {
        position++;
        if (position != source.end() && *position == '>') {
            curToken = "->";
            position++;
            return curTokenType = Token::FuncType;
        } else {
            curToken = "-";
            return curTokenType = Token::OpMinus;
        }
    }
    if (*position == '[') {
        position++;
        if (position != source.end() && *position == ']') {
            curToken = "[]";
            position++;
            return curTokenType = Token::List;
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
        if (position != source.end() && std::string(position, std::min(position + 2, source.end())) == ":=") {
            curToken = "::=";
            position += 2;
            return curTokenType = Token::TypeConstructorSpecifier;
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
    if (std::string(position, std::min(position + 5, source.end())) == "func ") {
        curToken = "func";
        position += 5;
        return curTokenType = Token::FuncDecl;
    }
    if (std::string(position, std::min(position + 5, source.end())) == "type ") {
        curToken = "type";
        position += 5;
        return curTokenType = Token::TypeDecl;
    }
    if (std::string(position, std::min(position + 6, source.end())) == "value ") {
        curToken = "value";
        position += 6;
        return curTokenType = Token::ValueDecl;
    }
    if (std::string(position, std::min(position + 6, source.end())) == "infix ") {
        curToken = "infix";
        position += 6;
        return curTokenType = Token::Infix;
    }
    if (std::string(position, std::min(position + 4, source.end())) == "let ") {
        curToken = "let";
        position += 4;
        return curTokenType = Token::LetSpecifier;
    }
    if (std::string(position, std::min(position + 3, source.end())) == "in ") {
        curToken = "in";
        position += 3;
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
