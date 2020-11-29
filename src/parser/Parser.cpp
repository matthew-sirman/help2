//
// Created by matthew on 23/11/2020.
//

#include <iostream>

#include "../../include/parser/Parser.h"

Parser::Parser(const Tokeniser &tokeniser)
        : tokeniser(tokeniser) {

}

std::unique_ptr<ParseTree> Parser::parse() {
    std::unique_ptr<ParseTree> tree = std::make_unique<ParseTree>();
    while (tokeniser.nextToken() != Token::EndOfFile) {
        switch (tokeniser.currentTokenType()) {
            case Token::InvalidToken:
                return nullptr;
            case Token::Infix:
                // If the source defines an infix function or type, jump to the next token and see if
                // it is a function or a type. Otherwise return an error.
                switch (tokeniser.nextToken()) {
                    case Token::FuncDecl:
                        if (!(tree = parseInfixFunction(std::move(tree)))) {
                            return nullptr;
                        }
                        break;
                    case Token::TypeDecl:
                        if (!(tree = parseInfixType(std::move(tree)))) {
                            return nullptr;
                        }
                        break;
                    default:
                        logError("Invalid use of 'infix' specifier.");
                        return nullptr;
                }
                break;
            case Token::FuncDecl:
                if (!(tree = parsePrefixFunction(std::move(tree)))) {
                    return nullptr;
                }
                break;
            case Token::ValueDecl:
                if (!(tree = parseValue(std::move(tree)))) {
                    return nullptr;
                }
                break;
            case Token::TypeDecl:
                if (!(tree = parsePrefixType(std::move(tree)))) {
                    return nullptr;
                }
                break;
            case Token::Identifier:
                if (!(tree = parseDefinition(std::move(tree)))) {
                    return nullptr;
                }
                break;
            case Token::EndOfFile:
            case Token::EndDecl:
                break;
            default:
                logError("Invalid top level token '" + tokeniser.currentToken() + "'.");
                return nullptr;
        }
    }
    return std::move(tree);
}

std::unique_ptr<ParseTree> Parser::parsePrefixFunction(std::unique_ptr<ParseTree> tree) {
    // Syntax:
    //      func name: a -> b;
    // The next token should be the name.
    std::string functionName;
    if (tokeniser.nextToken() == Token::Identifier) {
        functionName = tokeniser.currentToken();
    } else {
        logError("'func' keyword should be followed by function identifier.");
        return nullptr;
    }
    // Next check if the function already exists. If it does, this is a redefinition and thus invalid
    if (tree->functionExists(functionName)) {
        logError("Redefinition of function '" + functionName + "'.");
        return nullptr;
    }

    // The next token should be a colon (ExprTypeSpecifier) to specify the type of the function
    if (tokeniser.nextToken() != Token::ExprTypeSpecifier) {
        logError("Function name should be followed by ':' and then function type (e.g. 'func " +
                 functionName + ": a -> b;').");
        return nullptr;
    }

    std::unique_ptr<TypeInstanceASTNode> funcType = parseTypeInstance();

    if (!funcType) {
        return nullptr;
    }



    return std::move(tree);
}

std::unique_ptr<ParseTree> Parser::parseInfixFunction(std::unique_ptr<ParseTree> tree) {
    return std::move(tree);
}

std::unique_ptr<ParseTree> Parser::parseValue(std::unique_ptr<ParseTree> tree) {
    return std::move(tree);
}

std::unique_ptr<ParseTree> Parser::parsePrefixType(std::unique_ptr<ParseTree> tree) {
    return std::move(tree);
}

std::unique_ptr<ParseTree> Parser::parseInfixType(std::unique_ptr<ParseTree> tree) {
    return std::move(tree);
}

std::unique_ptr<ParseTree> Parser::parseDefinition(std::unique_ptr<ParseTree> tree) {
    return std::move(tree);
}

std::unique_ptr<TypeInstanceASTNode> Parser::parseTypeInstance() {
    return std::unique_ptr<TypeInstanceASTNode>();
}

void Parser::logError(const std::string &message) {
    std::cerr << "[Parser] " << tokeniser.sourceFileName() << " (line " << tokeniser.lineNumber() << "): " << message
              << std::endl;
}
