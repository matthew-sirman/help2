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
    // Consume the ':' token
    tokeniser.nextToken();

    std::unique_ptr<TypeInstanceASTNode> funcType = parseTypeInstance(tree);

    if (!funcType) {
        return nullptr;
    }

    if (funcType->typeUsage() != TypeUsage::Function) {
        logError("Function must have function type (a -> b)");
        return nullptr;
    }

    if (tokeniser.currentTokenType() != Token::EndDecl) {
        logError("Function declaration should end with ';'.");
        return nullptr;
    }

    tree->addFunctionDeclaration(std::make_unique<FunctionDeclASTNode>(functionName, std::move(funcType)));

    return std::move(tree);
}

std::unique_ptr<ParseTree> Parser::parseInfixFunction(std::unique_ptr<ParseTree> tree) {
    // Syntax:
    //      infix func name: a -> b;
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
        logError("Function name should be followed by ':' and then function type (e.g. 'infix func " +
                 functionName + ": a -> b -> c;').");
        return nullptr;
    }
    // Consume the ':' token
    tokeniser.nextToken();

    std::unique_ptr<TypeInstanceASTNode> funcType = parseTypeInstance(tree);

    if (!funcType) {
        return nullptr;
    }
    if (funcType->typeUsage() != TypeUsage::Function) {
        logError("Infix function must have function-to-function type (a -> b -> c)");
        return nullptr;
    }
    if (dynamic_cast<FunctionTypeInstanceASTNode *>(funcType.get())->right()->typeUsage() != TypeUsage::Function) {
        logError("Infix function must have function-to-function type (a -> b -> c)");
        return nullptr;
    }

    if (tokeniser.currentTokenType() != Token::EndDecl) {
        logError("Infix function declaration should end with ';'.");
        return nullptr;
    }

    tree->addFunctionDeclaration(std::make_unique<FunctionDeclASTNode>(functionName, std::move(funcType)));

    return std::move(tree);
}

std::unique_ptr<ParseTree> Parser::parseValue(std::unique_ptr<ParseTree> tree) {
    return std::move(tree);
}

std::unique_ptr<ParseTree> Parser::parsePrefixType(std::unique_ptr<ParseTree> tree) {
    // Syntax:
    //      type Name [a0 [a1 ...]] [::= Cons0 [| Cons1 [| ...]]];
    // The next token should be the type name
    if (tokeniser.nextToken() != Token::Identifier) {
        logError("'type' keyword should be followed by type name.");
        return nullptr;
    }
    std::string typeName = tokeniser.currentToken();

    if (tree->typeExists(typeName)) {
        logError("Redefinition of type '" + typeName + "'.");
        return nullptr;
    }

    // Next look for 0 or more type variables
    std::unordered_set<std::string> typeParameters;
    while (tokeniser.nextToken() != Token::TypeConstructorSpecifier) {
        switch (tokeniser.currentTokenType()) {
            case Token::Identifier:
                break;
            case Token::EndDecl:
                // If there are no constructors, then we don't expect to see a '::=' token.
                // Construct the new prefix type and pass it to the tree
                tree->addTypeDeclaration(std::make_unique<PrefixTypeDeclASTNode>(
                        typeName, std::vector<std::string>(typeParameters.begin(), typeParameters.end()))
                );
                return std::move(tree);
            default:
                logError("Unexpected token '" + tokeniser.currentToken() +
                         "' in type constructor declaration.\n "
                         "Type name should be followed by type variables and optionally "
                         "type constructor list (e.g. type " + typeName + " [a [b [c ...]]] [::= ...].");
                return nullptr;
        }
        if (typeParameters.contains(tokeniser.currentToken())) {
            logError("Duplicate type variable '" + tokeniser.currentToken() + "' in type constructor.");
            return nullptr;
        }
        if (tree->typeExists(tokeniser.currentToken())) {
            logError("Type variable '" + tokeniser.currentToken() +
                     "' is already a type name.\n "
                     "Consider using uppercase names for types and lowercase for type variables.");
            return nullptr;
        }
        typeParameters.insert(tokeniser.currentToken());
    }
    // Add the type declaration to the parse tree - this allows for recursive types
    tree->addTypeDeclaration(std::make_unique<PrefixTypeDeclASTNode>(
            typeName, std::vector<std::string>(typeParameters.begin(), typeParameters.end())
    ));
    // Skip over '::=' token
    tokeniser.nextToken();

    // Now we are looking for constructors of which there should be one or more
    return parseDataConstructors(std::move(tree), typeName, typeParameters);
}

std::unique_ptr<ParseTree> Parser::parseInfixType(std::unique_ptr<ParseTree> tree) {
    // Syntax:
    //      infix type a Name b [::= Cons0 [| Cons1 [| ...]]];
    // The next token should be the lhs type variable
    if (tokeniser.nextToken() != Token::Identifier) {
        logError("'infix type' should be followed by a type variable.");
        return nullptr;
    }
    if (tree->typeExists(tokeniser.currentToken())) {
        logError("Type variable '" + tokeniser.currentToken() +
                 "' is already a type name.\n "
                 "Consider using uppercase names for types and lowercase for type variables.");
        return nullptr;
    }
    std::string lhs = tokeniser.currentToken();
    // The next token should be the type name
    if (tokeniser.nextToken() != Token::Identifier) {
        logError("'infix type " + lhs + "' should be followed by a type name.");
        return nullptr;
    }
    std::string typeName = tokeniser.currentToken();
    // The next token should be the rhs type variable
    if (tokeniser.nextToken() != Token::Identifier) {
        logError("'infix type " + lhs + " " + typeName + "' should be followed by a type variable.");
        return nullptr;
    }
    if (tree->typeExists(tokeniser.currentToken())) {
        logError("Type variable '" + tokeniser.currentToken() +
                 "' is already a type name.\n "
                 "Consider using uppercase names for types and lowercase for type variables.");
        return nullptr;
    }
    std::string rhs = tokeniser.currentToken();
    if (lhs == rhs) {
        logError("Left hand type variable cannot have the same name as right hand type variable.");
    }

    if (tree->typeExists(typeName)) {
        logError("Redefinition of type '" + typeName + "'.");
        return nullptr;
    }

    // Add the type declaration to the parse tree - this allows for recursive types
    tree->addTypeDeclaration(std::make_unique<InfixTypeDeclASTNode>(
            typeName, lhs, rhs
    ));
    switch (tokeniser.nextToken()) {
        case Token::EndDecl:
            return std::move(tree);
        case Token::TypeConstructorSpecifier:
            break;
        default:
            logError("Expected ';' or '::=' after type declaration.");
            return nullptr;
    }
    // Skip over '::=' token
    tokeniser.nextToken();

    // Now we are looking for constructors of which there should be one or more
    return parseDataConstructors(std::move(tree), typeName, { lhs, rhs });
}

std::unique_ptr<ParseTree> Parser::parseDataConstructors(std::unique_ptr<ParseTree> tree, const std::string &typeName,
                                                         const std::unordered_set<std::string> &typeParameters) {
    std::unordered_set<std::string> addedConstructors;

    do {
        switch (tokeniser.currentTokenType()) {
            case Token::Infix: {
                // If the constructor is infix we expect 'infix l Cons r'.
                // Skip the token
                tokeniser.nextToken();
                // Next we expect a type instance. This will either be a polymorphic type from the type constructor,
                // a preexisting type, or a nested type which we can then outsource to the type parser.
                std::unique_ptr<TypeInstanceASTNode> lhs = parseConstructorTypeInstance(tree, typeParameters);
                if (!lhs) {
                    return nullptr;
                }
                // The current token should now be a closing parenthesis, or the type we have just put into
                // lhs, so skip over and the next should be the constructor name.
                if (tokeniser.nextToken() != Token::Identifier) {
                    logError("Expected constructor name, but found '" + tokeniser.currentToken() + "'.");
                    return nullptr;
                }
                std::string constructorName = tokeniser.currentToken();
                if (addedConstructors.contains(constructorName)) {
                    logError("Duplicate constructor name '" + constructorName + "' in type '" + typeName + "'.");
                    return nullptr;
                }
                // Skip to the next token
                tokeniser.nextToken();
                // Finally, we get the rhs with the same procedure as above
                std::unique_ptr<TypeInstanceASTNode> rhs = parseConstructorTypeInstance(tree, typeParameters);
                if (!rhs) {
                    return nullptr;
                }

                // Skip to the next token and check that it is either the '|' or ';' token
                switch (tokeniser.nextToken()) {
                    case Token::TypeUnionSplitter:
                        // Skip over the '|'
                        tokeniser.nextToken();
                    case Token::EndDecl:
                        break;
                    default:
                        logError("Unexpected token '" + tokeniser.currentToken() + "' after infix type declaration.");
                        return nullptr;
                }

                tree->addDataConstructor(
                        typeName,
                        std::make_unique<InfixDataConstructorASTNode>(constructorName, std::move(lhs), std::move(rhs))
                );
                addedConstructors.insert(constructorName);
                break;
            }
            case Token::Identifier: {
                std::string constructorName = tokeniser.currentToken();
                std::vector<std::unique_ptr<TypeInstanceASTNode>> params;

                tokeniser.nextToken();

                while (tokeniser.currentTokenType() != Token::TypeUnionSplitter &&
                       tokeniser.currentTokenType() != Token::EndDecl) {
                    std::unique_ptr<TypeInstanceASTNode> param = parseConstructorTypeInstance(tree, typeParameters);
                    if (!param) {
                        return nullptr;
                    }
                    params.push_back(std::move(param));

                    tokeniser.nextToken();
                }

                tree->addDataConstructor(
                        typeName,
                        std::make_unique<PrefixDataConstructorASTNode>(constructorName, std::move(params))
                );

                if (tokeniser.currentTokenType() == Token::EndDecl) {
                    return std::move(tree);
                }
                // Skip over '|' token
                tokeniser.nextToken();
            }
            case Token::EndDecl:
                break;
            default:
                logError("Unexpected token '" + tokeniser.currentToken() +
                         "' in data constructor.\n"
                         "Constructors should be either 'Cons [a [b ...]]' or 'infix l Cons r'.");
                return nullptr;
        }
    } while (tokeniser.currentTokenType() != Token::EndDecl);

    return std::move(tree);
}

std::unique_ptr<ParseTree> Parser::parseDefinition(std::unique_ptr<ParseTree> tree) {
    return std::move(tree);
}

std::unique_ptr<TypeInstanceASTNode> Parser::parseTypeInstance(const std::unique_ptr<ParseTree> &tree) {
    return parseTypeInstance(tree, false, std::unordered_set<std::string>());
}

std::unique_ptr<TypeInstanceASTNode>
Parser::parseTypeInstance(const std::unique_ptr<ParseTree> &tree, const std::unordered_set<std::string> &polyTypes) {
    return parseTypeInstance(tree, true, polyTypes);
}

std::unique_ptr<TypeInstanceASTNode> Parser::parseTypeInstance(const std::unique_ptr<ParseTree> &tree,
                                                               bool checkPolyTypes,
                                                               const std::unordered_set<std::string> &polyTypes) {
    std::unique_ptr<TypeInstanceASTNode> lhs;
    switch (tokeniser.currentTokenType()) {
        case Token::Identifier:
            if (tree->typeExists(tokeniser.currentToken())) {
                lhs = std::make_unique<PrefixTypeInstanceASTNode>(tokeniser.currentToken());
            } else {
                if (checkPolyTypes && !polyTypes.contains(tokeniser.currentToken())) {
                    logError("Unrecognised type '" + tokeniser.currentToken() + "'.");
                    return nullptr;
                }
                lhs = std::make_unique<PolymorphicTypeInstanceASTNode>(tokeniser.currentToken());
            }
            break;
        case Token::OpenParenthesis:
            tokeniser.nextToken();
            lhs = parseTypeInstance(tree);
            if (tokeniser.currentTokenType() != Token::CloseParenthesis) {
                logError("Expected closing ')' after nested type.");
                return nullptr;
            }
            break;
        case Token::FuncType:
            logError("Function type is an infix type constructor (a -> b).");
            return nullptr;
        case Token::EndDecl:
            logError("Expected type before ';'.");
            return nullptr;
        case Token::InvalidToken:
            return nullptr;
        default:
            // TODO: Handle list and unit types
            logError("Invalid token '" + tokeniser.currentToken() + "' in type instance.");
            return nullptr;
    }

    while (true) {
        switch (tokeniser.nextToken()) {
            case Token::Identifier:
                if (tree->typeExists(tokeniser.currentToken())) {
                    // If the identifier is not an infix type constructor, bind the new type
                    if (tree->getTypeByName(tokeniser.currentToken())->typeUsage() != TypeUsage::Infix) {
                        switch (lhs->typeUsage()) {
                            case TypeUsage::Prefix:
                                dynamic_cast<PrefixTypeInstanceASTNode *>(lhs.get())->bindParameter(
                                        std::make_unique<PrefixTypeInstanceASTNode>(tokeniser.currentToken())
                                );
                                break;
                            case TypeUsage::Infix:
                            case TypeUsage::Function:
                                logError("Cannot bind type to infix type in left position.");
                                return nullptr;
                            case TypeUsage::Polymorphic:
                                logError("Cannot bind type to polymorphic type variable.");
                                break;
                        }
                    } else {
                        // If the token is an infix type constructor, create an infix type
                        std::unique_ptr<InfixTypeInstanceASTNode> infixType = std::make_unique<InfixTypeInstanceASTNode>(
                                tokeniser.currentToken()
                        );
                        infixType->bindLeft(std::move(lhs));
                        // Consume the infix type name token
                        tokeniser.nextToken();
                        // Get the right hand side by recursively parsing the type
                        std::unique_ptr<TypeInstanceASTNode> rhs = parseTypeInstance(tree);
                        // If this failed, return null
                        if (!rhs) {
                            return nullptr;
                        }
                        // otherwise, bind the rhs and return the infix type
                        infixType->bindRight(std::move(rhs));
                        return infixType;
                    }
                } else {
                    switch (lhs->typeUsage()) {
                        case TypeUsage::Prefix:
                            if (checkPolyTypes && !polyTypes.contains(tokeniser.currentToken())) {
                                logError("Unrecognised type '" + tokeniser.currentToken() + "'.");
                                return nullptr;
                            }
                            dynamic_cast<PrefixTypeInstanceASTNode *>(lhs.get())->bindParameter(
                                    std::make_unique<PolymorphicTypeInstanceASTNode>(tokeniser.currentToken())
                            );
                            break;
                        case TypeUsage::Infix:
                        case TypeUsage::Function:
                            logError("Cannot bind polymorphic type variable to infix type in left position.");
                            return nullptr;
                        case TypeUsage::Polymorphic:
                            logError("Cannot bind polymorphic type variable to polymorphic type variable.");
                            break;
                    }
                }
                break;
            case Token::FuncType: {
                // Here, we are handling an infix function type, but it is very similar to other infix types
                std::unique_ptr<FunctionTypeInstanceASTNode> funcType = std::make_unique<FunctionTypeInstanceASTNode>(
                        tokeniser.currentToken()
                );
                funcType->bindLeft(std::move(lhs));
                // Consume the '->' token
                tokeniser.nextToken();
                // Get the rhs recursively
                std::unique_ptr<TypeInstanceASTNode> rhs = parseTypeInstance(tree);
                // Propagate failure
                if (!rhs) {
                    return nullptr;
                }
                // Bind the rhs and return the function type
                funcType->bindRight(std::move(rhs));
                return funcType;
            }
            case Token::CloseParenthesis:
            case Token::EndDecl:
                return std::move(lhs);
            case Token::InvalidToken:
                return nullptr;
            default:
                logError("Invalid token '" + tokeniser.currentToken() + "' in type instance.");
                return nullptr;
        }
    }
}

std::unique_ptr<TypeInstanceASTNode> Parser::parseConstructorTypeInstance(const std::unique_ptr<ParseTree> &tree,
                                                                          const std::unordered_set<std::string> &polyTypes) {
    std::unique_ptr<TypeInstanceASTNode> type;
    switch (tokeniser.currentTokenType()) {
        case Token::OpenParenthesis:
            type = parseTypeInstance(tree, polyTypes);
            if (!type) {
                return nullptr;
            }
            break;
        case Token::Identifier:
            if (tree->typeExists(tokeniser.currentToken())) {
                type = std::make_unique<PrefixTypeInstanceASTNode>(tokeniser.currentToken());
                break;
            }
            if (polyTypes.contains(tokeniser.currentToken())) {
                type = std::make_unique<PolymorphicTypeInstanceASTNode>(tokeniser.currentToken());
                break;
            }
            logError("Unrecognised type '" + tokeniser.currentToken() +
                     "'. Parameters for data constructors must name types or reference type variables.");
            return nullptr;
        default:
            logError("Invalid token '" + tokeniser.currentToken() + "' in infix data constructor.");
            return nullptr;
    }
    return std::move(type);
}

void Parser::logError(const std::string &message) {
    std::cerr << "[Syntax Error] " << tokeniser.sourceFileName() << " (line " << tokeniser.lineNumber() << "): "
              << message
              << std::endl;
}
