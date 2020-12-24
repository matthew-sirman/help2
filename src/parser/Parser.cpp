//
// Created by matthew on 23/11/2020.
//

#include <iostream>

#include "../../include/parser/Parser.h"

Parser::Parser(const Tokeniser &tokeniser)
        : tokeniser(tokeniser) {

}

std::unique_ptr<ParseTree> Parser::parse(std::unique_ptr<ParseTree> &&tree) {
    fileIndex = tree->addFile(tokeniser.sourceFileName());
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
            case Token::OpenParenthesis:
            case Token::Identifier:
                // TODO: Catch empty list and unit
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

    tree->addFunctionDeclaration(std::make_unique<PrefixFunctionDeclASTNode>(
            tokeniser.lineNumber(), fileIndex, functionName, std::move(funcType))
    );

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

    // TODO: Customisable precedence and associativity?
    tree->addFunctionDeclaration(std::make_unique<InfixFunctionDeclASTNode>(
            tokeniser.lineNumber(), fileIndex, functionName, std::move(funcType), 5, Associativity::Left
    ));

    return std::move(tree);
}

std::unique_ptr<ParseTree> Parser::parseValue(std::unique_ptr<ParseTree> tree) {
    // Syntax:
    //      value name: a;
    // The next token should be a name
    if (tokeniser.nextToken() != Token::Identifier) {
        logError("'value' keyword should be followed by value identifier.");
        return nullptr;
    }
    std::string valueName = tokeniser.currentToken();
    if (tree->functionExists(valueName)) {
        logError("Redefinition of function '" + valueName + "'.");
        return nullptr;
    }

    // The next token should be a colon
    if (tokeniser.nextToken() != Token::ExprTypeSpecifier) {
        logError("Value name should be followed by ':' and then value type (e.g. 'value " +
                 valueName + ": a;').");
        return nullptr;
    }
    // Consume the ':' token
    tokeniser.nextToken();

    std::unique_ptr<TypeInstanceASTNode> valueType = parseTypeInstance(tree);

    if (!valueType) {
        return nullptr;
    }

    tree->addFunctionDeclaration(std::make_unique<ValueFunctionDeclASTNode>(
            tokeniser.lineNumber(), fileIndex, valueName, std::move(valueType)
    ));

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
                        tokeniser.lineNumber(), fileIndex, typeName,
                        std::vector<std::string>(typeParameters.begin(), typeParameters.end()))
                );
                return std::move(tree);
            default:
                logError("Unexpected token '" + tokeniser.currentToken() +
                         "' in type constructor declaration.\n"
                         "Type name should be followed by type variables and optionally "
                         "type constructor list (e.g. type " + typeName + " [a [b [c ...]]] [::= ...]).");
                return nullptr;
        }
        if (typeParameters.contains(tokeniser.currentToken())) {
            logError("Duplicate type variable '" + tokeniser.currentToken() + "' in type constructor.");
            return nullptr;
        }
        if (tree->typeExists(tokeniser.currentToken())) {
            logError("Type variable '" + tokeniser.currentToken() +
                     "' is already a type name.\n"
                     "Consider using uppercase names for types and lowercase for type variables.");
            return nullptr;
        }
        typeParameters.insert(tokeniser.currentToken());
    }
    // Add the type declaration to the parse tree - this allows for recursive types
    const std::unique_ptr<TypeDeclASTNode> &typeNode = tree->addTypeDeclaration(std::make_unique<PrefixTypeDeclASTNode>(
            tokeniser.lineNumber(), fileIndex, typeName,
            std::vector<std::string>(typeParameters.begin(), typeParameters.end())
    ));
    // Skip over '::=' token
    tokeniser.nextToken();

    // Now we are looking for constructors of which there should be one or more
    return parseDataConstructors(std::move(tree), typeNode, typeParameters);
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
        return nullptr;
    }

    if (tree->typeExists(typeName)) {
        logError("Redefinition of type '" + typeName + "'.");
        return nullptr;
    }

    // Add the type declaration to the parse tree - this allows for recursive types
    const std::unique_ptr<TypeDeclASTNode> &typeNode =
            tree->addTypeDeclaration(std::make_unique<InfixTypeDeclASTNode>(
                    tokeniser.lineNumber(), fileIndex, typeName, lhs, rhs
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
    return parseDataConstructors(std::move(tree), typeNode, {lhs, rhs});
}

std::unique_ptr<ParseTree> Parser::parseDataConstructors(std::unique_ptr<ParseTree> tree,
                                                         const std::unique_ptr<TypeDeclASTNode> &type,
                                                         const std::unordered_set<std::string> &typeParameters) {
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
                if (tokeniser.currentTokenType() != Token::Identifier) {
                    logError("Expected constructor name, but found '" + tokeniser.currentToken() + "'.");
                    return nullptr;
                }
                std::string constructorName = tokeniser.currentToken();
                if (tree->constructorExists(constructorName)) {
                    logError("Re-used constructor name '" + constructorName + "' in type '" + type->typeName() + "'.");
                    return nullptr;
                }
                // Skip to the next token
                tokeniser.nextToken();
                // Finally, we get the rhs with the same procedure as above
                std::unique_ptr<TypeInstanceASTNode> rhs = parseConstructorTypeInstance(tree, typeParameters);
                if (!rhs) {
                    return nullptr;
                }

                // Check that the token is either the '|' or ';' token
                switch (tokeniser.currentTokenType()) {
                    case Token::TypeUnionSplitter:
                        // Skip over the '|'
                        tokeniser.nextToken();
                    case Token::EndDecl:
                        break;
                    default:
                        logError("Unexpected token '" + tokeniser.currentToken() + "' after infix type declaration.");
                        return nullptr;
                }

                tree->addDataConstructor(type, std::make_unique<InfixDataConstructorASTNode>(
                        tokeniser.lineNumber(), fileIndex, type, constructorName, std::move(lhs), std::move(rhs)
                ));
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

                    if (tokeniser.currentTokenType() == Token::TypeUnionSplitter ||
                        tokeniser.currentTokenType() == Token::EndDecl) {
                        break;
                    }

                    tokeniser.nextToken();
                }

                tree->addDataConstructor(type, std::make_unique<PrefixDataConstructorASTNode>(
                        tokeniser.lineNumber(), fileIndex, type, constructorName, std::move(params)
                ));

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
    // Definitions come in three forms: values, prefix functions and infix functions.
    // Prefix function syntax:
    //      f [a [b ...]] => expr;
    // Infix function syntax:
    //      a f b => expr;
    // Value function syntax:
    //      f = expr;
    // (note that a, b, ... are patterns)

    // First, we need to work out which of the above three this definition is.
    std::string functionName;
    FunctionUsage usage;
    // Create a set of binders to avoid duplication of names and for parsing the body
    std::unordered_set<std::string> usedBinders;
    // We know that the token is either an identifier or bracket because that is the only
    // time "parse" calls this function.
    if (tokeniser.currentTokenType() == Token::Identifier) {
        if (tree->functionExists(tokeniser.currentToken())) {
            functionName = tokeniser.currentToken();
            const std::unique_ptr<FunctionDeclASTNode> &decl = tree->getFuncByName(functionName)->decl();
            usage = decl->funcUsage();
            if (usage == FunctionUsage::Infix) {
                logError(
                        "Infix function definitions should not start with function name. (e.g. a " + functionName +
                        " b => expr;).");
                return nullptr;
            } else if (usage == FunctionUsage::Value) {
                if (tokeniser.nextToken() != Token::ValueBodySpecifier) {
                    logError("Value definition should be followed by '=' and have no parameters.");
                    return nullptr;
                }
                tokeniser.nextToken();
                std::unique_ptr<ExpressionASTNode> body = parseExpression(tree, {}, 0);
                if (!body) {
                    return nullptr;
                }
                tree->addFunctionImplementation(functionName, std::make_unique<PrefixFunctionImplASTNode>(
                        tokeniser.lineNumber(), fileIndex, std::move(body)
                ));
                // If the next token is not a ';', there as an error
                if (tokeniser.currentTokenType() != Token::EndDecl) {
                    return nullptr;
                }

                return std::move(tree);
            }

            // Otherwise, this is a prefix function definition
            std::vector<std::unique_ptr<PatternASTNode>> patterns;
            // Skip over name
            tokeniser.nextToken();
            while (tokeniser.currentTokenType() != Token::FuncBodySpecifier) {
                std::unique_ptr<PatternASTNode> pattern = parsePattern(tree, usedBinders);
                if (!pattern) {
                    return nullptr;
                }
                patterns.push_back(std::move(pattern));
            }
            // If too many arguments are applied to the function
            size_t maxArgs = decl->maxArgs();
            if (maxArgs < patterns.size()) {
                logError("Too many arguments for function '" + functionName + "' (expected <= " +
                         std::to_string(maxArgs) + ").");
                return nullptr;
            }
            // Skip over '=>'
            tokeniser.nextToken();
            std::unique_ptr<ExpressionASTNode> body = parseExpression(tree, usedBinders, 0);
            if (!body) {
                return nullptr;
            }
            tree->addFunctionImplementation(functionName, std::make_unique<PrefixFunctionImplASTNode>(
                    tokeniser.lineNumber(), fileIndex, std::move(body), std::move(patterns)
            ));
            // If the next token is not a ';', there as an error
            if (tokeniser.currentTokenType() != Token::EndDecl) {
                return nullptr;
            }

            return std::move(tree);
        }
    }

    // If we have fallen through to this point, the function is either infix or erroneous.
    std::unique_ptr<PatternASTNode> lhs = parsePattern(tree, usedBinders);
    if (!lhs) {
        return nullptr;
    }
    // The next token should now be an identifier
    switch (tokeniser.currentTokenType()) {
        case Token::ValueBodySpecifier:
            // If we mistakenly parsed a missing value name as a pattern
            if (lhs->patternUsage() == PatternUsage::Variable) {
                logError(
                        "Unrecognised value name '" + dynamic_cast<VariablePatternASTNode *>(lhs.get())->name() + "'.");
            } else {
                logError("Cannot assign expression to data constructor.");
            }
            return nullptr;
        case Token::FuncBodySpecifier:
            // If we mistakenly parsed a missing function name as a pattern
            if (lhs->patternUsage() == PatternUsage::Variable) {
                logError("Unrecognised function name '" + dynamic_cast<VariablePatternASTNode *>(lhs.get())->name() +
                         "'.");
            } else {
                logError("Cannot assign expression to data constructor.");
            }
            return nullptr;
        case Token::Identifier:
            break;
        default:
            logError("Expected infix function identifier after argument.");
            return nullptr;
    }
    // Next, we check the function exists
    if (!tree->functionExists(tokeniser.currentToken())) {
        if (lhs->patternUsage() == PatternUsage::Variable) {
            logError("Unrecognised function name '" + dynamic_cast<VariablePatternASTNode *>(lhs.get())->name() +
                     "'.");
        } else {
            logError("Unrecognised infix function name '" + tokeniser.currentToken() + "'.");
        }
        return nullptr;
    }
    functionName = tokeniser.currentToken();
    // Skip to next token and check for func body error
    if (tokeniser.nextToken() == Token::FuncBodySpecifier) {
        logError("Expected right hand side argument after infix function name before '=>' (e.g. a " + functionName +
                 " b => expr;).");
        return nullptr;
    }
    std::unique_ptr<PatternASTNode> rhs = parsePattern(tree, usedBinders);
    if (!rhs) {
        return nullptr;
    }
    if (tokeniser.currentTokenType() != Token::FuncBodySpecifier) {
        logError("Unexpected token '" + tokeniser.currentToken() + "'. Expected '=>'.");
        return nullptr;
    }
    // Skip over '=>'
    tokeniser.nextToken();
    std::unique_ptr<ExpressionASTNode> body = parseExpression(tree, usedBinders, 0);
    if (!body) {
        return nullptr;
    }
    tree->addFunctionImplementation(functionName, std::make_unique<InfixFunctionImplASTNode>(
            tokeniser.lineNumber(), fileIndex, std::move(body), std::move(lhs), std::move(rhs)
    ));
    // If the next token is not a ';', there as an error
    if (tokeniser.currentTokenType() != Token::EndDecl) {
        return nullptr;
    }

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
                lhs = std::make_unique<PrefixTypeInstanceASTNode>(
                        tokeniser.lineNumber(), fileIndex, tokeniser.currentToken()
                );
            } else {
                if (checkPolyTypes && !polyTypes.contains(tokeniser.currentToken())) {
                    logError("Unrecognised type '" + tokeniser.currentToken() + "'.");
                    return nullptr;
                }
                lhs = std::make_unique<PolymorphicTypeInstanceASTNode>(
                        tokeniser.lineNumber(), fileIndex, tokeniser.currentToken()
                );
            }
            break;
        case Token::OpenParenthesis:
            tokeniser.nextToken();
            lhs = parseTypeInstance(tree);
            if (!lhs) {
                return nullptr;
            }
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

    // Skip to next token
    tokeniser.nextToken();

    while (true) {
        switch (tokeniser.currentTokenType()) {
            case Token::Identifier:
                if (tree->typeExists(tokeniser.currentToken())) {
                    // If the identifier is not an infix type constructor, bind the new type
                    if (tree->getTypeByName(tokeniser.currentToken())->typeUsage() != TypeUsage::Infix) {
                        switch (lhs->typeUsage()) {
                            case TypeUsage::Prefix:
                                dynamic_cast<PrefixTypeInstanceASTNode *>(lhs.get())->bindParameter(
                                        std::make_unique<PrefixTypeInstanceASTNode>(
                                                tokeniser.lineNumber(), fileIndex, tokeniser.currentToken()
                                        ));
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
                                tokeniser.lineNumber(), fileIndex, tokeniser.currentToken()
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
                                    std::make_unique<PolymorphicTypeInstanceASTNode>(
                                            tokeniser.lineNumber(), fileIndex, tokeniser.currentToken()
                                    ));
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
                        tokeniser.lineNumber(), fileIndex, tokeniser.currentToken()
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
            case Token::TypeUnionSplitter:
                return std::move(lhs);
            case Token::InvalidToken:
                return nullptr;
            default:
                logError("Invalid token '" + tokeniser.currentToken() + "' in type instance.");
                return nullptr;
        }
        // Skip the current token
        tokeniser.nextToken();
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
                type = std::make_unique<PrefixTypeInstanceASTNode>(
                        tokeniser.lineNumber(), fileIndex, tokeniser.currentToken()
                );
                tokeniser.nextToken();
                break;
            }
            if (polyTypes.contains(tokeniser.currentToken())) {
                type = std::make_unique<PolymorphicTypeInstanceASTNode>(
                        tokeniser.lineNumber(), fileIndex, tokeniser.currentToken()
                );
                tokeniser.nextToken();
                break;
            }
            logError("Unrecognised type '" + tokeniser.currentToken() +
                     "'. Parameters for data constructors must name types or reference type variables.");
            return nullptr;
        default:
            logError("Invalid token '" + tokeniser.currentToken() + "' in data constructor.");
            return nullptr;
    }
    return std::move(type);
}

std::unique_ptr<PatternASTNode>
Parser::parsePattern(const std::unique_ptr<ParseTree> &tree, std::unordered_set<std::string> &usedBinders) {
    switch (tokeniser.currentTokenType()) {
        case Token::OpenParenthesis:
            // Skip over parenthesis
            tokeniser.nextToken();
            // TODO: Handle infix constructors
            if (tokeniser.currentTokenType() != Token::Identifier) {
                logError("Unexpected token '" + tokeniser.currentToken() + "'. Expected identifier in nested pattern.");
                return nullptr;
            }
            if (tree->constructorExists(tokeniser.currentToken())) {
                std::string constructorName = tokeniser.currentToken();
                std::vector<std::unique_ptr<PatternASTNode>> subPatterns;
                tokeniser.nextToken();
                while (tokeniser.currentTokenType() != Token::CloseParenthesis) {
                    std::unique_ptr<PatternASTNode> pattern = parsePattern(tree, usedBinders);
                    if (!pattern) {
                        return nullptr;
                    }
                    subPatterns.push_back(std::move(pattern));
                }
                size_t expectedArgs = tree->getConstructorByName(constructorName)->args();
                if (subPatterns.size() != expectedArgs) {
                    logError("Incorrect number of arguments for constructor '" + constructorName + "' (expected " +
                             std::to_string(expectedArgs) + ").");
                    return nullptr;
                }
                // Skip over close parenthesis
                tokeniser.nextToken();
                return std::make_unique<ConstructorPatternASTNode>(
                        tokeniser.lineNumber(), fileIndex, constructorName, std::move(subPatterns)
                );
            } else {
                if (usedBinders.contains(tokeniser.currentToken())) {
                    logError("Duplicate variable name '" + tokeniser.currentToken() + "' used in pattern expression.");
                    return nullptr;
                }
                std::string binder = tokeniser.currentToken();
                // Check that the next token is a close parenthesis
                if (tokeniser.nextToken() != Token::CloseParenthesis) {
                    logError("Unrecognised data constructor '" + binder + "'.");
                    return nullptr;
                }
                // Skip over the close parenthesis
                tokeniser.nextToken();
                usedBinders.insert(binder);
                return std::make_unique<VariablePatternASTNode>(tokeniser.lineNumber(), fileIndex, binder);
            }
        case Token::Identifier:
            if (tree->constructorExists(tokeniser.currentToken())) {
                std::string constructor = tokeniser.currentToken();
                // Skip over token
                tokeniser.nextToken();
                return std::make_unique<ConstructorPatternASTNode>(tokeniser.lineNumber(), fileIndex, constructor);
            } else {
                if (usedBinders.contains(tokeniser.currentToken())) {
                    logError("Duplicate variable name '" + tokeniser.currentToken() + "' used in pattern expression.");
                    return nullptr;
                }
                std::string binder = tokeniser.currentToken();
                // Skip to next token
                tokeniser.nextToken();
                usedBinders.insert(binder);
                return std::make_unique<VariablePatternASTNode>(tokeniser.lineNumber(), fileIndex, binder);
            }
        default:
            logError("Unexpected token '" + tokeniser.currentToken() + "' in pattern.");
            return nullptr;
    }
}

std::unique_ptr<ExpressionASTNode>
Parser::parseExpression(const std::unique_ptr<ParseTree> &tree, const std::unordered_set<std::string> &binders,
                        int currentPrecedence) {
    // We want to build up an expression tree going left to right
    std::unique_ptr<ExpressionASTNode> expr;
    while (true) {
        switch (tokeniser.currentTokenType()) {
            // Handle nested expressions
            case Token::OpenParenthesis: {
                // Skip over '('
                tokeniser.nextToken();
                // Parse sub expression with precedence 0 (reset)
                std::unique_ptr<ExpressionASTNode> nested = parseExpression(tree, binders, 0);
                if (!nested) {
                    return nullptr;
                }
                if (tokeniser.currentTokenType() != Token::CloseParenthesis) {
                    logError("Unmatched '(' detected.");
                    return nullptr;
                }
                tokeniser.nextToken();
                if (!expr) {
                    expr = std::move(nested);
                } else {
                    expr = std::make_unique<ApplicationASTNode>(
                            tokeniser.lineNumber(), fileIndex, std::move(expr), std::move(nested)
                    );
                }
                break;
            }
                // Handle unravelling from the recursion
            case Token::CloseParenthesis:
            case Token::InSpecifier:
            case Token::EndDecl:
                return std::move(expr);
            case Token::Identifier:
                // First, check if this is in the binders
                // Next, check if it is a known function
                // Then, check if it is a type constructor
                // Finally, fail
                if (binders.contains(tokeniser.currentToken())) {
                    std::unique_ptr<VariableASTNode> varExpr = std::make_unique<VariableASTNode>(
                            tokeniser.lineNumber(), fileIndex, tokeniser.currentToken()
                    );
                    if (!expr) {
                        expr = std::move(varExpr);
                    } else {
                        expr = std::make_unique<ApplicationASTNode>(
                                tokeniser.lineNumber(), fileIndex, std::move(expr), std::move(varExpr)
                        );
                    }
                    // Skip to next
                    tokeniser.nextToken();
                } else if (tree->functionExists(tokeniser.currentToken())) {
                    std::unique_ptr<FunctionASTNode> func = std::make_unique<FunctionASTNode>(
                            tokeniser.lineNumber(), fileIndex, tokeniser.currentToken()
                    );
                    const std::unique_ptr<FunctionDeclASTNode> &decl = tree->getFuncByName(
                            tokeniser.currentToken())->decl();
                    switch (decl->funcUsage()) {
                        case FunctionUsage::Prefix:
                        case FunctionUsage::Value: {
                            if (!expr) {
                                expr = std::move(func);
                            } else {
                                expr = std::make_unique<ApplicationASTNode>(
                                        tokeniser.lineNumber(), fileIndex, std::move(expr), std::move(func)
                                );
                            }
                            // Skip to next
                            tokeniser.nextToken();
                            break;
                        }
                        case FunctionUsage::Infix: {
                            if (!expr) {
                                logError("Expected expression before infix function '" + tokeniser.currentToken() +
                                         "'.");
                                return nullptr;
                            }
                            InfixFunctionDeclASTNode *infixDecl = dynamic_cast<InfixFunctionDeclASTNode *>(decl.get());
                            // Check that this operator has sufficiently high precedence. If it doesn't,
                            // return the current expression.
                            if (infixDecl->operatorPrecedence() * 2 < currentPrecedence) {
                                return std::move(expr);
                            }
                            // Skip over infix operator
                            tokeniser.nextToken();
                            // Recursively parse the right hand side of the operator. Here, we set the precedence
                            // as 2 * the operator precedence + associativity. This way, we can preserve
                            // precedence of operators.
                            std::unique_ptr<ExpressionASTNode> rhs = parseExpression(
                                    tree, binders,
                                    infixDecl->operatorPrecedence() * 2 + (int) infixDecl->associativity()
                            );
                            // Propagate error
                            if (!rhs) {
                                return nullptr;
                            }
                            // Construct the application if this was a sufficiently high precedence operator
                            expr = std::make_unique<ApplicationASTNode>(
                                    tokeniser.lineNumber(), fileIndex, std::move(func), std::move(expr)
                            );
                            expr = std::make_unique<ApplicationASTNode>(
                                    tokeniser.lineNumber(), fileIndex, std::move(expr), std::move(rhs)
                            );
                            break;
                        }
                    }
                } else if (tree->constructorExists(tokeniser.currentToken())) {
                    std::unique_ptr<ConstructorASTNode> cons = std::make_unique<ConstructorASTNode>(
                            tokeniser.lineNumber(), fileIndex, tokeniser.currentToken()
                    );
                    switch (tree->getConstructorByName(tokeniser.currentToken())->usage()) {
                        case TypeUsage::Prefix:
                            if (expr) {
                                // If there is already an expression, this is an application,
                                // so this constructor is applied to a function directly (i.e.
                                // not called with any further values)
                                expr = std::make_unique<ApplicationASTNode>(
                                        tokeniser.lineNumber(), fileIndex, std::move(expr), std::move(cons)
                                );
                            } else {
                                // Otherwise, just set the expression to this value
                                expr = std::move(cons);
                            }
                            // Skip to next
                            tokeniser.nextToken();
                            break;
                        case TypeUsage::Infix: {
                            if (!expr) {
                                logError("Expected expression before infix constructor '" + tokeniser.currentToken() +
                                         "'.");
                                return nullptr;
                            }
                            // Skip over the constructor name
                            tokeniser.nextToken();
                            // Get right hand argument
                            std::unique_ptr<ExpressionASTNode> rhs = parseExpression(tree, binders, currentPrecedence);
                            if (!rhs) {
                                return nullptr;
                            }
                            // Apply the left argument
                            expr = std::make_unique<ApplicationASTNode>(
                                    tokeniser.lineNumber(), fileIndex, std::move(cons), std::move(expr)
                            );
                            // Apply the right argument
                            expr = std::make_unique<ApplicationASTNode>(
                                    tokeniser.lineNumber(), fileIndex, std::move(expr), std::move(rhs)
                            );
                            break;
                        }
                        default:
                            return nullptr;
                    }
                } else {
                    logError("Unrecognised symbol '" + tokeniser.currentToken() + "'.");
                    return nullptr;
                }
                break;
            case Token::SpecialInfixOperator: {
                // These should always be in the parse tree
                if (!tree->functionExists(tokeniser.currentToken())) {
                    logError("DEVELOPER: Missing builtin '" + tokeniser.currentToken() + "'.");
                    return nullptr;
                }
                // This should follow an expression
                if (!expr) {
                    logError("Expected expression before infix operator '" + tokeniser.currentToken() +
                             "'.");
                    return nullptr;
                }
                InfixFunctionDeclASTNode *decl = dynamic_cast<InfixFunctionDeclASTNode *>(tree->getFuncByName(
                        tokeniser.currentToken())->decl().get());
                // If this operator has lower precedence than the previous then just drop out with the expression
                if (decl->operatorPrecedence() * 2 < currentPrecedence) {
                    return std::move(expr);
                }
                // Otherwise, make the function node and skip over the token
                std::unique_ptr<FunctionASTNode> func = std::make_unique<FunctionASTNode>(
                        tokeniser.lineNumber(), fileIndex, tokeniser.currentToken()
                );
                tokeniser.nextToken();
                // Now parse the right hand side of the operator with this precedence plus associativity as
                // the current precedence
                std::unique_ptr<ExpressionASTNode> rhs = parseExpression(
                        tree, binders, decl->operatorPrecedence() * 2 + (int) decl->associativity()
                );
                // Propagate null
                if (!rhs) {
                    return nullptr;
                }
                // Build expression calls
                expr = std::make_unique<ApplicationASTNode>(
                        tokeniser.lineNumber(), fileIndex, std::move(func), std::move(expr)
                );
                expr = std::make_unique<ApplicationASTNode>(
                        tokeniser.lineNumber(), fileIndex, std::move(expr), std::move(rhs)
                );
                break;
            }
            case Token::SpecialPrefixOperator: {
                // These should also always be in the parse tree
                if (!tree->functionExists(tokeniser.currentToken())) {
                    logError("DEVELOPER: Missing builtin '" + tokeniser.currentToken() + "'.");
                    return nullptr;
                }
                std::unique_ptr<FunctionASTNode> func = std::make_unique<FunctionASTNode>(
                        tokeniser.lineNumber(), fileIndex, tokeniser.currentToken()
                );
                if (!expr) {
                    expr = std::move(func);
                } else {
                    expr = std::make_unique<ApplicationASTNode>(
                            tokeniser.lineNumber(), fileIndex, std::move(expr), std::move(func)
                    );
                }
                // Skip to next
                tokeniser.nextToken();
                break;
            }
            case Token::LetSpecifier: {
                if (expr) {
                    logError("Cannot directly apply 'let' expression to function.");
                    return nullptr;
                }
                std::unique_ptr<LetBindingASTNode> letBinding = parseLetBinding(tree, binders);
                if (!letBinding) {
                    return nullptr;
                }
                expr = std::move(letBinding);
                break;
            }
            case Token::LambdaSpecifier: {
                if (expr) {
                    logError("Cannot directly apply lambda ($) expression to function.");
                    return nullptr;
                }
                std::unique_ptr<LambdaExpressionASTNode> lambda = parseLambda(tree, binders);
                if (!lambda) {
                    return nullptr;
                }
                expr = std::move(lambda);
                break;
            }
            case Token::IntegralLiteral: {
                std::unique_ptr<IntegralConstructorASTNode> literal = std::make_unique<IntegralConstructorASTNode>(
                        tokeniser.lineNumber(), fileIndex, std::stoll(tokeniser.currentToken())
                );
                if (!expr) {
                    expr = std::move(literal);
                } else {
                    expr = std::make_unique<ApplicationASTNode>(
                            tokeniser.lineNumber(), fileIndex, std::move(expr), std::move(literal)
                    );
                }
                break;
            }
            case Token::DecimalLiteral: {
                std::unique_ptr<DecimalConstructorASTNode> literal = std::make_unique<DecimalConstructorASTNode>(
                        tokeniser.lineNumber(), fileIndex, std::stod(tokeniser.currentToken())
                );
                if (!expr) {
                    expr = std::move(literal);
                } else {
                    expr = std::make_unique<ApplicationASTNode>(
                            tokeniser.lineNumber(), fileIndex, std::move(expr), std::move(literal)
                    );
                }
                break;
            }
            case Token::StringLiteral:
            case Token::OpenList:
            case Token::EmptyList:
            case Token::Unit:
                // TODO: Implement these!
                logError("NOT YET IMPLEMENTED!");
                return nullptr;
            default:
                logError("Unexpected token '" + tokeniser.currentToken() + "' in expression.");
                return nullptr;
        }
    }
}

std::unique_ptr<LetBindingASTNode>
Parser::parseLetBinding(const std::unique_ptr<ParseTree> &tree, const std::unordered_set<std::string> &binders) {
    if (tokeniser.nextToken() != Token::Identifier) {
        logError("Expected identifier after 'let'.");
        return nullptr;
    }
    std::string var = tokeniser.currentToken();
    if (tokeniser.nextToken() != Token::ValueBodySpecifier) {
        logError("Expected '=' after let variable name.");
        return nullptr;
    }
    tokeniser.nextToken();
    std::unique_ptr<ExpressionASTNode> body = parseExpression(tree, binders, 0);
    if (!body) {
        return nullptr;
    }
    if (tokeniser.currentTokenType() != Token::InSpecifier) {
        logError("Expected 'in' after let binding body.");
        return nullptr;
    }
    // Skip over 'in'
    tokeniser.nextToken();
    std::unordered_set<std::string> usageBinders = binders;
    usageBinders.insert(var);
    std::unique_ptr<ExpressionASTNode> usage = parseExpression(tree, usageBinders, 0);
    if (!usage) {
        return nullptr;
    }
    return std::make_unique<LetBindingASTNode>(
            tokeniser.lineNumber(), fileIndex, var, std::move(body), std::move(usage)
    );
}

std::unique_ptr<LambdaExpressionASTNode>
Parser::parseLambda(const std::unique_ptr<ParseTree> &tree, const std::unordered_set<std::string> &binders) {
    if (tokeniser.nextToken() != Token::Identifier) {
        logError("Expected identifier after '$' (anonymous function).");
        return nullptr;
    }
    std::string var = tokeniser.currentToken();
    if (tokeniser.nextToken() != Token::FuncBodySpecifier) {
        logError("Expected '=>' after anonymous function variable name.");
        return nullptr;
    }
    tokeniser.nextToken();
    std::unordered_set<std::string> bodyBinders = binders;
    bodyBinders.insert(var);
    std::unique_ptr<ExpressionASTNode> body = parseExpression(tree, bodyBinders, 0);
    if (!body) {
        return nullptr;
    }
    return std::make_unique<LambdaExpressionASTNode>(
            tokeniser.lineNumber(), fileIndex, var, std::move(body)
    );
}

void Parser::logError(const std::string &message) {
    std::cerr << "[Parse Error] " << tokeniser.sourceFileName() << " (line " << tokeniser.lineNumber() << "): "
              << message << std::endl;
}
