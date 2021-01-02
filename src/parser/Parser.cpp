//
// Created by matthew on 30/12/2020.
//

#include "../../include/parser/Parser.h"

#include <numeric>
#include <fstream>

Parser::Parser(FileStructure &fileStructure, const std::filesystem::path &sourceFile)
        : Parser(fileStructure, sourceFile, loadSourceFile(sourceFile)) {

}

Parser::Parser(FileStructure &fileStructure, const std::filesystem::path &sourceFile, const std::string &source)
        : fileStructure(fileStructure), fileIndex(fileStructure.allocateFileIndex(sourceFile)), tokeniser(source) {

}

void Parser::parse(ParseTree &tree) {
    TokeniseFunc importStatement =
            [this, &tree](Token &token) {
                if (!Tokeniser::tokenImport(token)) {
                    return false;
                }
                parseImport(token, tree);
                return true;
            };

    TokeniseFunc funcDecl =
            [this, &tree](Token &token) {
                std::unique_ptr<FunctionDeclASTNode> function;
                if (!parseAnyFunctionDecl(token, tree, function)) {
                    return false;
                }
                if (function) {
                    tree.addFunctionDeclaration(std::move(function));
                }
                return true;
            };

    TokeniseFunc typeclassDecl =
            [this, &tree](Token &token) {
                if (!Tokeniser::tokenTypeclass(token)) {
                    return false;
                }
                parseTypeclass(token, tree);
                return true;
            };

    TokeniseFunc typeclassInstance =
            [this, &tree](Token &token) {
                if (!Tokeniser::tokenInstance(token)) {
                    return false;
                }
                parseTypeclassInstanceDecl(token, tree);
                return true;
            };

    for (const Tokeniser &statement : tokeniser.findAllStatements()) {
        Token topLevel = statement.startToken();
        // Check for each possible top level token type

        // Try parsing all of the declaration statements
        if (!Tokeniser::firstOf(
                topLevel,
                importStatement,
                funcDecl,
                topLevelTypeDecl(tree, true),
                topLevelTypeDecl(tree, false),
                typeclassDecl,
                typeclassInstance)) {
            // If this fails, we didn't find anything, so try parsing an implementation
            // Note: the call succeeds as soon as a statement type is determined even if the parsing of the
            // statement fails, so falling through to this point indicates that we did not find any of the above
            // declarations, not that we found one which failed to parse
            if (Tokeniser::peek(topLevel, Tokeniser::identifierToken)) {
                parseImplementation(topLevel, tree);
            }
        }

        // After parsing the statement, the token should necessarily be a top level token, otherwise there was
        // an error.
        if (!Tokeniser::tokenEndDecl(topLevel)) {
            appendError(topLevel) << "Unexpected token: '" << Tokeniser::scanToken(topLevel).token << "'.";
        }
    }
}

std::string Parser::loadSourceFile(const std::filesystem::path &sourcePath) {
    std::ifstream sourceFile(sourcePath);

    std::string sourceData;
    sourceFile.seekg(std::ios::end);
    sourceData.reserve(sourceFile.tellg());
    sourceFile.seekg(std::ios::beg);

    sourceData.assign(std::istreambuf_iterator<char>(sourceFile), std::istreambuf_iterator<char>());

    return std::move(sourceData);
}

void Parser::parseImport(Token &token, ParseTree &tree) {
    // Import statement should look like either:
    // 1.
    //  import Path.To.File;
    // 2.
    //  import "Path/To/File.help";

    std::filesystem::path pathToFile;

    StringToken pathToken = Tokeniser::stringToken(token);
    bool qualified;
    if (Tokeniser::tokenStringLiteral(pathToken)) {
        pathToFile = std::move(pathToken.value);
        token.update(pathToken);
        qualified = true;
    } else {
        IdentifierToken idToken = Tokeniser::identifierToken(token);
        do {
            if (!Tokeniser::tokenIdentifier(idToken)) {
                appendError(idToken) << "Expected name identifier in import statement.";
                return;
            }
            pathToFile /= idToken.identifier;
        } while (Tokeniser::tokenImportDelimiter(idToken));
        token.update(idToken);
        qualified = false;
    }

    // Check that the next token is a ;
    if (!Tokeniser::peek(token, Tokeniser::tokenEndDecl)) {
        appendError(token) << "Expected ';' after import statement.";
        return;
    }

    // Now actually find the file, or report an error.
    std::optional<std::filesystem::path> qualifiedPath = qualified ?
                                                         fileStructure.searchForQualifiedFile(pathToFile) :
                                                         fileStructure.searchForFile(pathToFile);

    if (!qualifiedPath.has_value()) {
        appendError(token) << "Could not find imported file '" << pathToFile.generic_string() << "'.";
        return;
    }

    // Finally, create the parser and parse the module
    Parser subParser(fileStructure, qualifiedPath.value());
    subParser.parse(tree);
    // Move the error list to the end of this module's error list
    errorList.resize(errorList.size() + subParser.errorList.size());
    std::move(subParser.errorList.begin(), subParser.errorList.end(), errorList.end());
}

bool
Parser::parseAnyFunctionDecl(Token &token, const ParseTree &tree, std::unique_ptr<FunctionDeclASTNode> &function) {
    // Next we lay out pattern functions for each possible type of function declaration. Each of these
    // also captures the function pointer, and assigns the declared function pointer to that value. This
    // has the effect of extracting the correct declaration because the call to the specific parseFunctionDecl
    // will return the function pointer if successful, otherwise will return null. This will then be set
    // to the function pointer, and then returning true will break us out of the firstOf call below. Therefore,
    // the return is propagated.
    TokeniseFunc prefixFunc =
            [this, &tree, &function](Token &token) {
                if (!Tokeniser::tokenFunc(token)) {
                    return false;
                }
                function = parseFunctionDecl(token, tree, FunctionUsage::Prefix);
                return true;
            };
    TokeniseFunc infixFunc =
            [this, &tree, &function](Token &token) {
                return Tokeniser::compose(
                        token,
                        Tokeniser::tokenInfix, Tokeniser::tokenFunc,
                        [this, &tree, &function](Token &t2) {
                            function = parseFunctionDecl(t2, tree, FunctionUsage::Infix, Associativity::Left);
                            return true;
                        });
            };
    TokeniseFunc infixLeftFunc =
            [this, &tree, &function](Token &token) {
                return Tokeniser::compose(
                        token,
                        Tokeniser::tokenInfix, Tokeniser::tokenLeft,
                        Tokeniser::tokenAssociates, Tokeniser::tokenFunc,
                        [this, &tree, &function](Token &t2) {
                            function = parseFunctionDecl(t2, tree, FunctionUsage::Infix, Associativity::Left);
                            return true;
                        });
            };
    TokeniseFunc infixRightFunc =
            [this, &tree, &function](Token &token) {
                return Tokeniser::compose(
                        token,
                        Tokeniser::tokenInfix, Tokeniser::tokenRight,
                        Tokeniser::tokenAssociates, Tokeniser::tokenFunc,
                        [this, &tree, &function](Token &t2) {
                            function = parseFunctionDecl(t2, tree, FunctionUsage::Infix, Associativity::Right);
                            return true;
                        });
            };
    TokeniseFunc valueFunc =
            [this, &tree, &function](Token &token) {
                if (!Tokeniser::tokenValue(token)) {
                    return false;
                }
                function = parseFunctionDecl(token, tree, FunctionUsage::Value);
                return true;
            };
    // Finally, we return the first of call to these functions. This will return true if any succeeded,
    // indicating that the function variable now contains something of use (either nullptr indicating failure,
    // or the actual function). Will return false if the token wasn't found, in which case function was not
    // changed.
    return Tokeniser::firstOf(
            token,
            prefixFunc,
            infixFunc,
            infixLeftFunc,
            infixRightFunc,
            valueFunc
    );
}

std::unique_ptr<FunctionDeclASTNode> Parser::parseFunctionDecl(Token &token, const ParseTree &tree, FunctionUsage mode,
                                                               std::optional<Associativity> assoc) {
    // We have already parsed the func keyword, so we expect the total statement to look like:
    //  [infix [(left|right) associates] func name: a -> b;

    std::string usageHelper;
    std::string keyword;
    switch (mode) {
        case FunctionUsage::Value:
            usageHelper = "value name: a;";
            keyword = "value";
            break;
        case FunctionUsage::Prefix:
            usageHelper = "func name: a -> b;";
            keyword = "func";
            break;
        case FunctionUsage::Infix:
            usageHelper = "infix func name: a -> b -> c;";
            keyword = "func";
            break;
    }

    // Get a string token for the name
    IdentifierToken idToken = Tokeniser::identifierToken(token);
    if (!Tokeniser::tokenIdentifier(idToken)) {
        appendError(idToken) << "Expected identifier after '" << keyword << "' keyword. Use like: " << std::endl
                             << usageHelper;
        return nullptr;
    }

    // Check if the function already exists
    if (tree.constructorExists(idToken.identifier)) {
        appendError(idToken) << "Constructor with name '" << idToken.identifier << "' already exists." << std::endl
                             << "Consider using Upper camel casing for constructor names, and lower camel casing for function names.";
        return nullptr;
    }
    if (tree.functionExists(idToken.identifier)) {
        appendError(idToken) << "Function with name '" << idToken.identifier << "' already exists.";
        return nullptr;
    }

    // Next check that we have a colon token
    if (!Tokeniser::tokenExprTypeSpecifier(idToken)) {
        appendError(idToken) << "Expected ':' after function name. Use like: " << std::endl
                             << usageHelper;
        return nullptr;
    }

    // Now parse the type instance
    std::unique_ptr<TypeInstanceASTNode> fType;
    if (!parseTypeInstance(idToken, tree, fType)) {
        return nullptr;
    }
    if (!fType) {
        appendError(idToken) << "Expected type instance after function declaration. Use like: " << std::endl
                             << usageHelper;
        return nullptr;
    }

    std::unique_ptr<FunctionDeclASTNode> func;

    // Add the function to the tree depending on its usage
    switch (mode) {
        case FunctionUsage::Prefix:
            // Prefix functions must have at least a function depth of 1
            if (fType->functionDepth() < 1) {
                appendError(token) << "Function must have function type (a -> b). Use like: " << std::endl
                                   << usageHelper;
                return nullptr;
            }
            func = std::make_unique<PrefixFunctionDeclASTNode>(
                    token.lineNumber, fileIndex, idToken.identifier, std::move(fType)
            );
            break;
        case FunctionUsage::Infix:
            // Infix functions must have a at least a function depth of 2
            if (fType->functionDepth() < 2) {
                appendError(token) << "Infix function must have function-to-function type (a -> b -> c). Use like: "
                                   << std::endl
                                   << usageHelper;
                return nullptr;
            }
            func = std::make_unique<InfixFunctionDeclASTNode>(
                    token.lineNumber, fileIndex, idToken.identifier, std::move(fType), assoc.value()
            );
            break;
        case FunctionUsage::Value:
            // There is no restriction on function depth for values.
            func = std::make_unique<ValueFunctionDeclASTNode>(
                    token.lineNumber, fileIndex, idToken.identifier, std::move(fType)
            );
            break;
    }

    // Update the token to the endpoint
    token.update(idToken);
    return func;
}

Parser::TokeniseFunc Parser::topLevelTypeDecl(ParseTree &tree, bool infix) {
    if (infix) {
        return
                [this, &tree](Token &token) {
                    return Tokeniser::compose(
                            token,
                            Tokeniser::tokenInfix,
                            Tokeniser::tokenType,
                            [this, &tree](Token &t2) {
                                parseTypeDecl(t2, tree, true);
                                return true;
                            });
                };
    } else {
        return
                [this, &tree](Token &token) {
                    if (!Tokeniser::tokenType(token)) {
                        return false;
                    }
                    parseTypeDecl(token, tree);
                    return true;
                };
    }
}

void Parser::parseTypeDecl(Token &token, ParseTree &tree, bool infix) {
    // We have already parsed the type keyword. We are expecting something like:
    // Prefix case:
    //  type Type [a [b ...]] [::= c1 [| c2 [| c3 ...]]];
    // Infix case:
    //  infix type a Type b [::= c1 [| c2 [| c3 ...]]];

    std::string usageHelper;
    if (infix) {
        usageHelper = "type Type [a [b ...]] [::= c1 [| c2 [| c3 ...]]];";
    } else {
        usageHelper = "infix type a Type b [::= c1 [| c2 [| c3 ...]]];";
    }

    IdentifierToken idToken = Tokeniser::identifierToken(token);
    std::unique_ptr<TypeDeclASTNode> decl;
    std::vector<std::string> typeVars;
    if (infix) {
        std::string leftVar, typeName, rightVar;
        if (!Tokeniser::tokenIdentifier(idToken)) {
            appendError(idToken) << "Expected type variable identifier after 'infix type'. Use like: " << std::endl
                                 << usageHelper;
            return;
        }
        leftVar = std::move(idToken.identifier);
        if (!Tokeniser::tokenIdentifier(idToken)) {
            appendError(idToken) << "Expected type name after 'infix type " << leftVar << "'. Use like: "
                                 << std::endl
                                 << usageHelper;
            return;
        }
        typeName = std::move(idToken.identifier);
        if (!Tokeniser::tokenIdentifier(idToken)) {
            appendError(idToken) << "Expected type variable identifier after 'infix type " << leftVar << " "
                                 << typeName << "'. Use like: " << std::endl
                                 << usageHelper;
            return;
        }
        rightVar = std::move(idToken.identifier);
        decl = std::make_unique<InfixTypeDeclASTNode>(
                idToken.lineNumber, fileIndex, typeName, leftVar, rightVar
        );

        // Move the type variable strings
        typeVars.push_back(std::move(leftVar));
        typeVars.push_back(std::move(rightVar));
    } else {
        std::string typeName;
        // Get the type name
        if (!Tokeniser::tokenIdentifier(idToken)) {
            appendError(idToken) << "Expected type name after 'type'. Use like: " << std::endl
                                 << usageHelper;
            return;
        }
        typeName = std::move(idToken.identifier);
        // Steal identifiers for as long as there are them
        while (Tokeniser::tokenIdentifier(idToken)) {
            typeVars.push_back(std::move(idToken.identifier));
        }
        decl = std::make_unique<PrefixTypeDeclASTNode>(
                idToken.lineNumber, fileIndex, typeName, std::move(typeVars)
        );
    }

    if (tree.typeExists(decl->typeName())) {
        appendError(idToken) << "Type '" << decl->typeName() << "' already exists.";
        return;
    }

    const TypeDeclASTNode &type = tree.addTypeDeclaration(std::move(decl));
    if (Tokeniser::tokenDataConstructorSpecifier(idToken)) {
        parseDataConstructors(idToken, type, tree, std::unordered_set<std::string>(typeVars.begin(), typeVars.end()));
    }
    token.update(idToken);
}

void Parser::parseTypeclass(Token &token, ParseTree &tree) {
    // We have already parsed the typeclass keyword. We expect something like:
    //  typeclass [PrerequisiteList =>] Class a {
    //      funcDecl,
    //      funcDecl,
    //      ...
    //  };

    // This vector will contian zero or more prerequesite typeclass implications. Note that in this case,
    // we have not yet added this typeclass to the set of typeclasses, so we cannot recursively define a typeclass
    std::vector<std::unique_ptr<TypeclassInstanceASTNode>> prerequisites;
    if (!parseImplicationList(token, tree, prerequisites)) {
        return;
    }

    const std::string usageHelper = "typeclass [Prerequisites =>] Class a { declaration0, declaration1, ... };";

    IdentifierToken idToken = Tokeniser::identifierToken(token);
    if (!Tokeniser::tokenIdentifier(idToken)) {
        if (prerequisites.empty()) {
            appendError(idToken) << "Expected typeclass name after 'typeclass' keyword. Use like: " << std::endl
                                 << usageHelper;
        } else {
            appendError(idToken) << "Expected typeclass name after prerequisite list. Use like: " << std::endl
                                 << usageHelper;
        }
        return;
    }
    std::string typeclassName = std::move(idToken.identifier);
    if (!Tokeniser::tokenIdentifier(idToken)) {
        appendError(idToken) << "Expected type variable after typeclass name. Use like: " << std::endl
                             << usageHelper;
        return;
    }
    std::string typeclassVariable = std::move(idToken.identifier);

    std::unique_ptr<TypeclassASTNode> typeclass = std::make_unique<TypeclassASTNode>(
            idToken.lineNumber, fileIndex, std::move(typeclassName),
            std::move(typeclassVariable), std::move(prerequisites)
    );

    // Check for an empty typeclass definition
    if (Tokeniser::compose(idToken, Tokeniser::tokenOpenBrace, Tokeniser::tokenCloseBrace)) {
        tree.addTypeclass(std::move(typeclass));
        token.update(idToken);
        return;
    }

    // Otherwise check that there is an open brace for the body
    if (!Tokeniser::tokenOpenBrace(idToken)) {
        appendError(idToken) << "Expected open brace after typeclass definition. Use like: " << std::endl
                             << usageHelper;
        return;
    }

    // Keep reading function declarations (of which we know there are at least one).
    // If there is then a comma, proceed to parse another declaration.
    // Stop when we have parsed a declaration, and then did not see a comma
    std::unique_ptr<FunctionDeclASTNode> decl;
    do {
        // In the case that we don't find any function declaration, throw an error
        if (!parseAnyFunctionDecl(idToken, tree, decl)) {
            appendError(idToken)
                    << "Expected function or value declaration list inside typeclass definition. Use like: "
                    << std::endl
                    << usageHelper;
            return;
        }
        // Otherwise, if the function failed to parse, return (we have already added the error)
        if (!decl) {
            return;
        }
        // If these both succeed, add the function to the typeclass
        typeclass->addMethod(std::move(decl));
    } while (Tokeniser::tokenComma(idToken));

    if (!Tokeniser::tokenCloseBrace(idToken)) {
        appendError(idToken) << "Expected closing brace after typeclass method declarations. Use like: " << std::endl
                             << usageHelper;
    }

    tree.addTypeclass(std::move(typeclass));
    token.update(idToken);
}

void Parser::parseTypeclassInstanceDecl(Token &token, ParseTree &tree) {
    // Here, we are parsing an instance of a typeclass. We expect something like:
    //  instance [PrerequisiteList =>] Class Type {
    //      funcImpl,
    //      funcImpl,
    //      ...
    //  };

    // This vector will contian zero or more prerequesite typeclass implications. Note that in this case,
    // we have added this typeclass to the set of typeclasses, so can recursively define a typeclass instance
    // (e.g. 'instance Class T1 => Class [T1]')
    std::vector<std::unique_ptr<TypeclassInstanceASTNode>> prerequisites;
    if (!parseImplicationList(token, tree, prerequisites)) {
        return;
    }

    const std::string usageHelper = "instance [Prerequisites =>] Class Type { implementation0, implementation1, ... };";

    Token typeclassToken = Tokeniser::token(token);

    // Next we expect the typeclass instance
    std::unique_ptr<TypeclassInstanceASTNode> instance = parseTypeclassInstance(typeclassToken, tree);
    if (!instance) {
        return;
    }

    // Get a reference to the instance and add it to the tree
    TypeclassInstanceImplASTNode &instanceRef = tree.addTypeclassInstance(
            instance->typeclass().name(), std::make_unique<TypeclassInstanceImplASTNode>(
                    typeclassToken.lineNumber, fileIndex, std::move(instance), std::move(prerequisites)
            )
    );


    // Next we check for either an end of declaration (meaning this was just a type instance declaration with no
    // implementations)
    if (Tokeniser::peek(typeclassToken, Tokeniser::tokenEndDecl)) {
        // Here, we just update and return
        token.update(typeclassToken);
        return;
    }

    // Here, we know that there was not an empty instance so we have at least one implementation to parse
    if (!Tokeniser::tokenOpenBrace(typeclassToken)) {
        appendError(typeclassToken) << "Expected open brace after typeclass instance. Use like: " << std::endl
                                    << usageHelper;
        return;
    }
    // If we don't see a close brace, then we want to parse implementations
    if (!Tokeniser::tokenCloseBrace(typeclassToken)) {
        do {
            std::string name;
            std::unique_ptr<FunctionImplASTNode> implementation = parseImplementation(typeclassToken, tree, name);
            if (!implementation) {
                return;
            }
            if (instanceRef.typeclass().hasMethod(name)) {
                appendError(typeclassToken) << "Unexpected method '" << name << "' in typeclass '"
                                            << instanceRef.typeclass().name() << "'.";
                return;
            }
            instanceRef.addImplementation(name, std::move(implementation));
        } while (Tokeniser::tokenComma(typeclassToken));
        // Check that we now see a close brace
        if (!Tokeniser::tokenCloseBrace(typeclassToken)) {
            appendError(typeclassToken) << "Expected close brace after typeclass instance. Use like: " << std::endl
                                        << usageHelper;
            return;
        }
    }

    // Finally, check that the instance was fully implemented
    if (!instanceRef.fullyImplemented()) {
        appendError(typeclassToken) << "Incomplete instance implementation of typeclass '"
                                    << instanceRef.typeclass().name() << "'." << std::endl
                                    << "Typeclass instances must implement every method declared by the class. In this case that is: "
                                    << std::endl
                                    << std::accumulate(instanceRef.typeclass().methodNames().begin(),
                                                       instanceRef.typeclass().methodNames().end(),
                                                       std::string(),
                                                       [](const std::string &acc, const std::string &name) {
                                                           return acc + " " + name;
                                                       });
    }
    token.update(typeclassToken);
}

std::unique_ptr<FunctionImplASTNode> Parser::parseImplementation(Token &token, const ParseTree &tree) {
    std::string name;
    return parseImplementation(token, tree, name);
}

std::unique_ptr<FunctionImplASTNode>
Parser::parseImplementation(Token &token, const ParseTree &tree, std::string &name) {
    return std::unique_ptr<FunctionImplASTNode>();
}

bool Parser::parseTypeInstance(
        Token &token, const ParseTree &tree, std::unique_ptr<TypeInstanceASTNode> &inst,
        std::optional<std::reference_wrapper<const std::unordered_set<std::string>>> typeVars) {
    // A type instance can be any of:
    // 1. Nested instance expression: (expr)
    // 2. Prefix instance application: Type [a [b [c ...]]]
    // 3. Infix instance application: a Type b
    // 4. A polymorphic type variable: a
    // It may also be prepended with a typeclass precondition.

    // Base case: seeing a closing bracket, a comma or a semicolon
    if (Tokeniser::peekAny(token, Tokeniser::tokenCloseParenthesis, Tokeniser::tokenComma, Tokeniser::tokenEndDecl)) {
        return true;
    }

    IdentifierToken idToken = Tokeniser::identifierToken(token);

    std::unique_ptr<TypeInstanceASTNode> atom;
    if (Tokeniser::tokenOpenParenthesis(idToken)) {
        // Propagate error
        if (!parseTypeInstance(idToken, tree, atom, typeVars)) {
            return false;
        }
        // After parsing a nested type instance, we should now see a closing parenthesis
        if (!Tokeniser::tokenCloseParenthesis(idToken)) {
            appendError(idToken) << "Expected closing parenthesis after nested type instance.";
            return false;
        }
        // atom is null, but we got this far, then we actually parsed a unit type
        if (!atom) {
            atom = std::make_unique<PrimitiveTypeInstanceASTNode>(tree.core().unitType());
        }
    } else if (Tokeniser::tokenIdentifier(idToken)) {
        if (tree.typeExists(idToken.identifier)) {
            const TypeDeclASTNode &type = tree.getTypeByName(idToken.identifier);
            switch (type.typeUsage()) {
                case TypeUsage::Infix: {
                    if (!inst) {
                        appendError(idToken) << "Expected left hand type instance for infix type constructor '"
                                             << type.typeName() << "'.";
                        return false;
                    }
                    // Parse right subtree into rhs
                    std::unique_ptr<TypeInstanceASTNode> rhs;
                    // Propagate error from parsing rhs
                    if (!parseTypeInstance(idToken, tree, rhs, typeVars)) {
                        return false;
                    }
                    if (!rhs) {
                        appendError(idToken) << "Expected right hand type instance for infix type constructor '"
                                             << type.typeName() << "'.";
                        return false;
                    }
                    std::unique_ptr<InfixTypeInstanceASTNode> infix = std::make_unique<InfixTypeInstanceASTNode>(
                            idToken.lineNumber, fileIndex, idToken.identifier
                    );
                    infix->bindLeft(std::move(inst));
                    infix->bindRight(std::move(rhs));
                    atom = std::move(infix);
                    break;
                }
                case TypeUsage::Prefix:
                    atom = std::make_unique<PrefixTypeInstanceASTNode>(
                            idToken.lineNumber, fileIndex, idToken.identifier
                    );
                    break;
                default:
                    appendError(idToken) << "DEVELOPER: Unexepected type usage in type instance!";
                    return false;
            }
        } else if (tree.core().primitiveExists(idToken.identifier)) {
            atom = std::make_unique<PrimitiveTypeInstanceASTNode>(tree.core().getPrimitiveType(idToken.identifier));
        } else {
            // Only exclude type variables if there is a type variable set and this is not a member of it
            if (typeVars.has_value() && !typeVars.value().get().contains(idToken.identifier)) {
                appendError(idToken) << "Unrecognised type name or type variable '" << idToken.identifier << "'.";
                return false;
            }
            atom = std::make_unique<PolymorphicTypeInstanceASTNode>(
                    idToken.lineNumber, fileIndex, idToken.identifier
            );
        }
    } else if (Tokeniser::tokenFuncType(idToken)) {
        if (!inst) {
            appendError(idToken) << "Expected left hand type instance before function type (a -> b).";
            return false;
        }
        // Parse the rhs
        std::unique_ptr<TypeInstanceASTNode> rhs;
        if (!parseTypeInstance(idToken, tree, rhs, typeVars)) {
            return false;
        }
        if (!rhs) {
            appendError(idToken) << "Expected right hand type instance after function type (a -> b).";
            return false;
        }
        std::unique_ptr<FunctionTypeInstanceASTNode> func = std::make_unique<FunctionTypeInstanceASTNode>(
                idToken.lineNumber, fileIndex, "->"
        );
        func->bindLeft(std::move(inst));
        func->bindRight(std::move(rhs));
    } else {
        appendError(idToken) << "Unexpected token '" << Tokeniser::scanToken(idToken).token << "' in type instance.";
        return false;
    }

    if (inst) {
        if (inst->typeUsage() != TypeUsage::Prefix && inst->typeUsage() != TypeUsage::Polymorphic) {
            appendError(idToken) << "Cannot apply type to non prefix type construction.";
            return false;
        }
        PrefixTypeInstanceInterface *pInst = dynamic_cast<PrefixTypeInstanceInterface *>(inst.get());
        pInst->bindParameter(std::move(atom));
    } else {
        inst = std::move(atom);
    }

    // Recurse to parse the rest of the type instance
    bool success = parseTypeInstance(idToken, tree, inst, typeVars);
    if (!success) {
        return false;
    }
    token.update(idToken);
    return true;
}

std::unique_ptr<TypeclassInstanceASTNode> Parser::parseTypeclassInstance(Token &token, const ParseTree &tree) {
    // Typeclass instances look like:
    //  Class Type
    // where Type is a type instance, so the parser for these is relatively simple

    IdentifierToken idToken = Tokeniser::identifierToken(token);
    if (!Tokeniser::tokenIdentifier(idToken)) {
        appendError(idToken) << "Expected a typeclass name identifier.";
        return nullptr;
    }
    std::string typeclassName = std::move(idToken.identifier);

    // Check the typeclass exists
    if (!tree.typeclassExists(typeclassName)) {
        appendError(idToken) << "Unrecongised typeclass '" << typeclassName << "'.";
        return nullptr;
    }

    // Then we parse the type instance with no fixed binder set
    std::unique_ptr<TypeInstanceASTNode> type = parseTypeInstance(idToken, tree);
    if (!type) {
        return nullptr;
    }

    // Update the token
    token.update(idToken);

    // Finally, we make and return the typeclass instance
    return std::make_unique<TypeclassInstanceASTNode>(
            idToken.lineNumber, fileIndex, tree.getTypeclassByName(typeclassName), std::move(type)
    );
}

void Parser::parseDataConstructors(Token &token, const TypeDeclASTNode &type, ParseTree &tree,
                                   const std::unordered_set<std::string> &typeVars) {
    std::unique_ptr<DataConstructorASTNode> cons;
    IdentifierToken consToken = Tokeniser::identifierToken(token);
    if (Tokeniser::tokenInfix(consToken)) {
        std::unique_ptr<TypeInstanceASTNode> lhs;
        if (!parseTypeInstance(consToken, tree, lhs, typeVars)) {
            return;
        }
        if (!lhs) {
            appendError("Expected left hand type instance for infix constructor.");
            return;
        }
        if (!Tokeniser::tokenIdentifier(consToken)) {
            appendError(consToken) << "Expected constructor name identifier after left parameter in infix constructor.";
            return;
        }
        std::unique_ptr<TypeInstanceASTNode> rhs;
        if (!parseTypeInstance(consToken, tree, rhs, typeVars)) {
            return;
        }
        if (!rhs) {
            appendError(consToken) << "Expected right hand type instance for infix constructor.";
            return;
        }

        cons = std::make_unique<InfixDataConstructorASTNode>(
                consToken.lineNumber, fileIndex, type, consToken.identifier, std::move(lhs), std::move(rhs)
        );
    } else {
        if (!Tokeniser::tokenIdentifier(consToken)) {
            appendError(consToken) << "Expected constructor name in constructor list.";
            return;
        }
        std::vector<std::unique_ptr<TypeInstanceASTNode>> consParams;
        while (!Tokeniser::peekAny(token, Tokeniser::tokenTypeUnion, Tokeniser::tokenEndDecl)) {
            // Add the type instance to the vector
            consParams.push_back(parseTypeInstance(token, tree, typeVars));
            // If what we just added was null, return
            if (!consParams.back()) {
                return;
            }
        }

        cons = std::make_unique<PrefixDataConstructorASTNode>(
                consToken.lineNumber, fileIndex, type, consToken.identifier, std::move(consParams)
        );
    }

    // Check if the constructor already exists
    if (tree.constructorExists(consToken.identifier)) {
        appendError(consToken) << "Constructor with name '" << consToken.identifier << "' already exists.";
        return;
    }
    if (tree.functionExists(consToken.identifier)) {
        appendError(consToken) << "Function with name '" << consToken.identifier << "' already exists." << std::endl
                               << "Consider using Upper camel casing for constructor names, and lower camel casing for function names.";
        return;
    }

    tree.addDataConstructor(type, std::move(cons));

    token.update(consToken);
    // If the next token is a type union, consume and recurse
    if (Tokeniser::tokenTypeUnion(token)) {
        parseDataConstructors(token, type, tree, typeVars);
    }
}

std::vector<Parser::Error> Parser::errors() const {
    std::vector<Error> errorVector(errorList.size());
    std::transform(errorList.begin(), errorList.end(), errorVector.begin(),
                   [](const std::stringstream &e) { return e.str(); });
    return std::move(errorVector);
}

std::stringstream &Parser::appendError(const Token &errorPoint) {
    errorList.emplace_back();
    errorList.back() << "[Parse Error] " << fileStructure.getFileName(fileIndex) << " (line " << errorPoint.lineNumber
                     << "): ";
    return errorList.back();
}
