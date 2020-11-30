//
// Created by matthew on 28/11/2020.
//

#ifndef HELP2_FUNCTIONASTNODES_H
#define HELP2_FUNCTIONASTNODES_H

#include "TypeASTNodes.h"

class FunctionDeclASTNode {
public:
    FunctionDeclASTNode(std::string name, std::unique_ptr<TypeInstanceASTNode> &&funcType);

    const std::string &name() const { return funcName; }

private:
    std::string funcName;
    std::unique_ptr<TypeInstanceASTNode> funcType;
};

/*
 * Prefix Function declaration syntax:
 * func name: a -> b;
 */
class PrefixFunctionDeclASTNode : public FunctionDeclASTNode {

};

/*
 * Infix Function declaration syntax:
 * infix func name: a -> b -> c;
 */
class InfixFunctionDeclASTNode : public FunctionDeclASTNode {

};

class FunctionImplASTNode {

};

/*
 * Prefix Function implementation syntax:
 * name [pattern0 [pattern1 ...]] => expression;
 */
class PrefixFunctionImplASTNode : public FunctionImplASTNode {

};

/*
 * Infix Function implementation syntax:
 * lPattern name rPattern => expression;
 */
class InfixFunctionImplASTNode : public FunctionImplASTNode {

};

class FunctionDefinitionASTNode {
public:
    FunctionDefinitionASTNode(std::unique_ptr<FunctionDeclASTNode> &&declaration);

    const std::string &name() const { return declaration->name(); }

private:
    std::unique_ptr<FunctionDeclASTNode> declaration;
    std::vector<std::unique_ptr<FunctionImplASTNode>> implementations;
};

#endif //HELP2_FUNCTIONASTNODES_H
