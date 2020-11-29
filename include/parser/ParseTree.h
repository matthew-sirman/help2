//
// Created by matthew on 28/11/2020.
//

#ifndef HELP2_PARSETREE_H
#define HELP2_PARSETREE_H

#include <unordered_map>

#include "ast/ExpressionASTNodes.h"
#include "ast/TypeASTNodes.h"
#include "ast/FunctionASTNodes.h"

class ParseTree {
public:
    ParseTree() = default;

    bool functionExists(const std::string &name) const;

    bool typeExists(const std::string &name) const;

private:
    std::unordered_map<std::string, std::unique_ptr<TypeDeclASTNode>> declaredTypeNodes;
    std::unordered_map<std::string, std::unique_ptr<FunctionDefinitionASTNode>> definedFunctionNodes;
};

#endif //HELP2_PARSETREE_H
