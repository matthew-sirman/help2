//
// Created by matthew on 06/12/2020.
//

#ifndef HELP2_TYPECHECKER_H
#define HELP2_TYPECHECKER_H

#include <memory>

#include "../parser/ParseTree.h"

class TypeChecker {
public:
    TypeChecker(ParseTree &tree);

    bool typeCheck() const;

private:
    ParseTree &tree;

    bool typeCheckType(const std::unique_ptr<TypeDeclASTNode> &type) const;

    bool typeCheckFunction(const std::unique_ptr<FunctionDefinitionASTNode> &func) const;

    bool typeCheckConstructor(const std::unique_ptr<DataConstructorASTNode> &cons) const;

    bool typeCheckTypeInstance(const std::unique_ptr<TypeInstanceASTNode> &instance) const;

    bool typeCheckFunctionImplementation(const std::unique_ptr<FunctionImplASTNode> &impl,
                                         const std::unique_ptr<TypeInstanceASTNode> &type) const;

    bool typeCheckPattern(const std::unique_ptr<PatternASTNode> &pattern,
                          TypeInstanceASTNode *type) const;

    bool typeCheckExpression(const std::unique_ptr<ExpressionASTNode> &expr,
                             TypeInstanceASTNode *type) const;

    void logError(const std::string &message) const;

    void logError(const std::string &message, const ASTNode *node) const;
};


#endif //HELP2_TYPECHECKER_H
