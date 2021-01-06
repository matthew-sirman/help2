//
// Created by matthew on 06/12/2020.
//

#ifndef HELP2_TYPECHECKER_H
#define HELP2_TYPECHECKER_H

#include <memory>

#include "../parser/ParseTree.h"

class TypeChecker {
public:
    explicit TypeChecker(ParseTree &tree);

    bool typeCheck();

    constexpr const ErrorList &errors() const { return errorList; };

private:
    ParseTree &tree;

    bool typeCheckFunction(const FunctionDefinitionASTNode &func);

    bool typeCheckTypeInstance(const TypeInstanceASTNode &instance);

    bool typeCheckFunctionImplementation(const FunctionImplASTNode &impl,
                                         const TypeInstanceASTNode &type);

    bool typeCheckPattern(const PatternASTNode &pattern,
                          const TypeInstanceASTNode &type);

    bool typeCheckExpression(const ExpressionASTNode &expr,
                             const TypeInstanceASTNode &type);

    bool typeMatch(const TypeInstanceASTNode &inst, const TypeDeclASTNode &type);

    std::ostream &appendError();

    std::ostream &appendError(const ASTNode &node);

    ErrorList errorList;
};


#endif //HELP2_TYPECHECKER_H
