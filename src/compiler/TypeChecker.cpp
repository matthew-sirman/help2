//
// Created by matthew on 06/12/2020.
//

#include <iostream>

#include "../../include/compiler/TypeChecker.h"

TypeChecker::TypeChecker(const std::unique_ptr<ParseTree> &tree)
        : tree(tree) {

}

bool TypeChecker::typeCheck() const {
    // First, we can check that every declared type in the tree has validly typed constructors.
    // This essentially means that each constructor which references another type should have a well typed
    // reference to the other type.

    // Then, we need to check that each function is validly typed, i.e. any patterns on the arguments
    // are appropriate and the body is of the correct type

    // Return true if and only if every type and function type checks

    bool checked =
            std::all_of(tree->types().begin(), tree->types().end(),
                        [this](const std::pair<const std::string, std::unique_ptr<TypeDeclASTNode>> &type) {
                            return typeCheckType(type.second);
                        }) &&
            std::all_of(tree->functions().begin(), tree->functions().end(),
                        [this](const std::pair<const std::string, std::unique_ptr<FunctionDefinitionASTNode>> &func) {
                            return typeCheckFunction(func.second);
                        });

    if (checked) {
        tree->markAsTypeChecked();
    }
    return checked;
}

bool TypeChecker::typeCheckType(const std::unique_ptr<TypeDeclASTNode> &type) const {
    // Type check each data constructor.
    // Return true if and only if all data constructors type check
    return std::all_of(type->dataConstructors().begin(), type->dataConstructors().end(),
                       [this](const std::unique_ptr<DataConstructorASTNode> &cons) {
                           return typeCheckConstructor(cons);
                       });
}

bool TypeChecker::typeCheckFunction(const std::unique_ptr<FunctionDefinitionASTNode> &func) const {
    return std::all_of(func->implementationVariants().begin(), func->implementationVariants().end(),
                       [this, &func](const std::unique_ptr<FunctionImplASTNode> &impl) {
                           return typeCheckFunctionImplementation(impl, func->decl()->functionType());
                       });
}

bool TypeChecker::typeCheckConstructor(const std::unique_ptr<DataConstructorASTNode> &cons) const {
    switch (cons->usage()) {
        case TypeUsage::Infix: {
            // Get a reference to the infix constructor
            const InfixDataConstructorASTNode *infixCons = dynamic_cast<InfixDataConstructorASTNode *>(cons.get());

            // Type check each parameter and return true if and only if both type check
            return typeCheckTypeInstance(infixCons->leftParameter()) &&
                   typeCheckTypeInstance(infixCons->rightParameter());
        }
        case TypeUsage::Prefix: {
            // Get a reference to the prefix constructor
            const PrefixDataConstructorASTNode *prefixCons = dynamic_cast<PrefixDataConstructorASTNode *>(cons.get());

            // Type check each parameter and return true if and only if all type check
            return std::all_of(prefixCons->parameters().begin(), prefixCons->parameters().end(),
                               [this](const std::unique_ptr<TypeInstanceASTNode> &instance) {
                                   return typeCheckTypeInstance(instance);
                               });
        }
        case TypeUsage::Function:
        case TypeUsage::Polymorphic:
            // These cases should never occur
            logError("DEVELOPER: Invalid constructor usage!");
            return false;
    }
    return false;
}

bool TypeChecker::typeCheckTypeInstance(const std::unique_ptr<TypeInstanceASTNode> &instance) const {
    switch (instance->typeUsage()) {
        case TypeUsage::Prefix: {
            const PrefixTypeInstanceASTNode *prefixInstance = dynamic_cast<PrefixTypeInstanceASTNode *>(instance.get());

            // Fail if applied to an invalid number of arguments
            if (tree->getTypeByName(prefixInstance->typeName())->args() != prefixInstance->parameters().size()) {
                logError("Invalid number of arguments for type '" + prefixInstance->typeName() + "'.", instance.get());
                return false;
            }

            // Return true if and only if every parameter type checks
            return std::all_of(prefixInstance->parameters().begin(), prefixInstance->parameters().end(),
                               [this](const std::unique_ptr<TypeInstanceASTNode> &inst) {
                                   return typeCheckTypeInstance(inst);
                               });
        }
        case TypeUsage::Infix:
        case TypeUsage::Function: {
            const InfixTypeInstanceASTNode *infixInstance = dynamic_cast<InfixTypeInstanceASTNode *>(instance.get());

            // Return true if and only if both parameters type check
            return typeCheckTypeInstance(infixInstance->left()) && typeCheckTypeInstance(infixInstance->right());
        }
        case TypeUsage::Polymorphic:
            // This is always correct - we know that using an invalid type variable
            // is already caught in the parser
            return true;
    }
    return false;
}

bool TypeChecker::typeCheckFunctionImplementation(const std::unique_ptr<FunctionImplASTNode> &impl,
                                                  const std::unique_ptr<TypeInstanceASTNode> &type) const {
    switch (impl->functionUsage()) {
        case FunctionUsage::Prefix: {
            PrefixFunctionImplASTNode *prefixImpl = dynamic_cast<PrefixFunctionImplASTNode *>(impl.get());
            FunctionTypeInstanceASTNode *typeNode = dynamic_cast<FunctionTypeInstanceASTNode *>(type.get());

            const std::vector<std::unique_ptr<PatternASTNode>> &patterns = prefixImpl->parameterPatterns();

            // The patterns should appear in order, and so each one should be of the type specified
            // by the left branch of the type node, and at each stage, we recurse down the right branch.
            for (std::vector<std::unique_ptr<PatternASTNode>>::const_iterator it = patterns.begin();
                 it != patterns.end(); ++it) {
                if (!typeCheckPattern(*it, typeNode->left())) {
                    return false;
                }
                // Don't recurse down the final right subtree
                if (it != patterns.end() - 1) {
                    typeNode = dynamic_cast<FunctionTypeInstanceASTNode *>(typeNode->right().get());
                } else {
                    return typeCheckExpression(impl->functionBody(), typeNode->right());
                }
            }
        }
        case FunctionUsage::Infix: {
            InfixFunctionImplASTNode *infixImpl = dynamic_cast<InfixFunctionImplASTNode *>(impl.get());
            FunctionTypeInstanceASTNode *typeNode = dynamic_cast<FunctionTypeInstanceASTNode *>(type.get());

            if (!typeCheckPattern(infixImpl->leftPattern(), typeNode->left())) {
                return false;
            }
            typeNode = dynamic_cast<FunctionTypeInstanceASTNode *>(typeNode->right().get());
            if (!typeCheckPattern(infixImpl->rightPattern(), typeNode->left())) {
                return false;
            }

            return typeCheckExpression(impl->functionBody(), typeNode->right());
        }
        case FunctionUsage::Value:
            return typeCheckExpression(impl->functionBody(), type);
    }
    return false;
}

bool TypeChecker::typeCheckPattern(const std::unique_ptr<PatternASTNode> &pattern,
                                   const std::unique_ptr<TypeInstanceASTNode> &type) const {
    switch (pattern->patternUsage()) {
        case PatternUsage::Variable:
            // A variable matches any type, so return true
            return true;
        case PatternUsage::Constructor: {
            // A constructor must be of the right type
            ConstructorPatternASTNode *consPattern = dynamic_cast<ConstructorPatternASTNode *>(pattern.get());

            break;
        }
    }
    return false;
}

bool TypeChecker::typeCheckExpression(const std::unique_ptr<ExpressionASTNode> &expr,
                                      const std::unique_ptr<TypeInstanceASTNode> &type) const {
    return false;
}

void TypeChecker::logError(const std::string &message) const {
    std::cerr << "[Type Error] " << message << std::endl;
}

void TypeChecker::logError(const std::string &message, const ASTNode *node) const {
    std::cerr << "[Type Error] " << tree->getFileName(node->fileIndex()) << " (line " << node->lineNumber()
              << "): " << message << std::endl;

}
