//
// Created by matthew on 06/12/2020.
//

#include <iostream>

#include "../../include/compiler/TypeChecker.h"

TypeChecker::TypeChecker(ParseTree &tree)
        : tree(tree) {

}

bool TypeChecker::typeCheck() {
    // First, we can check that every declared type in the tree has validly typed constructors.
    // This essentially means that each constructor which references another type should have a well typed
    // reference to the other type.

    // We need to check that each constructor and function is validly typed, i.e. any patterns on the arguments
    // are appropriate and the body is of the correct type

    // Return true if and only if every constructor and function type checks

    bool checked =
            std::all_of(tree.functions().begin(), tree.functions().end(),
                        [this](const FunctionDefinitionASTNode &func) {
                            return typeCheckFunction(func);
                        });

    if (checked) {
        tree.markAsTypeChecked();
    }
    return checked;
}

bool TypeChecker::typeCheckFunction(const FunctionDefinitionASTNode &func) {
    return std::all_of(func.implementationVariants().begin(), func.implementationVariants().end(),
                       [this, &func](const FunctionImplASTNode &impl) {
                           return typeCheckFunctionImplementation(impl, func.decl().functionType());
                       });
}

bool TypeChecker::typeCheckTypeInstance(const TypeInstanceASTNode &instance) {
    switch (instance.typeUsage()) {
        case TypeUsage::Primitive:
            // We can assume a primitive is always correctly typed
            return true;
        case TypeUsage::Prefix: {
            const PrefixTypeInstanceASTNode &prefixInstance = dynamic_cast<const PrefixTypeInstanceASTNode &>(instance);

            // Fail if applied to an invalid number of arguments
            if (tree.getTypeByName(prefixInstance.typeName()).args() != prefixInstance.parameters().size()) {
                appendError(prefixInstance) << "Invalid number of arguments for type '" << prefixInstance.typeName()
                                            << "'.";
                return false;
            }

            // Return true if and only if every parameter type checks
            return std::all_of(prefixInstance.parameters().begin(), prefixInstance.parameters().end(),
                               [this](const TypeInstanceASTNode &inst) {
                                   return typeCheckTypeInstance(inst);
                               });
        }
        case TypeUsage::Infix:
        case TypeUsage::Function: {
            const InfixTypeInstanceASTNode &infixInstance = dynamic_cast<const InfixTypeInstanceASTNode &>(instance);

            // Return true if and only if both parameters type check
            return typeCheckTypeInstance(infixInstance.left()) && typeCheckTypeInstance(infixInstance.right());
        }
        case TypeUsage::Polymorphic:
            // This is always correct - we know that using an invalid type variable
            // is already caught in the parser
            return true;
    }
    return false;
}

bool TypeChecker::typeCheckFunctionImplementation(const FunctionImplASTNode &impl,
                                                  const TypeInstanceASTNode &type) {
    const TypeInstanceASTNode *typeNode = &type;

    // The patterns should appear in order, and so each one should be of the type specified
    // by the left branch of the type node, and at each stage, we recurse down the right branch.
    // We stop after parsing the last pattern, not after the last function type, so the left over type
    // may be a function type. This means the expression should be of a function type, and so everything
    // still works out
    for (const PatternASTNode &pattern : impl.parameterPatterns()) {
        const FunctionTypeInstanceASTNode &fTypeNode = dynamic_cast<const FunctionTypeInstanceASTNode &>(*typeNode);
        if (!typeCheckPattern(pattern, fTypeNode.left())) {
            return false;
        }
        typeNode = &fTypeNode.right();
    }

    return typeCheckExpression(impl.functionBody(), *typeNode);
}

bool TypeChecker::typeCheckPattern(const PatternASTNode &pattern, const TypeInstanceASTNode &type) {
    switch (pattern.patternUsage()) {
        case PatternUsage::Variable:
            // A variable matches any type, so return true
            return true;
        case PatternUsage::Constructor: {
            // A constructor must be of the right type
            const ConstructorPatternASTNode &consPattern = dynamic_cast<const ConstructorPatternASTNode &>(pattern);
            // This constructor should always exist if we have made it to this point
            const DataConstructorASTNode &cons = tree.getConstructorByName(consPattern.constructorName());
            if (!typeMatch(type, cons.constructorType())) {
                appendError(consPattern) << "Expected pattern of type '" << type.typeString()
                                         << "'. Found pattern of type '" << cons.constructorType().typeName() << "'.";
                return false;
            }
            // Type check each nested sub pattern
            const TypeInstanceASTNode *consType = &cons.constructorFunction().decl().functionType();
            for (const PatternASTNode &subPattern : consPattern.nestedPatterns()) {
                const FunctionTypeInstanceASTNode *fType = dynamic_cast<const FunctionTypeInstanceASTNode *>(consType);
                typeCheckPattern(subPattern, fType->left());
                consType = &fType->right();
            }
            return true;
        }
        case PatternUsage::Integral: {
            if (type.typeUsage() != TypeUsage::Primitive) {
                return false;
            }
            const PrimitiveTypeInstanceASTNode &primType = dynamic_cast<const PrimitiveTypeInstanceASTNode &>(type);
            // TODO: Formalise this rather than using string
            return primType.primitiveType().name() == "Int";
        }
        case PatternUsage::Decimal: {
            if (type.typeUsage() != TypeUsage::Primitive) {
                return false;
            }
            const PrimitiveTypeInstanceASTNode &primType = dynamic_cast<const PrimitiveTypeInstanceASTNode &>(type);
            // TODO: Formalise this rather than using string
            return primType.primitiveType().name() == "Float";
        }
        case PatternUsage::Char: {
            if (type.typeUsage() != TypeUsage::Primitive) {
                return false;
            }
            const PrimitiveTypeInstanceASTNode &primType = dynamic_cast<const PrimitiveTypeInstanceASTNode &>(type);
            // TODO: Formalise this rather than using string
            return primType.primitiveType().name() == "Char";
        }
    }
    return false;
}

bool TypeChecker::typeCheckExpression(const ExpressionASTNode &expr,
                                      const TypeInstanceASTNode &type) {
    // TODO: Implement this properly!
    return true;
    switch (expr.type()) {
        case ExpressionType::Lambda: {
            // A lambda should have a function type
            if (type.typeUsage() != TypeUsage::Function) {
                appendError(expr) << "Found lambda expression when expecting type '" << type.typeString() << "'.";
                return false;
            }
            const FunctionTypeInstanceASTNode &fType = dynamic_cast<const FunctionTypeInstanceASTNode &>(type);
            const LambdaExpressionASTNode &lambda = dynamic_cast<const LambdaExpressionASTNode &>(expr);
            // The pattern should be of the "from" type
            if (!typeCheckPattern(lambda.binderPattern(), fType.left())) {
                return false;
            }
            // The expression body should be of the "to" type
            if (!typeCheckExpression(lambda.bodyExpression(), fType.right())) {
                return false;
            }
            return true;
        }
        case ExpressionType::Application: {
            // An application should have a function parameter of type X -> type,
            // and then argument should have type X
            return true;
        }
        case ExpressionType::LetBinding: {
            const LetBindingASTNode &let = dynamic_cast<const LetBindingASTNode &>(expr);
            return typeCheckExpression(let.usageExpression(), type);
        }
        case ExpressionType::Function: {
            return tree.getFuncByName(
                    dynamic_cast<const FunctionASTNode &>(expr).functionName()
            ).decl().functionType() == type;
        }
        case ExpressionType::Variable:
            return true;
        case ExpressionType::PrimitiveConstructor: {
            if (type.typeUsage() != TypeUsage::Primitive) {
                appendError(expr) << "Found primitive constructor when expecting type '" << type.typeString() << "'.";
                return false;
            }
            return true;
        }
    }
}

bool TypeChecker::typeMatch(const TypeInstanceASTNode &inst, const TypeDeclASTNode &type) {
    switch (inst.typeUsage()) {
        case TypeUsage::Infix: {
            if (type.typeUsage() != TypeUsage::Infix) {
                return false;
            }
            const InfixTypeInstanceASTNode &infixInst = dynamic_cast<const InfixTypeInstanceASTNode &>(inst);
            const InfixTypeDeclASTNode &infixType = dynamic_cast<const InfixTypeDeclASTNode &>(type);
            return infixInst.typeName() == infixType.typeName();
        }
        case TypeUsage::Prefix: {
            if (type.typeUsage() != TypeUsage::Prefix) {
                return false;
            }
            const PrefixTypeInstanceASTNode &prefixInst = dynamic_cast<const PrefixTypeInstanceASTNode &>(inst);
            const PrefixTypeDeclASTNode &prefixType = dynamic_cast<const PrefixTypeDeclASTNode &>(type);
            return prefixInst.typeName() == prefixType.typeName();
        }
        case TypeUsage::Function:
            // Never a match
            return false;
        case TypeUsage::Polymorphic:
            // Always a match
            return true;
        case TypeUsage::Primitive: {
            // Never a match
            return false;
        }
    }
    return false;
}

std::ostream &TypeChecker::appendError() {
    errorList->emplace_back();
    errorList->back() << "[Type Error] ";
    return errorList->back();
}

std::ostream &TypeChecker::appendError(const ASTNode &node) {
    return appendError() << tree.options.fileStructure().getFileName(node.fileIndex()).generic_string()
                         << " (line " << node.lineNumber() << "): ";
}
