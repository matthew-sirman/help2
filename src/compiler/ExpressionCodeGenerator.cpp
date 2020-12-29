//
// Created by matthew on 24/12/2020.
//

#include "../../include/compiler/CodeGenerator.h"
#include "../../include/compiler/Compiler.h"

#include <list>

template<>
llvm::Value *ExpressionCodeGenerator::generate<LambdaExpressionASTNode>(const LambdaExpressionASTNode::View &nodeView) {

}

template<>
llvm::Value *ExpressionCodeGenerator::generate<ApplicationASTNode>(const ApplicationASTNode::View &nodeView) {
    ExpressionASTNode *arg = nodeView.argument.get();
    ExpressionASTNode *func = nodeView.function.get();

    std::list<llvm::Value *> args;

    // Get the list of arguments and descend to the bottom of the function
    while (true) {
        args.push_front(arg->generate(*this));
        if (func->type() == ExpressionType::Application) {
            ApplicationASTNode *app = dynamic_cast<ApplicationASTNode *>(func);
            arg = app->appArgument().get();
            func = app->appFunction().get();
        } else {
            break;
        }
    }

    return context.builder()->CreateCall(
            func->generate(*this),
            std::vector<llvm::Value *>(args.begin(), args.end()),
            "calltmp"
    );
}

template<>
llvm::Value *ExpressionCodeGenerator::generate<LetBindingASTNode>(const LetBindingASTNode::View &nodeView) {

}

template<>
llvm::Value *ExpressionCodeGenerator::generate<FunctionASTNode>(const FunctionASTNode::View &nodeView) {
    // TODO: Provide appropriate bindings
    llvm::Function *func = context.parseTree()->getFuncByName(nodeView.name)->generate(funcCodeGenerator, {});
    // If the function is nullary, we call it and return the value (like a nullary application)
    if (nodeView.nullary) {
        return context.builder()->CreateCall(func, {}, "calltmp");
    }
    return func;
}

template<>
llvm::Value *ExpressionCodeGenerator::generate<BuiltinFunctionASTNode>(const BuiltinFunctionASTNode::View &nodeView) {
    return nodeView.func->generate(context);
}

template<>
llvm::Value *ExpressionCodeGenerator::generate<VariableASTNode>(const VariableASTNode::View &nodeView) {
    // Don't recompute a parameter value (this may involve multiple pointer indirections)
    if (instantiatedParameters.contains(nodeView.variableRef->name())) {
        return instantiatedParameters[nodeView.variableRef->name()];
    }
    // Start at the base node of this variable
    const PatternASTNode *node = nodeView.variableRef;

    std::list<std::pair<std::string, unsigned>> binderPath;

    // For as long as there are parent nodes, we add the constructor name and the binding position to a path
    // list
    while (node->parentNode()) {
        binderPath.emplace_front(node->parentNode()->constructorName(), node->constructorPosition());
        node = node->parentNode();
    }

    // Start with the parameter
    llvm::Value *var = parameterRootMap[nodeView.variableRef->name()];

    // Follow the path down to the variable, each time performing a struct lookup followed by a bit cast
    for (const std::pair<std::string, unsigned> &pathNode : binderPath) {
        var = context.builder()->CreateStructGEP(var, pathNode.second, "pmatchtmp");
        var = context.builder()->CreateBitCast(var, context.lookupConstructor(pathNode.first), "pcasttmp");
    }

    // Add the value to the caching map
    instantiatedParameters[nodeView.variableRef->name()] = var;

    return var;
}

template<>
llvm::Value *ExpressionCodeGenerator::generate<ConstructorASTNode>(const ConstructorASTNode::View &nodeView) {

}

template<>
llvm::Value *ExpressionCodeGenerator::generate<IntegralConstructorASTNode>(const IntegralConstructorASTNode::View &nodeView) {
    return context.intValue(nodeView.value);
}
