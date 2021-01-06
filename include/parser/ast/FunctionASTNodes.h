//
// Created by matthew on 28/11/2020.
//

#ifndef HELP2_FUNCTIONASTNODES_H
#define HELP2_FUNCTIONASTNODES_H

#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>

#include "TypeASTNodes.h"
#include "ExpressionASTNodes.h"

class FunctionCodeGenerator;

enum class FunctionUsage {
    Prefix,
    Infix,
    Value
};

class FunctionDeclASTNode : public ASTNode {
    friend class ParseTree;
public:
    FunctionDeclASTNode(size_t lineNum, size_t fileIndex, std::string name,
                        PrerequisiteList &&prerequisites,
                        std::unique_ptr<TypeInstanceASTNode> &&funcType);

    const std::string &name() const { return funcName; }

    virtual constexpr FunctionUsage funcUsage() const = 0;

    virtual size_t maxArgs() const = 0;

    const TypeInstanceASTNode &functionType() const { return *funcType; }

    void setVirtual() { isVirtualFunc = true; }

    bool isVirtual() const { return isVirtualFunc; }

protected:
    void setSymbolicName(std::string name) { symbolicName = std::move(name); }

    std::string funcName;
    std::string symbolicName;
    std::unique_ptr<TypeInstanceASTNode> funcType;
    PrerequisiteList prerequisites;
    bool isVirtualFunc;
};

/*
 * Prefix Function declaration syntax:
 * func name: a -> b;
 */
class PrefixFunctionDeclASTNode : public FunctionDeclASTNode {
public:
    PrefixFunctionDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                              PrerequisiteList &&prerequisites,
                              std::unique_ptr<TypeInstanceASTNode> &&funcType);

    constexpr FunctionUsage funcUsage() const override { return FunctionUsage::Prefix; }

    size_t maxArgs() const override;
};

enum class Associativity;

/*
 * Infix Function declaration syntax:
 * infix func name: a -> b -> c;
 */
class InfixFunctionDeclASTNode : public FunctionDeclASTNode {
public:
    InfixFunctionDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                             PrerequisiteList &&prerequisites,
                             std::unique_ptr<TypeInstanceASTNode> &&funcType,
                             Associativity assoc);

    constexpr FunctionUsage funcUsage() const override { return FunctionUsage::Infix; }

    constexpr size_t maxArgs() const override { return 2; }

    constexpr int operatorPrecedence() const { return precedence; }

    void setPrecedence(int newPrecedence) { precedence = newPrecedence; }

    constexpr Associativity associativity() const { return assoc; }

private:
    int precedence;

    Associativity assoc;
};

/*
 * Value Function declaration syntax:
 * value name: a;
 */
class ValueFunctionDeclASTNode : public FunctionDeclASTNode {
public:
    ValueFunctionDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                             PrerequisiteList &&prerequisites,
                             std::unique_ptr<TypeInstanceASTNode> &&valueType);

    constexpr FunctionUsage funcUsage() const override { return FunctionUsage::Value; }

    constexpr size_t maxArgs() const override { return 0; }
};

class FunctionImplASTNode : public ASTNode {
public:
    struct View {
        const std::vector<PatternASTNode *> &patterns;
        const std::unique_ptr<ExpressionASTNode> &body;
        unsigned variant;
        llvm::Function *parentFunction;
    };

    FunctionImplASTNode(size_t lineNum, size_t fileIndex, std::vector<std::unique_ptr<PatternASTNode>> &&patterns,
                        std::unique_ptr<ExpressionASTNode> &&body);

    const ExpressionASTNode &functionBody() const { return *body; }

    constexpr const RefList<PatternASTNode> &parameterPatterns() const { return patternList; }

    llvm::BasicBlock *generate(FunctionCodeGenerator &generator, unsigned variant, llvm::Function *parentFunction);

private:
    std::vector<std::unique_ptr<PatternASTNode>> patterns;
    RefList<PatternASTNode> patternList;
    std::unique_ptr<ExpressionASTNode> body;
};

/*
class FunctionImplASTNode : public ASTNode {
public:
    struct View {
        const std::vector<PatternASTNode *> &patterns;
        const std::unique_ptr<ExpressionASTNode> &body;
        unsigned variant;
        llvm::Function *parentFunction;
    };

    FunctionImplASTNode(size_t lineNum, size_t fileIndex);

    virtual constexpr FunctionUsage functionUsage() const = 0;

    void setFunctionBody(std::unique_ptr<ExpressionASTNode> &&newBody) { body = std::move(newBody); }

    constexpr const std::unique_ptr<ExpressionASTNode> &functionBody() const { return body; }

    virtual llvm::BasicBlock *generate(FunctionCodeGenerator &generator, unsigned variant,
                                       llvm::Function *parentFunction) const = 0;

protected:
    std::unique_ptr<ExpressionASTNode> body;
};
 */

/*
 * Prefix Function implementation syntax:
 * name [pattern0 [pattern1 ...]] => expression;
 */
/*
class PrefixFunctionImplASTNode : public FunctionImplASTNode {
public:
    PrefixFunctionImplASTNode(size_t lineNum, size_t fileIndex);

    PrefixFunctionImplASTNode(size_t lineNum, size_t fileIndex,
                              std::vector<std::unique_ptr<PatternASTNode>> &&patterns);

    constexpr FunctionUsage functionUsage() const override { return FunctionUsage::Prefix; }

    constexpr const std::vector<std::unique_ptr<PatternASTNode>> &parameterPatterns() const { return patterns; }

    llvm::BasicBlock *generate(FunctionCodeGenerator &generator, unsigned variant,
                               llvm::Function *parentFunction) const override;

private:
    std::vector<std::unique_ptr<PatternASTNode>> patterns;
};
 */

/*
 * Infix Function implementation syntax:
 * lPattern name rPattern => expression;
 */
/*
class InfixFunctionImplASTNode : public FunctionImplASTNode {
public:
    InfixFunctionImplASTNode(size_t lineNum, size_t fileIndex,
                             std::unique_ptr<PatternASTNode> &&left, std::unique_ptr<PatternASTNode> &&right);

    constexpr FunctionUsage functionUsage() const override { return FunctionUsage::Infix; }

    constexpr const std::unique_ptr<PatternASTNode> &leftPattern() const { return lhs; }

    constexpr const std::unique_ptr<PatternASTNode> &rightPattern() const { return rhs; }

    llvm::BasicBlock *generate(FunctionCodeGenerator &generator, unsigned variant,
                               llvm::Function *parentFunction) const override;

private:
    std::unique_ptr<PatternASTNode> lhs, rhs;
};
 */

class FunctionDefinitionASTNode {
public:
    struct View {
        const std::unique_ptr<FunctionDeclASTNode> &declaration;
        const std::vector<std::unique_ptr<FunctionImplASTNode>> &implementations;
        bool hasOverloads;
    };

    FunctionDefinitionASTNode(std::unique_ptr<FunctionDeclASTNode> &&declaration);

    void addImplementation(std::unique_ptr<FunctionImplASTNode> &&implementation);

    const std::string &name() const { return declaration->name(); }

    FunctionDeclASTNode &decl() { return *declaration; }

    const FunctionDeclASTNode &decl() const { return *declaration; }

    constexpr const RefList<FunctionImplASTNode> &implementationVariants() const { return implementationList; }

    llvm::Function *generate(FunctionCodeGenerator &generator, const BindingMap &bindingMap) const;

    bool isPolymorphic() const { return declaration->functionType().isPolymorphic(); }

    bool containsClosures() const { return declaration->functionType().containsClosures(); };

    bool hasOverloads() const { return implementations.size() > 1; }

private:
    std::unique_ptr<FunctionDeclASTNode> declaration;
    std::vector<std::unique_ptr<FunctionImplASTNode>> implementations;
    RefList<FunctionImplASTNode> implementationList;
};

#endif //HELP2_FUNCTIONASTNODES_H
