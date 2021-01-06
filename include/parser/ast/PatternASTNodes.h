//
// Created by matthew on 29/12/2020.
//

#ifndef HELPC_PATTERNASTNODES_H
#define HELPC_PATTERNASTNODES_H

#include <memory>
#include <vector>
#include <unordered_map>

#include "ASTNode.h"

class VariablePatternASTNode;
class ConstructorPatternASTNode;

using BinderMap = std::unordered_map<std::string, std::reference_wrapper<const VariablePatternASTNode>>;

enum class PatternUsage {
    Variable,
    Constructor,
    Integral,
    Decimal,
    Char
};

class PatternASTNode : public ASTNode {
    friend class ConstructorPatternASTNode;
public:
    PatternASTNode(size_t lineNum, size_t fileIndex);

    virtual constexpr PatternUsage patternUsage() const = 0;

    constexpr const ConstructorPatternASTNode *parentNode() const { return parent; }

    constexpr unsigned constructorPosition() const { return position; }

    virtual void addAllBinders(BinderMap &map) const = 0;

    virtual void removeAllBinders(BinderMap &map) const = 0;

protected:
    const ConstructorPatternASTNode *parent;
    unsigned position;

    void setParent(const ConstructorPatternASTNode *newParent, unsigned newPos) { parent = newParent; position = newPos; }
};

class VariablePatternASTNode : public PatternASTNode {
public:
    VariablePatternASTNode(size_t lineNum, size_t fileIndex, std::string binder);

    constexpr PatternUsage patternUsage() const override { return PatternUsage::Variable; }

    constexpr const std::string &name() const { return binderName; }

    void addAllBinders(BinderMap &map) const override;

    void removeAllBinders(BinderMap &map) const override;

private:
    std::string binderName;
};

class ConstructorPatternASTNode : public PatternASTNode {
public:
    ConstructorPatternASTNode(size_t lineNum, size_t fileIndex, std::string dataConstructor);

    constexpr PatternUsage patternUsage() const override { return PatternUsage::Constructor; }

    constexpr const std::string &constructorName() const { return dataConstructorName; }

    constexpr const RefList<PatternASTNode> &nestedPatterns() const { return subPatternList; }

    void bindSubPattern(std::unique_ptr<PatternASTNode> &&subPattern);

    void addAllBinders(BinderMap &map) const override;

    void removeAllBinders(BinderMap &map) const override;

private:
    std::string dataConstructorName;
    std::vector<std::unique_ptr<PatternASTNode>> subPatterns;
    RefList<PatternASTNode> subPatternList;
};

template<typename ValueType, PatternUsage usage>
class LiteralPatternASTNode : public PatternASTNode {
public:
    LiteralPatternASTNode(size_t lineNum, size_t fileIndex, ValueType value);

    constexpr PatternUsage patternUsage() const override { return usage; }

    constexpr long long constructorValue() const { return value; }

    void addAllBinders(BinderMap &map) const override {}

    void removeAllBinders(BinderMap &map) const override {}

private:
    ValueType value;
};

using IntegralPatternASTNode = LiteralPatternASTNode<long long, PatternUsage::Integral>;
using DecimalPatternASTNode = LiteralPatternASTNode<double, PatternUsage::Decimal>;
using CharPatternASTNode = LiteralPatternASTNode<char, PatternUsage::Char>;

#endif //HELPC_PATTERNASTNODES_H
