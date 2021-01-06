//
// Created by matthew on 29/12/2020.
//

#include "../../../include/parser/ast/PatternASTNodes.h"

PatternASTNode::PatternASTNode(size_t lineNum, size_t fileIndex)
        : ASTNode(lineNum, fileIndex), parent(nullptr), position(0) {

}

VariablePatternASTNode::VariablePatternASTNode(size_t lineNum, size_t fileIndex, std::string binder)
        : PatternASTNode(lineNum, fileIndex), binderName(std::move(binder)) {

}

void VariablePatternASTNode::addAllBinders(BinderMap &map) const {
    map.emplace(binderName, *this);
}

void VariablePatternASTNode::removeAllBinders(BinderMap &map) const {
    map.erase(binderName);
}

ConstructorPatternASTNode::ConstructorPatternASTNode(size_t lineNum, size_t fileIndex, std::string dataConstructor)
        : PatternASTNode(lineNum, fileIndex), dataConstructorName(std::move(dataConstructor)) {

}

void ConstructorPatternASTNode::bindSubPattern(std::unique_ptr<PatternASTNode> &&subPattern) {
    // Set the parent of the new pattern to this node, and its position to the current number of patterns
    subPattern->setParent(this, subPatterns.size());
    subPatternList.emplace_back(*subPattern);
    subPatterns.push_back(std::move(subPattern));
}

void ConstructorPatternASTNode::removeAllBinders(BinderMap &map) const {
    std::for_each(subPatterns.begin(), subPatterns.end(),
                  [&map](const std::unique_ptr<PatternASTNode> &node) {
                      node->removeAllBinders(map);
                  });
}

void ConstructorPatternASTNode::addAllBinders(BinderMap &map) const {
    std::for_each(subPatterns.begin(), subPatterns.end(),
                  [&map](const std::unique_ptr<PatternASTNode> &node) {
                      node->addAllBinders(map);
                  });
}

template<typename ValueType, PatternUsage usage>
LiteralPatternASTNode<ValueType, usage>::LiteralPatternASTNode(size_t lineNum, size_t fileIndex, ValueType value)
        : PatternASTNode(lineNum, fileIndex), value(value) {

}

template class LiteralPatternASTNode<long long, PatternUsage::Integral>;
template class LiteralPatternASTNode<double, PatternUsage::Decimal>;
template class LiteralPatternASTNode<char, PatternUsage::Char>;
