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
    map[binderName] = this;
}

ConstructorPatternASTNode::ConstructorPatternASTNode(size_t lineNum, size_t fileIndex, std::string dataConstructor)
        : PatternASTNode(lineNum, fileIndex), dataConstructorName(std::move(dataConstructor)) {

}

ConstructorPatternASTNode::ConstructorPatternASTNode(size_t lineNum, size_t fileIndex, std::string dataConstructor,
                                                     std::vector<std::unique_ptr<PatternASTNode>> &&subPatterns)
        : PatternASTNode(lineNum, fileIndex), dataConstructorName(std::move(dataConstructor)),
          subPatterns(std::move(subPatterns)) {
    // Make pattern subtrees doubly linked
    unsigned posIdx = 0;
    std::for_each(this->subPatterns.begin(), this->subPatterns.end(),
                  [this, &posIdx](const std::unique_ptr<PatternASTNode> &node) {
                      node->setParent(this, posIdx++);
                  });
}

void ConstructorPatternASTNode::addAllBinders(BinderMap &map) const {
    std::for_each(subPatterns.begin(), subPatterns.end(),
                  [&map](const std::unique_ptr<PatternASTNode> &node) {
                      node->addAllBinders(map);
                  });
}
