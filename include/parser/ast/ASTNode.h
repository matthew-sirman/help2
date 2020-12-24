//
// Created by matthew on 10/12/2020.
//

#ifndef HELP2_ASTNODE_H
#define HELP2_ASTNODE_H

#include <string>

class ASTNode {
public:
    ASTNode(size_t lineNumber, size_t fileIndex);

    constexpr size_t lineNumber() const { return lineNum; }

    constexpr size_t fileIndex() const { return fileIdx; }

private:
    size_t lineNum;
    size_t fileIdx;
};

#endif //HELP2_ASTNODE_H
