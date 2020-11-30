//
// Created by matthew on 28/11/2020.
//

#include "../../../include/parser/ast/FunctionASTNodes.h"

#include <utility>

FunctionDeclASTNode::FunctionDeclASTNode(std::string name, std::unique_ptr<TypeInstanceASTNode> &&funcType)
        : funcName(std::move(name)), funcType(std::move(funcType)) {

}

FunctionDefinitionASTNode::FunctionDefinitionASTNode(std::unique_ptr<FunctionDeclASTNode> &&declaration)
        : declaration(std::move(declaration)) {

}
