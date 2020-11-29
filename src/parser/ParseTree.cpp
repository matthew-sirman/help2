//
// Created by matthew on 29/11/2020.
//

#include "../../include/parser/Parser.h"
#include "../../include/parser/ParseTree.h"


bool ParseTree::functionExists(const std::string &name) const {
    return definedFunctionNodes.find(name) != definedFunctionNodes.end();
}

bool ParseTree::typeExists(const std::string &name) const {
    return declaredTypeNodes.find(name) != declaredTypeNodes.end();
}
