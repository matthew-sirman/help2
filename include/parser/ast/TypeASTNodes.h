//
// Created by matthew on 28/11/2020.
//

#ifndef HELP2_TYPEASTNODES_H
#define HELP2_TYPEASTNODES_H

#include <string>
#include <memory>
#include <vector>

class DataConstructorASTNode {

};

class TypeDeclASTNode {

public:
    std::string name;
    std::vector<DataConstructorASTNode> constructors;
};

/*
 * Prefix Type syntax:
 * type Name [a0 [a1 ...]] [::= Cons0 [arg0 [arg1 ...]] [| Cons1 [arg0 [arg1 ...]] [| ...]]];
 */
class PrefixTypeDeclASTNode : public TypeDeclASTNode {

private:
    std::vector<std::string> typeConstructorParameters;
};

/*
 * Infix Type syntax:
 * infix type a Name b [::= Cons0 [arg0 [arg1 ...]] [| Cons1 [arg0 [arg1 ...]] [| ...]]];
 */
class InfixTypeDeclASTNode : public TypeDeclASTNode {

private:
    std::string leftParameter, rightParameter;
};

class TypeInstanceASTNode {

};

class InfixTypeInstanceASTNode : public TypeInstanceASTNode {

};

class PrefixTypeInstanceASTNode : public TypeInstanceASTNode {

};

#endif //HELP2_TYPEASTNODES_H
