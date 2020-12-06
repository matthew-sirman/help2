//
// Created by matthew on 28/11/2020.
//

#ifndef HELP2_TYPEASTNODES_H
#define HELP2_TYPEASTNODES_H

#include <string>
#include <memory>
#include <vector>

enum class TypeUsage {
    Infix,
    Prefix,
    Function,
    Polymorphic
};

class TypeInstanceASTNode;

class DataConstructorASTNode {
public:
    DataConstructorASTNode(std::string constructorName);

    constexpr const std::string &name() const { return constructorName; }

    virtual constexpr size_t args() const = 0;

    virtual constexpr TypeUsage usage() const = 0;

private:
    std::string constructorName;
};

class PrefixDataConstructorASTNode : public DataConstructorASTNode {
public:
    PrefixDataConstructorASTNode(const std::string &constructorName);

    PrefixDataConstructorASTNode(const std::string &constructorName, std::vector<std::unique_ptr<TypeInstanceASTNode>> &&parameters);

    size_t args() const override { return parameters.size(); }

    constexpr TypeUsage usage() const override { return TypeUsage::Prefix; }

private:
    std::vector<std::unique_ptr<TypeInstanceASTNode>> parameters;
};

class InfixDataConstructorASTNode : public DataConstructorASTNode {
public:
    InfixDataConstructorASTNode(const std::string &constructorName, std::unique_ptr<TypeInstanceASTNode> &&leftParam,
                                std::unique_ptr<TypeInstanceASTNode> &&rightParam);

    constexpr size_t args() const override { return 2; }

    constexpr TypeUsage usage() const override { return TypeUsage::Infix; }

private:
    std::unique_ptr<TypeInstanceASTNode> lhs, rhs;
};

class TypeDeclASTNode {
public:
    TypeDeclASTNode(std::string name);

    TypeDeclASTNode(std::string name, std::vector<std::unique_ptr<DataConstructorASTNode>> &&constructors);

    void addDataConstructor(std::unique_ptr<DataConstructorASTNode> &&constructor);

    virtual constexpr TypeUsage typeUsage() const = 0;

    const std::string &typeName() const { return name; }
private:
    std::string name;
    std::vector<std::unique_ptr<DataConstructorASTNode>> constructors;
};

/*
 * Prefix Type syntax:
 * type Name [a0 [a1 ...]] [::= Cons0 [arg0 [arg1 ...]] [| Cons1 [arg0 [arg1 ...]] [| ...]]];
 */
class PrefixTypeDeclASTNode : public TypeDeclASTNode {
public:
    PrefixTypeDeclASTNode(const std::string &name, std::vector<std::string> &&typeVariables);

    PrefixTypeDeclASTNode(const std::string &name, std::vector<std::string> &&typeVariables,
                          std::vector<std::unique_ptr<DataConstructorASTNode>> &&constructors);

    TypeUsage typeUsage() const override { return TypeUsage::Prefix; }

private:
    std::vector<std::string> typeConstructorParameters;
};

/*
 * Infix Type syntax:
 * infix type a Name b [::= Cons0 [arg0 [arg1 ...]] [| Cons1 [arg0 [arg1 ...]] [| ...]]];
 */
class InfixTypeDeclASTNode : public TypeDeclASTNode {
public:
    InfixTypeDeclASTNode(const std::string &name, std::string left, std::string right);

    InfixTypeDeclASTNode(const std::string &name, std::string left, std::string right,
                         std::vector<std::unique_ptr<DataConstructorASTNode>> &&constructors);

    TypeUsage typeUsage() const override { return TypeUsage::Infix; }

private:
    std::string leftParameter, rightParameter;
};


class TypeInstanceASTNode {
public:
    TypeInstanceASTNode(std::string name);

    virtual constexpr TypeUsage typeUsage() const = 0;

    const std::string &typeName() const { return name; }

private:
    std::string name;
};

class PolymorphicTypeInstanceASTNode : public TypeInstanceASTNode {
public:
    PolymorphicTypeInstanceASTNode(const std::string &name);

    constexpr TypeUsage typeUsage() const override { return TypeUsage::Polymorphic; }
};

class InfixTypeInstanceASTNode : public TypeInstanceASTNode {
public:
    InfixTypeInstanceASTNode(const std::string &name);

    virtual constexpr TypeUsage typeUsage() const { return TypeUsage::Infix; }

    virtual const TypeInstanceASTNode *left() const { return lhs.get(); }

    virtual const TypeInstanceASTNode *right() const { return rhs.get(); }

    virtual void bindLeft(std::unique_ptr<TypeInstanceASTNode> &&param);

    virtual void bindRight(std::unique_ptr<TypeInstanceASTNode> &&param);

private:
    std::unique_ptr<TypeInstanceASTNode> lhs, rhs;
};

class FunctionTypeInstanceASTNode : public InfixTypeInstanceASTNode {
public:
    FunctionTypeInstanceASTNode(const std::string &name);

    constexpr TypeUsage typeUsage() const override { return TypeUsage::Function; }
};

class PrefixTypeInstanceASTNode : public TypeInstanceASTNode {
public:
    PrefixTypeInstanceASTNode(const std::string &name);

    void bindParameter(std::unique_ptr<TypeInstanceASTNode> &&param);

    constexpr TypeUsage typeUsage() const override { return TypeUsage::Prefix; }

private:
    std::vector<std::unique_ptr<TypeInstanceASTNode>> parameters;
};

#endif //HELP2_TYPEASTNODES_H
