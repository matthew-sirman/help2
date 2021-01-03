//
// Created by matthew on 28/11/2020.
//

#ifndef HELP2_TYPEASTNODES_H
#define HELP2_TYPEASTNODES_H

#include <string>
#include <memory>
#include <vector>
#include <unordered_set>

#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

#include "ASTNode.h"

#include "../../lang/core/PrimitiveType.h"

class TypeCodeGenerator;
class TypeInstanceASTNode;
class TypeDeclASTNode;
class FunctionImplASTNode;
class FunctionDeclASTNode;
class TypeclassInstanceASTNode;
class TypeclassInstanceImplASTNode;

enum class TypeUsage {
    Infix,
    Prefix,
    Function,
    Polymorphic,
    Primitive
};

using BindingMap = std::unordered_map<std::string, llvm::Type *>;

class DataConstructorASTNode : public ASTNode {
public:
    struct View {
        const TypeDeclASTNode &type;
        const std::string &constructorName;
        const std::vector<TypeInstanceASTNode *> &params;
        const std::string &uniqueTypeName;
        unsigned char tagSize;
        unsigned short tag;
        const BindingMap &bindingMap;
    };

    DataConstructorASTNode(size_t lineNum, size_t fileIndex, const TypeDeclASTNode &type,
                           std::string constructorName);

    constexpr const std::string &name() const { return constructorName; }

    virtual constexpr size_t args() const = 0;

    virtual constexpr TypeUsage usage() const = 0;

    virtual llvm::Type *generate(TypeCodeGenerator &generator, const std::string &uniqueTypeName, unsigned char tagSize,
                                 unsigned short tag, const BindingMap &bindingMap) const = 0;

protected:
    const TypeDeclASTNode &type;
    std::string constructorName;
};

class PrefixDataConstructorASTNode : public DataConstructorASTNode {
public:
    PrefixDataConstructorASTNode(size_t lineNum, size_t fileIndex, const TypeDeclASTNode &type,
                                 const std::string &constructorName);

    PrefixDataConstructorASTNode(size_t lineNum, size_t fileIndex, const TypeDeclASTNode &type,
                                 const std::string &constructorName,
                                 std::vector<std::unique_ptr<TypeInstanceASTNode>> &&parameters);

    size_t args() const override { return params.size(); }

    constexpr TypeUsage usage() const override { return TypeUsage::Prefix; }

    const std::vector<std::unique_ptr<TypeInstanceASTNode>> &parameters() const { return params; }

    llvm::Type *generate(TypeCodeGenerator &generator, const std::string &uniqueTypeName, unsigned char tagSize,
                         unsigned short tag, const BindingMap &bindingMap) const override;

private:
    std::vector<std::unique_ptr<TypeInstanceASTNode>> params;
};

class InfixDataConstructorASTNode : public DataConstructorASTNode {
public:
    InfixDataConstructorASTNode(size_t lineNum, size_t fileIndex, const TypeDeclASTNode &type,
                                const std::string &constructorName, std::unique_ptr<TypeInstanceASTNode> &&leftParam,
                                std::unique_ptr<TypeInstanceASTNode> &&rightParam);

    constexpr size_t args() const override { return 2; }

    constexpr TypeUsage usage() const override { return TypeUsage::Infix; }

    const std::unique_ptr<TypeInstanceASTNode> &leftParameter() const { return lhs; }

    const std::unique_ptr<TypeInstanceASTNode> &rightParameter() const { return rhs; }

    llvm::Type *generate(TypeCodeGenerator &generator, const std::string &uniqueTypeName, unsigned char tagSize,
                         unsigned short tag, const BindingMap &bindingMap) const override;

private:
    std::unique_ptr<TypeInstanceASTNode> lhs, rhs;
};

class TypeDeclASTNode : public ASTNode {
public:
    struct View {
        size_t fileIndex;
        const std::string &name;
        const std::vector<std::unique_ptr<DataConstructorASTNode>> &constructors;
        const std::vector<std::string> &typeConstructorParameters;
        const std::vector<llvm::Type *> &bindings;
    };

    TypeDeclASTNode(size_t lineNum, size_t fileIndex, std::string name);

    TypeDeclASTNode(size_t lineNum, size_t fileIndex, std::string name,
                    std::vector<std::unique_ptr<DataConstructorASTNode>> &&constructors);

    void addDataConstructor(std::unique_ptr<DataConstructorASTNode> &&constructor);

    virtual constexpr TypeUsage typeUsage() const = 0;

    virtual constexpr size_t args() const = 0;

    const std::string &typeName() const { return name; }

    const std::vector<std::unique_ptr<DataConstructorASTNode>> &dataConstructors() const { return constructors; };

protected:
    std::string name;
    std::vector<std::unique_ptr<DataConstructorASTNode>> constructors;
};

/*
 * Prefix Type syntax:
 * type Name [a0 [a1 ...]] [::= Cons0 [arg0 [arg1 ...]] [| Cons1 [arg0 [arg1 ...]] [| ...]]];
 */
class PrefixTypeDeclASTNode : public TypeDeclASTNode {
public:
    PrefixTypeDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                          std::vector<std::string> &&typeVariables);

    PrefixTypeDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name,
                          std::vector<std::string> &&typeVariables,
                          std::vector<std::unique_ptr<DataConstructorASTNode>> &&constructors);

    TypeUsage typeUsage() const override { return TypeUsage::Prefix; }

    size_t args() const override { return typeConstructorParameters.size(); }

    llvm::Type *generate(TypeCodeGenerator &generator) const;

    llvm::Type *generate(TypeCodeGenerator &generator, const std::vector<llvm::Type *> &bindings) const;

    constexpr const std::vector<std::string> &typeVariables() const { return typeConstructorParameters; }

private:
    std::vector<std::string> typeConstructorParameters;
};

/*
 * Infix Type syntax:
 * infix type a Name b [::= Cons0 [arg0 [arg1 ...]] [| Cons1 [arg0 [arg1 ...]] [| ...]]];
 */
class InfixTypeDeclASTNode : public TypeDeclASTNode {
public:
    InfixTypeDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name, std::string left,
                         std::string right);

    InfixTypeDeclASTNode(size_t lineNum, size_t fileIndex, const std::string &name, std::string left, std::string right,
                         std::vector<std::unique_ptr<DataConstructorASTNode>> &&constructors);

    TypeUsage typeUsage() const override { return TypeUsage::Infix; }

    constexpr size_t args() const override { return 2; }

    llvm::Type *generate(TypeCodeGenerator &generator, llvm::Type *leftBinding, llvm::Type *rightBinding) const;

private:
    std::string leftParameter, rightParameter;
};

class TypeInstanceASTNode {
public:
    TypeInstanceASTNode();

    virtual constexpr TypeUsage typeUsage() const = 0;

    virtual bool isPolymorphic() const = 0;

    virtual bool containsClosures() const = 0;

    virtual bool containsFunctionType() const = 0;

    constexpr virtual std::size_t functionDepth() const { return 0; }

    virtual llvm::Type *instantiate(TypeCodeGenerator &generator, const BindingMap &bindingMap) const = 0;

    friend bool operator==(const TypeInstanceASTNode &lhs, const TypeInstanceASTNode &rhs);

    friend bool operator!=(const TypeInstanceASTNode &lhs, const TypeInstanceASTNode &rhs);
};

class UserTypeInstanceASTNode : public TypeInstanceASTNode, public ASTNode {
public:
    UserTypeInstanceASTNode(size_t lineNum, size_t fileIndex, std::string name);

    const std::string &typeName() const { return name; }

protected:
    std::string name;
};

class PrefixTypeInstanceInterface {
public:
    void bindParameter(std::unique_ptr<TypeInstanceASTNode> &&parameter);

    const std::vector<std::unique_ptr<TypeInstanceASTNode>> &parameters() const { return params; }

protected:
    std::vector<std::unique_ptr<TypeInstanceASTNode>> params;
};

class PolymorphicTypeInstanceASTNode : public UserTypeInstanceASTNode, public PrefixTypeInstanceInterface {
public:
    PolymorphicTypeInstanceASTNode(size_t lineNum, size_t fileIndex, const std::string &name);

    constexpr TypeUsage typeUsage() const override { return TypeUsage::Polymorphic; }

    bool isPolymorphic() const override;

    bool containsClosures() const override;

    bool containsFunctionType() const override;

    llvm::Type *instantiate(TypeCodeGenerator &generator, const BindingMap &bindingMap) const override;
};

class InfixTypeInstanceASTNode : public UserTypeInstanceASTNode {
public:
    struct View {
        const std::string &name;
        const std::unique_ptr<TypeInstanceASTNode> &lhs, &rhs;
        const BindingMap &bindingMap;
    };

    InfixTypeInstanceASTNode(size_t lineNum, size_t fileIndex, const std::string &name);

    constexpr TypeUsage typeUsage() const override { return TypeUsage::Infix; }

    virtual const std::unique_ptr<TypeInstanceASTNode> &left() const { return lhs; }

    virtual const std::unique_ptr<TypeInstanceASTNode> &right() const { return rhs; }

    virtual void bindLeft(std::unique_ptr<TypeInstanceASTNode> &&param);

    virtual void bindRight(std::unique_ptr<TypeInstanceASTNode> &&param);

    bool isPolymorphic() const override;

    bool containsClosures() const override;

    bool containsFunctionType() const override;

    llvm::Type *instantiate(TypeCodeGenerator &generator, const BindingMap &bindingMap) const override;

protected:
    std::unique_ptr<TypeInstanceASTNode> lhs, rhs;
};

class FunctionTypeInstanceASTNode : public InfixTypeInstanceASTNode {
public:
    using View = InfixTypeInstanceASTNode::View;

    FunctionTypeInstanceASTNode(size_t lineNum, size_t fileIndex, const std::string &name);

    constexpr TypeUsage typeUsage() const override { return TypeUsage::Function; }

    bool containsClosures() const override;

    bool containsFunctionType() const override;

    std::size_t functionDepth() const override;

    llvm::Type *instantiate(TypeCodeGenerator &generator, const BindingMap &bindingMap) const override;
};

class PrefixTypeInstanceASTNode : public UserTypeInstanceASTNode, public PrefixTypeInstanceInterface {
public:
    struct View {
        const std::string &name;
        const std::vector<std::unique_ptr<TypeInstanceASTNode>> &params;
        const BindingMap &bindingMap;
    };

    PrefixTypeInstanceASTNode(size_t lineNum, size_t fileIndex, const std::string &name);

    constexpr TypeUsage typeUsage() const override { return TypeUsage::Prefix; }

    bool isPolymorphic() const override;

    bool containsClosures() const override;

    bool containsFunctionType() const override;

    llvm::Type *instantiate(TypeCodeGenerator &generator, const BindingMap &bindingMap) const override;
};

class PrimitiveTypeInstanceASTNode : public TypeInstanceASTNode {
public:
    struct View {
        const PrimitiveType &type;
    };

    explicit PrimitiveTypeInstanceASTNode(const PrimitiveType &type);

    constexpr TypeUsage typeUsage() const override { return TypeUsage::Primitive; }

    constexpr bool isPolymorphic() const override { return false; }

    constexpr const PrimitiveType &primitiveType() const { return type; }

    bool containsClosures() const override { return false; };

    bool containsFunctionType() const override { return false; };

    llvm::Type * instantiate(TypeCodeGenerator &generator, const BindingMap &bindingMap) const override;

private:
    const PrimitiveType &type;
};

using PrerequisiteList = std::vector<std::unique_ptr<TypeclassInstanceASTNode>>;

class TypeclassASTNode : public ASTNode {
public:
    TypeclassASTNode(size_t lineNum, size_t fileIndex, std::string typeclassName, std::string variable,
                     PrerequisiteList &&prerequisites);

    void addMethod(std::unique_ptr<FunctionDeclASTNode> &&method);

    // Take this instance by value so it will be "stolen" even if we don't take it - this should help
    // to prevent having references to duplicated instances
    TypeclassInstanceImplASTNode &addInstance(std::unique_ptr<TypeclassInstanceImplASTNode> instance);

    constexpr const std::string &name() const { return typeclassName; }

    bool hasMethod(const std::string &methodName) const;

    std::size_t methodCount() const { return typeclassMethods.size(); }

    const std::unordered_set<std::string> &methodNames() const { return tcMethodNames; }

private:
    std::string typeclassName;
    std::string variable;
    PrerequisiteList prerequisites;
    std::unordered_set<std::string> tcMethodNames;
    std::unordered_map<std::string, std::unique_ptr<FunctionDeclASTNode>> typeclassMethods;
    std::vector<std::unique_ptr<TypeclassInstanceImplASTNode>> instances;
};

class TypeclassInstanceASTNode : public ASTNode {
public:
    TypeclassInstanceASTNode(size_t lineNum, size_t fileIndex, const TypeclassASTNode &typeclass,
                             std::unique_ptr<TypeInstanceASTNode> &&typeInstance);

    constexpr const TypeclassASTNode &typeclass() const { return typeclassReference; }

    const TypeInstanceASTNode &instance() const { return *typeInstance; }

    friend bool operator==(const TypeclassInstanceASTNode &lhs, const TypeclassInstanceASTNode &rhs);

    friend bool operator!=(const TypeclassInstanceASTNode &lhs, const TypeclassInstanceASTNode &rhs);

private:
    const TypeclassASTNode &typeclassReference;
    std::unique_ptr<TypeInstanceASTNode> typeInstance;
};

class TypeclassInstanceImplASTNode : public ASTNode {
    friend class TypeclassASTNode;
public:
    TypeclassInstanceImplASTNode(size_t lineNum, size_t fileIndex, std::unique_ptr<TypeclassInstanceASTNode> &&typeclass,
                                 PrerequisiteList &&prerequisites);

    const TypeclassASTNode &typeclass() const { return typeclassInstance->typeclass(); }

    const TypeInstanceASTNode &instance() const { return typeclassInstance->instance(); }

    bool fullyImplemented() const;

    void addImplementation(const std::string &name, std::unique_ptr<FunctionImplASTNode> &&implementation);

private:
    std::unique_ptr<TypeclassInstanceASTNode> typeclassInstance;
    std::unordered_map<std::string, std::unique_ptr<FunctionImplASTNode>> implementations;
    PrerequisiteList prerequisites;
};

#endif //HELP2_TYPEASTNODES_H
