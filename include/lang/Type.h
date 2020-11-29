//
// Created by matthew on 19/11/2020.
//

#ifndef HELP2_TYPE_H
#define HELP2_TYPE_H

#include <string>
#include <memory>
#include <vector>
#include <stdexcept>

enum class TypeUsageMode {
    Prefix,
    Infix
};

class TypeException : public std::exception {
public:
    TypeException(const std::string &message);

    const char *what() const noexcept override;

private:
    std::string message;
};

class ParameterPlaceholder {
public:
    explicit ParameterPlaceholder(std::string name);

    ParameterPlaceholder(const ParameterPlaceholder &other) = default;

    ParameterPlaceholder(ParameterPlaceholder &&other) noexcept = default;

private:
    std::string name;
};

class Type {
public:
    explicit Type(std::string name);

    Type(const Type &other) = default;

    Type(Type &&other) noexcept = default;

    std::string name() const;

    inline virtual TypeUsageMode usage() const = 0;

    inline virtual bool primitive() const = 0;

    inline virtual const std::vector<ParameterPlaceholder> &parameters() const = 0;

    inline virtual size_t parameterCount() const = 0;

    inline virtual const ParameterPlaceholder &leftParameter() const = 0;

    inline virtual const ParameterPlaceholder &rightParameter() const = 0;

private:
    std::string typeName;
};

class PrefixType : public Type {
public:
    PrefixType(const std::string &name);

    PrefixType(const std::string &name, std::vector<ParameterPlaceholder> parameters);

    PrefixType(const PrefixType &other) = default;

    PrefixType(PrefixType &&other) noexcept = default;

    inline TypeUsageMode usage() const override { return TypeUsageMode::Prefix; }

    inline bool primitive() const override { return params.empty(); };

    inline const std::vector<ParameterPlaceholder> &parameters() const override { return params; }

    inline size_t parameterCount() const override { return params.size(); }

    inline const ParameterPlaceholder &leftParameter() const override {
        throw TypeException("Can't access 'left' parameter of prefix type.");
    }

    inline const ParameterPlaceholder &rightParameter() const override {
        throw TypeException("Can't access 'right' parameter of prefix type.");
    }

private:
    std::vector<ParameterPlaceholder> params;
};

class InfixType : public Type {
public:
    InfixType(const std::string &name, ParameterPlaceholder left, ParameterPlaceholder right);

    InfixType(const InfixType &other) = default;

    InfixType(InfixType &&other) noexcept = default;

    inline TypeUsageMode usage() const override { return TypeUsageMode::Infix; }

    inline bool primitive() const override { return false; };

    inline size_t parameterCount() const override { return 2; }

    inline const std::vector<ParameterPlaceholder> & parameters() const override {
        throw TypeException("Can't access parameter list for infix type.");
    }

    inline const ParameterPlaceholder &leftParameter() const override { return left; }

    inline const ParameterPlaceholder &rightParameter() const override { return right; }

private:
    ParameterPlaceholder left, right;
};

enum class InstanceParameterType {
    Parametric,
    Polymorphic
};

// A TypeInstance must specify all of the type parameters
class TypeInstance {
    friend std::ostream &operator<<(std::ostream &os, const TypeInstance &typeInstance);

public:

protected:
    inline virtual InstanceParameterType instanceType() const = 0;

    inline virtual size_t parameterCount() const = 0;
};

class ParametricTypeInstance : public TypeInstance {
    friend std::ostream &operator<<(std::ostream &os, const TypeInstance &typeInstance);

public:
    explicit ParametricTypeInstance(const Type &type);

protected:
    inline InstanceParameterType instanceType() const override { return InstanceParameterType::Parametric; };

    virtual TypeUsageMode usage() const = 0;

    const Type &type;
};

class PolymorphicTypeInstance : public TypeInstance {
    friend std::ostream &operator<<(std::ostream &os, const TypeInstance &typeInstance);

public:
    explicit PolymorphicTypeInstance(std::string name);

    PolymorphicTypeInstance(const PolymorphicTypeInstance &other) = default;

    PolymorphicTypeInstance(PolymorphicTypeInstance &&other) noexcept = default;

protected:
    inline InstanceParameterType instanceType() const override { return InstanceParameterType::Polymorphic; };

    inline size_t parameterCount() const override { return 0; }

private:
    std::string name;
};

class PrefixTypeInstance;
class InfixTypeInstance;

class PrefixTypeInstance : public ParametricTypeInstance {
    friend std::ostream &operator<<(std::ostream &os, const TypeInstance &typeInstance);

public:
    explicit PrefixTypeInstance(const Type &type);

//    template<typename ..._Args>
//    PrefixTypeInstance(const Type &type, _Args &&...parameterisation);

    PrefixTypeInstance(const Type &type, std::vector<std::unique_ptr<TypeInstance>> &&parameters);

    PrefixTypeInstance(const PrefixTypeInstance &other) = delete;

    PrefixTypeInstance(PrefixTypeInstance &&other) noexcept = default;

protected:
    inline size_t parameterCount() const override { return parameterisation.size(); }

    inline TypeUsageMode usage() const override { return TypeUsageMode::Prefix; }

private:
//    template<typename Arg, typename ..._Args>
//    void setParameters(Arg &&front, _Args &&...args);
//
//    template<typename Arg>
//    void setParameters(Arg &&last);
//
//    void setParameter(PolymorphicTypeInstance &&param);
//
//    void setParameter(PrefixTypeInstance &&param);
//
//    void setParameter(InfixTypeInstance &&param);

    std::vector<std::unique_ptr<TypeInstance>> parameterisation;
};

class InfixTypeInstance : public ParametricTypeInstance {
    friend std::ostream &operator<<(std::ostream &os, const TypeInstance &typeInstance);

public:
    template<typename _Left, typename _Right> requires std::is_base_of_v<TypeInstance, _Left> && std::is_base_of_v<TypeInstance, _Right>
    InfixTypeInstance(const Type &type, _Left &&left, _Right &&right);

    InfixTypeInstance(const InfixTypeInstance &other) = delete;

    InfixTypeInstance(InfixTypeInstance &&other) noexcept = default;

protected:
    inline size_t  parameterCount() const override { return 2; }

    inline TypeUsageMode usage() const override { return TypeUsageMode::Infix; }

private:
    std::unique_ptr<TypeInstance> left, right;
};

class Constructor {
public:
    Constructor(std::string name, std::shared_ptr<TypeInstance> constructorType,
                std::vector<std::unique_ptr<TypeInstance>> &&parameters);

    virtual bool operator==(const Constructor &other) const;

    virtual bool operator!=(const Constructor &other) const;

private:
    std::string name;
    std::shared_ptr<TypeInstance> constructorType;
    std::vector<std::unique_ptr<TypeInstance>> parameters;
};

//template<typename ..._Args>
//PrefixTypeInstance::PrefixTypeInstance(const Type &type, _Args &&...parameterisation)
//        : ParametricTypeInstance(type) {
//    setParameters(std::move(parameterisation)...);
//    if (type.parameterCount() != parameterCount()) {
//        throw TypeException("Mismatched number of arguments applied to type.");
//    }
//}
//
//
//template<typename _Front, typename ..._Args>
//void PrefixTypeInstance::setParameters(_Front &&param, _Args &&...rest) {
//    setParameter(std::move(param));
//    setParameters(rest...);
//}
//
//template<typename Arg>
//void PrefixTypeInstance::setParameters(Arg &&arg) {
//    setParameter(std::move(arg));
//}

template<typename _Left, typename _Right> requires std::is_base_of_v<TypeInstance, _Left> && std::is_base_of_v<TypeInstance, _Right>
InfixTypeInstance::InfixTypeInstance(const Type &type, _Left &&left, _Right &&right)
        : ParametricTypeInstance(type), left(std::make_unique<_Left>(std::move(left))), right(std::make_unique<_Right>(std::move(right))) {

}

#endif //HELP2_TYPE_H
