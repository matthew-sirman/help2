//
// Created by matthew on 19/11/2020.
//

#include "../../include/lang/Type.h"

#include <ostream>
#include <utility>

TypeException::TypeException(const std::string &message) : message("TypeException: " + message) {

}

const char *TypeException::what() const noexcept {
    return message.c_str();
}

ParameterPlaceholder::ParameterPlaceholder(std::string name)
        : name(std::move(name)) {

}

Type::Type(std::string typeName)
        : typeName(std::move(typeName)) {

}

std::string Type::name() const {
    return typeName;
}

PrefixType::PrefixType(const std::string &name)
        : Type(name) {

}

PrefixType::PrefixType(const std::string &name, std::vector<ParameterPlaceholder> parameters)
        : Type(name), params(std::move(parameters)) {

}

InfixType::InfixType(const std::string &name, ParameterPlaceholder left, ParameterPlaceholder right)
        : Type(name), left(std::move(left)), right(std::move(right)) {

}

ParametricTypeInstance::ParametricTypeInstance(const Type &type)
        : type(type) {

}

PolymorphicTypeInstance::PolymorphicTypeInstance(std::string name)
        : name(std::move(name)) {

}

PrefixTypeInstance::PrefixTypeInstance(const Type &type)
        : ParametricTypeInstance(type) {

}

PrefixTypeInstance::PrefixTypeInstance(const Type &type, std::vector<std::unique_ptr<TypeInstance>> &&parameters)
        : ParametricTypeInstance(type), parameterisation(std::move(parameters)) {
    if (type.parameterCount() != parameterisation.size()) {
        throw TypeException("Mismatched number of arguments applied to type.");
    }
}

Constructor::Constructor(std::string name, std::shared_ptr<TypeInstance> constructorType,
                         std::vector<std::unique_ptr<TypeInstance>> &&parameters)
        : name(std::move(name)), constructorType(std::move(constructorType)), parameters(std::move(parameters)) {

}

bool Constructor::operator==(const Constructor &other) const {
    // TODO: Do a proper check!
    return constructorType.get() == other.constructorType.get();
}

bool Constructor::operator!=(const Constructor &other) const {
    return constructorType.get() != other.constructorType.get();
}

//void PrefixTypeInstance::setParameter(PolymorphicTypeInstance &&param) {
//    parameterisation.push_back(std::make_unique<PolymorphicTypeInstance>(std::move(param)));
//}
//
//void PrefixTypeInstance::setParameter(PrefixTypeInstance &&param) {
//    parameterisation.push_back(std::make_unique<PrefixTypeInstance>(std::move(param)));
//}
//
//void PrefixTypeInstance::setParameter(InfixTypeInstance &&param) {
//    parameterisation.push_back(std::make_unique<InfixTypeInstance>(std::move(param)));
//}

std::ostream &operator<<(std::ostream &os, const TypeInstance &typeInstance) {
    switch (typeInstance.instanceType()) {
        case InstanceParameterType::Parametric: {
            switch (((const ParametricTypeInstance &) typeInstance).usage()) {
                case TypeUsageMode::Prefix: {
                    const PrefixTypeInstance &prefix = (const PrefixTypeInstance &) typeInstance;
                    os << prefix.type.name();
                    for (const std::unique_ptr<TypeInstance> &param : prefix.parameterisation) {
                        if (param->parameterCount() == 0) {
                            os << " " << *param;
                        } else {
                            os << " (" << *param << ")";
                        }
                    }
                    break;
                }
                case TypeUsageMode::Infix: {
                    const InfixTypeInstance &infix = (const InfixTypeInstance &) typeInstance;
                    if (infix.left->parameterCount() == 0) {
                        os << *infix.left;
                    } else {
                        os << "(" << *infix.left << ")";
                    }
                    os << " " << infix.type.name() << " ";
                    if (infix.right->parameterCount() == 0) {
                        os << *infix.right;
                    } else {
                        os << "(" << *infix.right << ")";
                    }
                    break;
                }
            }
            break;
        }
        case InstanceParameterType::Polymorphic: {
            const PolymorphicTypeInstance &polymorphic = (const PolymorphicTypeInstance &) typeInstance;
            os << polymorphic.name;
            break;
        }
    }
    return os;
}
