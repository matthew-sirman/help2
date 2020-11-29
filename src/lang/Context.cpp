//
// Created by matthew on 22/11/2020.
//

#include "../../include/lang/Context.h"

Context::Context() = default;

std::shared_ptr<Type> Context::getType(const std::string &name) {
    return typeMap[name];
}

InfixType &Context::createInfixType(const std::string &name, ParameterPlaceholder &&left, ParameterPlaceholder &&right) {
    std::shared_ptr<InfixType> newType = std::make_shared<InfixType>(name, std::move(left), std::move(right));
    typeMap[name] = newType;
    return *newType;
}

PrefixType &Context::createPrefixType(const std::string &name) {
    std::shared_ptr<PrefixType> newType = std::make_shared<PrefixType>(name);
    typeMap[name] = newType;
    return *newType;
}

PrefixType &Context::createPrefixType(const std::string &name, std::vector<ParameterPlaceholder> &&placeholders) {
    std::shared_ptr<PrefixType> newType = std::make_shared<PrefixType>(name, std::move(placeholders));
    typeMap[name] = newType;
    return *newType;
}

