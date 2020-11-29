//
// Created by matthew on 22/11/2020.
//

#ifndef HELP2_CONTEXT_H
#define HELP2_CONTEXT_H

#include <unordered_map>
#include <string>
#include <memory>

#include "Type.h"
#include "Expression.h"

class Context {
public:
    Context();

    Context(const Context &other) = delete;

    Context(Context &&other) noexcept = default;

    Context &operator=(const Context &other) = delete;

    std::shared_ptr<Type> getType(const std::string &name);

    InfixType &createInfixType(const std::string &name, ParameterPlaceholder &&left, ParameterPlaceholder &&right);

    PrefixType &createPrefixType(const std::string &name);

    PrefixType &createPrefixType(const std::string &name, std::vector<ParameterPlaceholder> &&placeholders);

private:
    std::unordered_map<std::string, std::shared_ptr<Type>> typeMap;
    // std::unordered_map<std::string, std::shared_ptr<Function>> functionMap;
};


#endif //HELP2_CONTEXT_H
