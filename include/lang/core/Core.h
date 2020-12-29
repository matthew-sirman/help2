//
// Created by matthew on 27/12/2020.
//

#ifndef HELP2_CORE_H
#define HELP2_CORE_H

#include <unordered_map>

#include "PrimitiveType.h"
#include "BuiltinFunction.h"

class CoreBuilder;

class Core {
    friend class CoreBuilder;
public:
    static Core create();

    static Core createEmpty();

    bool primitiveExists(const std::string &name) const;

    const PrimitiveType &getPrimitiveType(const std::string &name) const;

    bool builtinTypeExists(const std::string &name) const;

    bool builtinFunctionExists(const std::string &name) const;

    const std::unique_ptr<BuiltinFunction> &getBuiltinFunction(const std::string &name) const;

private:
    Core() = default;

    void addPrimitiveType(PrimitiveType &&prim);

    void addBuiltinFunction(std::unique_ptr<BuiltinFunction> &&func);

    std::unordered_map<std::string, PrimitiveType> primitiveTypeMap;
    // TODO: Fix this to actually take builtin types!
    std::unordered_map<std::string, int> builtinTypeMap;
    std::unordered_map<std::string, std::unique_ptr<BuiltinFunction>> builtinFunctionMap;
};

#endif //HELP2_CORE_H