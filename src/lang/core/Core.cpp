//
// Created by matthew on 27/12/2020.
//

#include "../../../include/lang/core/Core.h"
#include "../../../include/lang/core/CoreBuilder.h"

Core Core::create() {
    Core core;
    CoreBuilder builder(core);

    builder.populateBuiltins();

    return core;
}

Core Core::createEmpty() {
    return Core();
}

bool Core::primitiveExists(const std::string &name) const {
    return primitiveTypeMap.contains(name);
}

const PrimitiveType &Core::getPrimitiveType(const std::string &name) const {
    return primitiveTypeMap.at(name);
}

bool Core::builtinTypeExists(const std::string &name) const {
    return primitiveExists(name) || builtinTypeMap.contains(name);
}

bool Core::builtinFunctionExists(const std::string &name) const {
    return builtinFunctionMap.contains(name);
}

const std::unique_ptr<BuiltinFunction> &Core::getBuiltinFunction(const std::string &name) const {
    return builtinFunctionMap.at(name);
}

void Core::addPrimitiveType(PrimitiveType &&prim) {
    primitiveTypeMap.emplace(prim.name(), std::move(prim));
}

void Core::addBuiltinFunction(std::unique_ptr<BuiltinFunction> &&func) {
    builtinFunctionMap.emplace(func->name(), std::move(func));
}
