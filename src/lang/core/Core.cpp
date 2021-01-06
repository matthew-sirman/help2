//
// Created by matthew on 27/12/2020.
//

#include "../../../include/lang/core/Core.h"
#include "../../../include/lang/core/CoreBuilder.h"

#include <iostream>

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

bool Core::builtinFunctionExists(const std::string &name) const {
    return builtinFunctionMap.contains(name);
}

const std::unique_ptr<BuiltinFunction> &Core::getBuiltinFunction(const std::string &name) const {
    return builtinFunctionMap.at(name);
}

const PrimitiveType &Core::unitType() const {
    if (!unit.has_value()) {
        std::cerr << "DEVELOPER: No unit type in core!" << std::endl;
        throw;
    }
    return unit.value();
}

void Core::populateTree(ParseTree &tree) const {

}

void Core::addPrimitiveType(PrimitiveType &&prim) {
    primitiveTypeMap.emplace(prim.name(), std::move(prim));
}

void Core::addBuiltinFunction(std::unique_ptr<BuiltinFunction> &&func) {
    builtinFunctionMap.emplace(func->name(), std::move(func));
}
