//
// Created by matthew on 24/12/2020.
//

#include "../../include/compiler/CodeGenerator.h"

#include <sstream>
#include <array>

constexpr std::array<char, 16> hexLUT = {
        '0', '1', '2', '3',
        '4', '5', '6', '7',
        '8', '9', 'a', 'b',
        'c', 'd', 'e', 'f',
};

std::hash<std::string> TypeCodeGenerator::stringHasher {};

template<typename T>
static std::string toHexString(T src) {
    std::string hex;
    hex.resize(sizeof(T) * 2);

    const unsigned char *srcRaw = (unsigned char *) &src;

    for (size_t i = 0; i < sizeof(T); i++) {
        const unsigned char b = srcRaw[i];
        hex[2 * i] = hexLUT[b & 0xFu];
        hex[2 * i + 1] = hexLUT[(b >> 4u) & 0xFu];
    }

    return hex;
}

TypeCodeGenerator::TypeCodeGenerator(CompileContext &context)
        : context(context) {

}

std::string TypeCodeGenerator::generateUniqueTypeName(const std::string &name, const std::vector<llvm::Type *> &bindings) {
    if (bindings.empty()) {
        return name;
    }

    std::stringstream concatName;

    std::for_each(bindings.begin(), bindings.end(),
                  [&concatName](const llvm::Type *type) {
                      concatName << type->getStructName().str();
                  });

    size_t hash = stringHasher(concatName.str());

    // '-' characters are not allowed in identifiers, so this is guaranteed not to clash with another type
    return name + "-" + toHexString(hash);
}

FunctionCodeGenerator::FunctionCodeGenerator(CompileContext &context, TypeCodeGenerator &typeCodeGenerator)
        : context(context), typeCodeGenerator(typeCodeGenerator) {

}

ExpressionCodeGenerator::ExpressionCodeGenerator(CompileContext &context, FunctionCodeGenerator &funcCodeGenerator)
        : context(context), funcCodeGenerator(funcCodeGenerator) {

}

void ExpressionCodeGenerator::addParameterRoot(const std::string &parameterName, llvm::Argument *root) {
    parameterRootMap[parameterName] = root;
}
