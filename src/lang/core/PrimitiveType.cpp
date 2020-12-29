//
// Created by matthew on 27/12/2020.
//

#include "../../../include/lang/core/PrimitiveType.h"

#include <iostream>

PrimitiveType::PrimitiveType(std::string name, std::function<llvm::Type *(llvm::LLVMContext &)> typeBuilder)
        : typeName(std::move(name)), type(nullptr), typeBuilder(std::move(typeBuilder)) {

}

llvm::Type *PrimitiveType::llvmType() const {
    if (!type) {
        std::cerr << "DEVELOPER: LLVM Type requested from primitive '" << typeName << "' before building." << std::endl;
        throw;
    }
    return type;
}

void PrimitiveType::build(llvm::LLVMContext &context) {
    type = typeBuilder(context);
}
