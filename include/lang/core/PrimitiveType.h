//
// Created by matthew on 27/12/2020.
//

#ifndef HELP2_PRIMITIVETYPE_H
#define HELP2_PRIMITIVETYPE_H

#include <functional>

#include <llvm/IR/Type.h>

class CoreBuilder;

class PrimitiveType {
    friend class CoreBuilder;
public:
    llvm::Type *llvmType() const;

    constexpr const std::string &name() const { return typeName; }

private:
    PrimitiveType(std::string name, std::function<llvm::Type *(llvm::LLVMContext &)> typeBuilder);

    void build(llvm::LLVMContext &context);

    std::string typeName;
    llvm::Type *type;

    std::function<llvm::Type *(llvm::LLVMContext &)> typeBuilder;
};


#endif //HELP2_PRIMITIVETYPE_H
