//
// Created by matthew on 27/12/2020.
//

#include "../../../include/lang/core/CoreBuilder.h"
#include "../../../include/lang/core/Core.h"

CoreBuilder::CoreBuilder(Core &core)
        : core(core) {

}

void CoreBuilder::populateBuiltins() {
    populateTypes();
    populateFunctions();
}

void CoreBuilder::build(llvm::LLVMContext &ctx) {
    std::for_each(core.primitiveTypeMap.begin(), core.primitiveTypeMap.end(),
                  [&ctx](std::pair<const std::string, PrimitiveType> &typeEntry) {
                      typeEntry.second.build(ctx);
                  });
}

void CoreBuilder::populateTypes() {
    core.addPrimitiveType(PrimitiveType(
            "Int",
            [](llvm::LLVMContext &ctx) { return (llvm::Type *) llvm::Type::getInt64Ty(ctx); }
    ));

    core.addPrimitiveType(PrimitiveType(
            "Float",
            [](llvm::LLVMContext &ctx) { return (llvm::Type *) llvm::Type::getDoubleTy(ctx); }
    ));

    core.addPrimitiveType(PrimitiveType(
            "Bool",
            [](llvm::LLVMContext &ctx) { return (llvm::Type *) llvm::Type::getInt1Ty(ctx); }
    ));

    core.addPrimitiveType(PrimitiveType(
            "Char",
            [](llvm::LLVMContext &ctx) { return (llvm::Type *) llvm::Type::getInt8Ty(ctx); }
    ));
}

void CoreBuilder::populateFunctions() {
    core.addBuiltinFunction(std::unique_ptr<AddIntFunction>(
            new AddIntFunction("+", "__add_i64", 5, Associativity::Left)
    ));
    core.addBuiltinFunction(std::unique_ptr<SubIntFunction>(
            new SubIntFunction("-", "__sub_i64", 5, Associativity::Left)
    ));
    core.addBuiltinFunction(std::unique_ptr<MulIntFunction>(
            new MulIntFunction("*", "__mul_i64", 6, Associativity::Left)
    ));
}
