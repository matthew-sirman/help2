//
// Created by matthew on 27/12/2020.
//

#ifndef HELP2_COREBUILDER_H
#define HELP2_COREBUILDER_H

#include <llvm/IR/LLVMContext.h>

class Core;

class CoreBuilder {
public:
    CoreBuilder(Core &core);

    void populateBuiltins();

    void build(llvm::LLVMContext &ctx);

private:
    void populateTypes();

    void populateFunctions();

    Core &core;
};


#endif //HELP2_COREBUILDER_H
