//
// Created by matthew on 22/12/2020.
//

#include "../../include/compiler/Compiler.h"

#include "../../include/lang/core/CoreBuilder.h"

#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/IPO.h>

CompileContext::CompileContext(ParseTree &tree)
        : ctxContext(std::make_unique<llvm::LLVMContext>()),
          ctxBuilder(std::make_unique<llvm::IRBuilder<>>(*ctxContext)),
          ctxCurrentModule(nullptr),
          tree(tree) {
    // Create each module present in the parse tree
    for (const ParseTree::Module &ptMod : this->tree.allModules()) {
        std::unique_ptr<llvm::Module> module = std::make_unique<llvm::Module>(ptMod.moduleName, *ctxContext);
        module->setSourceFileName(ptMod.fileName.generic_string());
        ctxModules[ptMod.moduleName] = std::move(module);
    }
}

void CompileContext::setCurrentModule(const std::string &name) {
    ctxCurrentModule = &ctxModules[name];
}

void CompileContext::addInstantiatedType(const std::string &name, llvm::Type *type) {
    instantiatedTypes[name] = type;
}

void CompileContext::addConstructorType(const std::string &name, llvm::Type *type, bool tagged) {
    instantiatedConstructors[name] = { type, tagged };
}

llvm::Type *CompileContext::lookupType(const std::string &name) const {
    if (instantiatedTypes.contains(name)) {
        return instantiatedTypes.at(name);
    }
    return nullptr;
}

std::optional<TypeInfo> CompileContext::lookupConstructor(const std::string &name) const {
    if (instantiatedConstructors.contains(name)) {
        return instantiatedConstructors.at(name);
    }
    return std::nullopt;
}

llvm::IntegerType *CompileContext::int8Type() const {
    return llvm::IntegerType::getInt8Ty(*ctxContext);
}

llvm::IntegerType *CompileContext::int16Type() const {
    return llvm::IntegerType::getInt16Ty(*ctxContext);
}

llvm::IntegerType *CompileContext::int32Type() const {
    return llvm::IntegerType::getInt32Ty(*ctxContext);
}

llvm::IntegerType *CompileContext::int64Type() const {
    return llvm::IntegerType::getInt64Ty(*ctxContext);
}

llvm::FunctionType *CompileContext::intBinaryOpType() const {
    llvm::IntegerType *i64Ty = int64Type();
    return llvm::FunctionType::get(i64Ty, {i64Ty, i64Ty}, false);
}

llvm::ConstantInt *CompileContext::intValue(long long int val) const {
    return llvm::ConstantInt::get(int64Type(), val);
}

Compiler::Compiler(ParseTree &tree)
        : context(tree) {

}


void Compiler::compile() {
    // First, we build the core
    CoreBuilder(context.tree.core()).build(*context.context());

    TypeCodeGenerator typeGenerator(context);
    FunctionCodeGenerator functionGenerator(context, typeGenerator);

    for (const std::pair<const std::string, std::unique_ptr<FunctionDefinitionASTNode>> &func : context.tree.functions()) {
        // Skip over polymorphic functions - they will be instantiated on demand from concrete functions
        // Note that it is impossible to ever "add" polymorphism in a function, i.e. a concrete function
        // can never become polymorphic in the body.
        if (func.second->isPolymorphic()) {
            continue;
        }
        // Generate the functions which will in turn generate any dependent functions or types and specialise any
        // polymorphic ones. Note this also means the binding map is empty at this point.
        func.second->generate(functionGenerator, {});
    }
}
