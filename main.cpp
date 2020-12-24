#include <iostream>
#include <fstream>
#include "include/parser/Parser.h"
#include "include/compiler/TypeChecker.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

void compilerTest() {
    using namespace llvm;

    LLVMContext ctx;

    StructType *llvm_List = StructType::create(ctx, "Int32List");
    llvm_List->setBody(IntegerType::get(ctx, 8));

    StructType *llvm_List_Cons = StructType::create(ctx, "Int32List$Cons");
    llvm_List_Cons->setBody(IntegerType::get(ctx, 8), IntegerType::get(ctx, 32),
                            PointerType::get(llvm_List, 0));

    outs() << *llvm_List << "\n";
    outs() << *llvm_List_Cons << "\n";

    /*Module *module = new Module("struct_test", ctx);

    std::string targetTriple = sys::getDefaultTargetTriple();
    std::cout << "Target triple: " << targetTriple << std::endl;
    module->setTargetTriple(targetTriple);

    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    std::string error;
    const Target *target = TargetRegistry::lookupTarget(targetTriple, error);

    if (!target) {
        std::cerr << error << std::endl;
        return;
    }
    std::string cpu = "generic";
    std::string features;

    TargetOptions opt;
    Optional<Reloc::Model> rm = Optional<Reloc::Model>();
    TargetMachine *targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

    module->setDataLayout(targetMachine->createDataLayout());*/
}

int main(int argc, char *argv[]) {
    compilerTest();

    return 0;

    if (argc < 2) {
        std::cout << "No source files specified." << std::endl;
        return 0;
    }

    std::ifstream sourceFile(argv[1]);

    if (sourceFile.is_open()) {
        std::string sourceString;
        sourceFile.seekg(0, std::ios::end);
        sourceString.reserve(sourceFile.tellg());
        sourceFile.seekg(0, std::ios::beg);
        sourceString.assign(std::istreambuf_iterator<char>(sourceFile), std::istreambuf_iterator<char>());

        Parser parser(Tokeniser(std::string(argv[1]), sourceString));

        std::unique_ptr<ParseTree> tree = std::make_unique<ParseTree>();
        tree = parser.parse(std::move(tree));

        if (!tree) {
            return 0;
        }

        TypeChecker typeChecker(tree);

        if (typeChecker.typeCheck()) {
            std::cout << "Success!" << std::endl;
        }
    } else {
        std::cout << "Failed to open source file: '" << argv[1] << "'." << std::endl;
    }

    return 0;
}
