#pragma clang diagnostic push
#pragma ide diagnostic ignored "bugprone-reserved-identifier"

#include <iostream>

#include "include/parser/Parser.h"
#include "include/compiler/TypeChecker.h"
#include "include/compiler/Compiler.h"
#include "include/compiler/Options.h"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        std::cout << "No source files specified." << std::endl;
        return 0;
    }

    Options options;
    options.parseArguments(argc, argv);

    for (const std::filesystem::path &sourceFile : options.sourceFiles()) {
        Parser parser(options, sourceFile);

        ParseTree tree(Core::create());
        parser.parse(tree);

        if (parser.hasErrors()) {
            std::cerr << "Error parsing " << sourceFile << ":" << std::endl;
            std::cerr << parser.errors();
            return 0;
        }
    }

    return 0;

    /*
    TypeChecker typeChecker(tree);

    if (typeChecker.typeCheck()) {
        std::cout << "Success!" << std::endl;
    }

    Compiler compiler(tree);

    compiler.compile();

    return 0;
     */
}

#pragma clang diagnostic pop