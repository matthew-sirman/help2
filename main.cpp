#include <iostream>
#include <fstream>
#include "include/parser/Parser.h"

int main(int argc, char *argv[]) {
//    Context ctx;
//    InfixType &functionType = ctx.createInfixType("->", ParameterPlaceholder("a"), ParameterPlaceholder("b"));
//    PrefixType &numberType = ctx.createPrefixType("Num");
//    PrefixType &eitherType = ctx.createPrefixType("Either", {ParameterPlaceholder("a"), ParameterPlaceholder("b")});
//
//    InfixTypeInstance functionInstance(
//            *ctx.getType("->"),
//            InfixTypeInstance(functionType, PrefixTypeInstance(numberType), PolymorphicTypeInstance("b")),
//            InfixTypeInstance(functionType, PolymorphicTypeInstance("a"), PolymorphicTypeInstance("b"))
//    );
//
//    PrefixTypeInstance numberInstance(numberType);
//
//    std::vector<std::unique_ptr<TypeInstance>> params;
//    params.push_back(std::make_unique<InfixTypeInstance>(functionType, PrefixTypeInstance(numberType),
//                                                         PrefixTypeInstance(numberType)));
//    params.push_back(std::make_unique<PrefixTypeInstance>(numberType));
//
//    PrefixTypeInstance eitherInstance(eitherType, std::move(params));
//
//    std::cout << functionInstance << std::endl;
//    std::cout << numberInstance << std::endl;
//    std::cout << eitherInstance << std::endl;
//
//    Context ctx;
//
//    PrefixType &numberType = ctx.createPrefixType("Num");

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

        if (std::unique_ptr<ParseTree> tree = parser.parse()) {
            std::cout << "Success!" << std::endl;
        }

    } else {
        std::cout << "Failed to open source file: '" << argv[1] << "'." << std::endl;
    }

    return 0;
}
