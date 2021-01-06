//
// Created by matthew on 03/01/2021.
//

#include "../../include/compiler/Options.h"

#include <iostream>

std::string Options::includeFlag;

void Options::parseArguments(int argc, char *argv[]) {
    // Start from the second argument; we don't care about the call to the program
    for (std::size_t i = 1; i < argc; i++) {
        std::string arg(argv[i]);

        // Check for include paths
        if (arg.starts_with(includeFlag)) {
            std::filesystem::path includePath = arg.substr(includeFlag.size());
            if (!std::filesystem::is_directory(includePath)) {
                std::cerr << "Failed to include directory '" << includePath << std::endl;
                errorFlag = true;
            } else {
                fileStruct.addIncludeDir(arg.substr(2));
            }
            continue;
        }

        // Finally, just add it as a source file
        std::filesystem::path sourceFile = arg;
        if (!std::filesystem::is_regular_file(sourceFile)) {
            std::cerr << "Failed to find source file '" << sourceFile << "'." << std::endl;
            errorFlag = true;
        } else {
            fileSet.push_back(sourceFile);
        }
    }

    fileStruct.addDefaultExtensions();
}

void Options::setup() {
    includeFlag = "-I";
}
