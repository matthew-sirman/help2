//
// Created by matthew on 03/01/2021.
//

#ifndef HELPC_OPTIONS_H
#define HELPC_OPTIONS_H

#include <vector>

#include "FileStructure.h"

class Options {
public:
    Options() = default;

    void parseArguments(int argc, char *argv[]);

    constexpr bool hasError() const { return errorFlag; }

    constexpr FileStructure &fileStructure() { return fileStruct; }

    constexpr const std::vector<std::filesystem::path> &sourceFiles() const { return fileSet; }

private:
    FileStructure fileStruct;
    std::vector<std::filesystem::path> fileSet;

    bool errorFlag = false;

    static const std::string includeFlag;
};


#endif //HELPC_OPTIONS_H
