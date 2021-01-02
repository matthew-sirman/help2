//
// Created by matthew on 02/01/2021.
//

#ifndef HELPC_FILESTRUCTURE_H
#define HELPC_FILESTRUCTURE_H

#include <string>
#include <filesystem>
#include <unordered_set>
#include <vector>
#include <optional>

class FileStructure {
public:
    void addIncludeDir(std::filesystem::path dir);

    void addExtension(std::string ext);

    void addDefaultExtensions();

    std::optional<std::filesystem::path> searchForFile(const std::filesystem::path &file) const;

    std::optional<std::filesystem::path> searchForQualifiedFile(const std::filesystem::path &path) const;

    std::size_t allocateFileIndex(std::filesystem::path file);

    const std::filesystem::path &getFileName(std::size_t index) const;

private:
    std::unordered_set<std::filesystem::path> includeDirs;
    std::unordered_set<std::string> extensions;
    std::vector<std::filesystem::path> programFiles;
};


#endif //HELPC_FILESTRUCTURE_H
