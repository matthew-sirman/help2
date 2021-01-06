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
    struct Module {
        std::filesystem::path filePath;
        std::string moduleName;
    };

    FileStructure() = default;

    void addIncludeDir(const std::filesystem::path& dir);

    void addExtension(std::string ext);

    void addDefaultExtensions();

    std::optional<std::filesystem::path> searchForFile(const std::filesystem::path &file) const;

    std::optional<std::filesystem::path> searchForQualifiedFile(const std::filesystem::path &path) const;

    std::size_t allocateFileIndex(std::filesystem::path file);

    const std::filesystem::path &getFileName(std::size_t index) const;

    void setModuleName(size_t index, std::string name);

    const std::string &getModuleName(std::size_t index) const;

private:
    std::unordered_set<std::string> includeDirs;
    std::unordered_set<std::string> extensions;
    std::vector<Module> modules;
};


#endif //HELPC_FILESTRUCTURE_H
