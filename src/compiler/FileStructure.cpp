//
// Created by matthew on 02/01/2021.
//

#include "../../include/compiler/FileStructure.h"

void FileStructure::addIncludeDir(const std::filesystem::path &dir) {
    includeDirs.insert(dir.generic_string());
}

void FileStructure::addExtension(std::string ext) {
    extensions.insert(std::move(ext));
}

void FileStructure::addDefaultExtensions() {
    extensions.insert("help");
    extensions.insert("hh");
}

std::optional<std::filesystem::path> FileStructure::searchForFile(const std::filesystem::path &file) const {
    // Check each include directory
    for (const std::string &include : includeDirs) {
        std::filesystem::path filePath = include / file;
        // Check for each possible extension
        for (const std::string &ext : extensions) {
            filePath.replace_extension(ext);
            if (std::filesystem::is_regular_file(filePath)) {
                return filePath;
            }
        }
    }
    return std::nullopt;
}

std::optional<std::filesystem::path> FileStructure::searchForQualifiedFile(const std::filesystem::path &path) const {
    // First check for the absolute file path
    if (std::filesystem::is_regular_file(path)) {
        return path;
    }
    // Otherwise check in each include directory
    for (const std::string &include : includeDirs) {
        std::filesystem::path filePath = include / path;
        if (std::filesystem::is_regular_file(filePath)) {
            return filePath;
        }
    }
    return std::nullopt;
}

std::size_t FileStructure::allocateFileIndex(std::filesystem::path file) {
    // Add the file to the vector and return the size - 1 (the last index of the file)
    programFiles.push_back(std::move(file));
    return programFiles.size() - 1;
}

const std::filesystem::path &FileStructure::getFileName(std::size_t index) const {
    return programFiles[index];
}
