//
// Created by matthew on 26/11/2020.
//

#ifndef HELP2_PATTERN_H
#define HELP2_PATTERN_H

#include "Expression.h"

class Pattern {
public:
    virtual bool patternMatch(const Value &value) = 0;
};

class ParametricPattern : public Pattern {
public:
    bool patternMatch(const Value &value) override;

private:
    std::shared_ptr<Constructor> patternConstructor;
    std::vector<std::unique_ptr<Pattern>> parameters;
};

class PolymorphicPattern : public Pattern {
public:
    inline bool patternMatch(const Value &value) override { return true; }

private:
    std::string binder;
};


#endif //HELP2_PATTERN_H
