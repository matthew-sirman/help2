//
// Created by matthew on 26/11/2020.
//

#include "../../include/lang/Pattern.h"

bool ParametricPattern::patternMatch(const Value &value) {
    if (*patternConstructor != *value.constructor()) {
        return false;
    }
    for (size_t i = 0; i < parameters.size(); i++) {
        if (!parameters[i]->patternMatch(value.parameter(i))) {
            return false;
        }
    }
    return true;
}
