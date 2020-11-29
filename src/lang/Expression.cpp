//
// Created by matthew on 10/11/2020.
//

#include "../../include/lang/Expression.h"

std::shared_ptr<Constructor> Value::constructor() const {
    return dataConstructor;
}

const Value &Value::parameter(size_t index) const {
    return parameters[index];
}

std::ostream &operator<<(std::ostream &os, const Expression &expr) {
    switch (expr.getExpressionType()) {
        case ExpressionType::Variable:
            break;
        case ExpressionType::Value:
            os << "Value";
            break;
        case ExpressionType::Abstraction:
            os << "Abstraction";
            break;
        case ExpressionType::Application:
            os << "Application";
            break;
        case ExpressionType::Builtin:
            os << "Builtin";
            break;
        case ExpressionType::Function:
            os << "Function";
            break;
    }

    return os;
}