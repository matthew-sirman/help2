//
// Created by matthew on 10/11/2020.
//

#ifndef HELP2_EXPRESSION_H
#define HELP2_EXPRESSION_H

#include <string>
#include <memory>
#include <ostream>

#include "Type.h"

enum class ExpressionType {
    Variable,
    Value,
    Abstraction,
    Application,
    Builtin,
    Function
};

class Expression {
    friend std::ostream &operator<<(std::ostream &os, const Expression &expr);

protected:
    virtual ExpressionType getExpressionType() const = 0;

    std::shared_ptr<TypeInstance> type;
};

class Variable : public Expression {
protected:
    inline ExpressionType getExpressionType() const override { return ExpressionType::Variable; }

private:
    size_t binderIndex;
};

class Pattern;

class Value : public Expression {
public:
    std::shared_ptr<Constructor> constructor() const;

    const Value &parameter(size_t index) const;

protected:
    inline ExpressionType getExpressionType() const override { return ExpressionType::Value; }

private:
    std::shared_ptr<Constructor> dataConstructor;
    std::vector<Value> parameters;
};

class Abstraction : public Expression {
protected:
    inline ExpressionType getExpressionType() const override { return ExpressionType::Abstraction; }

private:
    Variable var;
    std::shared_ptr<Expression> body;
};

class Application : public Expression {
protected:
    inline ExpressionType getExpressionType() const override { return ExpressionType::Application; }

private:
    std::shared_ptr<Expression> func;
    std::shared_ptr<Expression> arg;
};

class Builtin : public Expression {
protected:
    inline ExpressionType getExpressionType() const override { return ExpressionType::Builtin; }

private:

};

#endif //HELP2_EXPRESSION_H
