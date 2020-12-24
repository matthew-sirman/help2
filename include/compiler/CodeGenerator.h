//
// Created by matthew on 24/12/2020.
//

#ifndef HELP2_CODEGENERATOR_H
#define HELP2_CODEGENERATOR_H

#include "../parser/ast/Viewable.h"

class CompileContext;
class Compiler;

class CodeGenerator {
    friend class Compiler;
public:
    template<Viewable NodeT>
    void generate(const typename NodeT::View &nodeView) const;

protected:
    CodeGenerator(const CompileContext &context);

private:
    const CompileContext &context;
};


#endif //HELP2_CODEGENERATOR_H
