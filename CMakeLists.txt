cmake_minimum_required(VERSION 3.17)
project(helpc)

set(CMAKE_CXX_STANDARD 20)

find_package(LLVM REQUIRED CONFIG)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_executable(${PROJECT_NAME} main.cpp src/lang/Expression.cpp include/lang/Expression.h src/lang/Type.cpp include/lang/Type.h src/lang/Context.cpp include/lang/Context.h src/parser/Parser.cpp include/parser/Parser.h src/parser/Tokeniser.cpp include/parser/Tokeniser.h src/interpreter/Interpreter.cpp include/interpreter/Interpreter.h src/interpreter/PartialFunctionBinding.cpp include/interpreter/PartialFunctionBinding.h src/lang/Function.cpp include/lang/Function.h src/lang/Pattern.cpp include/lang/Pattern.h include/parser/ParseTree.h src/parser/ast/ExpressionASTNodes.cpp include/parser/ast/ExpressionASTNodes.h src/parser/ast/TypeASTNodes.cpp include/parser/ast/TypeASTNodes.h src/parser/ast/FunctionASTNodes.cpp include/parser/ast/FunctionASTNodes.h src/parser/ParseTree.cpp src/compiler/TypeChecker.cpp include/compiler/TypeChecker.h include/parser/ast/ASTNode.h src/parser/ast/ASTNode.cpp src/compiler/Compiler.cpp include/compiler/Compiler.h include/compiler/CodeGenerator.h include/parser/ast/Viewable.h src/compiler/TypeCodeGenerator.cpp src/compiler/FunctionCodeGenerator.cpp src/compiler/ExpressionCodeGenerator.cpp src/compiler/CodeGenerator.cpp src/lang/core/Core.cpp include/lang/core/Core.h src/lang/core/PrimitiveType.cpp include/lang/core/PrimitiveType.h src/lang/core/CoreBuilder.cpp include/lang/core/CoreBuilder.h src/lang/core/BuiltinFunction.cpp include/lang/core/BuiltinFunction.h src/parser/ast/PatternASTNodes.cpp include/parser/ast/PatternASTNodes.h)

target_link_libraries(${PROJECT_NAME} LLVM-10)