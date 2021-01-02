#pragma clang diagnostic push
#pragma ide diagnostic ignored "bugprone-reserved-identifier"

#include <iostream>
#include <fstream>
#include "include/parser/old/Parser.h"
#include "include/compiler/TypeChecker.h"
#include "include/compiler/Compiler.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/Support/MemAlloc.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/Inliner.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

void compilerDataTest() {
    using namespace llvm;

    LLVMContext ctx;

    StructType *llvm_List = StructType::create(ctx, "Int32List");
    llvm_List->setBody(IntegerType::get(ctx, 8));

    FunctionType *fType = FunctionType::get(IntegerType::get(ctx, 32), {IntegerType::get(ctx, 32)}, false);

    StructType *llvm_List_Cons = StructType::create(ctx, "Int32List-Cons");
    llvm_List_Cons->setBody(IntegerType::get(ctx, 8), IntegerType::get(ctx, 32),
                            PointerType::get(llvm_List, 0), PointerType::get(fType, 0));

    outs() << *llvm_List << "\n";
    outs() << *llvm_List_Cons << "\n";

    Module *mod = new Module("switch_test", ctx);

    IRBuilder<> builder(ctx);

    FunctionType *testFT = FunctionType::get(Type::getVoidTy(ctx), {IntegerType::get(ctx, 32)}, false);

    Function *testF = Function::Create(testFT, Function::ExternalLinkage, "func", *mod);
    testF->args().begin()->setName("x");

    BasicBlock *entry = BasicBlock::Create(ctx, "entry", testF);
    builder.SetInsertPoint(entry);

    BasicBlock *c0 = BasicBlock::Create(ctx, "c0", testF);
    BasicBlock *c1 = BasicBlock::Create(ctx, "c1", testF);
    BasicBlock *c2 = BasicBlock::Create(ctx, "c2", testF);

    BasicBlock *dst = BasicBlock::Create(ctx, "dest");

    SwitchInst *sw = builder.CreateSwitch(testF->args().begin(), dst, 3);
    sw->addCase(ConstantInt::get(IntegerType::get(ctx, 32), 1), c0);
    sw->addCase(ConstantInt::get(IntegerType::get(ctx, 32), 15), c1);
    sw->addCase(ConstantInt::get(IntegerType::get(ctx, 32), 15), c2);

    outs() << *mod << "\n";

    /*Module *module = new Module("struct_test", ctx);

    std::string targetTriple = sys::getDefaultTargetTriple();
    std::cout << "Target triple: " << targetTriple << std::endl;
    module->setTargetTriple(targetTriple);

    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    std::string error;
    const Target *target = TargetRegistry::lookupTarget(targetTriple, error);

    if (!target) {
        std::cerr << error << std::endl;
        return;
    }
    std::string cpu = "generic";
    std::string features;

    TargetOptions opt;
    Optional<Reloc::Model> rm = Optional<Reloc::Model>();
    TargetMachine *targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

    module->setDataLayout(targetMachine->createDataLayout());*/
}

void compilerFunctionTest() {
    using namespace llvm;

    LLVMContext ctx;
    Module *mod = new Module("func_test", ctx);
    IRBuilder<> builder(ctx);

    /*
     * Application test
     * Consider the following:
     *
     * func const: Int -> Int -> Int;
     * const x y => x;
     *
     * func test: Int -> Int;
     * test x => const x x;
     *
     * Expected IR code: (roughly)
     *
     *  define i64 @const(i64 %p0, i64 %p1) {
     *  entry:
     *      ret i64 %p0
     *  }
     *
     *  define i64 @test(i64 %p0) {
     *  entry:
     *      %tmp = call i64 @const(i64 %p0, i64 %p0)
     *      return i64 %tmp
     *  }
     */

    /*
    FunctionType *constType = FunctionType::get(IntegerType::getInt64Ty(ctx), {
            IntegerType::getInt64Ty(ctx), IntegerType::getInt64Ty(ctx)
    }, false);

    Function *constFn = Function::Create(constType, Function::ExternalLinkage, "const", mod);
    size_t argID = 0;
    std::for_each(constFn->arg_begin(), constFn->arg_end(),
                  [&argID](Argument &arg) { arg.setName("p" + std::to_string(argID)); }
    );

    BasicBlock *constEntry = BasicBlock::Create(ctx, "entry", constFn);
    builder.SetInsertPoint(constEntry);

    builder.CreateRet(constFn->arg_begin());

    FunctionType *testType = FunctionType::get(IntegerType::getInt64Ty(ctx), {
            IntegerType::getInt64Ty(ctx)
    }, false);

    Function *testFn = Function::Create(testType, Function::ExternalLinkage, "test", mod);
    argID = 0;
    std::for_each(testFn->arg_begin(), testFn->arg_end(),
                  [&argID](Argument &arg) { arg.setName("p" + std::to_string(argID)); }
    );

    BasicBlock *testEntry = BasicBlock::Create(ctx, "entry", testFn);
    builder.SetInsertPoint(testEntry);

    CallInst *call = builder.CreateCall(constFn, {testFn->arg_begin(), testFn->arg_begin()}, "calltmp");
    builder.CreateRet(call);

     */

    // outs() << *mod << "\n";

    // NOTE: For now, only support eager execution
    // TODO: Possibly add lazy execution?

    /*
     *  func range: Int -> (List Int);
     *  range 0 = Nil;
     *  range n = Cons n (range (n - 1));
     *
     *  ; Nil - tag 0
     *  ; Cons - tag 1
     *  declare %Int32List = type { i8 }
     *  declare %Int32List-Cons = type { i8, i32, %Int32List* }
     *
     *  define %Int32List* @range(i32 %n) {
     *  entry:
     *
     *  }
     */

    /*
    // Malloc extern
    FunctionType *mallocType = FunctionType::get(PointerType::getInt8PtrTy(ctx),
                                                 {IntegerType::getInt32Ty(ctx)}, false);

    Function *mallocFn = Function::Create(mallocType, Function::ExternalLinkage, "malloc", mod);
     */

    StructType *int32ListType = StructType::create(ctx, {IntegerType::getInt8Ty(ctx)}, "Int32List");
    StructType *int32ListConsType = StructType::create(ctx, {
            IntegerType::getInt8Ty(ctx),
            IntegerType::getInt32Ty(ctx),
            PointerType::get(int32ListType, 0)
    }, "Int32List-Cons");

    Type *intTy = IntegerType::getInt64Ty(ctx);

    // Constructor for Nil
    FunctionType *nilConsType = FunctionType::get(PointerType::get(int32ListType, 0), false);
    Function *nilConsFn = Function::Create(nilConsType, Function::ExternalLinkage, "Int32List-NilInit", mod);

    BasicBlock *nilConsEntry = BasicBlock::Create(ctx, "entry", nilConsFn);
    builder.SetInsertPoint(nilConsEntry);

    Constant *nilAllocSize = ConstantExpr::getSizeOf(int32ListType);
    nilAllocSize = ConstantExpr::getTruncOrBitCast(nilAllocSize, intTy);
    Instruction *nilPtr = CallInst::CreateMalloc(builder.GetInsertBlock(), intTy,
                                                 int32ListType, nilAllocSize,
                                                 nullptr, nullptr, "nilmalloctmp");
    nilConsEntry->getInstList().push_back(nilPtr);
    // CallInst *nilConsMalloc = builder.CreateCall(mallocFn, { ConstantInt::get(IntegerType::getInt32Ty(ctx), 1) }, "nilmalloctmp");
    // Value *nilPtr = builder.CreateBitCast(nilConsMalloc->getCalledValue(), PointerType::get(int32ListType, 0), "niltmp");
    Value *nilTagPtr = builder.CreateStructGEP(nilPtr, 0, "niltagtmp");
    builder.CreateStore(ConstantInt::get(IntegerType::getInt8Ty(ctx), 0), nilTagPtr);
    builder.CreateRet(nilPtr);

    verifyFunction(*nilConsFn, &errs());

    // Constructor for Cons
    FunctionType *consConsType = FunctionType::get(PointerType::get(int32ListConsType, 0), {
            IntegerType::getInt32Ty(ctx),
            PointerType::get(int32ListType, 0)
    }, false);
    Function *consConsFn = Function::Create(consConsType, Function::ExternalLinkage, "Int32List-ConsInit", mod);
    consConsFn->getArg(0)->setName("head");
    consConsFn->getArg(1)->setName("tail");

    BasicBlock *consConsEntry = BasicBlock::Create(ctx, "entry", consConsFn);
    builder.SetInsertPoint(consConsEntry);

    Constant *consAllocSize = ConstantExpr::getSizeOf(int32ListConsType);
    consAllocSize = ConstantExpr::getTruncOrBitCast(consAllocSize, intTy);
    Instruction *consPtr = CallInst::CreateMalloc(builder.GetInsertBlock(), intTy, int32ListConsType, consAllocSize,
                                                  nullptr, nullptr, "constmp");
    consConsEntry->getInstList().push_back(consPtr);
    // CallInst *consConsMalloc = builder.CreateCall(mallocFn, {ConstantInt::get(IntegerType::getInt32Ty(ctx), 16)}, "consmalloctmp");
    // Value *consPtr = builder.CreateBitCast(consConsMalloc->getCalledValue(), PointerType::get(int32ListConsType, 0), "constmp");
    // TODO: Look into index list?
    Value *consTagPtr = builder.CreateStructGEP(consPtr, 0, "constagtmp");
    Value *consHeadPtr = builder.CreateStructGEP(consPtr, 1, "consheadptr");
    Value *consTailPtr = builder.CreateStructGEP(consPtr, 2, "constailptr");
    builder.CreateStore(ConstantInt::get(IntegerType::getInt8Ty(ctx), 1), consTagPtr);
    builder.CreateStore(consConsFn->getArg(0), consHeadPtr);
    builder.CreateStore(consConsFn->getArg(1), consTailPtr);
    builder.CreateRet(consPtr);

    verifyFunction(*consConsFn, &errs());

    FunctionType *rangeType = FunctionType::get(PointerType::get(int32ListType, 0), {IntegerType::getInt32Ty(ctx)},
                                                false);

    Function *rangeFn = Function::Create(rangeType, Function::ExternalLinkage, "range", mod);
    rangeFn->getArg(0)->setName("n");

    BasicBlock *rangeEntry = BasicBlock::Create(ctx, "entry", rangeFn);
    builder.SetInsertPoint(rangeEntry);

    BasicBlock *c0 = BasicBlock::Create(ctx, "c0", rangeFn);
    BasicBlock *cDef = BasicBlock::Create(ctx, "default", rangeFn);

    SwitchInst *varSwitch = builder.CreateSwitch(rangeFn->arg_begin(), cDef, 1);
    varSwitch->addCase(ConstantInt::get(IntegerType::getInt32Ty(ctx), 0), c0);


    // 0 case
    builder.SetInsertPoint(c0);

    CallInst *nilVal = builder.CreateCall(nilConsFn, {}, "niltmp");
    builder.CreateRet(nilVal);

    // n case
    builder.SetInsertPoint(cDef);

    Value *sub = builder.CreateSub(rangeFn->getArg(0), ConstantInt::get(IntegerType::getInt32Ty(ctx), 1), "subtmp");
    CallInst *tailVal = builder.CreateCall(rangeFn, {sub}, "tailtmp");
    CallInst *consVal = builder.CreateCall(consConsFn, {rangeFn->getArg(0), tailVal}, "constmp");
    Value *castVal = builder.CreateBitCast(consVal, PointerType::get(int32ListType, 0), "casttmp");
    builder.CreateRet(castVal);

    verifyFunction(*rangeFn, &errs());

    outs() << *mod << "\n";

    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    std::string targetTriple = sys::getDefaultTargetTriple();

    std::string error;
    const Target *target = TargetRegistry::lookupTarget(targetTriple, error);

    if (!target) {
        std::cerr << error << std::endl;
        return;
    }

    std::string cpu = "generic";
    std::string features;

    TargetOptions opt;
    Optional<Reloc::Model> rm;
    TargetMachine *targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

    mod->setDataLayout(targetMachine->createDataLayout());
    mod->setTargetTriple(targetTriple);

    std::string objFilename = "../test/func_test.o";
    std::error_code ec;
    raw_fd_ostream dest(objFilename, ec, sys::fs::OF_None);

    if (ec) {
        std::cerr << "Could not open file: " << ec.message() << std::endl;
        return;
    }

    legacy::PassManager pass;
    CodeGenFileType fileType = CGFT_ObjectFile;

    if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType)) {
        std::cerr << "Target machine count not emit a file of this type." << std::endl;
        return;
    }

    pass.run(*mod);
    dest.flush();

    std::cout << "Compiled to " << objFilename << "." << std::endl;
}

void optimisationTest() {
    using namespace llvm;

    LLVMContext ctx;
    Module *mod = new Module("func_test", ctx);
    IRBuilder<> builder(ctx);

    legacy::PassManager mpm;

    PassManagerBuilder pmb;
    pmb.populateModulePassManager(mpm);

    mpm.add(createFunctionInliningPass());

    FunctionType *addFnType = FunctionType::get(IntegerType::getInt64Ty(ctx), {
            IntegerType::getInt64Ty(ctx),
            IntegerType::getInt64Ty(ctx)
    }, false);

    Function *addFn = Function::Create(addFnType, Function::InternalLinkage, "add", mod);
    // addFn->addFnAttr(Attribute::AlwaysInline);
    BasicBlock *addEntry = BasicBlock::Create(ctx, "entry", addFn);
    builder.SetInsertPoint(addEntry);

    Value *sum = builder.CreateAdd(addFn->getArg(0), addFn->getArg(1), "addtmp");
    builder.CreateRet(sum);

    // verifyFunction(*addFn, &errs());

    FunctionType *sum3FnType = FunctionType::get(IntegerType::getInt64Ty(ctx), {
            IntegerType::getInt64Ty(ctx),
            IntegerType::getInt64Ty(ctx),
            IntegerType::getInt64Ty(ctx)
    }, false);

    Function *sum3Fn = Function::Create(sum3FnType, Function::ExternalLinkage, "sum3", mod);
    BasicBlock *sum3Entry = BasicBlock::Create(ctx, "entry", sum3Fn);
    builder.SetInsertPoint(sum3Entry);

    CallInst *sum0 = builder.CreateCall(addFn, {sum3Fn->getArg(0), sum3Fn->getArg(1)}, "s0");
    CallInst *sum1 = builder.CreateCall(addFn, {sum0, sum3Fn->getArg(2)}, "s1");
    builder.CreateRet(sum1);

    verifyFunction(*sum3Fn, &errs());

    mpm.run(*mod);

    outs() << *mod << "\n";
}

llvm::Module *closureTest(llvm::LLVMContext &ctx) {
    using namespace llvm;

    Module *mod = new Module("closure_test", ctx);
    IRBuilder<> builder(ctx);

    /*
     *  func apply: (Int -> Int) -> Int -> Int;
     *  apply f x => f x;
     *
     *  func add: Int -> Int -> Int;
     *  sum3 x y => x + y;
     *
     *  func someFunc: Int -> Int;
     *  someFunc x = x + 1;
     *
     *  func partial: Int -> Int;
     *  partial x = apply (add 1) x;
     *
     *  define i64 @someFunc(i64 %0) {
     *  entry:
     *      %addtmp = add i64 %0, i64 1
     *      ret i64 %addtmp
     *  }
     *
     *  define i64 @add(i64 %0, i64 %1) {
     *  entry:
     *      %addtmp = add i64 %0, %1
     *      ret i64 %addtmp
     *  }
     *
     *  ; Closure "base"
     *  %__closure_i64_i64 = type { i64 (%__closure_i64_i64*, i64) }
     *
     *  ; someFunc closure instance
     *  %__closure_i64_i64_someFunc { i64 (%__closure_i64_i64*, i64) }
     *
     *  define i64 @__closure_i64_i64_someFunc_call(%__closure_i64_i64* %0, i64 %1) {
     *  entry:
     *      ; here we actually just ignore the closure - there are no bindings
     *      %calltmp = call i64 @someFunc(i64 %1)
     *      ret i64 %calltmp
     *  }
     *
     *  define %__closure_i64_i64* @__closure_i64_i64_someFunc_create() {
     *  entry:
     *      %malloccall = tail call i8* @malloc(...)
     *      %closuretmp = bitcast i8* %malloccall to %__closure_i64_i64_someFunc*
     *      %closurefntmp = getelementptr inbounds %__closure_i64_i64_someFunc, %__closure_i64_i64_someFunc* %closuretmp, i32 0, i32 0
     *      store @__closure_i64_i64_someFunc_call, (...) %closurefntmp
     *      %rawtmp = bitcast i8* %closuretmp to %__closure_i64_i64*
     *      ret %__closure_i64_i64* %rawtmp
     *  }
     *
     *  ; add closure instance
     *  %__closure_i64_i64_add { i64 (%__closure_i64_i64*, i64), i64 }
     *
     *  define i64 @__closure_i64_i64_add_call(%__closure_i64_i64* %0, i64 %1) {
     *  entry:
     *      ; here we do use the closure to get the first binding
     *      %casttmp = bitcast %__closure_i64_i64* %0 to %__closure_i64_i64_add*
     *      ; get the first bound argument
     *      %a0 = getelementptr inbounds %__closure_i64_i64_add, %__closure_i64_i64_add* %casttmp, i32 0, i32 1
     *      ; invoke the function
     *      %calltmp = call i64 @add(i64 %a0, i64 %1)
     *      ret i64 %calltmp
     *  }
     *
     *  define %__closure_i64_i64* @__closure_i64_i64_add_create(i64 %0) {
     *  entry:
     *      %malloccall = tail call i8* @malloc(...)
     *      %closuretmp = bitcast i8* %malloccall to %__closure_i64_i64_add*
     *      %closurefntmp = getelementptr inbounds %__closure_i64_i64_add, %__closure_i64_i64_add* %closuretmp, i32 0, i32 0
     *      %a0ptr = getelementptr inbounds %__closure_i64_i64_add, %__closure_i64_i64_add* %closuretmp, i32 0, i32 1
     *      store @__closure_i64_i64_add_call, (...) %closurefntmp
     *      store i64 %0, i64* %a0ptr
     *      %rawtmp = bitcast i8* %closuretmp to %__closure_i64_i64*
     *      ret %__closure_i64_i64* %rawtmp
     *  }
     *
     *  define i64 @apply(%__closure_i64_i64* %0, i64 %1) {
     *  entry:
     *      %clsfunc = getelementptr inbounds %__closure_i64_i64, %__closure_i64_i64* %0, i32 0, i32 1
     *      %result = call i64 %clsfunc(%__closure_i64_i64* %0, %1)
     *      ret i64 %result
     *  }
     */

    //  func someFunc: Int -> Int;
    //  someFunc x => x + 1;

    IntegerType *i64 = IntegerType::getInt64Ty(ctx);

    FunctionType *someFuncType = FunctionType::get(i64, {i64}, false);
    Function *someFunc = Function::Create(someFuncType, Function::ExternalLinkage, "someFunc", mod);
    BasicBlock *someFuncEntry = BasicBlock::Create(ctx, "entry", someFunc);
    builder.SetInsertPoint(someFuncEntry);
    builder.CreateRet(builder.CreateAdd(someFunc->getArg(0), ConstantInt::get(i64, 1), "addtmp"));

    // func apply: (Int -> Int) -> Int -> Int;
    // apply f x = f x;

    // First, need the base closure
    StructType *__closure_i64_i64 = StructType::create(ctx, "__closure_i64_i64");
    __closure_i64_i64->setBody(
            PointerType::get(FunctionType::get(i64, {
                    PointerType::get(__closure_i64_i64, 0), i64
            }, false), 0)
    );

    // Next, the someFunc closure
    StructType *__closure_i64_i64_someFunc = StructType::create(ctx, "__closure_i64_i64_someFunc");
    __closure_i64_i64_someFunc->setBody(
            PointerType::get(FunctionType::get(i64, {
                    PointerType::get(__closure_i64_i64, 0), i64
            }, false), 0)
    );

    // The call type is independent of the function
    FunctionType *__closure_i64_i64_call_t = FunctionType::get(
            i64,
            {PointerType::get(__closure_i64_i64, 0), i64},
            false
    );

    // Then need the call function
    Function *__closure_i64_i64_someFunc_call = Function::Create(
            __closure_i64_i64_call_t, Function::ExternalLinkage,
            "__closure_i64_i64_someFunc_call", mod
    );
    BasicBlock *someFunc_call_entry = BasicBlock::Create(ctx, "entry", __closure_i64_i64_someFunc_call);
    builder.SetInsertPoint(someFunc_call_entry);
    builder.CreateRet(builder.CreateCall(someFunc, {__closure_i64_i64_someFunc_call->getArg(1)}, "calltmp"));

    // Then need the create function
    FunctionType *__closure_i64_i64_someFunc_create_t = FunctionType::get(
            PointerType::get(__closure_i64_i64, 0),
            {},
            false
    );
    Function *__closure_i64_i64_someFunc_create = Function::Create(
            __closure_i64_i64_someFunc_create_t, Function::ExternalLinkage,
            "__closure_i64_i64_someFunc_create", mod
    );
    BasicBlock *someFunc_create_entry = BasicBlock::Create(ctx, "entry", __closure_i64_i64_someFunc_create);
    builder.SetInsertPoint(someFunc_create_entry);
    Constant *someFunc_allocSize = ConstantExpr::getSizeOf(__closure_i64_i64_someFunc);
    someFunc_allocSize = ConstantExpr::getTruncOrBitCast(someFunc_allocSize, i64);
    Instruction *someFunc_closurePtr = CallInst::CreateMalloc(
            builder.GetInsertBlock(), i64, __closure_i64_i64_someFunc, someFunc_allocSize,
            nullptr, nullptr, "closuretmp"
    );
    builder.GetInsertBlock()->getInstList().push_back(someFunc_closurePtr);

    Value *someFunc_closureFn = builder.CreateStructGEP(someFunc_closurePtr, 0, "clfntmp");
    builder.CreateStore(__closure_i64_i64_someFunc_call, someFunc_closureFn);

    builder.CreateRet(
            builder.CreateBitCast(someFunc_closurePtr, PointerType::get(__closure_i64_i64, 0), "rawtmp")
    );

    // Finally, we are ready to create the apply function
    FunctionType *applyType = FunctionType::get(i64, {PointerType::get(__closure_i64_i64, 0), i64}, false);
    Function *applyFn = Function::Create(applyType, Function::ExternalLinkage, "apply", mod);
    BasicBlock *applyEntry = BasicBlock::Create(ctx, "entry", applyFn);
    builder.SetInsertPoint(applyEntry);
    Value *applyClosureFnPtr = builder.CreateStructGEP(applyFn->getArg(0), 0, "clfnptrtmp");
    Value *applyClosureFn = builder.CreateLoad(applyClosureFnPtr, "clfntmp");
    builder.CreateRet(builder.CreateCall(applyClosureFn, {applyFn->getArg(0), applyFn->getArg(1)}, "clcalltmp"));

    // func add: Int -> Int -> Int;
    // add x y = x + y;
    FunctionType *addType = FunctionType::get(i64, {i64, i64}, false);
    Function *addFn = Function::Create(addType, Function::ExternalLinkage, "add", mod);
    BasicBlock *addEntry = BasicBlock::Create(ctx, "entry", addFn);
    builder.SetInsertPoint(addEntry);
    builder.CreateRet(builder.CreateAdd(addFn->getArg(0), addFn->getArg(1), "addtmp"));

    // Now we try the add closure
    StructType *__closure_i64_i64_add = StructType::create(ctx, "__closure_i64_i64_add");
    __closure_i64_i64_add->setBody(
            PointerType::get(FunctionType::get(i64, {
                    PointerType::get(__closure_i64_i64, 0), i64
            }, false), 0),
            i64
    );
    Function *__closure_i64_i64_add_call = Function::Create(
            __closure_i64_i64_call_t, Function::ExternalLinkage,
            "__closure_i64_i64_add_call", mod
    );
    BasicBlock *add_call_entry = BasicBlock::Create(ctx, "entry", __closure_i64_i64_add_call);
    builder.SetInsertPoint(add_call_entry);
    Value *addClosureCast = builder.CreateBitCast(__closure_i64_i64_add_call->getArg(0),
                                                  PointerType::get(__closure_i64_i64_add, 0), "casttmp");
    Value *addA0Ptr = builder.CreateStructGEP(addClosureCast, 1, "a0ptr");
    Value *addA0 = builder.CreateLoad(addA0Ptr, "a0");
    Value *addCall = builder.CreateCall(addFn, {addA0, __closure_i64_i64_add_call->getArg(1)}, "calltmp");
    builder.CreateRet(addCall);

    // Then need the create function
    FunctionType *__closure_i64_i64_add_create_t = FunctionType::get(
            PointerType::get(__closure_i64_i64, 0),
            {i64},
            false
    );
    Function *__closure_i64_i64_add_create = Function::Create(
            __closure_i64_i64_add_create_t, Function::ExternalLinkage,
            "__closure_i64_i64_add_create", mod
    );
    BasicBlock *add_create_entry = BasicBlock::Create(ctx, "entry", __closure_i64_i64_add_create);
    builder.SetInsertPoint(add_create_entry);
    Constant *addFn_allocSize = ConstantExpr::getSizeOf(__closure_i64_i64_add);
    someFunc_allocSize = ConstantExpr::getTruncOrBitCast(addFn_allocSize, i64);
    Instruction *addFn_closurePtr = CallInst::CreateMalloc(
            builder.GetInsertBlock(), i64, __closure_i64_i64_add, addFn_allocSize,
            nullptr, nullptr, "closuretmp"
    );
    builder.GetInsertBlock()->getInstList().push_back(addFn_closurePtr);

    Value *addFn_closureFn = builder.CreateStructGEP(addFn_closurePtr, 0, "clfntmp");
    Value *addFn_a0 = builder.CreateStructGEP(addFn_closurePtr, 1, "a0ptr");
    builder.CreateStore(__closure_i64_i64_add_call, addFn_closureFn);
    builder.CreateStore(__closure_i64_i64_add_create->getArg(0), addFn_a0);

    builder.CreateRet(
            builder.CreateBitCast(addFn_closurePtr, PointerType::get(__closure_i64_i64, 0), "rawtmp")
    );

    // Now to try using it
    //
    // func partial: Int -> Int;
    // partial x => apply (add 1) x;
    FunctionType *partialFnType = FunctionType::get(i64, {i64}, false);
    Function *partialFn = Function::Create(partialFnType, Function::ExternalLinkage, "partial", mod);
    BasicBlock *partialEntry = BasicBlock::Create(ctx, "entry", partialFn);
    builder.SetInsertPoint(partialEntry);
    Value *addClosure = builder.CreateCall(__closure_i64_i64_add_create, {ConstantInt::get(i64, 1)}, "fcltmp");
    Value *applyCall = builder.CreateCall(applyFn, {addClosure, partialFn->getArg(0)}, "calltmp");
    builder.CreateRet(applyCall);

    // verifyModule(*mod, &errs());

    // outs() << *mod << "\n";

    return mod;
}

void writeOutModule(llvm::Module *mod) {
    llvm::legacy::PassManager pm;
    pm.add(llvm::createFunctionInliningPass());

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string targetTriple = llvm::sys::getDefaultTargetTriple();

    std::string error;
    const llvm::Target *target = llvm::TargetRegistry::lookupTarget(targetTriple, error);

    if (!target) {
        std::cerr << error << std::endl;
        return;
    }

    std::string cpu = "generic";
    std::string features;

    llvm::TargetOptions opt;
    llvm::Optional<llvm::Reloc::Model> rm = llvm::Reloc::Model::PIC_;
    llvm::TargetMachine *targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

//            llvm::outs() << "----- MODULE " << module.moduleName << " -----\n";
//            llvm::outs() << *m << "\n";

    // llvm::verifyModule(*m, &llvm::errs());

    mod->setDataLayout(targetMachine->createDataLayout());
    mod->setTargetTriple(targetTriple);

    std::string objFilename = "output.o";
    std::error_code ec;
    llvm::raw_fd_ostream dest(objFilename, ec, llvm::sys::fs::OF_None);

    if (ec) {
        std::cerr << "Could not open file: " << ec.message() << std::endl;
        return;
    }

    llvm::CodeGenFileType fileType = llvm::CGFT_ObjectFile;

    if (targetMachine->addPassesToEmitFile(pm, dest, nullptr, fileType)) {
        std::cerr << "Target machine count not emit a file of this type." << std::endl;
        return;
    }

    llvm::verifyModule(*mod, &llvm::errs());

    pm.run(*mod);
    dest.flush();
    dest.close();

    llvm::outs() << *mod << "\n";

    std::cout << "Compiled " << objFilename << "." << std::endl;
}

int main(int argc, char *argv[]) {
//    compilerDataTest();
//    return 0;

//    compilerFunctionTest();
//    return 0;

//    optimisationTest();
//    return 0;

//    llvm::LLVMContext ctx;

//    writeOutModule(closureTest(ctx));
//    return 0;

    if (argc < 2) {
        std::cout << "No source files specified." << std::endl;
        return 0;
    }

    std::ifstream sourceFile(argv[1]);

    if (sourceFile.is_open()) {
        std::string sourceString;
        sourceFile.seekg(0, std::ios::end);
        sourceString.reserve(sourceFile.tellg());
        sourceFile.seekg(0, std::ios::beg);
        sourceString.assign(std::istreambuf_iterator<char>(sourceFile), std::istreambuf_iterator<char>());

        Parser parser(Tokeniser(std::string(argv[1]), sourceString));

        std::unique_ptr<ParseTree> tree = std::make_unique<ParseTree>(Core::create());
        tree = parser.parse(std::move(tree));

        if (!tree) {
            return 0;
        }

        TypeChecker typeChecker(tree);

        if (typeChecker.typeCheck()) {
            std::cout << "Success!" << std::endl;
        }

        Compiler compiler(std::move(tree));

        compiler.compile();

        // TEMP CODE

        llvm::legacy::PassManager pm;
        pm.add(llvm::createFunctionInliningPass());

        llvm::InitializeAllTargetInfos();
        llvm::InitializeAllTargets();
        llvm::InitializeAllTargetMCs();
        llvm::InitializeAllAsmParsers();
        llvm::InitializeAllAsmPrinters();

        std::string targetTriple = llvm::sys::getDefaultTargetTriple();

        std::string error;
        const llvm::Target *target = llvm::TargetRegistry::lookupTarget(targetTriple, error);

        if (!target) {
            std::cerr << error << std::endl;
            return 0;
        }

        std::string cpu = "generic";
        std::string features;

        llvm::TargetOptions opt;
        llvm::Optional<llvm::Reloc::Model> rm;
        llvm::TargetMachine *targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

        llvm::outs() << "Generated modules: \n";

        for (const ParseTree::Module &module : compiler.context.parseTree()->allModules()) {
            llvm::Module *m = compiler.context.module(module.moduleName).get();
//            llvm::outs() << "----- MODULE " << module.moduleName << " -----\n";
//            llvm::outs() << *m << "\n";

            // llvm::verifyModule(*m, &llvm::errs());

            m->setDataLayout(targetMachine->createDataLayout());
            m->setTargetTriple(targetTriple);

            std::filesystem::path objFilename = module.fileName;
            objFilename.replace_extension(".o");
            std::error_code ec;
            llvm::raw_fd_ostream dest(objFilename.generic_string(), ec, llvm::sys::fs::OF_None);

            if (ec) {
                std::cerr << "Could not open file: " << ec.message() << std::endl;
                return 0;
            }

            llvm::CodeGenFileType fileType = llvm::CGFT_ObjectFile;

            if (targetMachine->addPassesToEmitFile(pm, dest, nullptr, fileType)) {
                std::cerr << "Target machine count not emit a file of this type." << std::endl;
                return 0;
            }

            llvm::verifyModule(*m, &llvm::errs());

            pm.run(*m);
            dest.flush();
            dest.close();

            llvm::outs() << *m << "\n";

            std::cout << "Compiled " << objFilename << "." << std::endl;
        }

        std::cout << "Done!" << std::endl;
    } else {
        std::cout << "Failed to open source file: '" << argv[1] << "'." << std::endl;
    }

    return 0;
}

#pragma clang diagnostic pop