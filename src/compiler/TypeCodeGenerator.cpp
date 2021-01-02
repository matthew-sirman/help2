//
// Created by matthew on 24/12/2020.
//

#include "../../include/compiler/CodeGenerator.h"
#include "../../include/compiler/Compiler.h"

template<>
llvm::Type *TypeCodeGenerator::generate<DataConstructorASTNode>(const DataConstructorASTNode::View &nodeView) {
    // Note: Constructors should not return types (as they are technically not types in the language!) so we
    // return nullptrs as a way of detecting errors

    std::vector<llvm::Type *> fields;
    llvm::StructType *cons;
    // If the tagSize is 0, this is an untagged constructor so we don't generate a tag field
    // Also, if the tagSize is 0, the struct already exists, otherwise it needs to be generated
    // Additionally, in the case where there are no parameters in this constructor, we still
    // want to generate a constructor function, so we again need a reference to the cons type
    if (nodeView.tagSize != 0 && !nodeView.params.empty()) {
        fields.push_back(llvm::IntegerType::get(*context.context(), nodeView.tagSize));
        // The name of the constructor type will be the unique type name, followed by a dash, followed by the
        // constructor name to uniquely identify it
        std::string consName = nodeView.uniqueTypeName + "-" + nodeView.constructorName;
        cons = llvm::StructType::create(*context.context(), consName);
        context.addInstantiatedType(consName, cons);
    } else {
        cons = reinterpret_cast<llvm::StructType *>(context.lookupType(nodeView.uniqueTypeName));
    }

    context.addConstructorType(nodeView.constructorName, cons, nodeView.tagSize != 0);

    // Next, we generate a new field for each of the parameters
    for (const TypeInstanceASTNode *paramType : nodeView.params) {
        // Here, we can just instantiate the new type. One possible issue would be with recursive infinite
        // descent, however we add a new root type to the map as soon as it is created, so the second time
        // we recurse into the generator for the same type, it will just return the pointer to the original.
        llvm::Type *t = paramType->instantiate(*this, nodeView.bindingMap);
        // If the type is to a structure (i.e. another linked data structure) we actually want
        // to add a pointer rather than the type itself
        // TODO: Check this for function pointers too - do they need to be pointers to function types or
        // just function types?
        if (t->isStructTy()) {
            t = llvm::PointerType::get(t, 0);
        }
        fields.push_back(t);
    }

    // Don't change the structure if this is a constructor with no fields
    if (!nodeView.params.empty()) {
        cons->setBody(fields);
    }

    if (nodeView.tagSize == 0) {
        generateConstructorFunction(nodeView, cons, cons->elements());
    } else {
        generateConstructorFunction(nodeView, cons,
                                    std::vector<llvm::Type *>(cons->element_begin() + 1, cons->element_end()));
    }

    return nullptr;
}

void TypeCodeGenerator::generateConstructorFunction(const DataConstructorASTNode::View &nodeView,
                                                    llvm::StructType *consType,
                                                    const std::vector<llvm::Type *> &fields) {
    // TODO: Handle constructors containing closures

    // Create function type and declaration
    llvm::FunctionType *consFunctionType = llvm::FunctionType::get(
            llvm::PointerType::get(consType, 0), fields, false
    );
    llvm::Function *consFunction = llvm::Function::Create(
            consFunctionType, llvm::Function::ExternalLinkage,
            nodeView.constructorName,
            context.currentModule().get()
    );

    // Create entry point to constructor
    llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(*context.context(), "entry", consFunction);

    // Save the builder insert point
    llvm::IRBuilderBase::InsertPoint startIP = context.builder()->saveIP();

    // Set the builder to insert into this function
    context.builder()->SetInsertPoint(entryBlock);

    // Get the size to malloc
    llvm::Constant *allocSize = llvm::ConstantExpr::getSizeOf(consType);
    allocSize = llvm::ConstantExpr::getTruncOrBitCast(allocSize, context.int64Type());
    // Create the malloc instruction - this can't be done in the builder, so we must add the instruction
    // manually
    llvm::Instruction *consPtr = llvm::CallInst::CreateMalloc(
            context.builder()->GetInsertBlock(), context.int64Type(),
            consType, allocSize, nullptr, nullptr, "consmalloctmp"
    );
    context.builder()->GetInsertBlock()->getInstList().push_back(consPtr);

    bool tagged = nodeView.tagSize != 0;
    llvm::Value *tagPtr = nullptr;
    std::vector<llvm::Value *> fieldPointers;
    fieldPointers.reserve(fields.size());

    // Next we set the tag, if there is one
    if (tagged) {
        tagPtr = context.builder()->CreateStructGEP(consPtr, 0, "tagptrtmp");
    }

    // Now we add instructions for getting the rest of the field pointers
    // Note that we offset by 1 if this is a tagged struct
    for (size_t fieldIndex = tagged; fieldIndex < fields.size() + tagged; fieldIndex++) {
        fieldPointers.push_back(context.builder()->CreateStructGEP(consPtr, fieldIndex, "fieldptrtmp"));
    }

    // Then, we create stores for each of the fields.
    if (tagPtr) {
        context.builder()->CreateStore(
                llvm::ConstantInt::get(llvm::IntegerType::get(*context.context(), nodeView.tagSize), nodeView.tag),
                tagPtr
        );
    }

    for (size_t argIndex = 0; argIndex < fieldPointers.size(); argIndex++) {
        context.builder()->CreateStore(consFunction->getArg(argIndex), fieldPointers[argIndex]);
    }

    // Finally, we return the constructed object
    context.builder()->CreateRet(consPtr);

    // Restore the builder
    context.builder()->restoreIP(startIP);
}

template<>
llvm::Type *TypeCodeGenerator::generate<TypeDeclASTNode>(const TypeDeclASTNode::View &nodeView) {
    if (nodeView.typeConstructorParameters.size() != nodeView.bindings.size()) {
        std::cerr << "DEVELOPER: Attempted to instantiate type with incorrect number of bindings" << std::endl;
        throw;
    }

    // Set the current module to whichever this type is declared it
    context.setCurrentModule(context.parseTree()->getModuleName(nodeView.fileIndex));

    // Construct a map for the bindings which should appear in order
    BindingMap bindingMap;
    for (size_t i = 0; i < nodeView.typeConstructorParameters.size(); i++) {
        bindingMap[nodeView.typeConstructorParameters[i]] = nodeView.bindings[i];
    }

    // First check if we have already generated this type
    std::string uniqueName = generateUniqueTypeName(nodeView.name, nodeView.bindings);
    llvm::Type *existingType = context.lookupType(uniqueName);
    if (existingType) {
        // If the type already exists, return the type
        return existingType;
    }

    // Otherwise, we must generate this type.

    // First, if there are no constructors, we don't generate a type at all
    if (nodeView.constructors.empty()) {
        return nullptr;
    }

    llvm::StructType *type = llvm::StructType::create(*context.context(), uniqueName);
    // Add the type to the context immediately - this allows for recursive types
    context.addInstantiatedType(uniqueName, type);

    // If there is exactly one constructor, we just construct the type - there is no need for a tag
    if (nodeView.constructors.size() == 1) {
        nodeView.constructors.front()->generate(*this, uniqueName, 0, 0, bindingMap);
        return type;
    }

    // Otherwise, the parent type is just a tag value, which can either be a uint8 or uint16 depending on
    // the number of constructors
    unsigned char tagSize;
    if (nodeView.constructors.size() < 256) {
        tagSize = 8;
    } else if (nodeView.constructors.size() < 65536) {
        tagSize = 16;
    } else {
        std::cerr << "DEVELOPER: Detected type with more than 65536 data constructors." << std::endl;
        return nullptr;
    }

    type->setBody(llvm::IntegerType::get(*context.context(), tagSize));

    // Create all the constructors
    unsigned short tag = 0;
    for (const std::unique_ptr<DataConstructorASTNode> &cons : nodeView.constructors) {
        cons->generate(*this, uniqueName, tagSize, tag++, bindingMap);
    }

    return type;
}

template<>
llvm::Type *TypeCodeGenerator::generate<PrefixTypeInstanceASTNode>(const PrefixTypeInstanceASTNode::View &nodeView) {
    // Generate nested dependencies and map into dependency list
    std::vector<llvm::Type *> dependencies;
    dependencies.resize(nodeView.params.size());
    bool constructable = true;
    std::transform(nodeView.params.begin(), nodeView.params.end(), dependencies.begin(),
                   [this, &constructable, &nodeView](const std::unique_ptr<TypeInstanceASTNode> &inst) {
                       llvm::Type *dep = inst->instantiate(*this, nodeView.bindingMap);
                       // Propagate if not constructable
                       if (!dep) {
                           constructable = false;
                       }
                       return dep;
                   });

    // If the type is not constructable then don't attempt generate it
    if (!constructable) {
        return nullptr;
    }

    // Instantiate this type with the new found dependencies
    return dynamic_cast<PrefixTypeDeclASTNode *>(
            context.parseTree()->getTypeByName(nodeView.name).get()
    )->generate(*this, dependencies);
}

template<>
llvm::Type *TypeCodeGenerator::generate<InfixTypeInstanceASTNode>(const InfixTypeInstanceASTNode::View &nodeView) {
    // Generate nested dependencies and store references into dependency pointers
    llvm::Type *lhs = nodeView.lhs->instantiate(*this, nodeView.bindingMap);
    llvm::Type *rhs = nodeView.rhs->instantiate(*this, nodeView.bindingMap);

    // If the type is not constructable then don't attempt to generate it
    if (!lhs || !rhs) {
        return nullptr;
    }

    // Instantiate this type with the new found dependencies
    return dynamic_cast<InfixTypeDeclASTNode *>(
            context.parseTree()->getTypeByName(nodeView.name).get()
    )->generate(*this, lhs, rhs);
}

template<>
llvm::Type *
TypeCodeGenerator::generate<FunctionTypeInstanceASTNode>(const FunctionTypeInstanceASTNode::View &nodeView) {
    // Generate nested dependencies but don't generate any top level type (functions are not constructable)
    llvm::Type *from = nodeView.lhs->instantiate(*this, nodeView.bindingMap);
    llvm::Type *to = nodeView.rhs->instantiate(*this, nodeView.bindingMap);

    // Propagate if non constructable
    if (!from || !to) {
        return nullptr;
    }

    // If "from" is a struct type, we want to use a pointer
    if (from->isStructTy()) {
        from = llvm::PointerType::get(from, 0);
    }

    // If "to" is a function type, then we can collapse the overall function type to have an additional prepending
    // argument
    // Then by induction we know that "to" is already flattened.
    if (to->isFunctionTy()) {
        // Get a reference to the return func type
        llvm::FunctionType *toFType = reinterpret_cast<llvm::FunctionType *>(to);
        // Create a new list of args by prepending from type
        std::vector<llvm::Type *> args;
        args.reserve(toFType->getNumParams() + 1);
        args.push_back(from);
        args.insert(args.end(), toFType->param_begin(), toFType->param_end());

        // Create the new function type and return
        llvm::FunctionType *type = llvm::FunctionType::get(toFType->getReturnType(), args, false);
        return type;
    }
    // Otherwise, we just construct a basic unary function type (base case)
    // If "to" is a struct type, we want to use pointer
    if (to->isStructTy()) {
        to = llvm::PointerType::get(to, 0);
    }
    return llvm::FunctionType::get(to, {from}, false);
}

template<>
llvm::Type *
TypeCodeGenerator::generate<PrimitiveTypeInstanceASTNode>(const PrimitiveTypeInstanceASTNode::View &nodeView) {
    // Just return the primitive's llvm type
    return nodeView.type.llvmType();
}
