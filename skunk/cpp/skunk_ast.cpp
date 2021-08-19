#include "skunk_ast.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Verifier.h"

#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"

namespace skunk::ast {

CodegenState::CodegenState(llvm::TargetMachine* targetMachine, std::string targetTriple) 
    : builder(context), module(nullptr), targetMachine(targetMachine), targetTriple(targetTriple), functionPassManager(nullptr) 
  { 

  }

llvm::Module* CodegenState::useModule(std::string moduleName) {
  module = std::make_unique<llvm::Module>(moduleName, context);
  module->setDataLayout(targetMachine->createDataLayout());
  module->setTargetTriple(targetTriple);
  initFunctionPassManager();
  return module.get();
}

void CodegenState::initFunctionPassManager() {
  functionPassManager = std::make_unique<llvm::legacy::FunctionPassManager>(module.get());

  functionPassManager->add(llvm::createPromoteMemoryToRegisterPass());
  functionPassManager->add(llvm::createInstructionCombiningPass());
  functionPassManager->add(llvm::createReassociatePass());
  functionPassManager->add(llvm::createGVNPass());
  functionPassManager->add(llvm::createCFGSimplificationPass());

  functionPassManager->doInitialization();
}

Module::Module(std::string name, std::vector<Handle> handles, std::vector<Listener> listeners) 
    : name(std::move(name)), handles(std::move(handles)), listeners(std::move(listeners)) { }

llvm::StructType* Module::irType(CodegenState& state) {
  std::vector<llvm::Type*> handleMembers(handles.size() * 2 + 1);
  for (unsigned i = 0; i < handles.size(); i++) {
    handleMembers[2 * i] = handles[i].irType(state);
    handleMembers[2 * i + 1] = handles[i].irType(state);
  }
  // for now, use a single 64-bit integer to represent all the fields. Might need to change
  // to an array at some point.
  handleMembers[handles.size() * 2] = llvm::IntegerType::get(state.context, 64);

  llvm::StructType *stateTy = llvm::StructType::get(state.context, handleMembers, false);
  return stateTy;
}

bool Module::codegen(CodegenState& state) {
  state.useModule(name);

  // Compute a trigger mask that we can use to determine whether a given update requires
  // invoking one of the listeners on this module.
  for (unsigned i = 0; i < listeners.size(); i++) {
    auto idx = idxForField(listeners[i].handle);
    triggerMask |= (1 << idx);
  }

  moduleType = irType(state);
  std::string stateName = "__s_" + name;
  std::vector<llvm::Constant*> arrayState(handles.size() * 2 + 1, llvm::ConstantInt::get(state.context, llvm::APInt(64, 0, false)));
  llvm::Constant* moduleConstant = llvm::ConstantStruct::get(moduleType, arrayState);
  moduleState = new llvm::GlobalVariable(*(state.module), moduleType, false, llvm::GlobalValue::ExternalLinkage, moduleConstant, stateName);
  for (unsigned i = 0; i < listeners.size(); i++) {
    if (!listeners[i].codegen(state, this)) {
      return false;
    }
  }

  return generateUpdateFunction(state);
}

bool Module::generateUpdateFunction(CodegenState& state) {
  llvm::FunctionType* type = irListenerType(state);
  std::string updateFuncName = name + "_update";
  llvm::Function* func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, updateFuncName, state.module.get());

  llvm::BasicBlock* entry = llvm::BasicBlock::Create(state.context, "entry", func);
  state.builder.SetInsertPoint(entry);

  // save the state, then extract the state ptr.
  llvm::PointerType* inputType = llvm::PointerType::getUnqual(irType(state));
  auto stackArg = state.builder.CreateAlloca(inputType, 0, "state");
  state.builder.CreateStore(func->getArg(0), stackArg);
  llvm::Value* statePtr = state.builder.CreateLoad(stackArg, "state-ptr");

  // extract the handle update state bitfield
  llvm::Value* bitfieldPtr = state.builder.CreateStructGEP(moduleType, statePtr, handles.size() * 2, "bitfield_ptr");
  llvm::Value* bitfield = state.builder.CreateLoad(bitfieldPtr, "bitfield");

  for (unsigned i = 0; i < handles.size(); i++) {
    // find all listeners that listen to the current handle
    std::vector<Listener> listenersForCurrentHandle;
    for (unsigned j = 0; j < listeners.size(); j++) {
      auto idx = idxForField(listeners[j].handle);
      if (idx == i) {
        listenersForCurrentHandle.push_back(listeners[j]);
      }
    }

    // Insert a test for whether this listener should be activated into the current block.
    llvm::Value* test = llvm::ConstantInt::get(state.context, llvm::APInt(64, 1<<i, false));
    llvm::Value* result = state.builder.CreateAnd(bitfield, test, "bitfield_test");
    llvm::Value* comp = state.builder.CreateICmpNE(result, llvm::ConstantInt::get(state.context, llvm::APInt(64, 0, false)), "has_listener");

    std::string blockName = "activateFor_" + handles[i].name;
    llvm::BasicBlock* activateListeners = llvm::BasicBlock::Create(state.context, blockName, func);

    std::string afterName = "after_" + handles[i].name;
    llvm::BasicBlock* afterListeners = llvm::BasicBlock::Create(state.context, afterName, func);

    // Jump over the listeners block if this handle has no update flag.
    state.builder.CreateCondBr(comp, activateListeners, afterListeners);

    state.builder.SetInsertPoint(activateListeners);

    // apply update and clear flag
    llvm::Value* writePtr = readPointerForField(state, statePtr, handles[i].name);
    llvm::Value* updatePtr = updatePointerForField(state, statePtr, handles[i].name, false);
    llvm::Value* update = state.builder.CreateLoad(updatePtr);
    state.builder.CreateStore(update, writePtr);
    state.builder.CreateStore(llvm::ConstantInt::get(state.context, llvm::APInt(64, 0, false)), updatePtr);

    // call any relevant listeners
    for (unsigned j = 0; j < listenersForCurrentHandle.size(); j++) {
      std::string functionName = listenersForCurrentHandle[j].functionName(this);
      llvm::Function* f = state.module->getFunction(functionName);
      state.builder.CreateCall(f, statePtr);
    }
    state.builder.CreateBr(afterListeners);

    state.builder.SetInsertPoint(afterListeners);
  }
  state.builder.CreateRetVoid();
  llvm::verifyFunction(*func);
  
  state.functionPassManager->run(*func);
  return true;
}

unsigned Module::idxForField(std::string name) {
  unsigned idx = 0;
  for (; idx < handles.size(); idx++) {
    if (name == handles[idx].name)
      return idx;
  }
  return -1;
}

llvm::Value* Module::updatePointerForField(CodegenState& state, llvm::Value* ptr, std::string name, bool forWriting) {
  auto idx = idxForField(name);
  if (idx == -1)
    return nullptr;
  assert(moduleState != nullptr);
  
  // Also set/clear the update bitfield to reflect a written new update or stored value.
  llvm::Value* bitfieldPtr = state.builder.CreateStructGEP(moduleType, ptr, handles.size() * 2, "bitfield_ptr");
  llvm::Value* bitfield = state.builder.CreateLoad(bitfieldPtr, "bitfield");
  llvm::Value* newBitfield = nullptr;
  if (forWriting) {
    llvm::Value* newWrite = llvm::ConstantInt::get(state.context, llvm::APInt(64, 1<<idx, false));
    newBitfield = state.builder.CreateOr(bitfield, newWrite);
  } else {
    llvm::Value* newClear = llvm::ConstantInt::get(state.context, llvm::APInt(64, ~(1LL<<idx), false));
    newBitfield = state.builder.CreateAnd(bitfield, newClear);
  }
  state.builder.CreateStore(newBitfield, bitfieldPtr);

  std::string ptrName = "upd_ptr_to_" + name;
  return state.builder.CreateStructGEP(moduleType, ptr, idx * 2 + 1, ptrName);
}

llvm::Value* Module::readPointerForField(CodegenState& state, llvm::Value* ptr, std::string name) {
  auto idx = idxForField(name);
  if (idx == -1)
    return nullptr;
  assert(moduleState != nullptr);
  std::string ptrName = "rd_ptr_to_" + name;
  return state.builder.CreateStructGEP(moduleType, ptr, idx * 2, ptrName);
}

llvm::FunctionType* Module::irListenerType(CodegenState& state) {
  std::vector<llvm::Type*> stateVector(1);
  stateVector[0] = llvm::PointerType::getUnqual(irType(state));
  return llvm::FunctionType::get(llvm::Type::getVoidTy(state.context), stateVector, false);
}

Handle::Handle(std::string name, Type* type, std::vector<Usage> usages) : name(std::move(name)), type(type), usages(std::move(usages)) { }

llvm::Type* Handle::irType(CodegenState& state) {
  return llvm::IntegerType::get(state.context, 64);
}

PrimitiveType::PrimitiveType(PrimitiveType::Name name) : name(name) { }

StateReference::StateReference(std::string name) : name(std::move(name)) { }

llvm::Value* StateReference::codegen(CodegenState& state, Module* module, llvm::Value* stackState) {
  llvm::Value* statePtr = state.builder.CreateLoad(stackState, "state-ptr");
  llvm::Value* ptr = module->readPointerForField(state, statePtr, name);
  std::string valueName = "value_of_" + name;
  return state.builder.CreateLoad(ptr, valueName);
}


Statement::Statement(StateReference* output, Expression* expression) : output(output), expression(expression) { }

bool Statement::codegen(CodegenState& state, Module* module, llvm::Function* func) {
  llvm::BasicBlock *functionBlock = llvm::BasicBlock::Create(state.context, "entry", func);
  state.builder.SetInsertPoint(functionBlock);

  llvm::PointerType* inputType = llvm::PointerType::getUnqual(module->irType(state));
  auto stackArg = state.builder.CreateAlloca(inputType, 0, "state");
  state.builder.CreateStore(func->getArg(0), stackArg);

  // Extract the return value from the expression and store it in the field referenced by this statement.
  if (llvm::Value* returnVal = expression->codegen(state, module, stackArg)) {
    llvm::Value* statePtr = state.builder.CreateLoad(stackArg, "state-ptr");
    llvm::Value* updatePtr = module->updatePointerForField(state, statePtr, output->name);
    state.builder.CreateStore(returnVal, updatePtr);
    state.builder.CreateRetVoid();
    return true;
  }
  return false;
}

Listener::Listener(std::string handle, std::string kind, Statement* statement) :  handle(std::move(handle)), kind(std::move(kind)), statement(statement) {}

std::string Listener::functionName(Module* module) {
  return module->name + "__" + kind + "__" + handle;
}

llvm::Function* Listener::codegen(CodegenState& state, Module* module) {
  llvm::FunctionType* type = module->irListenerType(state);
  // Note that the listener functions are generated with external linkage right now as the runtime 
  // is in C. This can change once/if module runtimes are internal to the modules.
  std::string listenerName = functionName(module);
  llvm::Function *func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, listenerName, state.module.get());

  if (statement->codegen(state, module, func)) {
    llvm::verifyFunction(*func);
    state.functionPassManager->run(*func);
    return func;
  }
  return nullptr;
}

}