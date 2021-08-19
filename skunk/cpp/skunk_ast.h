#ifndef _SKUNK_AST_H
#define _SKUNK_AST_H

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Target/TargetMachine.h"

#include <string>
#include <vector>

namespace skunk::ast {

class Module;

class CodegenState {
public:
  CodegenState(llvm::TargetMachine* targetMachine, std::string targetTriple);
  void initFunctionPassManager();
  llvm::LLVMContext context;
  llvm::IRBuilder<> builder;
  std::unique_ptr<llvm::Module> module;
  llvm::TargetMachine* targetMachine;
  llvm::Module* useModule(std::string name);
  std::string targetTriple;
  std::unique_ptr<llvm::legacy::FunctionPassManager> functionPassManager;
};

class Type {
public:
  virtual ~Type() {}
};

class PrimitiveType : public Type {
public:
  enum Name {
    intType, stringType
  };

  PrimitiveType(Name name);
  Name name;

  virtual ~PrimitiveType() {}
};

enum Usage {
  reads, writes
};

class Handle {
public:
  Handle(std::string name, Type* type, std::vector<Usage> usages);
  std::string name;
  Type* type;
  std::vector<Usage> usages;
  llvm::Type* irType(CodegenState& state);
};

class Expression {
public:
  virtual llvm::Value* codegen(CodegenState& state, Module* module, llvm::Value* stackState) = 0;
};

class StateReference : public Expression {
public:
  StateReference(std::string name);
  std::string name;
  llvm::Value* codegen(CodegenState& state, Module* module, llvm::Value* stackState) override;
};

class Statement {
public:
  Statement(StateReference* output, Expression* expression);
  StateReference* output;
  Expression* expression;
  bool codegen(CodegenState& state, Module* module, llvm::Function* func);
};

class Listener {
public:
   Listener(std::string handle, std::string kind, Statement* statement);
  std::string handle;
  std::string kind;
  Statement* statement;
  llvm::Function* codegen(CodegenState& state, Module* module);
  std::string functionName(Module* module);
};

class Module {
public:
  Module(std::string name, std::vector<Handle> handles, std::vector<Listener> listeners);
  std::string name;
  std::vector<Handle> handles;
  std::vector<Listener> listeners;

  bool codegen(CodegenState& state);
  llvm::StructType* irType(CodegenState& state);
  llvm::FunctionType* irListenerType(CodegenState& state);
  llvm::Value* updatePointerForField(CodegenState& state, llvm::Value* ptr, std::string field, bool forWriting = true);
  llvm::Value* readPointerForField(CodegenState& state, llvm::Value* ptr, std::string name);

  uint64_t triggerMask = 0LL;

private:
  llvm::StructType* moduleType = nullptr;
  llvm::GlobalVariable* moduleState = nullptr;
  unsigned idxForField(std::string field);
  bool generateUpdateFunction(CodegenState& state);
};

}

#endif