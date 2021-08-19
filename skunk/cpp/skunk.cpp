#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"

#include "parser.h"
#include "skunk_ast.h"

#include <iostream>
#include <fstream>

using namespace llvm;
using namespace llvm::sys;


int main(int argc, char* argv[]) {
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();

  if (argc != 2) {
    outs() << "Usage: skunk input.sk" << "\n";
    return 0;
  }

  auto parseOutput = skunk::parseFile(argv[1]);
  if (parseOutput.error != skunk::OK) {
    errs() << parseOutput.errorMessage << "\n";
    return 1;
  }

  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  InitializeAllAsmPrinters();

  auto TargetTriple = getDefaultTargetTriple();

  outs() << TargetTriple << "\n";

  std::string Error;
  auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

  if (!Target) {
    errs() << Error;
    return 1;
  }

  auto CPU = "generic";
  auto Features = "";

  TargetOptions opt;
  auto RM = Optional<Reloc::Model>();
  auto TheTargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

  skunk::ast::CodegenState cgState(TheTargetMachine, TargetTriple);
  parseOutput.modules[0]->codegen(cgState);

  cgState.module.get()->print(outs(), nullptr);

  auto Filename = "output.o";
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }

  legacy::PassManager pass;
  auto FileType = CGFT_ObjectFile;

  if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    errs() << "TheTargetMachine can't emit a file of this type";
    return 1;
  }

  outs() << "running Legacy PassManager\n";

  pass.run(*(cgState.module));

  outs() << "Done\n";

  dest.flush();

  outs() << "Wrote " << Filename << "\n";

  return 0;
}