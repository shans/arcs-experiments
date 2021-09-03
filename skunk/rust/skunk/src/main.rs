mod parser;
mod ast;
mod ir_gen;

use inkwell::targets::{InitializationConfig, Target, TargetMachine, TargetTriple, RelocMode, CodeModel};
use inkwell::OptimizationLevel;
use inkwell::context::Context;

use std::fs::File;
use std::io::prelude::*;

fn main() {
  let (target_triple, target_machine) = target_triple_and_machine();
  let context = Context::create();
  let cg = ir_gen::CodegenState::new(&context, &target_machine, &target_triple, "test");

  let mut f = File::open("test.skunk").unwrap();
  let mut buffer = String::new();
  f.read_to_string(&mut buffer).unwrap();

  let (_, ast) = parser::top_levels(&buffer).unwrap();

  let module = ast::modules(&ast)[0];
  ir_gen::module_codegen(&cg, &module);
  cg.module.print_to_stderr();
}

fn target_triple_and_machine() -> (TargetTriple, TargetMachine) {
  Target::initialize_all(&InitializationConfig::default());

  let target_triple = TargetMachine::get_default_triple();
  let target = Target::from_triple(&target_triple).unwrap();
  let target_machine = target.create_target_machine(&target_triple, "generic", "", OptimizationLevel::Default, RelocMode::Default, CodeModel::Default).unwrap();
  (target_triple, target_machine)
}
