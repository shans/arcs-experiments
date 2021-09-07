mod parser;
mod ast;
mod ir_gen;
mod graph;
mod graph_builder;

use inkwell::targets::{InitializationConfig, Target, TargetMachine, TargetTriple, RelocMode, CodeModel, FileType};
use inkwell::OptimizationLevel;
use inkwell::context::Context;

use std::path::Path;
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

  let mut graph = graph_builder::make_graph(ast::graphs(&ast));
  println!("The graph is: {:?}", graph);

  graph_builder::resolve_graph(&ast::modules(&ast), &mut graph).unwrap();
  println!("The graph is: {:?}", graph);

  let module = ast::modules(&ast)[0];
  ir_gen::module_codegen(&cg, &module);
  cg.module.print_to_stderr();

  let path = Path::new("output.o");
  target_machine.write_to_file(&cg.module, FileType::Object, path).unwrap();
}

fn target_triple_and_machine() -> (TargetTriple, TargetMachine) {
  Target::initialize_all(&InitializationConfig::default());

  let target_triple = TargetMachine::get_default_triple();
  let target = Target::from_triple(&target_triple).unwrap();
  let target_machine = target.create_target_machine(&target_triple, "generic", "", OptimizationLevel::Default, RelocMode::Default, CodeModel::Default).unwrap();
  (target_triple, target_machine)
}
