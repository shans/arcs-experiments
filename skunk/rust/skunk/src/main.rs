mod parser;
mod ast;
mod ir_gen;
mod graph;
mod graph_builder;
mod graph_to_module;

use inkwell::targets::{InitializationConfig, Target, TargetMachine, TargetTriple, RelocMode, CodeModel, FileType};
use inkwell::OptimizationLevel;
use inkwell::context::Context;

use nom::Err;

use std::path::Path;
use std::fs::File;
use std::io::prelude::*;

fn main() {
  let (target_triple, target_machine) = target_triple_and_machine();
  let context = Context::create();
  //let cg = ir_gen::CodegenState::new(&context, &target_machine, &target_triple, "test");

  let mut f = File::open("test.skunk").unwrap();
  let mut buffer = String::new();
  f.read_to_string(&mut buffer).unwrap();

  let (remainder, ast) = match parser::parse(&buffer) {
    Ok(result) => result,
    Err(Err::Failure(e) | Err::Error(e)) => { 
      println!("{}", e);
      return;
    }
    Err(Err::Incomplete(_n)) => panic!("Should not be possible")
  };

  println!("Left over: {}", remainder);

  let mut graph = graph_builder::make_graph(ast::graphs(&ast));
  println!("The graph is: {:?}", graph);

  graph_builder::resolve_graph(&ast::modules(&ast), &mut graph).unwrap();
  println!("\n\nThe resolved graph is: {:?}", graph);

  let main = graph_to_module::graph_to_module(&graph, &ast::modules(&ast), "Main").unwrap();
  println!("\n\nThe constructed main module is {:?}", main);

  /*
  let cg = ir_gen::CodegenState::new(&context, &target_machine, &target_triple, "test");
  ir_gen::module_codegen(&cg, ast::modules(&ast)[0]).unwrap();
  cg.module.print_to_stderr();
  */

  let mut target_info = ir_gen::codegen_state::TargetInfo { target_machine: &target_machine, target_triple: &target_triple };
  let cg_modules = ir_gen::codegen(&context, &mut target_info, &main).unwrap();

  for module in cg_modules {
    let name = module.get_name().to_str().unwrap();
    println!("Outputting object file for {}", name);
    // module.print_to_stderr();
    let object_name = name.to_string() + ".o";
    let path = Path::new(&object_name);
    target_machine.write_to_file(&module, FileType::Object, path).unwrap();
  }

}

// TODO: have this return a TargetInfo instead?
fn target_triple_and_machine() -> (TargetTriple, TargetMachine) {
  Target::initialize_all(&InitializationConfig::default());

  let target_triple = TargetMachine::get_default_triple();
  let target = Target::from_triple(&target_triple).unwrap();
  let target_machine = target.create_target_machine(&target_triple, "generic", "", OptimizationLevel::Default, RelocMode::Default, CodeModel::Default).unwrap();
  (target_triple, target_machine)
}
