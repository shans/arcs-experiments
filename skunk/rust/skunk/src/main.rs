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

use std::collections::HashMap;

#[derive(Debug)]
enum SkunkError {
  FileNotFound,
  FileUnreadable,
  ParseFailed,
  GraphBuilderError(graph_builder::GraphBuilderError),
  GraphToModuleError(graph_to_module::GraphToModuleError),
}

impl From<graph_builder::GraphBuilderError> for SkunkError {
  fn from(item: graph_builder::GraphBuilderError) -> SkunkError {
    SkunkError::GraphBuilderError(item)
  }
}

impl <'a> From<graph_to_module::GraphToModuleError> for SkunkError {
  fn from(item: graph_to_module::GraphToModuleError) -> SkunkError {
    SkunkError::GraphToModuleError(item)
  }
}

struct FileData {
  buffer: String,
  ast: Vec<ast::TopLevel>,
  main_module: Option<ast::Module>,
}


struct MainData {
  file_info: HashMap<String, FileData>,
}

impl MainData {
  fn new() -> Self {
    Self { file_info: HashMap::new() }
  }
  fn load_file(&mut self, location: &str) -> Result<(), SkunkError> {
    let file_data = FileData::new();
    self.file_info.insert(location.to_string(), file_data);

    self.file_info.get_mut(location).unwrap().prepare(location)?;
    Ok(())
  }
  fn main_module_for_file(&self, location: &str) -> Option<&ast::Module> {
    self.file_info.get(location).and_then(|file_info| (&file_info.main_module).as_ref())
  }
}

impl FileData {
  fn new() -> Self {
    Self { ast: Vec::new(), main_module: None, buffer: String::new() }
  }

  fn prepare(&mut self, location: &str) -> Result<(), SkunkError> {
    let mut f = File::open(location).or(Err(SkunkError::FileNotFound))?;
    f.read_to_string(&mut self.buffer).or(Err(SkunkError::FileUnreadable))?;

    let (remainder, ast) = match parser::parse(&self.buffer) {
      Ok(result) => result,
      Err(Err::Failure(e) | Err::Error(e)) => { 
        println!("{}", e);
        return Err(SkunkError::ParseFailed);
      }
      Err(Err::Incomplete(_n)) => panic!("Should not be possible")
    };
    
    if remainder.fragment().len() > 0 {
      println!("Left over: {}", remainder);
    }

    self.ast = ast;

    let mut graph = graph_builder::make_graph(ast::graphs(&self.ast));

    let modules = ast::modules(&self.ast);
    graph_builder::resolve_graph(&modules, &mut graph)?;

    let main = graph_to_module::graph_to_module(graph, modules, "Main")?;
    self.main_module = Some(main);
    Ok(())
  }
}

fn main() {
  let (target_triple, target_machine) = target_triple_and_machine();
  let context = Context::create();

  let mut main_data = MainData::new();
  main_data.load_file("test.skunk").unwrap();
  let main = main_data.main_module_for_file("test.skunk").unwrap();

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

fn build_test_examples(main_data: &mut MainData, location: &str) -> Result<(), SkunkError> {
  main_data.load_file(location)?;
  let main_module = main_data.main_module_for_file(location).unwrap();
  Ok(())
}