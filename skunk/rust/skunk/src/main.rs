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

use std::env;
use std::path::Path;
use std::fs::File;
use std::io::{prelude::*, stdout, stderr};

use std::collections::HashMap;

use std::process::Command;

#[derive(Debug)]
enum SkunkError {
  FileNotFound,
  FileUnreadable,
  ParseFailed,
  GraphBuilderError(graph_builder::GraphBuilderError),
  GraphToModuleError(graph_to_module::GraphToModuleError),
  CodegenError(ir_gen::codegen_state::CodegenError),
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

impl <'a> From<ir_gen::codegen_state::CodegenError> for SkunkError {
  fn from(item: ir_gen::codegen_state::CodegenError) -> SkunkError {
    SkunkError::CodegenError(item)
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

    let (remainder, mut ast) = match parser::parse(&self.buffer) {
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
   
    let mut processed_modules = Vec::new();
    { 
      let mut modules = ast::modules_mut(&mut ast);
      for i in 0..modules.len() {
        if modules[i].graph.len() > 0 {
          let mut graph = graph_builder::make_graph(modules[i].graph.iter().collect());
          let processed_refs = processed_modules.iter().collect();
          graph_builder::resolve_graph(&processed_refs, &mut graph)?;
          graph_to_module::graph_to_module(modules[i], graph, processed_refs, "")?;
        }
        processed_modules.push(modules[i].clone())
      }
    }
    self.ast = ast;
    let ast_graphs = ast::graphs(&self.ast);
    let processed_refs = processed_modules.iter().collect();

    // TODO: Instead of duplicating graph processing logic, push the main module onto the end of the mutable modules list and
    // deal with it in the same pass as the rest.
    if ast_graphs.len() > 0 {
      let mut graph = graph_builder::make_graph(ast::graphs(&self.ast));

      graph_builder::resolve_graph(&processed_refs, &mut graph)?;

      let mut main = ast::Module::create("Main", Vec::new(), Vec::new(), Vec::new(), ast::Examples { examples: Vec::new() }, Vec::new(), Vec::new());
      graph_to_module::graph_to_module(&mut main, graph, processed_refs, "Main")?;
      self.main_module = Some(main);
    } else if processed_refs.len() == 1 {
      // TODO: This isn't really correct - there needs to be some way
      // of determining which module is "main".
      self.main_module = Some(processed_refs[0].clone());
    } else {
      let slash_pos = location.rfind('/');
      // TODO: This maybe assumes ASCII (byte boundary == character boundary)
      let module_name = match slash_pos {
        None => location,
        Some(pos) => location.split_at(pos + 1).1,
      };
      let module_name = module_name.strip_suffix(".skunk").unwrap();
      dbg!(&module_name);
      self.main_module = processed_refs.iter().find(|module| module.name == module_name).map(|module| (*module).clone())
    }
    Ok(())
  }
}

fn main() {
  let args: Vec<String> = env::args().collect();

  if args.len() > 2 && args[1] == "examples" {
    let file = &args[2];
    let mut main_data = MainData::new();
    build_test_examples(&mut main_data, file).unwrap();
    return;
  }

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

  let (target_triple, target_machine) = target_triple_and_machine();
  let mut target_info = ir_gen::codegen_state::TargetInfo { target_machine: &target_machine, target_triple: &target_triple };

  let context = Context::create();

  // we need object code for all of these
  let cg_modules = ir_gen::codegen(&context, &mut target_info, &main_module)?;

  let mut objects: Vec<String> = Vec::new();
  for module in &cg_modules {
    module.print_to_stderr();
    let name = module.get_name().to_str().unwrap();
    let object_name = name.to_string() + ".o";
    let path = Path::new(&object_name);
    target_machine.write_to_file(&module, FileType::Object, path).unwrap();
    objects.push(object_name);
  }

  let main_module = ir_gen::main_for_examples(&context, &target_machine, &target_triple, &cg_modules)?;
  let object_name = "main.o";
  let path = Path::new(&object_name);
  target_machine.write_to_file(&main_module, FileType::Object, path).unwrap();
  objects.push(object_name.to_string());

  let mut command = Command::new("clang");
  let mut cmd = command.arg("-o").arg(&(location.to_string() + "_examples"));
  for object in objects {
    cmd = cmd.arg(object);
  }

  let output = cmd.arg("-lc").output().expect("failed to run clang");
  stdout().write_all(&output.stdout).unwrap();
  stderr().write_all(&output.stderr).unwrap();

  Ok(())
}