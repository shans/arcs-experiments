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
use std::collections::HashSet;

use std::process::Command;

use std::cell::RefCell;
use std::rc::Rc;

use walkdir::WalkDir;

#[derive(Debug)]
enum SkunkError {
  FileNotFound(String),
  FileUnreadable,
  ParseFailed,
  GraphBuilderError(graph_builder::GraphBuilderError),
  GraphToModuleError(graph_to_module::GraphToModuleError),
  CodegenError(ir_gen::codegen_state::CodegenError),
  WalkDirError(walkdir::Error),
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

impl <'a> From<walkdir::Error> for SkunkError {
  fn from(item: walkdir::Error) -> SkunkError {
    SkunkError::WalkDirError(item)
  }
}

#[derive(Default)]
struct FileData {
  buffer: String,
  ast: Vec<Rc<ast::TopLevel>>,
  main_module: Option<Rc<ast::Module>>,
  module_map: HashMap<String, Rc<ast::Module>>,
  newtype_map: HashMap<String, Rc<ast::NewType>>
}


struct MainData {
  file_info: RefCell<HashMap<String, Rc<FileData>>>,
}

impl MainData {
  fn new() -> Self {
    Self { file_info: RefCell::new(HashMap::new()) }
  }
  fn load_file(&self, location: &str) -> Result<(), SkunkError> {
    let mut file_data = FileData::default();
    file_data.prepare(self, location)?;
    {
      self.file_info.borrow_mut().insert(location.to_string(), Rc::new(file_data));
    }
    Ok(())
  }
  fn get_file_info(&self, location: &str) -> Rc<FileData> {
    let existing_data = {
      let file_info = self.file_info.borrow();
      file_info.get(location).map(|r| (*r).clone())
    };
    match existing_data {
      None => {
        self.load_file(location).ok();
        let file_info = self.file_info.borrow();
        file_info.get(location).unwrap().clone()
      }
      Some(info) => info.clone()
    }
  }
  fn main_module_for_file(&self, location: &str) -> Option<Rc<ast::Module>> {
    dbg!(format!("Getting main module for {}", location));
    self.get_file_info(location).main_module.clone()
  }
  fn named_module_from_file(&self, location: &str, name: &str) -> Option<Rc<ast::Module>> {
    self.get_file_info(location).module_map.get(name).map(|rc| rc.clone())
  }
  fn named_newtype_from_file(&self, location: &str, name: &str) -> Option<Rc<ast::NewType>> {
    self.get_file_info(location).newtype_map.get(name).map(|rc| rc.clone())
  }
}

impl FileData {
  fn prepare(&mut self, main_data: &MainData, location: &str) -> Result<(), SkunkError> {
    dbg!(location);
    let slash = location.rfind("/");
    let prefix = match slash {
      None => "./",
      Some(pos) => &location[0..pos+1]
    };
    dbg!(prefix);
    let mut f = File::open(location).or(Err(SkunkError::FileNotFound(location.to_string())))?;
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
  
    let dependencies = ast::uses(&ast);
    let mut processed_modules = Vec::new();
    let mut newtypes: Vec<ast::NewType> = ast::newtypes(&ast).drain(..).cloned().collect();

    for dependency in dependencies {
      // TODO: Absolute paths, imports from other places, etc. etc.
      let file_name = format!("{}{}.skunk", prefix, dependency.name);
      if dependency.children.is_empty() {
        let module = main_data.main_module_for_file(&file_name).ok_or(SkunkError::FileNotFound(file_name.clone()))?;
        processed_modules.push(module);
      } else {
        // TODO: handle compound children
        for child in &dependency.children {
          if !child.children.is_empty() {
            panic!("Don't know how to deal loading compound children yet");
          }
          let module = main_data.named_module_from_file(&file_name, &child.name);
          if let Some(module) = module {
            processed_modules.push(module);
          } else {
            let newtype = main_data.named_newtype_from_file(&file_name, &child.name).unwrap();
            newtypes.push(newtype.as_ref().clone()); 
          }
        }
      }
    }

    { 
      let mut modules = ast::modules_mut(&mut ast);
      for i in 0..modules.len() {
        modules[i].resolve_types(&newtypes);
        if modules[i].graph.len() > 0 {
          let mut graph = graph_builder::make_graph(modules[i].graph.iter().collect());
          let processed_refs = processed_modules.iter().map(|r| r.as_ref()).collect();
          graph_builder::resolve_graph(modules[i], &processed_refs, &mut graph)?;
          graph_to_module::graph_to_module(modules[i], graph, processed_refs)?;
          println!("{}", modules[i].minidump());
        }
        processed_modules.push(Rc::new(modules[i].clone()))
      }
    }
    let ast_graphs = ast::graphs(&ast);
    let processed_refs = processed_modules.iter().map(|r| r.as_ref()).collect();

    // TODO: Instead of duplicating graph processing logic, push the main module onto the end of the mutable modules list and
    // deal with it in the same pass as the rest.
    if ast_graphs.len() > 0 {
      let mut graph = graph_builder::make_graph(ast::graphs(&ast));
      let mut main = ast::Module::create("Main", Vec::new(), Vec::new(), Vec::new(), ast::Examples { examples: Vec::new() }, Vec::new(), Vec::new());

      graph_builder::resolve_graph(&main, &processed_refs, &mut graph)?;

      graph_to_module::graph_to_module(&mut main, graph, processed_refs)?;
      self.main_module = Some(Rc::new(main));
    } else if processed_refs.len() == 1 {
      // TODO: This isn't really correct - there needs to be some way
      // of determining which module is "main".
      self.main_module = Some(Rc::new(processed_refs[0].clone()));
    } else {
      let slash_pos = location.rfind('/');
      // TODO: This maybe assumes ASCII (byte boundary == character boundary)
      let module_name = match slash_pos {
        None => location,
        Some(pos) => location.split_at(pos + 1).1,
      };
      let module_name = module_name.strip_suffix(".skunk").unwrap();
      dbg!(&module_name);
      self.main_module = processed_refs.iter().find(|module| module.name == module_name).map(|module| Rc::new((*module).clone()))
    }

    for toplevel in ast {
      match toplevel {
        ast::TopLevel::Module(m) => { self.module_map.insert(m.name.to_string(), Rc::new(m)); },
        ast::TopLevel::NewType(t) => { self.newtype_map.insert(t.name.to_string(), Rc::new(t)); },
        _ => self.ast.push(Rc::new(toplevel))
      }
    }

    Ok(())
  }
}

const VERSION: &str = env!("CARGO_PKG_VERSION");
const USAGE: &str = include_str!("../USAGE");

fn main() {
  let command = env::args().nth(1);

  match command.as_deref() {
    None => {
      let (target_triple, target_machine) = target_triple_and_machine();
      let context = Context::create();

      let main_data = MainData::new();
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
    },
    Some("-h" | "--help" | "help") => eprintln!("skunk - {}\n{}", VERSION, USAGE),
    Some("examples") => {
        let file = &env::args().nth(2).expect("missing example path");
        let mut main_data = MainData::new();
        build_test_examples(&mut main_data, file).unwrap();
      },
    Some("all-examples") => {
      let location = &env::args().nth(2).expect("missing example path");
      let mut main_data = MainData::new();
      build_all_test_examples(&mut main_data, location).unwrap();
    },
    Some(command) => panic!("Unknown command '{}'", command),
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

fn build_all_test_examples(main_data:&mut MainData, location: &str) -> Result<(), SkunkError> {
  let (target_triple, target_machine) = target_triple_and_machine();
  let mut target_info = ir_gen::codegen_state::TargetInfo { target_machine: &target_machine, target_triple: &target_triple };
  let context = Context::create();
  
  let mut objects: HashSet<String> = HashSet::new();
  let mut deduped_modules: Vec<&inkwell::module::Module> = Vec::new();
 
  let mut main_modules: Vec<Rc<ast::Module>> = Vec::new();
  let mut cg_modules_list: Vec<Vec<inkwell::module::Module>> = Vec::new();

  for maybe_entry in WalkDir::new(location) {
    let entry = maybe_entry?;
    if entry.file_type().is_dir() {
      continue;
    }
    let path_str = entry.path().to_str().unwrap();

    if !path_str.ends_with(".skunk") {
      continue;
    }

    main_data.load_file(path_str)?;
    if let Some(main_module) = main_data.main_module_for_file(path_str) {
      main_modules.push(main_module);
    }
  }

  for main_module in &main_modules {
    let cg_modules = ir_gen::codegen(&context, &mut target_info, main_module)?;
    cg_modules_list.push(cg_modules);
  }

  for cg_modules in &cg_modules_list {
    for module in cg_modules {
      let name = module.get_name().to_str().unwrap();
      let object_name = name.to_string() + ".o";
      if objects.contains(&object_name) {
        continue;
      }
      let path = Path::new(&object_name);
      target_machine.write_to_file(&module, FileType::Object, path).unwrap();
      objects.insert(object_name);
      deduped_modules.push(&module);
    }
  }

  let example_module = ir_gen::main_for_examples(&context, &target_machine, &target_triple, &deduped_modules)?;
  let object_name = "main.o";
  let path = Path::new(&object_name);
  target_machine.write_to_file(&example_module, FileType::Object, path).unwrap();
  objects.insert(object_name.to_string());

  let mut command = Command::new("clang");
  let mut cmd = command.arg("-o").arg(&format!("{}/all_examples", location));
  for object in objects {
    cmd = cmd.arg(object);
  }

  let output = cmd.arg("-lc").output().expect("failed to run clang");
  stdout().write_all(&output.stdout).unwrap();
  stderr().write_all(&output.stderr).unwrap();

  Ok(())
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
    // module.print_to_stderr();
    let name = module.get_name().to_str().unwrap();
    let object_name = name.to_string() + ".o";
    let path = Path::new(&object_name);
    target_machine.write_to_file(&module, FileType::Object, path).unwrap();
    objects.push(object_name);
  }

  let main_module = ir_gen::main_for_examples(&context, &target_machine, &target_triple, &cg_modules.iter().collect())?;
  let object_name = "main.o";
  let path = Path::new(&object_name);
  target_machine.write_to_file(&main_module, FileType::Object, path).unwrap();
  objects.push(object_name.to_string());

  let mut command = Command::new("clang");
  let mut cmd = command.arg("-o").arg(&(location.to_string() + "_examples"));
  dbg!(&objects);
  for object in objects {
    cmd = cmd.arg(object);
  }

  let output = cmd.arg("-lc").output().expect("failed to run clang");
  stdout().write_all(&output.stdout).unwrap();
  stderr().write_all(&output.stderr).unwrap();

  Ok(())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn run_parser_examples() {
    let mut main_data = MainData::new();
    build_all_test_examples(&mut main_data, "./examples/parser").unwrap();
    let output = Command::new("./examples/parser/all_examples").output().expect("Could not run parser tests");
    stdout().write_all(&output.stdout).unwrap();
    stderr().write_all(&output.stderr).unwrap();
    assert!(output.status.success());
  }

}