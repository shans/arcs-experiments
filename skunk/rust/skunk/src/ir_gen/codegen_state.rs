use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::values::{FunctionValue, IntValue, PointerValue};
use inkwell::targets::{TargetMachine, TargetTriple};
use inkwell::types::PointerType;
use inkwell::AddressSpace;

use std::collections::HashMap;
use super::state_values::*;
use super::ast::ExpressionValue;

#[derive(Debug, PartialEq)]
pub enum CodegenError {
  BadListenerTrigger,
  BadReadFieldName(String),
  BadUpdateFieldName,
  InvalidStructPointer(String),
  InvalidIndex,
  FunctionMissing,
  NotInAFunction,
  NotInABlock,
  InvalidFunctionArgument(String),
  TypeMismatch(String),
  InvalidTupleID(usize),
  NakedBreak,
}

pub type CodegenStatus = Result<(), CodegenError>;
pub type CodegenResult<T> = Result<T, CodegenError>;

pub struct CodegenState<'ctx> {
  pub context: &'ctx Context,
  pub module: Module<'ctx>,
  pub builder: Builder<'ctx>,
  pub function_pass_manager: PassManager<FunctionValue<'ctx>>,
  pub locals: HashMap<String, StatePointer<'ctx>>,
  pub break_target: Vec<BasicBlock<'ctx>>,
  pub considering: Option<&'ctx ExpressionValue>,
  pub registered_strings: HashMap<String, PointerValue<'ctx>>
}

impl <'ctx> CodegenState<'ctx> {

  pub fn new(context: &'ctx Context, target_machine: &TargetMachine, target_triple: &TargetTriple, name: &str) -> CodegenState<'ctx> {
    let module = context.create_module(name);
    module.set_data_layout(&target_machine.get_target_data().get_data_layout());
    module.set_triple(target_triple);
    let builder = context.create_builder();
    let pass_manager_builder = PassManagerBuilder::create();
    let function_pass_manager = PassManager::create(&module);
    pass_manager_builder.populate_function_pass_manager(&function_pass_manager);
    CodegenState { context, module, builder, function_pass_manager, locals: HashMap::new(), break_target: Vec::new(), considering: None, registered_strings: HashMap::new() }
  }

  
  pub fn uint_const(&self, value: u64) -> IntValue<'ctx> {
    self.context.i64_type().const_int(value, false)
  }

  pub fn uint32_const(&self, value: u32) -> IntValue<'ctx> {
    self.context.i32_type().const_int(value as u64, false)
  }

  pub fn add_local(&mut self, name: &str, value: StateValue<'ctx>) -> CodegenStatus {
    if let Some(ptr) = self.locals.get(&name.to_string()) {
      value.store(self, ptr)
    } else {
      let alloca = self.builder.build_alloca(value.llvm_type(), &("alloca_".to_string() + name));
      let ptr = StatePointer::new_from_type_primitive(alloca, value.value_type.clone());
      value.store(self, &ptr)?;

      self.locals.insert(name.to_string(), ptr);
      Ok(())
    }
  }

  pub fn get_local(&self, name: &str) -> Option<StateValue<'ctx>> {
    if let Some(ptr) = self.locals.get(&name.to_string()) {
      Some(ptr.load(self, name).ok()?)
    } else {
      None
    }
  }

  pub fn char_ptr_type(&self) -> PointerType<'ctx> {
    self.context.i8_type().ptr_type(AddressSpace::Generic)
  }

  pub fn global_string(&mut self, value: &str) -> PointerValue<'ctx> {
    self.registered_strings.get(value).map(|rf| *rf).or_else(|| {
      let ptr = self.builder.build_global_string_ptr(value, "global_string").as_pointer_value();
      self.registered_strings.insert(value.to_string(), ptr);
      Some(ptr)
    }).unwrap()
  }
}

pub trait CodegenStateConstructor<'ctx> {
  fn construct(&mut self, context: &'ctx Context, name: &str) -> CodegenState<'ctx>;
}

pub struct TargetInfo<'a> {
  pub target_machine: &'a TargetMachine,
  pub target_triple: &'a TargetTriple,
}

impl <'a, 'ctx> CodegenStateConstructor<'ctx> for TargetInfo<'a> {
  fn construct(&mut self, context: &'ctx Context, name: &str) -> CodegenState<'ctx> {
    CodegenState::new(context, self.target_machine, self.target_triple, name)
  }
}
