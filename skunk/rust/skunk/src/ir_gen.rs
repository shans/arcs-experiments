use inkwell::{AddressSpace, IntPredicate};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{TargetMachine, TargetTriple};
use inkwell::types::{BasicTypeEnum, AnyTypeEnum};
use inkwell::values::{FunctionValue, PointerValue, BasicValueEnum};
use inkwell::passes::{PassManager, PassManagerBuilder};

use super::ast;

use std::convert::TryInto;

pub struct CodegenState<'ctx> {
  context: &'ctx Context,
  pub module: Module<'ctx>,
  builder: Builder<'ctx>,
  function_pass_manager: PassManager<FunctionValue<'ctx>>,
}

#[derive(Debug, PartialEq)]
pub enum CodegenError {
  BadListenerTrigger,
  BadReadFieldName,
  BadUpdateFieldName,
  InvalidStructPointer,
  InvalidIndex,
  FunctionMissing
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
    CodegenState { context, module, builder, function_pass_manager }
  }
}

trait Typeable {
  fn ir_type<'ctx>(&self, cg: &CodegenState<'ctx>) -> AnyTypeEnum<'ctx>;
}

type CodegenStatus = Result<(), CodegenError>;
type CodegenResult<T> = Result<T, CodegenError>;

trait Genable<'ctx> {
  fn codegen(&self, cg: &CodegenState<'ctx>) -> CodegenStatus;
}

trait Nameable {
  fn name(&self) -> String;
}

impl Typeable for ast::Module {
  fn ir_type<'ctx>(&self, cg: &CodegenState<'ctx>) -> AnyTypeEnum<'ctx> {
    let sub_types: Vec<BasicTypeEnum> = vec![cg.context.i64_type().into(); self.handles.len() * 2 + 1];
    AnyTypeEnum::StructType(cg.context.struct_type(&sub_types, false))
  }
}

pub fn module_codegen<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module) -> CodegenStatus {
  for listener in module.listeners.iter() {
    listener_codegen(cg, module, listener)?;
  }

  module_update_function(cg, module)
}

fn module_update_function<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module) -> CodegenStatus {
  // Compute a trigger mask - we only need to trigger when a listener is installed on a handle
  // TODO: we don't actually use this..
  let mut trigger_mask: u64 = 0;
  for listener in module.listeners.iter() {
    let idx = module.idx_for_field(&listener.trigger);
    if let Some(idx) = idx {
      trigger_mask |= 1 << idx;
    } else {
      return Err(CodegenError::BadListenerTrigger)
    }
  }

  let function_type = ir_listener_type(cg, module).into_function_type();
  let function = cg.module.add_function(&(module.name.clone() + "_update"), function_type, None);

  let entry_block = cg.context.append_basic_block(function, "entry");
  cg.builder.position_at_end(entry_block);
  let state_alloca = state_alloca_for_module_function(cg, module, function);
  let state_ptr = cg.builder.build_load(state_alloca, "state_ptr").into_pointer_value();

  let bitfield_ptr = module_bitfield_ptr(cg, module, state_ptr)?;
  let bitfield = cg.builder.build_load(bitfield_ptr, "bitfield").into_int_value();

  for (idx, handle) in module.handles.iter().enumerate() {
    let test_value = cg.context.i64_type().const_int(1 << idx, false);
    let bitfield_has_test = cg.builder.build_and(bitfield, test_value, "bitfield_test");
    let has_listener = cg.builder.build_int_compare(IntPredicate::NE, bitfield_has_test, cg.context.i64_type().const_int(0, false), "has_listener");

    let activate_for_block = cg.context.append_basic_block(function, &("activate_for_".to_owned() + &handle.name));
    let after_listeners_block = cg.context.append_basic_block(function, &("after_".to_owned() + &handle.name));

    cg.builder.build_conditional_branch(has_listener, activate_for_block, after_listeners_block);

    cg.builder.position_at_end(activate_for_block);
    let write_ptr = read_ptr_for_field(cg, module, state_ptr, &handle.name)?;
    let update_ptr = update_ptr_for_field(cg, module, state_ptr, &handle.name, false)?;
    let value = cg.builder.build_load(update_ptr, "value");
    cg.builder.build_store(write_ptr, value);
    cg.builder.build_store(update_ptr, cg.context.i64_type().const_int(0, false));

    for listener in module.listeners.iter() {
      if listener.trigger == handle.name {
        let function_name = (module, listener).name();
        let function = cg.module.get_function(&function_name).ok_or(CodegenError::FunctionMissing)?;
        cg.builder.build_call(function, &[state_ptr.into()], "_");
      }
    }
    cg.builder.build_unconditional_branch(after_listeners_block);

    cg.builder.position_at_end(after_listeners_block);
  }

  cg.builder.build_return(Option::None);
  cg.function_pass_manager.run_on(&function);
  Ok(())
}

fn module_bitfield_ptr<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module, state: PointerValue<'ctx>) -> CodegenResult<PointerValue<'ctx>> {
  let bitfield_idx = module.idx_for_bitfield().try_into().unwrap();
  cg.builder.build_struct_gep(state, bitfield_idx, "bitfield_ptr").or(Err(CodegenError::InvalidStructPointer))
}

fn update_ptr_for_field<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module, state: PointerValue<'ctx>, name: &str, for_writing: bool) -> CodegenResult<PointerValue<'ctx>> {
  let idx = module.idx_for_field(name).ok_or(CodegenError::BadUpdateFieldName)?;
  let struct_idx = (2 * idx + 1).try_into().or(Err(CodegenError::InvalidIndex))?;

  let bitfield_ptr = module_bitfield_ptr(cg, module, state)?;
  let bitfield = cg.builder.build_load(bitfield_ptr, "bitfield").into_int_value();
 
  let new_bitfield = if for_writing {
    let new_write = cg.context.i64_type().const_int(1 << idx, false);
    cg.builder.build_or(bitfield, new_write, "new_bitfield")
  } else {
    let new_clear = cg.context.i64_type().const_int(!(1 << idx), false);
    cg.builder.build_and(bitfield, new_clear, "new_bitfield")
  };
  cg.builder.build_store(bitfield_ptr, new_bitfield);
  cg.builder.build_struct_gep(state, struct_idx, &(String::from("update_ptr_to_") + name)).or(Err(CodegenError::InvalidStructPointer))
}

fn read_ptr_for_field<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module, state: PointerValue<'ctx>, name: &str) -> CodegenResult<PointerValue<'ctx>> {
  let idx = module.idx_for_field(name).ok_or(CodegenError::BadReadFieldName)?;
  let struct_idx = (2 * idx).try_into().or(Err(CodegenError::InvalidIndex))?;
  cg.builder.build_struct_gep(state, struct_idx, &(String::from("read_ptr_to_") + name)).or(Err(CodegenError::InvalidStructPointer))
}

impl Nameable for (&ast::Module, &ast::Listener) {
  fn name(&self) -> String {
    self.0.name.clone() + "__" + self.1.kind.to_string() + "__" + &self.1.trigger
  }
}

fn ir_listener_type<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module) -> AnyTypeEnum<'ctx> {
    AnyTypeEnum::FunctionType(cg.context.void_type().fn_type(&[module.ir_type(cg).into_struct_type().ptr_type(AddressSpace::Generic).into()], false))
}

impl Typeable for (&ast::Module, &ast::Listener) {
  fn ir_type<'ctx>(&self, cg: &CodegenState<'ctx>) -> AnyTypeEnum<'ctx> {
    ir_listener_type(cg, self.0)
  }
}

fn listener_codegen<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module, listener: &ast::Listener) -> CodegenStatus {
  let function_type = (module, listener).ir_type(cg).into_function_type();
  let listener_name = (module, listener).name();
  let function = cg.module.add_function(&listener_name, function_type, None);

  statement_codegen(cg, module, &listener.statement, function)
}

fn state_alloca_for_module_function<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module, function: FunctionValue<'ctx>) -> PointerValue<'ctx> {
  let state_ptr_type = module.ir_type(cg).into_struct_type().ptr_type(AddressSpace::Generic);
  let state_alloca = cg.builder.build_alloca(state_ptr_type, "state_alloca");
  cg.builder.build_store(state_alloca, function.get_first_param().unwrap().into_pointer_value());
  state_alloca
}

fn statement_codegen<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module, statement: &ast::Statement, function: FunctionValue<'ctx>) -> CodegenStatus {
  let entry_block = cg.context.append_basic_block(function, "entry");
  cg.builder.position_at_end(entry_block);
  let state_alloca = state_alloca_for_module_function(cg, module, function);

  let return_value = expression_codegen(cg, module, &statement.expression, state_alloca)?;
  let state_ptr = cg.builder.build_load(state_alloca, "state_ptr");
  let update_ptr = update_ptr_for_field(cg, module, state_ptr.into_pointer_value(), &statement.output, true)?;
  cg.builder.build_store(update_ptr, return_value);
  cg.builder.build_return(Option::None);
  Ok(())
}

fn expression_codegen<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module, expression: &ast::Expression, state_alloca: PointerValue<'ctx>) -> CodegenResult<BasicValueEnum<'ctx>> {
  match expression {
    ast::Expression::ReferenceToState(field) => {
      let state_ptr = cg.builder.build_load(state_alloca, "state_ptr");
      let value_ptr = read_ptr_for_field(cg, module, state_ptr.into_pointer_value(), &field)?;
      Ok(cg.builder.build_load(value_ptr, &(String::from("ref_to_state_") + field)))
    }
    ast::Expression::CopyToSubModule(info) => {
      panic!("I don't know how to do this yet!");
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use super::super::target_triple_and_machine;

  fn test_module() -> ast::Module {
    ast::Module {
      name: String::from("TestModule"),
      handles: vec!(ast::Handle { name: String::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::TypePrimitive::Int },
                    ast::Handle { name: String::from("far"), usages: vec!(ast::Usage::Read), h_type: ast::TypePrimitive::Int },
                    ast::Handle { name: String::from("bar"), usages: vec!(ast::Usage::Write), h_type: ast::TypePrimitive::Int }
                  ),
      listeners: vec!(ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, statement: ast::Statement {
        output: String::from("bar"), expression: ast::Expression::ReferenceToState(String::from("far"))
      }}),
      submodules: Vec::new(),
    }
  }

  fn invalid_module() -> ast::Module {
     ast::Module {
      name: String::from("InvalidModule"),
      handles: vec!(ast::Handle { name: String::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::TypePrimitive::Int }),
      listeners: vec!(ast::Listener { trigger: String::from("invalid"), kind: ast::ListenerKind::OnChange, statement: ast::Statement {
        output: String::from("foo"), expression: ast::Expression::ReferenceToState(String::from("foo"))
      }}),
      submodules: Vec::new(),
    }
  }

  #[test]
  fn module_ir_type() {
    let context = Context::create();
    let (target_triple, target_machine) = target_triple_and_machine();
    let cs = CodegenState::new(&context, &target_machine, &target_triple, "TestModule");
    let module = test_module();
    let ir_type = module.ir_type(&cs).into_struct_type();
    let i64_type = context.i64_type();
    assert_eq!(ir_type.count_fields(), 7);
    assert_eq!(ir_type.get_field_types(), &[i64_type.into(); 7]);
  }

  #[test]
  fn module_codegen_succeeds() {
    let context = Context::create();
    let (target_triple, target_machine) = target_triple_and_machine();
    let cs = CodegenState::new(&context, &target_machine, &target_triple, "TestModule");
    let module = test_module();
    assert_eq!(module_codegen(&cs, &module), Ok(()))
  }

  #[test]
  fn invalid_module_codegen_fails() {
    let context = Context::create();
    let (target_triple, target_machine) = target_triple_and_machine();
    let cs = CodegenState::new(&context, &target_machine, &target_triple, "InvalidModule");
    let module = invalid_module();
    assert_eq!(module_codegen(&cs, &module), Err(CodegenError::BadListenerTrigger))
  }
}
