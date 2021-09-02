use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, AnyTypeEnum};
use inkwell::values::{FunctionValue, PointerValue, BasicValueEnum};

use super::ast;

use std::borrow::Borrow;
use std::convert::TryInto;

struct CodegenState<'ctx> {
  context: &'ctx Context,
  module: Module<'ctx>,
  builder: Builder<'ctx>
}

#[derive(Debug, PartialEq)]
enum CodegenError {
  BadListenerTrigger,
  BadReadFieldName,
  BadUpdateFieldName,
  InvalidStructPointer,
  InvalidIndex
}

impl <'ctx> CodegenState<'ctx> {
  fn new(context: &'ctx Context, name: &str) -> CodegenState<'ctx> {
    let module = context.create_module(name);
    let builder = context.create_builder();
    CodegenState { context, module, builder }
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

fn module_codegen<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module) -> CodegenStatus {
  // Compute a trigger mask - we only need to trigger when a listener is installed on a handle
  let mut trigger_mask: u64 = 0;
  for listener in module.listeners.iter() {
    let idx = module.idx_for_field(&listener.trigger);
    if let Some(idx) = idx {
      trigger_mask |= 1 << idx;
    } else {
      return Err(CodegenError::BadListenerTrigger)
    }
  }

  for listener in module.listeners.iter() {
    listener_codegen(cg, module, listener)?;
  }

  Ok(())
}

fn update_ptr_for_field<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module, state: PointerValue<'ctx>, name: &str, for_writing: bool) -> CodegenResult<PointerValue<'ctx>> {
  let idx = module.idx_for_field(name).ok_or(CodegenError::BadUpdateFieldName)?;
  let struct_idx = (2 * idx + 1).try_into().or(Err(CodegenError::InvalidIndex))?;

  let bitfield_idx = module.idx_for_bitfield().try_into().unwrap();
  let bitfield_ptr = cg.builder.build_struct_gep(state, bitfield_idx, "bitfield_ptr").or(Err(CodegenError::InvalidStructPointer))?;
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

impl Typeable for (&ast::Module, &ast::Listener) {
  fn ir_type<'ctx>(&self, cg: &CodegenState<'ctx>) -> AnyTypeEnum<'ctx> {
    AnyTypeEnum::FunctionType(cg.context.void_type().fn_type(&[self.0.ir_type(cg).into_struct_type().ptr_type(AddressSpace::Generic).into()], false))
  }
}

fn listener_codegen<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module, listener: &ast::Listener) -> CodegenStatus {
  let function_type = (module, listener).ir_type(cg).into_function_type();
  let listener_name = (module, listener).name();
  let function = cg.module.add_function(&listener_name, function_type, None);

  statement_codegen(cg, module, &listener.statement, function)
}

fn statement_codegen<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module, statement: &ast::Statement, function: FunctionValue<'ctx>) -> CodegenStatus {
  let entry_block = cg.context.append_basic_block(function, "entry");
  cg.builder.position_at_end(entry_block);

  let state_ptr_type = module.ir_type(cg).into_struct_type().ptr_type(AddressSpace::Generic);
  let state_alloca = cg.builder.build_alloca(state_ptr_type, "state_alloca");
  cg.builder.build_store(state_alloca, function.get_first_param().unwrap().into_pointer_value());

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
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn test_module() -> ast::Module {
    ast::Module {
      name: String::from("TestModule"),
      handles: vec!(ast::Handle { name: String::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::TypePrimitive::Int },
                    ast::Handle { name: String::from("far"), usages: vec!(ast::Usage::Read), h_type: ast::TypePrimitive::Int },
                    ast::Handle { name: String::from("bar"), usages: vec!(ast::Usage::Write), h_type: ast::TypePrimitive::Int }
                  ),
      listeners: vec!(ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, statement: ast::Statement {
        output: String::from("bar"), expression: ast::Expression::ReferenceToState(String::from("far"))
      }})
    }
  }

  fn invalid_module() -> ast::Module {
     ast::Module {
      name: String::from("InvalidModule"),
      handles: vec!(ast::Handle { name: String::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::TypePrimitive::Int }),
      listeners: vec!(ast::Listener { trigger: String::from("invalid"), kind: ast::ListenerKind::OnChange, statement: ast::Statement {
        output: String::from("bar"), expression: ast::Expression::ReferenceToState(String::from("far"))
      }})
    }
  }

  #[test]
  fn module_ir_type() {
    let context = Context::create();
    let cs = CodegenState::new(&context.borrow(), "TestModule");
    let module = test_module();
    let ir_type = module.ir_type(&cs).into_struct_type();
    let i64_type = context.borrow().i64_type();
    assert_eq!(ir_type.count_fields(), 7);
    assert_eq!(ir_type.get_field_types(), &[i64_type.into(); 7]);
  }

  #[test]
  fn module_codegen_succeeds() {
    let context = Context::create();
    let cs = CodegenState::new(&context.borrow(), "TestModule");
    let module = test_module();
    assert_eq!(module_codegen(&cs, &module), Ok(()))
  }

  #[test]
  fn invalid_module_codegen_fails() {
    let context = Context::create();
    let cs = CodegenState::new(&context.borrow(), "InvalidModule");
    let module = invalid_module();
    assert_eq!(module_codegen(&cs, &module), Err(CodegenError::BadListenerTrigger))
  }
}
