use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, AnyTypeEnum};
use inkwell::values::FunctionValue;

use super::ast;

struct CodegenState<'ctx> {
  context: &'ctx Context,
  module: Module<'ctx>,
  builder: Builder<'ctx>
}

impl <'ctx> CodegenState<'ctx> {
  fn create(context: &'ctx Context, name: &str) -> CodegenState<'ctx> {
    let module = context.create_module(name);
    let builder = context.create_builder();
    CodegenState { context, module, builder }
  }
}

trait Typeable {
  fn ir_type<'ctx>(&self, cg: &CodegenState<'ctx>) -> AnyTypeEnum<'ctx>;
}

trait Genable<'ctx> {
  fn codegen(&self, cg: &CodegenState<'ctx>) -> bool;
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

impl <'ctx> Genable<'ctx> for ast::Module {
  fn codegen(&self, cg: &CodegenState<'ctx>) -> bool {
    // Compute a trigger mask - we only need to trigger when a listener is installed on a handle
    let mut trigger_mask: u64 = 0;
    for listener in self.listeners.iter() {
      let idx = self.idx_for_field(&listener.trigger);
      if let Some(idx) = idx {
        trigger_mask |= 1 << idx;
      } else {
        return false
      }
    }

    for listener in self.listeners.iter() {
      if !(self, listener).codegen(cg) {
        return false
      }
    }

    true
  }
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

impl <'ctx> Genable<'ctx> for (&ast::Module, &ast::Listener) {
  fn codegen(&self, cg: &CodegenState<'ctx>) -> bool {
    let function_type = self.ir_type(cg).into_function_type();
    let listener_name = self.name();
    let function = cg.module.add_function(&listener_name, function_type, None);

    if (self.0, &self.1.statement, function).codegen(cg) {
      // TODO: verify function & run through pass manager
      true
    } else {
      false
    }
  }
}

impl <'ctx> Genable<'ctx> for (&ast::Module, &ast::Statement, FunctionValue<'ctx>) {
  fn codegen(&self, cg: &CodegenState<'ctx>) -> bool {
    let entry_block = cg.context.append_basic_block(self.2, "entry");
    cg.builder.position_at_end(entry_block);

    let state_ptr_type = self.0.ir_type(cg).into_struct_type().ptr_type(AddressSpace::Generic);
    let state_alloca = cg.builder.build_alloca(state_ptr_type, "state_alloca");
    cg.builder.build_store(state_alloca, self.2.get_first_param().unwrap().into_pointer_value());
    true
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn test_module() -> ast::Module {
    ast::Module {
      name: String::from("TestModule"),
      handles: vec!(ast::Handle { name: String::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::TypePrimitive::Int }),
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
    let cs = CodegenState::create(&context, "TestModule");
    let module = test_module();
    let ir_type = module.ir_type(&cs).into_struct_type();
    let i64_type = context.i64_type();
    assert_eq!(ir_type.count_fields(), 3);
    assert_eq!(ir_type.get_field_types(), &[i64_type.into(), i64_type.into(), i64_type.into()]);
  }

  #[test]
  fn module_codegen_succeeds() {
    let context = Context::create();
    let cs = CodegenState::create(&context, "TestModule");
    let module = test_module();
    assert_eq!(module.codegen(&cs), true)
  }

  #[test]
  fn invalid_module_codegen_fails() {
    let context = Context::create();
    let cs = CodegenState::create(&context, "InvalidModule");
    let module = invalid_module();
    assert_eq!(module.codegen(&cs), false)
  }
}
