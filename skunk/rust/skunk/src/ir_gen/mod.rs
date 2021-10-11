use inkwell::{AddressSpace, IntPredicate, };
use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, AnyTypeEnum, StructType, BasicType};
use inkwell::values::{FunctionValue, PointerValue, BasicValueEnum, IntValue, ArrayValue};

use super::ast;

use std::convert::TryInto;

pub mod codegen_state;
mod state_values;

use codegen_state::*;
use state_values::*;

trait Typeable {
  fn ir_type<'ctx>(&self, cg: &CodegenState<'ctx>) -> AnyTypeEnum<'ctx>;
}

trait Genable<'ctx> {
  fn codegen(&self, cg: &CodegenState<'ctx>) -> CodegenStatus;
}

trait Nameable {
  fn fn_name(&self) -> String;
}

// TODO: probably this is better done via the TypePrimitive derived from the type?
fn handle_type<'ctx>(cg: &CodegenState<'ctx>, handle: &ast::Handle) -> BasicTypeEnum<'ctx> {
  let primitive_type = type_primitive_for_type(&handle.h_type);
  llvm_type_for_primitive(cg, &primitive_type)
}

fn llvm_type_for_primitive<'ctx>(cg: &CodegenState<'ctx>, primitive_type: &Vec<TypePrimitive>) -> BasicTypeEnum<'ctx> {
  if primitive_type.len() != 1 {
    let element_types: Vec<BasicTypeEnum<'ctx>> = primitive_type.iter().map(|t| llvm_type_for_primitive(cg, &vec!(t.clone()))).collect(); 
    cg.context.struct_type(&element_types, false).into()
  } else {
    match &primitive_type[0] {
      TypePrimitive::Int => cg.context.i64_type().into(),
      TypePrimitive::Char => cg.context.i8_type().into(),
      TypePrimitive::MemRegion => dptr_ir_type(cg, cg.context.i8_type().into()).into(),
      TypePrimitive::DynamicArrayOf(x) => dptr_ir_type(cg, llvm_type_for_primitive(cg, &x).into()).into(),
      TypePrimitive::PointerTo(x) => llvm_type_for_primitive(cg, x).ptr_type(AddressSpace::Generic).into(),
      TypePrimitive::FixedArrayOf(x, _s) => llvm_type_for_primitive(cg, x).ptr_type(AddressSpace::Generic).into(),
    }
  }
}

impl Typeable for ast::Module {
  fn ir_type<'ctx>(&self, cg: &CodegenState<'ctx>) -> AnyTypeEnum<'ctx> {
    let mut sub_types: Vec<BasicTypeEnum> = Vec::new();
    for handle in &self.handles {
      sub_types.push(handle_type(cg, handle));
      sub_types.push(handle_type(cg, handle));
    }
    sub_types.push(cg.context.i64_type().into());
    for submodule_info in &self.submodules {
      sub_types.push(submodule_info.module.ir_type(cg).into_struct_type().into());
    }
    cg.context.struct_type(&sub_types, false).into()
  }
}

impl Nameable for ast::Module {
  fn fn_name(&self) -> String {
    self.name.clone() + "_update"
  }
}

pub fn codegen<'ctx>(context: &'ctx Context, constructor: &mut dyn CodegenStateConstructor<'ctx>, module: &ast::Module) -> CodegenResult<Vec<Module<'ctx>>> {
  let mut result = Vec::new();
  let cg = constructor.construct(context, &module.name);
  module_codegen(&cg, module)?;
  result.push(cg.module);
  for submodule in &module.submodules {
    let cg = constructor.construct(context, &submodule.module.name);
    module_codegen(&cg, &submodule.module)?;
    result.push(cg.module);
  }
  Ok(result)
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
  let function = cg.module.add_function(&module.fn_name(), function_type, None);

  let entry_block = cg.context.append_basic_block(function, "entry");
  cg.builder.position_at_end(entry_block);
  let state_alloca = state_alloca_for_module_function(cg, module, function);
  let state_ptr = cg.builder.build_load(state_alloca, "state_ptr").into_pointer_value();

  let bitfield_ptr = cg.module_bitfield_ptr(module, state_ptr)?;
  let bitfield = cg.builder.build_load(bitfield_ptr, "bitfield").into_int_value();

  for (idx, handle) in module.handles.iter().enumerate() {
    let test_value = cg.uint_const(1 << idx);
    let bitfield_has_test = cg.builder.build_and(bitfield, test_value, "bitfield_test");
    let has_listener = cg.builder.build_int_compare(IntPredicate::NE, bitfield_has_test, cg.uint_const(0), "has_listener");

    let activate_for_block = cg.context.append_basic_block(function, &("activate_for_".to_owned() + &handle.name));
    let after_listeners_block = cg.context.append_basic_block(function, &("after_".to_owned() + &handle.name));

    cg.builder.build_conditional_branch(has_listener, activate_for_block, after_listeners_block);

    cg.builder.position_at_end(activate_for_block);
    let write_ptr = cg.read_ptr_for_field(module, state_ptr, &handle.name)?;
    let update_ptr = cg.update_ptr_for_field(module, state_ptr, &handle.name, UpdatePtrPurpose::ReadAndClear)?;
    let value = update_ptr.load(cg, "value")?;
    value.store(cg, write_ptr)?;
    update_ptr.clear_update_pointer(cg)?;

    for listener in module.listeners.iter() {
      if listener.trigger == handle.name {
        let function_name = (module, listener).fn_name();
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


impl Nameable for (&ast::Module, &ast::Listener) {
  fn fn_name(&self) -> String {
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
  let listener_name = (module, listener).fn_name();
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

  match statement {
    ast::Statement::Output(output_statement) => {
      let return_value = expression_codegen(cg, module, &output_statement.expression, state_alloca)?;
      // statement.output == "" is a workaround for an effectful expression (e.g. a CopyToSubModule). This is an 'orrible 'ack and should
      // be reverted once it's possible to track effected fields from CopyToSubModule + return a fieldSet for update.
      if output_statement.output.len() > 0 {
        let state_ptr = cg.builder.build_load(state_alloca, "state_ptr");
        let update_ptr = cg.update_ptr_for_field(module, state_ptr.into_pointer_value(), &output_statement.output, UpdatePtrPurpose::WriteAndSet)?;
        return_value.store(cg, update_ptr)?;
      }
      cg.builder.build_return(Option::None);
    }
    ast::Statement::Block(statements) => {
      panic!("can't handle blocks yet");
    }
  }
 Ok(())
}

// Type inference it aint, but this'll do for now.
fn expression_type(module: &ast::Module, expression: &ast::Expression) -> CodegenResult<Vec<TypePrimitive>> {
  match expression {
    ast::Expression::IntLiteral(_) => Ok(vec!(TypePrimitive::Int)),
    ast::Expression::StringLiteral(_) => Ok(vec!(TypePrimitive::DynamicArrayOf(vec!(TypePrimitive::Char)))),
    ast::Expression::ArrayLookup(arr_exp, _) => {
      let arr_type = expression_type(module, arr_exp)?;
      if arr_type.len() != 1 {
        Err(CodegenError::TypeMismatch("array lookup on non-array".to_string()))
      } else if let TypePrimitive::FixedArrayOf(x, _) | TypePrimitive::DynamicArrayOf(x) = &arr_type[0] {
        Ok(x.clone())
      } else {
        Err(CodegenError::TypeMismatch("array lookup on non-array".to_string()))
      }
    }
    ast::Expression::CopyToSubModule(_) => Ok(Vec::new()),
    ast::Expression::Function(name, _) => {
      match name.as_str() {
        "new" => Ok(vec!(TypePrimitive::MemRegion)),
        "size" => Ok(vec!(TypePrimitive::Int)),
        _name => panic!("Don't know function {}", name)
      }
    }
    ast::Expression::ReferenceToState(field) => {
      let h_type = module.type_for_field(field).ok_or(CodegenError::BadReadFieldName)?;
      Ok(type_primitive_for_type(&h_type))
    }
    ast::Expression::Tuple(exprs) => {
      let tuple_contents = exprs.iter().map(|expr| expression_type(module, expr)).flatten().flatten().collect();
      Ok(vec!(TypePrimitive::PointerTo(tuple_contents)))
    }
  }
}

fn expression_codegen<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module, expression: &ast::Expression, state_alloca: PointerValue<'ctx>) -> CodegenResult<StateValue<'ctx>> {
  match expression {
    ast::Expression::ReferenceToState(field) => {
      let state_ptr = cg.builder.build_load(state_alloca, "state_ptr");
      let value_ptr = cg.read_ptr_for_field(module, state_ptr.into_pointer_value(), &field)?;
      value_ptr.load(cg, &("ref_to_state_".to_string() + field))
    }
    ast::Expression::CopyToSubModule(info) => {
      let state_ptr = cg.builder.build_load(state_alloca, "state_ptr").into_pointer_value();
      let from_value_ptr = cg.read_ptr_for_field(module, state_ptr, &info.state)?;
      let submodule_state_ptr = cg.submodule_ptr(module, state_ptr, info.submodule_index)?;

      let submodule_info = &module.submodules[info.submodule_index];
      let to_update_ptr = cg.update_ptr_for_field(&submodule_info.module, submodule_state_ptr, &info.submodule_state, UpdatePtrPurpose::WriteAndSet)?;
      let value = from_value_ptr.load(cg, "value")?;
      value.store(cg, to_update_ptr)?;

      let invoke_loop_start = flow_to_new_block(cg, "invoke_loop_start")?;

      invoke_submodule(cg, &submodule_info.module, submodule_state_ptr)?;

      let bitfield_ptr = cg.module_bitfield_ptr(&submodule_info.module, submodule_state_ptr)?;
      let bitfield = cg.builder.build_load(bitfield_ptr, "bitfield").into_int_value();
      let has_update = cg.builder.build_int_compare(IntPredicate::NE, bitfield, cg.uint_const(0), "has_update");
      let copy_back = append_new_block(cg, "copy_back")?;
      // (1) The code below goes here (at the end of invoke_loop_start, before copy_back)
      cg.builder.position_at_end(copy_back);

      for (idx, handle) in submodule_info.module.handles.iter().enumerate() {
        if handle.is_output() {
          maybe_copy_back_to_module(cg, module, state_ptr, submodule_info, submodule_state_ptr, bitfield, idx)?;
        }
      }

      cg.builder.build_unconditional_branch(invoke_loop_start);

      // this needs to be at the end..
      let updates_complete = append_new_block(cg, "updates_complete")?;
      // ..which means we can't do this until now, even though it belongs at (1)
      cg.builder.position_at_end(invoke_loop_start);
      cg.builder.build_conditional_branch(has_update, copy_back, updates_complete);
      cg.builder.position_at_end(updates_complete);      

      Ok(StateValue::new_none())
    }
    ast::Expression::Function(name, expression) => {
      let value = expression_codegen(cg, module, expression, state_alloca)?;
      match name.as_str() {
        "new" => {
          let size = value.into_int_value()?;
          let raw_location = malloc(cg, size.into(), "mem_region_location").into_pointer_value();
          Ok(StateValue::new_dynamic_mem_region_of_type(raw_location, size, vec!(TypePrimitive::MemRegion)))
        }
        "size" => {
          let value = expression_codegen(cg, module, expression, state_alloca)?;
          value.size(cg)
        }
        _name => {
          panic!("Don't know function {}", name);
        }
      }
    }
    ast::Expression::StringLiteral(literal) => {
      let size = cg.context.i64_type().const_int(literal.len().try_into().unwrap(), false);
      let string = cg.builder.build_global_string_ptr(literal, "literal");
      Ok(StateValue::new_dynamic_mem_region_of_type(string.as_pointer_value(), size, vec!(TypePrimitive::DynamicArrayOf(vec!(TypePrimitive::Char)))))
    }
    ast::Expression::IntLiteral(literal) => {
      Ok(StateValue::new_int(cg.uint_const(*literal as u64)))
    }
    ast::Expression::ArrayLookup(value, index) => {
      let arr_ptr = expression_codegen(cg, module, value, state_alloca)?;
      let idx = expression_codegen(cg, module, index, state_alloca)?;
      arr_ptr.array_lookup(cg, idx)
    }
    ast::Expression::Tuple(entries) => {
      // TODO: this won't deal with inlined tuples; will need to create a new StateValue type for those.
      let tuple_type = expression_type(module, expression)?;
      if let TypePrimitive::PointerTo(members) = &tuple_type[0] {
        let tuple_size = type_size(&members);
        let tuple_ptr = malloc(cg, cg.uint_const(tuple_size).into(), "tuple_memory").into_pointer_value();
        let tuple_llvm_type = llvm_type_for_primitive(cg, &tuple_type);
        let typed_tuple_ptr = cg.builder.build_bitcast(tuple_ptr, tuple_llvm_type, "ptr_as_struct_ptr").into_pointer_value();
        for (idx, entry) in entries.iter().enumerate() {
          let value = expression_codegen(cg, module, entry, state_alloca)?;
          let ptr = cg.builder.build_struct_gep(typed_tuple_ptr, idx as u32, "tuple_entry").or(Err(CodegenError::InvalidStructPointer("Bad tuple pointer".to_string())))?;
          // TODO: This should all be wrapped up in a StateValue for tuples maybe? Or something anyway.
          let ptr_type = expression_type(module, entry)?;
          let ptr_kind = pointer_kind_for_type_primitive(&ptr_type);
          let typed_ptr = StatePointer { pointer_kind: ptr_kind, pointer_type: ptr_type, pointer: ptr };
          value.store(cg, typed_ptr)?;
        }
        Ok(StateValue { value: ValueParts::StaticMemRegion(tuple_ptr), value_type: tuple_type })
      } else {
        panic!("Should not be possible");
      }
    }
  }
}

fn invoke_submodule<'ctx>(cg: &CodegenState<'ctx>, submodule: &ast::Module, submodule_state_ptr: PointerValue<'ctx>) -> CodegenStatus {
  let function_name = submodule.fn_name();
  let function = cg.module.get_function(&function_name).or_else(|| {
    let function_type = ir_listener_type(cg, submodule).into_function_type();
    Some(cg.module.add_function(&submodule.fn_name(), function_type, None))
  }).unwrap();
  cg.builder.build_call(function, &[submodule_state_ptr.into()], "_");
  Ok(())
}

fn malloc<'ctx>(cg: &CodegenState<'ctx>, size: BasicValueEnum<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
  let malloc = get_malloc(cg);
  cg.builder.build_call(malloc, &[size], name).try_as_basic_value().left().unwrap()
}

fn get_malloc<'ctx>(cg: &CodegenState<'ctx>) -> FunctionValue<'ctx> {
  cg.module.get_function("malloc").or_else(|| {
    let function_type = cg.context.i8_type().ptr_type(AddressSpace::Generic).fn_type(&[cg.context.i64_type().into()], false);
    Some(cg.module.add_function("malloc", function_type, None))
  }).unwrap()
}

fn maybe_copy_back_to_module<'ctx>(
  cg: &CodegenState<'ctx>, 
  module: &ast::Module,
  state_ptr: PointerValue<'ctx>, 
  submodule_info: &ast::ModuleInfo, 
  submodule_state_ptr: PointerValue<'ctx>, 
  bitfield: IntValue<'ctx>, 
  index: usize) -> CodegenStatus
{
  let mask = cg.uint_const(1 << index);
  let test = cg.builder.build_and(bitfield, mask, "test");
  let needs_copy = cg.builder.build_int_compare(IntPredicate::NE, test, cg.uint_const(0), "needs_copy");

  let do_copy = append_new_block(cg, "do_copy")?;
  let after_copy = append_new_block(cg, "after_copy")?;

  cg.builder.build_conditional_branch(needs_copy, do_copy, after_copy);

  cg.builder.position_at_end(do_copy);

  let src_name = &submodule_info.module.handles[index].name;
  let src_ptr = cg.update_ptr_for_field(&submodule_info.module, submodule_state_ptr, src_name, UpdatePtrPurpose::ReadWithoutClearing)?;

  let dest_name = &submodule_info.handle_map.get(src_name).unwrap();
  let dest_ptr = cg.update_ptr_for_field(module, state_ptr, dest_name, UpdatePtrPurpose::WriteAndSet)?;

  let value = src_ptr.load(cg, "value")?;
  value.store(cg, dest_ptr)?;

  cg.builder.build_unconditional_branch(after_copy);
  cg.builder.position_at_end(after_copy);

  Ok(())
}

fn append_new_block<'ctx>(cg: &CodegenState<'ctx>, block_name: &str) -> CodegenResult<BasicBlock<'ctx>> {
  let block = cg.builder.get_insert_block().ok_or(CodegenError::NotInABlock)?;
  let function = block.get_parent().ok_or(CodegenError::NotInAFunction)?;
  Ok(cg.context.append_basic_block(function, block_name))
}

fn flow_to_new_block<'ctx>(cg: &CodegenState<'ctx>, block_name: &str) -> CodegenResult<BasicBlock<'ctx>> {
  let new_block = append_new_block(cg, block_name)?;
  cg.builder.build_unconditional_branch(new_block);
  cg.builder.position_at_end(new_block);
  Ok(new_block)
}

#[cfg(test)]
mod tests {
  use super::*;
  use super::super::target_triple_and_machine;

  use inkwell::execution_engine::{JitFunction, ExecutionEngine};

  use super::super::parser;
  use super::super::graph_builder;
  use super::super::graph_to_module;

  use codegen_state::tests::*;

  use paste::paste;

  use std::ptr;

  fn test_module() -> ast::Module {
    ast::Module {
      name: String::from("TestModule"),
      handles: vec!(ast::Handle { name: String::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::Type::Int },
                    ast::Handle { name: String::from("far"), usages: vec!(ast::Usage::Read), h_type: ast::Type::Int },
                    ast::Handle { name: String::from("bar"), usages: vec!(ast::Usage::Write), h_type: ast::Type::Int }
                  ),
      listeners: vec!(ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, statement: ast::Statement::Output (
        ast::OutputStatement {
          output: String::from("bar"), expression: ast::Expression::ReferenceToState(String::from("far"))
        }
      )}),
      submodules: Vec::new(),
    }
  }

  fn invalid_module() -> ast::Module {
     ast::Module {
      name: String::from("InvalidModule"),
      handles: vec!(ast::Handle { name: String::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::Type::Int }),
      listeners: vec!(ast::Listener { trigger: String::from("invalid"), kind: ast::ListenerKind::OnChange, statement: ast::Statement::Output (
        ast::OutputStatement {
          output: String::from("foo"), expression: ast::Expression::ReferenceToState(String::from("foo"))
        })
      }),
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

  fn ee_for_string<F>(module: &str, func: F) -> CodegenStatus
      where F: FnOnce(ExecutionEngine) -> () {
    let context = Context::create();
    let mut jit_info = JitInfo::new();
    
    let (_, ast) = parser::parse(module).unwrap();
    if ast::modules(&ast).len() == 1 && ast::graphs(&ast).len() == 0 {
      codegen(&context, &mut jit_info, &ast::modules(&ast)[0])?;
    } else {
      let mut graph = graph_builder::make_graph(ast::graphs(&ast));
      graph_builder::resolve_graph(&ast::modules(&ast), &mut graph).unwrap();
      let main = graph_to_module::graph_to_module(&graph, &ast::modules(&ast), "Main").unwrap();
      codegen(&context, &mut jit_info, &main)?;
    }
    let ee = jit_info.execution_engine.unwrap();
    func(ee);
    Ok(())
  }

  #[derive(Debug)]
  #[repr(C)]
  pub struct MemRegion {
    data: u64, // TODO is there a better representation than this?
    size: u64,
  }

  impl MemRegion {
    fn empty() -> Self {
      MemRegion { data: 0, size: 0 }
    }
  }

  #[macro_export]
  macro_rules! state_struct {
    ( $name:ident, $($field_name:ident:$field_type:ty),* $(| $($module_field:ident:$module_type:ty),*)? ) => {
      paste! {
        #[derive(Debug)]
        #[repr(C)]
        pub struct [<$name State>] {
          $(
            pub $field_name: $field_type,
            pub [<$field_name _upd>]: $field_type,
          )*
          pub bitfield: u64,
          $( $(
            pub $module_field: $module_type,
          )* )?
        } 

        type [<$name Func>] = unsafe extern "C" fn(*mut [<$name State>]) -> ();
      }
    }
  }

  state_struct!(TestModule, foo: u64, far: u64, bar: u64);

  #[test]
  fn jit_module_codegen_runs() -> CodegenStatus {
    let context = Context::create();
    let mut jit_info = JitInfo::new();
    let cg = jit_info.construct(&context, "TestModule");
    let ee = jit_info.execution_engine.unwrap();

    let module = test_module();
    module_codegen(&cg, &module)?;
    unsafe {
      let function: JitFunction<TestModuleFunc> = ee.get_function("TestModule_update").unwrap();
      let mut state = TestModuleState { foo: 0, foo_upd: 10, bar: 0, bar_upd: 0, far: 20, far_upd: 0, bitfield: 0x1 };
      function.call(&mut state);
      assert_eq!(state.bitfield, 0x4);
      assert_eq!(state.bar_upd, 20);
    }

    Ok(())
  }

  static MULTI_MODULE_STRING: &str = "
module MyModule {
  foo: reads Int;
  bar: writes Int;

  foo.onChange: bar <- foo;
}

module MyModule2 {
  foo: reads Int;
  bar: writes Int;

  foo.onChange: bar <- foo;
}

MyModule -> MyModule2;
";

  state_struct!(NestedModule, foo: u64, bar: u64);
  state_struct!(MultiModule, foo: u64, bar: u64, h0: u64 | my_module:NestedModuleState, my_module2:NestedModuleState);


  #[test]
  fn jit_multi_module_codegen_runs() -> CodegenStatus {
    ee_for_string(MULTI_MODULE_STRING, |ee: ExecutionEngine| {
      unsafe {
        let function: JitFunction<MultiModuleFunc> = ee.get_function("Main_update").unwrap();
        let mut state = MultiModuleState { foo: 0, foo_upd: 10, h0: 0, h0_upd: 0, bar: 0, bar_upd: 0, bitfield: 0x1, 
                          my_module: NestedModuleState { foo: 0, foo_upd: 0, bar: 0, bar_upd: 0, bitfield: 0 },
                          my_module2: NestedModuleState { foo: 0, foo_upd: 0, bar: 0, bar_upd: 0, bitfield: 0 },
                        };

        // After a single call to update, foo_upd has been copied into my_module.foo_upd, then my_module has been
        // iterated until stable (foo_upd -> foo & bar_upd, bar_upd -> bar); new state has been copied back (h0_upd).
        // my_module2 has not been changed.
        function.call(&mut state);
        assert_eq!(state.bitfield, 0x4);
        assert_eq!(state.foo, 10);
        assert_eq!(state.foo_upd, 0);
        assert_eq!(state.h0_upd, 10);
        assert_eq!(state.my_module.foo, 10);
        assert_eq!(state.my_module.bar, 10);
        assert_eq!(state.my_module2.foo, 0);
        assert_eq!(state.my_module2.bar, 0);

        // After a second call to update, h0_upd has been copied into my_module2.foo_upd, then my_module2 has been
        // iterated until stable (foo_upd -> foo & bar_upd, bar_upd -> bar); new state has been copied back (bar_upd).
        // my_module has not been changed.
        function.call(&mut state);
        assert_eq!(state.bitfield, 0x2);
        assert_eq!(state.h0, 10);
        assert_eq!(state.h0_upd, 0);
        assert_eq!(state.bar_upd, 10);
        assert_eq!(state.my_module.foo, 10);
        assert_eq!(state.my_module.bar, 10);
        assert_eq!(state.my_module2.foo, 10);
        assert_eq!(state.my_module2.bar, 10);
      }
    })
  }

  static NEW_MEMREGION_TEST_STRING: &str = "
module ModuleWithNew {
  bar: writes MemRegion;
  foo: reads Int;

  foo.onChange: bar <- new(foo);
}
  ";

  state_struct!(ModuleWithNew, bar: MemRegion, foo: u64);

  #[test]
  fn jit_new_memregion_codegen_runs() -> CodegenStatus {
    ee_for_string(NEW_MEMREGION_TEST_STRING, |ee: ExecutionEngine| {
      unsafe {
        let function: JitFunction<ModuleWithNewFunc> = ee.get_function("ModuleWithNew_update").unwrap();
        let mut state = ModuleWithNewState { bar: MemRegion::empty(), bar_upd: MemRegion::empty(), foo: 0, foo_upd: 10, bitfield: 0x2 };
        function.call(&mut state);
        assert_eq!(state.bitfield, 0x1);
        assert_eq!(state.foo, 10);
        assert_eq!(state.foo_upd, 0);
        assert_eq!(state.bar_upd.size, 10);
        assert_ne!(state.bar_upd.data, 0);
      }
    })
  }

  static COPY_MEMREGION_TEST_STRING: &str = "
module Writer {
  size_in: reads Int;
  region: writes MemRegion;
  
  size_in.onChange: region <- new(size_in);
}

module Reader {
  region: reads MemRegion;
  size_out: writes Int;

  region.onChange: size_out <- size(region);
}

Writer -> Reader;
  ";

  state_struct!(Writer, size_in: u64, region: MemRegion);
  state_struct!(Reader, region: MemRegion, size_out: u64);
  state_struct!(CopyMemRegionMain, size_in: u64, size_out: u64, h0: MemRegion | writer: WriterState, reader: ReaderState);

  #[test]
  fn jit_copy_memregion_codegen_runs() -> CodegenStatus {
    ee_for_string(COPY_MEMREGION_TEST_STRING, |ee: ExecutionEngine| {
      unsafe {
        let function: JitFunction<CopyMemRegionMainFunc> = ee.get_function("Main_update").unwrap();  
        let mut state = CopyMemRegionMainState {
          size_in: 0, size_in_upd: 50, size_out: 0, size_out_upd: 0, h0: MemRegion::empty(), h0_upd: MemRegion::empty(), bitfield: 0x1,
          writer: WriterState { size_in: 0, size_in_upd: 0, region: MemRegion::empty(), region_upd: MemRegion::empty(), bitfield: 0},
          reader: ReaderState { size_out: 0, size_out_upd: 0, region: MemRegion::empty(), region_upd: MemRegion::empty(), bitfield: 0},
        };

        function.call(&mut state);
        assert_eq!(state.bitfield, 0x4);
        assert_eq!(state.h0_upd.size, 50);
        assert_ne!(state.h0_upd.data, 0);

        function.call(&mut state);
        assert_eq!(state.bitfield, 0x2);
        assert_eq!(state.size_out_upd, 50);
      }
    })
  }

  static STRING_TEST_STRING: &str = "
module CreateString {
  inp: reads Int;
  out: writes String;

  inp.onChange: out <- \"static string\";
}

module CharFromString {
  inp: reads String;
  out: writes Char;
  
  inp.onChange: out <- inp[5];
}

CreateString -> CharFromString;
  ";

  state_struct!(CreateString, inp: u64, out: MemRegion);
  state_struct!(CharFromString, inp: MemRegion, out: u8);
  state_struct!(StringTestMain, inp: u64, out: u8, h0: MemRegion | writer: CreateStringState, reader: CharFromStringState);

  #[test]
  fn jit_create_string_codegen_runs() -> CodegenStatus {
    ee_for_string(STRING_TEST_STRING, |ee: ExecutionEngine| {
      unsafe {
        let function: JitFunction<StringTestMainFunc> = ee.get_function("Main_update").unwrap();
        let mut state = StringTestMainState {
          inp: 0, inp_upd: 10, h0: MemRegion::empty(), h0_upd: MemRegion::empty(), out: 0, out_upd: 0, bitfield: 0x1,
          writer: CreateStringState { inp: 0, inp_upd: 0, out: MemRegion::empty(), out_upd: MemRegion::empty(), bitfield: 0x0},
          reader: CharFromStringState { inp: MemRegion::empty(), inp_upd: MemRegion::empty(), out: 0, out_upd: 0, bitfield: 0x0}
        };

        function.call(&mut state);
        function.call(&mut state);
        assert_eq!(state.out_upd, 'c' as u8);
      }
    })
  }

  static TUPLE_TEST_STRING: &str = "
module CreateTuple {
  inp: reads Int;
  out: writes (Int, String);

  inp.onChange: out <- (inp, \"static string\");
}

module ReadTuple {
  inp: reads (Int, String);
  out1: writes Int;
  out2: writes String;

  inp.onChange: {
    out1 <- inp[0];
    out2 <- inp[1];
  }
}

CreateTuple -> ReadTuple;
  ";

  state_struct!(CreateTuple, inp: u64, out: *const (u64, MemRegion));

  #[test]
  fn jit_create_tuple_codegen_runs() -> CodegenStatus {
    ee_for_string(TUPLE_TEST_STRING, |ee: ExecutionEngine| {
      unsafe {
        let function: JitFunction<CreateTupleFunc> = ee.get_function("CreateTuple_update").unwrap();
        let mut state = CreateTupleState {
          inp: 0, inp_upd: 10, out: ptr::null(), out_upd: ptr::null(), bitfield: 0x1
        };

        function.call(&mut state);
        println!("{:?}", state);
      }
    })
  }
}
