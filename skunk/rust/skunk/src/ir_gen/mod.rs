use inkwell::{AddressSpace, IntPredicate, };
use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, AnyTypeEnum, StructType};
use inkwell::values::{FunctionValue, PointerValue, BasicValueEnum, IntValue};

use super::ast;

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

fn handle_type<'ctx>(cg: &CodegenState<'ctx>, handle: &ast::Handle) -> BasicTypeEnum<'ctx> {
  match handle.h_type {
    ast::TypePrimitive::Int => cg.context.i64_type().into(),
    ast::TypePrimitive::MemRegion => MemRegionPointer::struct_ir_type(cg).into(),
    ast::TypePrimitive::String => panic!("we don't yet handle strings",)
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
    update_ptr.clear(cg)?;

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

  let return_value = expression_codegen(cg, module, &statement.expression, state_alloca)?;
  // statement.output == "" is a workaround for an effectful expression (e.g. a CopyToSubModule). This is an 'orrible 'ack and should
  // be reverted once it's possible to track effected fields from CopyToSubModule + return a fieldSet for update.
  if statement.output.len() > 0 {
    let state_ptr = cg.builder.build_load(state_alloca, "state_ptr");
    let update_ptr = cg.update_ptr_for_field(module, state_ptr.into_pointer_value(), &statement.output, UpdatePtrPurpose::WriteAndSet)?;
    return_value.store(cg, update_ptr)?;
  }
  cg.builder.build_return(Option::None);
  Ok(())
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

      Ok(StateValue::None)
    }
    ast::Expression::Function(name, expression) => {
      let value = expression_codegen(cg, module, expression, state_alloca)?;
      match name.as_str() {
        "new" => {
          let size = value.into();
          let raw_location = malloc(cg, size, "mem_region_location");
          Ok(StateValue::MemRegion(raw_location, size))
        }
        "size" => {
          let value = expression_codegen(cg, module, expression, state_alloca)?;
          // TODO: size should (maybe?) work on both state pointers to allocated memranges, and
          // directly computed memrange values. Probably requires being able to represent pointers
          // as values, and may not be useful.
          match value {
            StateValue::Integer(v) => Err(CodegenError::InvalidFunctionArgument("Can't call size() on Integer result".to_string())),
            StateValue::MemRegion(_data, size) => {
              Ok(StateValue::Integer(size))
            }
            StateValue::None => Err(CodegenError::InvalidFunctionArgument("Can't call size() on None result".to_string()))
          }
        }
        _name => {
          panic!("Don't know function {}", name);
        }
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

  fn ee_for_string<F>(module: &str, func: F) -> CodegenStatus
      where F: FnOnce(ExecutionEngine) -> () {
    let context = Context::create();
    let mut jit_info = JitInfo::new();
    
    let (_, ast) = parser::parse(module).unwrap();
    if ast::modules(&ast).len() == 1 && ast::graphs(&ast).len() == 0 {
      println!("here {:?}", &ast::modules(&ast));
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


}
