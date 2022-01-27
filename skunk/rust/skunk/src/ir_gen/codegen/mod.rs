use inkwell::{AddressSpace, IntPredicate, };
use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, AnyTypeEnum, StructType, BasicType};
use inkwell::values::{FunctionValue, PointerValue, BasicValueEnum, IntValue};

use super::ast;

use std::convert::TryInto;
use std::collections::hash_set::HashSet;

use super::codegen_state::*;
use super::state_values::*;

mod expression_codegen;
mod examples_codegen;
pub use examples_codegen::main_for_examples;
mod debug_codegen;
mod c_functions;

pub use expression_codegen::{expression_codegen, expression_logical_and, if_else_expression};
use examples_codegen::examples_codegen;
pub use debug_codegen::*;
pub use c_functions::*;

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

fn param_type<'ctx>(cg: &CodegenState<'ctx>, param: &ast::ValueParam) -> BasicTypeEnum<'ctx> {
  let primitive_type = type_primitive_for_type(&param.vp_type);
  llvm_type_for_primitive(cg, &primitive_type)
}

pub fn llvm_type_for_primitive<'ctx>(cg: &CodegenState<'ctx>, primitive_type: &Vec<TypePrimitive>) -> BasicTypeEnum<'ctx> {
  if primitive_type.len() != 1 {
    let element_types: Vec<BasicTypeEnum<'ctx>> = primitive_type.iter().map(|t| llvm_type_for_primitive(cg, &vec!(t.clone()))).collect(); 
    cg.context.struct_type(&element_types, false).into()
  } else {
    match &primitive_type[0] {
      TypePrimitive::Int => cg.context.i64_type().into(),
      TypePrimitive::Char => cg.context.i8_type().into(),
      TypePrimitive::Bool => cg.context.custom_width_int_type(1).into(),
      TypePrimitive::MemRegion => dptr_ir_type(cg, cg.context.i8_type().into()).into(),
      TypePrimitive::DynamicArrayOf(x) => dptr_ir_type(cg, llvm_type_for_primitive(cg, &x).into()).into(),
      TypePrimitive::PointerTo(x) => llvm_type_for_primitive(cg, x).ptr_type(AddressSpace::Generic).into(),
      TypePrimitive::FixedArrayOf(x, _s) => llvm_type_for_primitive(cg, x).ptr_type(AddressSpace::Generic).into(),
    }
  }
}

impl <'a> Typeable for ast::Module {
  fn ir_type<'ctx>(&self, cg: &CodegenState<'ctx>) -> AnyTypeEnum<'ctx> {
    let mut sub_types: Vec<BasicTypeEnum> = Vec::new();
    // a value & update for each handle
    for handle in &self.handles {
      sub_types.push(handle_type(cg, handle));
      sub_types.push(handle_type(cg, handle));
    }
    // a bitmap that describes which handles have updates
    // TODO: size this based on number of handles
    sub_types.push(cg.context.i64_type().into());

    // an optional field for tuple restructuring
    // TODO: size this based on number of tuples
    if self.tuples.len() > 0 {
      sub_types.push(cg.context.i64_type().into());
    }

    // a value for each param
    for value_param in &self.value_params {
      sub_types.push(param_type(cg, value_param));
    }
    for submodule_info in &self.submodules {
      sub_types.push(submodule_info.module.ir_type(cg).into_struct_type().into());
    }
    cg.context.struct_type(&sub_types, false).into()
  }
}

impl <'a> Nameable for ast::Module {
  fn fn_name(&self) -> String {
    self.name.clone() + "_update"
  }
}

pub fn codegen<'ctx>(context: &'ctx Context, constructor: &mut dyn CodegenStateConstructor<'ctx>, module: &'ctx ast::Module) -> CodegenResult<Vec<Module<'ctx>>> {
  let mut result = Vec::new();
  let mut cg = constructor.construct(context, &module.name);
  module_codegen(&mut cg, module)?;
  result.push(cg.module);
  let mut seen_names = HashSet::<String>::new();
  for submodule in &module.submodules {
    if seen_names.contains(&submodule.module.name) {
      continue;
    }
    seen_names.insert(submodule.module.name.clone());
    let mut cg = constructor.construct(context, &submodule.module.name);
    module_codegen(&mut cg, &submodule.module)?;
    result.push(cg.module);
  }
  Ok(result)
}

pub fn module_codegen<'ctx>(cg: &mut CodegenState<'ctx>, module: &'ctx ast::Module) -> CodegenStatus {
 module_init_codegen(cg, module)?;

  //module_deinit_codegen(cg, module)?;

  for listener in module.listeners.iter() {
    listener_codegen(cg, module, listener)?;
  }

  module_update_function(cg, module)?;

  // TODO: Gate producing this on test or debug modes
  debug_codegen(cg, module)?;

  // TODO: Gate producing this on test compilation mode
  examples_codegen(cg, module)
}

fn module_init_codegen<'ctx>(cg: &mut CodegenState<'ctx>, module: &'ctx ast::Module) -> CodegenStatus {
  let module_type = module.ir_type(cg).into_struct_type();
  let module_ptr_type = module_type.ptr_type(AddressSpace::Generic);
  let function_type = module_ptr_type.fn_type(&[], false);
  let function = cg.module.add_function(&format!("{}_init", module.name), function_type, None);

  let entry_block = cg.context.append_basic_block(function, "entry");
  cg.builder.position_at_end(entry_block);

  // allocate & clear space
  let module_size = module_type.size_of().unwrap();
  let module_size32 = cg.builder.build_int_cast(module_size, cg.context.i32_type(), "module_size32");
  let state_ptr_as_char_ptr = malloc(cg, module_size32.into(), "malloced-state").into_pointer_value();
  memset(cg, state_ptr_as_char_ptr, cg.context.i8_type().const_zero(), module_size32);
  let state_ptr = cg.builder.build_bitcast(state_ptr_as_char_ptr, module_ptr_type, "state_ptr").into_pointer_value();

  for submodule_idx in 0..module.submodules.len() {
    let submodule = &module.submodules[submodule_idx];
    // init submodule
    let submodule_init_fn_name = format!("{}_init", &submodule.module.name);
    let submodule_init_fn = cg.module.get_function(&submodule_init_fn_name).or_else(|| {
      let function_type = submodule.module.ir_type(cg).into_struct_type().ptr_type(AddressSpace::Generic).fn_type(&[], false);
      Some(cg.module.add_function(&submodule_init_fn_name, function_type, None))
    }).unwrap();
    let submodule_state = cg.builder.build_call(submodule_init_fn, &[], "ptr_to_submodule_state").try_as_basic_value().left().unwrap().into_pointer_value();
    let submodule_struct = cg.builder.build_load(submodule_state, "struct");
    let submodule_state_ptr = cg.submodule_ptr(module, state_ptr, submodule_idx)?;
    cg.builder.build_store(submodule_state_ptr, submodule_struct);

    // set generic params for submodule
    for idx in 0..submodule.params.params.len() {
      let param_expr = &submodule.params.params[idx];
      let field_info = &submodule.module.value_params[idx];
      let submodule_alloca = cg.builder.build_alloca(submodule.module.ir_type(cg).into_struct_type().ptr_type(AddressSpace::Generic), "alloca");
      let param_ptr = cg.read_ptr_for_field(&submodule.module, submodule_state_ptr, &field_info.name)?;
      // TODO: Should the expression run in the context of the parent module or the submodule? Parent module might almost make more sense.
      // The main purpose would be to let params be computed from parent module params. However, note that this would require
      // that params be set as a second pass.
      let result = expression_codegen(cg, &submodule.module, submodule_alloca, &param_expr.value)?;
      result.store(cg, &param_ptr)?;
    }
  }

  cg.builder.build_return(Some(&state_ptr));
  Ok(())  
}

fn module_update_function<'ctx, 'a>(cg: &CodegenState<'ctx>, module: &ast::Module) -> CodegenStatus {
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
    value.store(cg, &write_ptr)?;
    update_ptr.clear_update_pointer(cg)?;

    for listener in module.listeners.iter() {
      if listener.trigger == *handle.name {
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


impl <'a> Nameable for (&ast::Module, &ast::Listener) {
  fn fn_name(&self) -> String {
    self.0.name.clone() + "__" + self.1.kind.to_string() + "__" + &self.1.trigger
  }
}

fn ir_listener_type<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module) -> AnyTypeEnum<'ctx> {
    AnyTypeEnum::FunctionType(cg.context.void_type().fn_type(&[module.ir_type(cg).into_struct_type().ptr_type(AddressSpace::Generic).into()], false))
}

impl <'a> Typeable for (&ast::Module, &ast::Listener) {
  fn ir_type<'ctx>(&self, cg: &CodegenState<'ctx>) -> AnyTypeEnum<'ctx> {
    ir_listener_type(cg, self.0)
  }
}

fn listener_codegen<'ctx>(cg: &mut CodegenState<'ctx>, module: &ast::Module, listener: &'ctx ast::Listener) -> CodegenStatus {
  let function_type = (module, listener).ir_type(cg).into_function_type();
  let listener_name = (module, listener).fn_name();
  let function = cg.module.add_function(&listener_name, function_type, None);

  listener_body_codegen(cg, module, &listener.implementation, function)
}

fn state_alloca_for_module_function<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module, function: FunctionValue<'ctx>) -> PointerValue<'ctx> {
  let state_ptr_type = module.ir_type(cg).into_struct_type().ptr_type(AddressSpace::Generic);
  let state_alloca = cg.builder.build_alloca(state_ptr_type, "state_alloca");
  cg.builder.build_store(state_alloca, function.get_first_param().unwrap().into_pointer_value());
  state_alloca
}

fn listener_body_codegen<'ctx>(cg: &mut CodegenState<'ctx>, module: &ast::Module, implementation: &'ctx ast::ExpressionValue, function: FunctionValue<'ctx>) -> CodegenStatus {
  let entry_block = cg.context.append_basic_block(function, "entry");
  cg.builder.position_at_end(entry_block);
  let state_alloca = state_alloca_for_module_function(cg, module, function);
  expression_codegen(cg, module, state_alloca, implementation)?;
  cg.builder.build_return(Option::None);
  Ok(())
}

// Type inference it aint, but this'll do for now.
fn expression_type<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module, expression: &ast::ExpressionValue) -> CodegenResult<Vec<TypePrimitive>> {
  match &expression.info {
    ast::ExpressionValueEnum::Output(output_expression) => expression_type(cg, module, output_expression.expression.as_ref()),
    ast::ExpressionValueEnum::Block(expressions) => {
      if expressions.len() > 0 {
        expression_type(cg, module, &expressions[expressions.len() - 1])
      } else {
        Ok(Vec::new())
      }
    }
    ast::ExpressionValueEnum::Let(let_expression) => {
      expression_type(cg, module, let_expression.expression.as_ref())
    }
    ast::ExpressionValueEnum::If(if_expression) => todo!("Implement if expression typing"),

    ast::ExpressionValueEnum::Empty => Ok(Vec::new()),
    //TODO: This is wrong if while loops have non-none type.
    ast::ExpressionValueEnum::Break => Ok(Vec::new()),

    ast::ExpressionValueEnum::IntLiteral(_) => Ok(vec!(TypePrimitive::Int)),
    ast::ExpressionValueEnum::StringLiteral(_) => Ok(vec!(TypePrimitive::DynamicArrayOf(vec!(TypePrimitive::Char)))),
    ast::ExpressionValueEnum::CharLiteral(_) => Ok(vec!(TypePrimitive::Char)),
    ast::ExpressionValueEnum::ArrayLookup(arr_exp, _) => {
      let arr_type = expression_type(cg, module, arr_exp.as_ref())?;
      if arr_type.len() != 1 {
        Err(CodegenError::TypeMismatch("array lookup on non-array".to_string()))
      } else if let TypePrimitive::FixedArrayOf(x, _) | TypePrimitive::DynamicArrayOf(x) = &arr_type[0] {
        Ok(x.clone())
      } else {
        Err(CodegenError::TypeMismatch("array lookup on non-array".to_string()))
      }
    }
    ast::ExpressionValueEnum::CopyToSubModule(_) => Ok(Vec::new()),
    ast::ExpressionValueEnum::FunctionCall(name, _) => {
      match name.as_str() {
        "new" => Ok(vec!(TypePrimitive::MemRegion)),
        "size" => Ok(vec!(TypePrimitive::Int)),
        _name => panic!("Don't know function {}", name)
      }
    }
    ast::ExpressionValueEnum::ReferenceToState(field) => {
      if let Some(local) = cg.get_local(&field) {
        Ok(local.value_type.clone())
      } else {
        let h_type = module.type_for_field(&field).unwrap(); //ok_or(CodegenError::BadReadFieldName(field.clone()))?;
        Ok(type_primitive_for_type(&h_type))
      }
    }
    ast::ExpressionValueEnum::Tuple(exprs) => {
      let tuple_contents = exprs.iter().map(|expr| expression_type(cg, module, expr)).flatten().flatten().collect();
      let size = type_size(&tuple_contents);
      if size <= 16 {
        Ok(tuple_contents)
      } else {
        Ok(vec!(TypePrimitive::PointerTo(tuple_contents)))
      }
    }
    ast::ExpressionValueEnum::TupleLookup(expr, pos) => {
      let tuple_type = expression_type(cg, module, expr.as_ref())?;
      if tuple_type.len() == 1 {
        if let TypePrimitive::PointerTo(tuple_type) = &tuple_type[0] {
          Ok(vec!(tuple_type[*pos as usize].clone()))
        } else {
          Err(CodegenError::TypeMismatch("non-pointer tuple field of size 1".to_string()))
        }
      } else {
        todo!("Implement type lookup by field # for inline tuples")
      }
    }
    ast::ExpressionValueEnum::BinaryOperator(lhs, op, rhs) => {
      match op {
        ast::Operator::Add => {
          let lhs_type = expression_type(cg, module, lhs)?;
          let rhs_type = expression_type(cg, module, rhs)?;
          if lhs_type != rhs_type {
            Err(CodegenError::TypeMismatch(format!("Can't add {:?} to {:?}", lhs_type, rhs_type)))
          } else {
            Ok(lhs_type)
          }
        }
        _ => todo!("Implement binary operation typing for {:?}", op)
      }
    }    
    ast::ExpressionValueEnum::While(while_expr) => todo!("Implement while typing"),
    _ => todo!("Need to implement typing for {:?}", expression.info),
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

pub fn debug_str<'ctx>(cg: &CodegenState<'ctx>, s: &str) {
  let printf = get_printf(cg);
  let sk_str = cg.builder.build_global_string_ptr(&("skunk> ".to_string() + s), "sk_str").as_pointer_value();
  cg.builder.build_call(printf, &[sk_str.into()], "_");
}

pub fn debug<'ctx>(cg: &mut CodegenState<'ctx>, s: StateValue<'ctx>) -> CodegenStatus {
  let mut ds = DebugState::new(cg, false);
  s.debug(cg, &mut ds)?;
  ds.print_to_console(cg);
  Ok(())
}

fn maybe_copy_back_to_module<'ctx, 'a>(
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

  let dest_name = &submodule_info.handle_map.get(&src_name.to_string()).unwrap();
  let dest_ptr = cg.update_ptr_for_field(module, state_ptr, dest_name, UpdatePtrPurpose::WriteAndSet)?;

  let value = src_ptr.load(cg, "value")?;
  value.store(cg, &dest_ptr)?;

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
mod codegen_string_tests;

#[cfg(test)]
mod tests {
  use super::*;
  use super::super::super::target_triple_and_machine;


  use super::super::super::ast::Expr;
  use super::super::super::parser;

  pub fn test_module<'a>() -> ast::Module {
    ast::Module::create( 
      "TestModule",
      vec!(
        ast::Handle { position: ast::SafeSpan { offset: 0, line: 1 }, name: "foo".to_string(), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::Type::Int },
        ast::Handle { position: ast::SafeSpan { offset: 0, line: 1 }, name: "far".to_string(), usages: vec!(ast::Usage::Read), h_type: ast::Type::Int },
        ast::Handle { position: ast::SafeSpan { offset: 0, line: 1 }, name: "bar".to_string(), usages: vec!(ast::Usage::Write), h_type: ast::Type::Int }
      ),
      vec!(
        ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, implementation: 
          ast::Expression::output(ast::SafeSpan { offset: 0, line: 1 }, "bar", ast::Expression::state_reference(ast::SafeSpan { offset: 0, line: 1 }, "far"), false).value,
        }
      ),
      Vec::new(),
      ast::Examples { examples: Vec::new() },
      Vec::new(),
      Vec::new(),
    )
  }

  fn invalid_module<'a>() -> ast::Module {
     ast::Module::create(
      "InvalidModule",
      vec!(ast::Handle { position: ast::SafeSpan { offset: 0, line: 1 }, name: "foo".to_string(), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::Type::Int }),
      vec!(ast::Listener { trigger: String::from("invalid"), kind: ast::ListenerKind::OnChange, implementation:
        ast::Expression::output(ast::SafeSpan { offset: 0, line: 1 }, "foo", ast::Expression::state_reference(ast::SafeSpan { offset: 0, line: 1 }, "foo"), false).value,
      }),
      Vec::new(),
      ast::Examples { examples: Vec::new() },
      Vec::new(),
      Vec::new(),
    )
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
    let module = test_module();
    let mut cs = CodegenState::new(&context, &target_machine, &target_triple, "TestModule");
    assert_eq!(module_codegen(&mut cs, &module), Ok(()))
  }

  #[test]
  fn invalid_module_codegen_fails() {
    let context = Context::create();
    let (target_triple, target_machine) = target_triple_and_machine();
    let module = invalid_module();
    let mut cs = CodegenState::new(&context, &target_machine, &target_triple, "InvalidModule");
    assert_eq!(module_codegen(&mut cs, &module), Err(CodegenError::BadListenerTrigger))
  }

  #[test]
  fn expression_types() -> CodegenStatus {
    let (_, ast) = parser::parse("
module ExpressionTypes {
  i_tuple: reads (String, Int);
  offset: reads Int;
}
    ").unwrap();
    let modules = ast::modules(&ast);
    let module = modules[0];

    let context = Context::create();
    let (target_triple, target_machine) = target_triple_and_machine();
    let cg = CodegenState::new(&context, &target_machine, &target_triple, "InvalidModule");
    let t = expression_type(&cg, module, &Expr::tuple(0, 0, vec!(Expr::sref(0, 0, "i_tuple").tuple_ref(0, 0, 0), Expr::sref(0, 0, "offset"))).build())?;
    let u = expression_type(&cg, module, &Expr::sref(0, 0, "i_tuple").build())?;
    let v = expression_type(&cg, module, &Expr::sref(0, 0, "i_tuple").tuple_ref(0, 0, 0).build())?;
    dbg!(t, u, v);
    Ok(())
  }
}
