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
      TypePrimitive::Bool => cg.context.custom_width_int_type(1).into(),
      TypePrimitive::MemRegion => dptr_ir_type(cg, cg.context.i8_type().into()).into(),
      TypePrimitive::DynamicArrayOf(x) => dptr_ir_type(cg, llvm_type_for_primitive(cg, &x).into()).into(),
      TypePrimitive::PointerTo(x) => llvm_type_for_primitive(cg, x).ptr_type(AddressSpace::Generic).into(),
      TypePrimitive::FixedArrayOf(x, _s) => llvm_type_for_primitive(cg, x).ptr_type(AddressSpace::Generic).into(),
    }
  }
}

impl <'a> Typeable for ast::Module<'a> {
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

impl <'a> Nameable for ast::Module<'a> {
  fn fn_name(&self) -> String {
    self.name.clone() + "_update"
  }
}

pub fn codegen<'ctx>(context: &'ctx Context, constructor: &mut dyn CodegenStateConstructor<'ctx>, module: &'ctx ast::Module<'ctx>) -> CodegenResult<Vec<Module<'ctx>>> {
  let mut result = Vec::new();
  let mut cg = constructor.construct(context, &module.name);
  module_codegen(&mut cg, module)?;
  result.push(cg.module);
  for submodule in &module.submodules {
    let mut cg = constructor.construct(context, &submodule.module.name);
    module_codegen(&mut cg, &submodule.module)?;
    result.push(cg.module);
  }
  Ok(result)
}

pub fn module_codegen<'ctx>(cg: &mut CodegenState<'ctx>, module: &'ctx ast::Module<'ctx>) -> CodegenStatus {
  for listener in module.listeners.iter() {
    listener_codegen(cg, module, listener)?;
  }

  module_update_function(cg, module)
}

fn module_update_function<'ctx, 'a>(cg: &CodegenState<'ctx>, module: &ast::Module<'a>) -> CodegenStatus {
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
      if listener.trigger == *handle.name.fragment() {
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


impl <'a> Nameable for (&ast::Module<'a>, &ast::Listener<'a>) {
  fn fn_name(&self) -> String {
    self.0.name.clone() + "__" + self.1.kind.to_string() + "__" + &self.1.trigger
  }
}

fn ir_listener_type<'ctx>(cg: &CodegenState<'ctx>, module: &ast::Module) -> AnyTypeEnum<'ctx> {
    AnyTypeEnum::FunctionType(cg.context.void_type().fn_type(&[module.ir_type(cg).into_struct_type().ptr_type(AddressSpace::Generic).into()], false))
}

impl <'a> Typeable for (&ast::Module<'a>, &ast::Listener<'a>) {
  fn ir_type<'ctx>(&self, cg: &CodegenState<'ctx>) -> AnyTypeEnum<'ctx> {
    ir_listener_type(cg, self.0)
  }
}

fn listener_codegen<'ctx>(cg: &mut CodegenState<'ctx>, module: &ast::Module<'ctx>, listener: &'ctx ast::Listener<'ctx>) -> CodegenStatus {
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

fn listener_body_codegen<'ctx>(cg: &mut CodegenState<'ctx>, module: &ast::Module<'ctx>, implementation: &'ctx ast::ExpressionValue<'ctx>, function: FunctionValue<'ctx>) -> CodegenStatus {
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
        let h_type = module.type_for_field(&field).ok_or(CodegenError::BadReadFieldName(field.clone()))?;
        Ok(type_primitive_for_type(&h_type))
      }
    }
    ast::ExpressionValueEnum::Tuple(exprs) => {
      let tuple_contents = exprs.iter().map(|expr| expression_type(cg, module, expr)).flatten().flatten().collect();
      Ok(vec!(TypePrimitive::PointerTo(tuple_contents)))
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
    ast::ExpressionValueEnum::BinaryOperator(lhs, op, rhs) => todo!("Implement binary operation typing"),
    ast::ExpressionValueEnum::While(while_expr) => todo!("Implement while typing"),
  }
}

fn expression_codegen<'ctx>(cg: &mut CodegenState<'ctx>, module: &ast::Module<'ctx>, state_alloca: PointerValue<'ctx>, expression: &'ctx ast::ExpressionValue<'ctx>) -> CodegenResult<StateValue<'ctx>> {
  let old_considering = cg.considering;
  cg.considering = Some(expression);
  let result = match &expression.info {
    ast::ExpressionValueEnum::Output(output_expression) => {
      let return_value = expression_codegen(cg, module, state_alloca, &output_expression.expression)?;
      // expression.output == "" is a workaround for an effectful subexpression (e.g. a CopyToSubModule). This is an 'orrible 'ack and should
      // be reverted once it's possible to track effected fields from CopyToSubModule + return a fieldSet for update.
      if output_expression.output.len() > 0 {
        let state_ptr = cg.builder.build_load(state_alloca, "state_ptr");
        let update_ptr = cg.update_ptr_for_field(module, state_ptr.into_pointer_value(), &output_expression.output, UpdatePtrPurpose::WriteAndSet)?;
        return_value.store(cg, &update_ptr)?;
      }
      if output_expression.and_return {
        cg.builder.build_return(None);
        let junk_block = append_new_block(cg, "junk_block")?;
        cg.builder.position_at_end(junk_block);
      }
      Ok(return_value)
    }
    ast::ExpressionValueEnum::Block(expressions) => {
      let mut final_result = StateValue::new_none();
      for expression in expressions {
        final_result = expression_codegen(cg, module, state_alloca, &expression)?;
      }
      Ok(final_result)
    }
    ast::ExpressionValueEnum::Let(let_expression) => {
      let value = expression_codegen(cg, module, state_alloca, &let_expression.expression)?;
      cg.add_local(&let_expression.var_name, value.clone())?;
      Ok(value)
    }

    ast::ExpressionValueEnum::If(if_expression) => {
      let test = expression_codegen(cg, module, state_alloca, if_expression.test.as_ref())?;
      // TODO: test should be forced to be a boolean expression rather than just something that can be intified.
      let test_as_int = test.into_int_value()?;
      let test_against = test_as_int.get_type().const_zero();
      let cmp = cg.builder.build_int_compare(IntPredicate::NE, test_as_int, test_against, "if_true");
      let if_true = append_new_block(cg, "if_true_block")?;
      let if_false = append_new_block(cg, "if_false_block")?;
      let after_if = append_new_block(cg, "after_if")?;
      cg.builder.build_conditional_branch(cmp, if_true, if_false);
      cg.builder.position_at_end(if_true);
      let result_if_true = expression_codegen(cg, module, state_alloca, if_expression.if_true.as_ref())?;
      cg.builder.build_unconditional_branch(after_if);
      cg.builder.position_at_end(if_false);
      let result_if_false = expression_codegen(cg, module, state_alloca, if_expression.if_false.as_ref())?;
      cg.builder.build_unconditional_branch(after_if);
      cg.builder.position_at_end(after_if);

      if result_if_true.is_none() {
        Ok(result_if_true)
      } else {
        let phi = cg.builder.build_phi(result_if_true.llvm_type(), "if_result");
        result_if_true.add_to_phi_node(phi, if_true)?;
        result_if_false.add_to_phi_node(phi, if_false)?;
        Ok(StateValue::new_int(phi.as_basic_value().into_int_value()))
      }
    }

    ast::ExpressionValueEnum::While(while_expression) => {
      let while_condition = flow_to_new_block(cg, "while_condition")?;
      let test = expression_codegen(cg, module, state_alloca, while_expression.test.as_ref())?.into_int_value()?;
      let while_body = append_new_block(cg, "while_body")?;
      let after_while = append_new_block(cg, "after_while")?;
      let cmp = cg.builder.build_int_compare(IntPredicate::NE, test, test.get_type().const_zero(), "while_true");
      cg.builder.build_conditional_branch(cmp, while_body, after_while);
      cg.builder.position_at_end(while_body);
      cg.break_target.push(after_while);
      expression_codegen(cg, module, state_alloca, while_expression.body.as_ref())?;
      cg.break_target.pop();
      cg.builder.build_unconditional_branch(while_condition);
      cg.builder.position_at_end(after_while);
      // TODO: work out whether while loops should have non-none return type
      Ok(StateValue::new_none())
    }

    ast::ExpressionValueEnum::Empty => Ok(StateValue::new_none()),

    ast::ExpressionValueEnum::Break => {
      cg.builder.build_unconditional_branch(*cg.break_target.last().ok_or(CodegenError::NakedBreak)?);
      let junk_block = append_new_block(cg, "junk_block")?;
      cg.builder.position_at_end(junk_block);
      Ok(StateValue::new_suppress())
    }

    ast::ExpressionValueEnum::ReferenceToState(field) => {
      if let Some(local) = cg.get_local(&field) {
        Ok(local)
      } else {
        let state_ptr = cg.builder.build_load(state_alloca, "state_ptr");
        let value_ptr = cg.read_ptr_for_field(module, state_ptr.into_pointer_value(), &field)?;
        value_ptr.load(cg, &("ref_to_state_".to_string() + &field))
      }
    }
    ast::ExpressionValueEnum::CopyToSubModule(info) => {
      let state_ptr = cg.builder.build_load(state_alloca, "state_ptr").into_pointer_value();
      let from_value_ptr = cg.read_ptr_for_field(module, state_ptr, &info.state)?;
      let submodule_state_ptr = cg.submodule_ptr(module, state_ptr, info.submodule_index)?;

      let submodule_info = &module.submodules[info.submodule_index];
      let to_update_ptr = cg.update_ptr_for_field(&submodule_info.module, submodule_state_ptr, &info.submodule_state, UpdatePtrPurpose::WriteAndSet)?;
      let value = from_value_ptr.load(cg, "value")?;
      value.store(cg, &to_update_ptr)?;

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
    ast::ExpressionValueEnum::FunctionCall(name, expression) => {
      let value = expression_codegen(cg, module, state_alloca, &expression)?;
      match name.as_str() {
        "new" => {
          let size = value.into_int_value()?;
          let raw_location = malloc(cg, size.into(), "mem_region_location").into_pointer_value();
          Ok(StateValue::new_dynamic_mem_region_of_type(raw_location, size, vec!(TypePrimitive::MemRegion)))
        }
        "size" => {
          let value = expression_codegen(cg, module, state_alloca, &expression)?;
          value.size(cg)
        }
        "dump" => {
          let value = expression_codegen(cg, module, state_alloca, &expression)?;
          debug(cg, value)?;
          Ok(StateValue::new_none())
        }
        _name => {
          panic!("Don't know function {}", name);
        }
      }
    }
    ast::ExpressionValueEnum::StringLiteral(literal) => {
      let size = cg.context.i64_type().const_int(literal.len().try_into().unwrap(), false);
      let string = cg.builder.build_global_string_ptr(&literal, "literal");
      Ok(StateValue::new_dynamic_mem_region_of_type(string.as_pointer_value(), size, vec!(TypePrimitive::DynamicArrayOf(vec!(TypePrimitive::Char)))))
    }
    ast::ExpressionValueEnum::IntLiteral(literal) => {
      Ok(StateValue::new_int(cg.uint_const(*literal as u64)))
    }
    ast::ExpressionValueEnum::CharLiteral(literal) => {
      Ok(StateValue::new_char(cg.context.i8_type().const_int(*literal as u64, false)))
    }
    ast::ExpressionValueEnum::ArrayLookup(value, index) => {
      let arr_ptr = expression_codegen(cg, module, state_alloca, &value)?;
      let idx = expression_codegen(cg, module, state_alloca, &index)?;
      arr_ptr.array_lookup(cg, idx)
    }
    ast::ExpressionValueEnum::Tuple(entries) => {
      // TODO: this won't deal with inlined tuples; will need to create a new StateValue type for those.
      let tuple_type = expression_type(cg, module, expression)?;
      let tuple_ptr = StateValue::new_tuple(cg, tuple_type)?;
      for (idx, entry) in entries.iter().enumerate() {
        let value = expression_codegen(cg, module, state_alloca, entry)?;
        tuple_ptr.set_tuple_index(cg, idx as u32, value)?;
      }
      Ok(tuple_ptr)
    },
    ast::ExpressionValueEnum::TupleLookup(tuple_expr, pos) => {
      let tuple = expression_codegen(cg, module, state_alloca, &tuple_expr)?;
      tuple.get_tuple_index(cg, *pos as u32)
    },
    ast::ExpressionValueEnum::BinaryOperator(lhs, op, rhs) => {
      if op.is_logical() {
        match op {
          ast::Operator::LogicalOr => {
            let lhs_value = expression_codegen(cg, module, state_alloca, &lhs)?.into_int_value()?;
            let current_block = cg.builder.get_insert_block().unwrap();
            let lhs_false = append_new_block(cg, "lhs_false")?;
            let end_of_test = append_new_block(cg, "end_of_test")?;
            cg.builder.build_conditional_branch(lhs_value, end_of_test, lhs_false);
            cg.builder.position_at_end(lhs_false);
            let rhs_value = expression_codegen(cg, module, state_alloca, &rhs)?.into_int_value()?;
            cg.builder.build_unconditional_branch(end_of_test);

            cg.builder.position_at_end(end_of_test);
            let phi = cg.builder.build_phi(lhs_value.get_type(), "logical-or-result");
            phi.add_incoming(&[(&lhs_value, current_block), (&rhs_value, lhs_false)]);
            Ok(StateValue::new_bool(phi.as_basic_value().into_int_value()))
          }
          ast::Operator::LogicalAnd => {
            let lhs_value = expression_codegen(cg, module, state_alloca, &lhs)?.into_int_value()?;
            let current_block = cg.builder.get_insert_block().unwrap();
            let lhs_true = append_new_block(cg, "lhs_true")?;
            let end_of_test = append_new_block(cg, "end_of_test")?;
            cg.builder.build_conditional_branch(lhs_value, lhs_true, end_of_test);
            cg.builder.position_at_end(lhs_true);
            let rhs_value = expression_codegen(cg, module, state_alloca, &rhs)?.into_int_value()?;
            cg.builder.build_unconditional_branch(end_of_test);

            cg.builder.position_at_end(end_of_test);
            let phi = cg.builder.build_phi(lhs_value.get_type(), "logical-and-result");
            phi.add_incoming(&[(&lhs_value, current_block), (&rhs_value, lhs_true)]);
            Ok(StateValue::new_bool(phi.as_basic_value().into_int_value()))
          }
          _ => todo!("Operator {:?} not yet implemented", op)
        }
      } else {
        let lhs_value = expression_codegen(cg, module, state_alloca, &lhs)?;
        let rhs_value = expression_codegen(cg, module, state_alloca, &rhs)?;
        match op {
          ast::Operator::Equality => lhs_value.equals(cg, &rhs_value),
          ast::Operator::LessThan => lhs_value.less_than(cg, &rhs_value),
          ast::Operator::GreaterThan => lhs_value.greater_than(cg, &rhs_value),
          ast::Operator::LessThanOrEqual => lhs_value.less_than_or_equal(cg, &rhs_value),
          ast::Operator::GreaterThanOrEqual => lhs_value.greater_than_or_equal(cg, &rhs_value),
          ast::Operator::Add => lhs_value.add(cg, &rhs_value),
          ast::Operator::Subtract => lhs_value.subtract(cg, &rhs_value),
          ast::Operator::Multiply => lhs_value.multiply(cg, &rhs_value),
          _ => todo!("Operator {:?} not yet implemented", op)
        }
      }
    }
  };
  cg.considering = old_considering;
  result
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

fn debug<'ctx>(cg: &CodegenState<'ctx>, s: StateValue<'ctx>) -> CodegenStatus {
  let printf = get_printf(cg);
  s.debug(cg, printf);
  Ok(())
}

fn get_printf<'ctx>(cg: &CodegenState<'ctx>) -> FunctionValue<'ctx> {
  cg.module.get_function("printf").or_else(|| {
    let function_type = cg.context.i32_type().fn_type(&[cg.context.i8_type().ptr_type(AddressSpace::Generic).into()], true);
    Some(cg.module.add_function("printf", function_type, None))
  }).unwrap()
}

fn maybe_copy_back_to_module<'ctx, 'a>(
  cg: &CodegenState<'ctx>, 
  module: &ast::Module<'a>,
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

  let src_name = submodule_info.module.handles[index].name.fragment();
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
mod tests {
  use super::*;
  use super::super::target_triple_and_machine;

  use inkwell::execution_engine::{JitFunction, ExecutionEngine};

  use super::super::ast::Expr;
  use super::super::parser;
  use super::super::graph_builder;
  use super::super::graph_to_module;

  use codegen_state::tests::*;

  use paste::paste;

  use std::ptr;

  fn test_module<'a>() -> ast::Module<'a> {
    ast::Module {
      name: String::from("TestModule"),
      handles: vec!(ast::Handle { position: ast::Span::from(""), name: ast::Span::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::Type::Int },
                    ast::Handle { position: ast::Span::from(""), name: ast::Span::from("far"), usages: vec!(ast::Usage::Read), h_type: ast::Type::Int },
                    ast::Handle { position: ast::Span::from(""), name: ast::Span::from("bar"), usages: vec!(ast::Usage::Write), h_type: ast::Type::Int }
                  ),
      listeners: vec!(ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, implementation: 
        ast::Expression::output(ast::Span::new(""), "bar", ast::Expression::state_reference(ast::Span::new(""), "far"), false).value,
      }),
      submodules: Vec::new(),
    }
  }

  fn invalid_module<'a>() -> ast::Module<'a> {
     ast::Module {
      name: String::from("InvalidModule"),
      handles: vec!(ast::Handle { position: ast::Span::from(""), name: ast::Span::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::Type::Int }),
      listeners: vec!(ast::Listener { trigger: String::from("invalid"), kind: ast::ListenerKind::OnChange, implementation:
        ast::Expression::output(ast::Span::new(""), "foo", ast::Expression::state_reference(ast::Span::new(""), "foo"), false).value,
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

  fn ee_for_string<F>(module: &str, func: F) -> CodegenStatus
      where F: FnOnce(ExecutionEngine) -> () {
    let context = Context::create();
    let (_, ast) = parser::parse(module).unwrap();
    
    if ast::modules(&ast).len() == 1 && ast::graphs(&ast).len() == 0 {
      let mut jit_info = JitInfo::new();
      let modules = codegen(&context, &mut jit_info, &ast::modules(&ast)[0])?;
      let ee = jit_info.execution_engine.unwrap();
      func(ee);
      // modules[0].print_to_stderr();
    } else {
      let mut graph = graph_builder::make_graph(ast::graphs(&ast));
      graph_builder::resolve_graph(&ast::modules(&ast), &mut graph).unwrap();
      let modules = ast::modules(&ast);
      let main = graph_to_module::graph_to_module(&graph, &modules, "Main").unwrap();
      let mut jit_info = JitInfo::new();
      codegen(&context, &mut jit_info, &main)?;
      let ee = jit_info.execution_engine.unwrap();
      func(ee);
    }
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
    fn from_str(data: &'static str) -> Self {
      let size = data.len() as u64;
      MemRegion { data: data.as_ptr() as u64, size }
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
    let module = test_module();
    let mut jit_info = JitInfo::new();
    let mut cg = jit_info.construct(&context, "TestModule");
    let ee = jit_info.execution_engine.unwrap();

    module_codegen(&mut cg, &module)?;
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
    out1 <- inp.0;
    out2 <- inp.1;
  }
}

CreateTuple -> ReadTuple;
  ";

  state_struct!(CreateTuple, inp: u64, out: *const (u64, MemRegion));
  state_struct!(ReadTuple, inp: *const (u64, MemRegion), out1: u64, out2: MemRegion);
  state_struct!(TupleMain, inp: u64, out1: u64, out2: MemRegion, h0: *const (u64, MemRegion) | writer: CreateTupleState, reader: ReadTupleState);

  #[test]
  fn jit_create_tuple_codegen_runs() -> CodegenStatus {
    ee_for_string(TUPLE_TEST_STRING, |ee: ExecutionEngine| {
      unsafe {
        let function: JitFunction<TupleMainFunc> = ee.get_function("Main_update").unwrap();
        let mut state = TupleMainState {
          inp: 0, inp_upd: 10, out1: 0, out1_upd: 0, out2: MemRegion::empty(), out2_upd: MemRegion::empty(), 
          h0: ptr::null(), h0_upd: ptr::null(),
          bitfield: 0x1,
          writer: CreateTupleState { inp: 0, inp_upd: 0, out: ptr::null(), out_upd: ptr::null(), bitfield: 0x0 },
          reader: ReadTupleState { inp: ptr::null(), inp_upd: ptr::null(), out1: 0, out1_upd: 0, out2: MemRegion::empty(), out2_upd: MemRegion::empty(), bitfield: 0x0}
        };

        function.call(&mut state);
        function.call(&mut state);
        assert_eq!(state.out1_upd, 10);
        assert_eq!(state.out2_upd.size, 13);
      }
    })
  }

  static SYNTAX_TEST_STRING: &str = "
module SyntaxTest {
  input: reads (String, Int);
  output: writes (String, Int);
  result: writes Int;
  error: writes Int;

  input.onChange: {
    let offset = input.1;
    if offset == size(input.0) || input.0[offset] < '0' || input.0[offset] > '9' {
      error <!- 1;
    }
    
    let result = 0;
    while input.0[offset] >= '0' && input.0[offset] <= '9' {
      result = result * 10 + input.0[offset] - '0';
      offset = offset + 1;
      if offset == size(input.0) {
        break;
      }
    }

    result <- result;
    output <- (input.0, offset);
  }
}
  ";

  state_struct!(SyntaxTest, inp: *const (MemRegion, u64), out: *const(MemRegion, u64), result: u64, error: u64);

  #[test]
  fn jit_syntax_test_codegen_runs() -> CodegenStatus {
    ee_for_string(SYNTAX_TEST_STRING, |ee: ExecutionEngine| {
      unsafe {
        let function: JitFunction<SyntaxTestFunc> = ee.get_function("SyntaxTest_update").unwrap();
        let mut state = SyntaxTestState { inp: ptr::null(), inp_upd: &(MemRegion::from_str("Delicious boots"), 15), out: ptr::null(), out_upd: ptr::null(), result: 0, result_upd: 0, error: 0, error_upd: 0, bitfield: 0x1 };
        function.call(&mut state);
        assert_eq!(state.bitfield, 0x8); // error updated
        let mut state = SyntaxTestState { inp: ptr::null(), inp_upd: &(MemRegion::from_str("15 Delicious boots"), 0), out: ptr::null(), out_upd: ptr::null(), result: 0, result_upd: 0, error: 0, error_upd: 0, bitfield: 0x1 };
        function.call(&mut state);
        assert_eq!(state.bitfield, 0x6); // result, output updated
        assert_eq!((*state.out_upd).1, 2); // offset advanced by 2
        assert_eq!(state.result_upd, 15); // result parsed from string
        let mut state = SyntaxTestState { inp: ptr::null(), inp_upd: &(MemRegion::from_str("Delicious boots"), 0), out: ptr::null(), out_upd: ptr::null(), result: 0, result_upd: 0, error: 0, error_upd: 0, bitfield: 0x1 };
        function.call(&mut state);
        assert_eq!(state.bitfield, 0x8); // error updated
      }
    }) 
  }

  static WHILE_TEST_STRING: &str = "
module WhileTest {
  len: reads Int;
  string: reads String;
  result: writes Int;

  len.onChange: {
    let x = 0;
    let counter = 0;
    while string[counter] >= '0' && string[counter] <= '9' {
      x = x * 10 + string[counter] - '0';
      counter = counter + 1;
      if counter == len {
        break;
      }
    }

    result <- x;
  }
}  
  ";

  state_struct!(WhileTest, len: u64, string: MemRegion, result: u64);

  #[test]
  fn jit_while_test_codegen_runs() -> CodegenStatus {
    ee_for_string(WHILE_TEST_STRING, |ee: ExecutionEngine| {
      unsafe {
        let function: JitFunction<WhileTestFunc> = ee.get_function("WhileTest_update").unwrap();
        let mut state = WhileTestState { len: 0, len_upd: 5, string: MemRegion::from_str("42360 "), string_upd: MemRegion::empty(), result: 0, result_upd: 0, bitfield: 0x1 };
        function.call(&mut state);
        dbg!(state);
      }
    })
  }
}
