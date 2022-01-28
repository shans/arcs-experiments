use super::*;

pub fn expression_codegen<'ctx>(cg: &mut CodegenState<'ctx>, module: &ast::Module, state_alloca: PointerValue<'ctx>, expression: &'ctx ast::ExpressionValue) -> CodegenResult<StateValue<'ctx>> {
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
      if_else_expression(cg, cmp, 
        |cg| expression_codegen(cg, module, state_alloca, if_expression.if_true.as_ref()),
        |cg| expression_codegen(cg, module, state_alloca, if_expression.if_false.as_ref())
      )
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
      Ok(StateValue::new_none())
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
          let size32 = cg.builder.build_int_cast(size, cg.context.i32_type(), "size32");
          let raw_location = malloc(cg, size32.into(), "mem_region_location").into_pointer_value();
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
      let mut tuple_ptr = StateValue::new_tuple(cg, tuple_type)?;
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
            expression_logical_and(cg, |cg| expression_codegen(cg, module, state_alloca, &lhs), |cg| expression_codegen(cg, module, state_alloca, &rhs))
          }
          _ => todo!("Operator {:?} not yet implemented", op)
        }
      } else {
        let lhs_value = expression_codegen(cg, module, state_alloca, &lhs)?;
        let rhs_value = expression_codegen(cg, module, state_alloca, &rhs)?;
        match op {
          ast::Operator::Equality => lhs_value.equals(cg, &rhs_value),
          ast::Operator::Inequality => lhs_value.not_equals(cg, &rhs_value),
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
    },
    ast::ExpressionValueEnum::WriteToTuple(write_to_tuple) => {
      let state_ptr = cg.builder.build_load(state_alloca, "state_ptr").into_pointer_value();
      let tuple_field_ptr = cg.module_tuple_field_ptr(module, state_ptr)?;
      let tuple_offset = module.offset_for_tuple(write_to_tuple.tuple_id).ok_or(CodegenError::InvalidTupleID(write_to_tuple.tuple_id))?;
      let tuple_field_value = cg.builder.build_load(tuple_field_ptr, "tuple_field").into_int_value();
      let new_bit = cg.uint_const(1 << (tuple_offset + write_to_tuple.tuple_index));
      let new_tuple_field_value = cg.builder.build_or(tuple_field_value, new_bit, "new_tuple_field_value");
      cg.builder.build_store(tuple_field_ptr, new_tuple_field_value);

      let tuple_size = module.tuples.get(&write_to_tuple.tuple_id).ok_or(CodegenError::InvalidTupleID(write_to_tuple.tuple_id))?;
      let mask = cg.uint_const((1 << tuple_size) - 1);

      let pre_masked_value = cg.builder.build_right_shift(new_tuple_field_value, cg.uint_const(tuple_offset as u64), false, "pre_masked_value");
      let masked_value = cg.builder.build_and(pre_masked_value, mask, "masked_value");
      let test = cg.builder.build_int_compare(IntPredicate::EQ, masked_value, mask, "test");

      conditional_expression(cg, test, |cg| {
        for idx in 0..write_to_tuple.tuple_fields.len() {
          let tuple_field = &write_to_tuple.tuple_fields[idx];
          let from_value_ptr = cg.read_ptr_for_field(module, state_ptr, tuple_field)?;
          let to_value_ptr = cg.update_ptr_for_field(module, state_ptr, &write_to_tuple.state, UpdatePtrPurpose::WriteAndSet)?;
          let to_ptr = to_value_ptr.get_element_pointer(cg, idx)?;
          from_value_ptr.load(cg, "from")?.store(cg, &to_ptr)?;
        }
        Ok(())
      })?;

      Ok(StateValue::new_none())
    },
    // It's useful to have this catch-all while implementing new expressions
    #[allow(unreachable_patterns)]
    _ => todo!("Need to implement support for {:?}", expression.info),
  };
  cg.considering = old_considering;
  result
}

pub fn expression_logical_and<'ctx>(
  cg: &mut CodegenState<'ctx>,
  lhs: impl Fn(&mut CodegenState<'ctx>) -> CodegenResult<StateValue<'ctx>>, // ast::ExpressionValue<'ctx>,
  rhs: impl Fn(&mut CodegenState<'ctx>) -> CodegenResult<StateValue<'ctx>>, // ast::ExpressionValue<'ctx>
) -> CodegenResult<StateValue<'ctx>> {
  let lhs_value = lhs(cg)?.into_int_value()?; // expression_codegen(cg, module, state_alloca, &lhs)?.into_int_value()?;
  let lhs_block = cg.builder.get_insert_block().unwrap();
  let lhs_true = append_new_block(cg, "lhs_true")?;
  let end_of_test = append_new_block(cg, "end_of_test")?;
  cg.builder.build_conditional_branch(lhs_value, lhs_true, end_of_test);
  cg.builder.position_at_end(lhs_true);
  let rhs_value = rhs(cg)?.into_int_value()?; // expression_codegen(cg, module, state_alloca, &rhs)?.into_int_value()?;
  let rhs_block = cg.builder.get_insert_block().unwrap();
  cg.builder.build_unconditional_branch(end_of_test);

  cg.builder.position_at_end(end_of_test);
  let phi = cg.builder.build_phi(lhs_value.get_type(), "logical-and-result");
  phi.add_incoming(&[(&lhs_value, lhs_block), (&rhs_value, rhs_block)]);
  Ok(StateValue::new_bool(phi.as_basic_value().into_int_value()))
}

pub fn if_else_expression<'ctx>(
  cg: &mut CodegenState<'ctx>,
  test: IntValue<'ctx>,
  true_expr: impl Fn(&mut CodegenState<'ctx>) -> CodegenResult<StateValue<'ctx>>,
  false_expr: impl Fn(&mut CodegenState<'ctx>) -> CodegenResult<StateValue<'ctx>>
) -> CodegenResult<StateValue<'ctx>> {
  let if_true = append_new_block(cg, "if_true_block")?;
  let if_false = append_new_block(cg, "if_false_block")?;
  let after_if = append_new_block(cg, "after_if")?;
  cg.builder.build_conditional_branch(test, if_true, if_false);
  cg.builder.position_at_end(if_true);
  let result_if_true = true_expr(cg)?;
  let if_true = cg.builder.get_insert_block().unwrap();
  cg.builder.build_unconditional_branch(after_if);
  cg.builder.position_at_end(if_false);
  let result_if_false = false_expr(cg)?;
  let if_false = cg.builder.get_insert_block().unwrap();
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

pub fn conditional_expression<'ctx>(
  cg: &mut CodegenState<'ctx>,
  test: IntValue<'ctx>,
  conditional_exp: impl Fn(&mut CodegenState<'ctx>) -> CodegenStatus
) -> CodegenStatus {
  let if_true = append_new_block(cg, "if_truew_block")?;
  let after_if = append_new_block(cg, "after_if")?;
  cg.builder.build_conditional_branch(test, if_true, after_if);
  cg.builder.position_at_end(if_true);
  conditional_exp(cg)?;
  cg.builder.build_unconditional_branch(after_if);
  cg.builder.position_at_end(after_if);
  Ok(())
}