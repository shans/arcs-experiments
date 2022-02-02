use super::*;

use inkwell::values::CallableValue;
use inkwell::targets::{TargetMachine, TargetTriple};

use std::convert::TryFrom;

/*

Simple examples - inputs on the left, result on the right.

examples {
  a: 4, !b: 5 -> c: 9
}

a is already set to 4, b is updated to 5; after running update until bitfield is 0 then c should
be 9.

*/

pub fn examples_codegen<'ctx>(cg: &mut CodegenState<'ctx>, module: &'ctx ast::Module) -> CodegenStatus {
  let num_examples = module.examples.examples.len();
  if num_examples == 0 {
    return Ok(());
  }
  // generate "prep" functions - which create a state pointer and set up initial example state
  // and "check" functions - which confirm that the final state matches requirements, then clean up the pointer.
  let mut prep_ptr_values = Vec::new();
  let mut check_ptr_values = Vec::new();
  for (idx, example) in module.examples.examples.iter().enumerate() {
    let prep_value = example_prep_codegen(cg, module, example, idx)?;
    let prep_ptr = prep_value.as_global_value().as_pointer_value();
    prep_ptr_values.push(prep_ptr);

    let check_value = example_check_codegen(cg, module, example, idx)?;
    let check_ptr = check_value.as_global_value().as_pointer_value();
    check_ptr_values.push(check_ptr);
  }

  // Create a count global to store the number of examples, and an accessor function ("<module>__get_example_count")
  let example_count = cg.module.add_global(cg.context.i64_type(), None, &(module.name.clone() + "__example_count"));
  example_count.set_initializer(&cg.uint_const(num_examples as u64));
  let example_count_fn = cg.module.add_function(&(module.name.clone() + "__get_example_count"), cg.context.i64_type().fn_type(&[], false), None);
  let entry_block = cg.context.append_basic_block(example_count_fn, "entry");
  cg.builder.position_at_end(entry_block);
  cg.builder.build_return(Some(&cg.uint_const(num_examples as u64)));

  // create a vector of prep functions
  let example_prep_type = prep_ptr_values[0].get_type();
  let example_prep_array_type = example_prep_type.array_type(num_examples as u32);
  let example_prep_global = cg.module.add_global(example_prep_array_type, None, &(module.name.clone() + "__example_prep_vector"));
  example_prep_global.set_initializer(&example_prep_type.const_array(&prep_ptr_values));

  let module_type = module.ir_type(cg).into_struct_type();
  let module_ptr_type = module_type.ptr_type(AddressSpace::Generic);

  // create a prep function accessor ("<module>__get_example_prep"). This looks up the prep vector by index.
  let example_prep_fn = cg.module.add_function(&(module.name.clone() + "__get_example_prep"), module_ptr_type.fn_type(&[cg.context.i64_type().into()], false), None);
  let entry_block = cg.context.append_basic_block(example_prep_fn, "entry");
  cg.builder.position_at_end(entry_block);
  let ptr = unsafe { cg.builder.build_gep(example_prep_global.as_pointer_value(), &[cg.uint_const(0), example_prep_fn.get_first_param().unwrap().into_int_value()], "prep_ptr") };
  let value = cg.builder.build_load(ptr, "prep").into_pointer_value();
  let callable = CallableValue::try_from(value).unwrap();
  let return_val = cg.builder.build_call(callable, &[], "return_val").try_as_basic_value().left().unwrap();
  cg.builder.build_return(Some(&return_val));

  // create a vector of check functions
  let example_check_type = check_ptr_values[0].get_type();
  let example_check_array_type = example_check_type.array_type(num_examples as u32);
  let example_check_global = cg.module.add_global(example_check_array_type, None, &(module.name.clone() + "__example_check_vector"));
  example_check_global.set_initializer(&example_check_type.const_array(&check_ptr_values)); 

  // create a check function accessor ("<module>__get_example_check"). This looks up the check vector by index.
  let example_check_fn = cg.module.add_function(&(module.name.clone() + "__get_example_check"), cg.context.i64_type().fn_type(&[cg.context.i64_type().into(), module_ptr_type.into()], false), None);
  let entry_block = cg.context.append_basic_block(example_check_fn, "entry");
  cg.builder.position_at_end(entry_block);
  let ptr = unsafe { cg.builder.build_gep(example_check_global.as_pointer_value(), &[cg.uint_const(0), example_check_fn.get_first_param().unwrap().into_int_value()], "check_ptr") };
  let value = cg.builder.build_load(ptr, "check").into_pointer_value();
  let callable = CallableValue::try_from(value).unwrap();
  let return_val = cg.builder.build_call(callable, &[example_check_fn.get_nth_param(1).unwrap()], "return_val").try_as_basic_value().left().unwrap();
  cg.builder.build_return(Some(&return_val));

  // create a function to run a single example end-to-end ("<module>__run_example").
  let example_run_fn = cg.module.add_function(&(module.name.clone() + "__run_example"), cg.context.i64_type().fn_type(&[cg.context.i64_type().into()], false), None);
  let entry_block = cg.context.append_basic_block(example_run_fn, "entry");
  cg.builder.position_at_end(entry_block);
  let state_ptr = cg.builder.build_call(example_prep_fn, &[example_run_fn.get_first_param().unwrap()], "state_ptr").try_as_basic_value().left().unwrap().into_pointer_value();

  let run_update = cg.context.append_basic_block(example_run_fn, "run_update");
  let after_update = cg.context.append_basic_block(example_run_fn, "after_update");

  let bitfield_ptr = cg.module_bitfield_ptr(module, state_ptr)?;
  let bitfield = cg.builder.build_load(bitfield_ptr, "bitfield").into_int_value();
  let test = cg.builder.build_int_compare(IntPredicate::NE, bitfield, cg.uint_const(0), "test");
  cg.builder.build_conditional_branch(test, run_update, after_update);

  cg.builder.position_at_end(run_update);
  let update_fn = cg.module.get_function(&(module.name.clone() + "_update")).unwrap();
  cg.builder.build_call(update_fn, &[state_ptr.into()], "_"); 
  let bitfield = cg.builder.build_load(bitfield_ptr, "bitfield").into_int_value();
  let test = cg.builder.build_int_compare(IntPredicate::NE, bitfield, cg.uint_const(0), "test");
  cg.builder.build_conditional_branch(test, run_update, after_update);

  cg.builder.position_at_end(after_update);
  let status_code = cg.builder.build_call(example_check_fn, &[example_run_fn.get_first_param().unwrap(), state_ptr.into()], "status_code").try_as_basic_value().left().unwrap();
  cg.builder.build_return(Some(&status_code));

  // create a function to run all examples in a module ("<module>_run_examples").
  // This returns 1 on error, or 0 on success.
  let examples_fn = cg.module.add_function(&(module.name.clone() + "_run_examples"), cg.context.i64_type().fn_type(&[], false), None);
  let entry_block = cg.context.append_basic_block(examples_fn, "entry");
  cg.builder.position_at_end(entry_block);
  let count = cg.builder.build_call(example_count_fn, &[], "count").try_as_basic_value().left().unwrap().into_int_value();
  let count_alloca = cg.builder.build_alloca(cg.context.i64_type(), "count_alloca");
  cg.builder.build_store(count_alloca, count);

  let run_example = cg.context.append_basic_block(examples_fn, "run_example");
  let run_example_2 = cg.context.append_basic_block(examples_fn, "run_example_2");
  let return_block = cg.context.append_basic_block(examples_fn, "return_block");

  let test = cg.builder.build_int_compare(IntPredicate::EQ, count, cg.uint_const(0), "test");
  cg.builder.build_conditional_branch(test, return_block, run_example);

  cg.builder.position_at_end(run_example);
  let count = cg.builder.build_load(count_alloca, "count").into_int_value();
  let new_count = cg.builder.build_int_sub(count, cg.uint_const(1), "new_count");
  cg.builder.build_store(count_alloca, new_count);
  let result = cg.builder.build_call(example_run_fn, &[new_count.into()], "result").try_as_basic_value().left().unwrap().into_int_value();
  let result_test = cg.builder.build_int_compare(IntPredicate::EQ, result, cg.uint_const(0), "result_test");
  cg.builder.build_conditional_branch(result_test, run_example_2, return_block);
  
  cg.builder.position_at_end(run_example_2);
  let test = cg.builder.build_int_compare(IntPredicate::EQ, new_count, cg.uint_const(0), "test");
  cg.builder.build_conditional_branch(test, return_block, run_example);

  cg.builder.position_at_end(return_block);
  let return_value = cg.builder.build_phi(cg.context.i64_type(), "return");
  let success = cg.uint_const(0);
  let failure = cg.uint_const(1);
  return_value.add_incoming(&[(&success, entry_block), (&failure, run_example), (&success, run_example_2)]);
  cg.builder.build_return(Some(&return_value.as_basic_value()));
  Ok(())
}

// Prep functions:
// (1) construct the appropriate state struct
// (2) initialize the members to the expressions stored in the example description
pub fn example_prep_codegen<'ctx>(cg: &mut CodegenState<'ctx>, module: &'ctx ast::Module, example: &'ctx ast::Example, idx: usize) -> CodegenResult<FunctionValue<'ctx>> {
  let module_type = module.ir_type(cg).into_struct_type();
  let module_ptr_type = module_type.ptr_type(AddressSpace::Generic);
  let prep_function_type = module_ptr_type.fn_type(&[], false);
  let prep_function_name = module.name.clone() + "__example_prep_" + &idx.to_string();
  let function = cg.module.add_function(&prep_function_name, prep_function_type, None);
  let entry_block = cg.context.append_basic_block(function, "entry");
  cg.builder.position_at_end(entry_block);

  /*
  let module_size = module_type.size_of().unwrap();
  let module_size32 = cg.builder.build_int_cast(module_size, cg.context.i32_type(), "module_size32");
  let state_ptr_as_char_ptr = malloc(cg, module_size32.into(), "malloced-state").into_pointer_value();
  memset(cg, state_ptr_as_char_ptr, cg.context.i8_type().const_zero(), module_size32);
  let state_ptr = cg.builder.build_bitcast(state_ptr_as_char_ptr, module_ptr_type, "state_ptr").into_pointer_value();
  */
  let init_fn = cg.module.get_function(&format!("{}_init", &module.name)).unwrap();
  let state_ptr = cg.builder.build_call(init_fn, &[], "state_ptr").try_as_basic_value().left().unwrap().into_pointer_value();
  let state_alloca = cg.builder.build_alloca(module_ptr_type, "state_alloca");
  cg.builder.build_store(state_alloca, state_ptr);

  let bitfield_ptr = cg.module_bitfield_ptr(module, state_ptr)?;
  cg.builder.build_store(bitfield_ptr, cg.uint_const(0));


  for (field, value_expression) in &example.inputs {
    let value = expression_codegen(cg, module, state_alloca, &value_expression.value.value)?;
    if value_expression.is_update {
      let ptr = cg.update_ptr_for_field(module, state_ptr, field, UpdatePtrPurpose::WriteAndSet)?;
      value.store(cg, &ptr)?;
    } else {
      let ptr = cg.read_ptr_for_field(module, state_ptr, field)?;
      value.store(cg, &ptr)?;
    }
  }
  /*
  let dump = cg.module.get_function(&(module.name.clone() + "__dump")).unwrap();
  cg.builder.build_call(dump, &[state_ptr.into()], "_");
  */

  cg.builder.build_return(Some(&state_ptr));
  Ok(function)
}

// Check functions:
// (1) compare the members of the provided state struct to the expressions stored in the example description
//    (1a) print an error message if the comparison fails
// (2) free the provided state struct
pub fn example_check_codegen<'ctx>(cg: &mut CodegenState<'ctx>, module: &'ctx ast::Module, example: &'ctx ast::Example, idx: usize) -> CodegenResult<FunctionValue<'ctx>> {
  let module_type = module.ir_type(cg).into_struct_type();
  let module_ptr_type = module_type.ptr_type(AddressSpace::Generic);
  let check_function_type = cg.context.i64_type().fn_type(&[module_ptr_type.into()], false);
  let check_function_name = module.name.clone() + "__example_check_" + &idx.to_string();
  let function = cg.module.add_function(&check_function_name, check_function_type, None);
  let entry_block = cg.context.append_basic_block(function, "entry");
  cg.builder.position_at_end(entry_block);
  let status_code = cg.uint_const(0);
  let status_code_alloca = cg.builder.build_alloca(cg.context.i64_type(), "status_code");
  cg.builder.build_store(status_code_alloca, status_code);

  let state_ptr = function.get_first_param().unwrap().into_pointer_value();
  let state_alloca = cg.builder.build_alloca(module_ptr_type, "state_alloca");
  cg.builder.build_store(state_alloca, state_ptr);

  // if a field doesn't match the result of an output expression for that field,
  // store this in the status_code against the field's bit offset.
  for (field, value_expression) in &example.expected {
    let ptr = cg.read_ptr_for_field(module, state_ptr, field)?;
    let value = ptr.load(cg, "value_to_check")?;
    let test_value = expression_codegen(cg, module, state_alloca, &value_expression.value.value)?;
    let cmp = value.equals(cg, &test_value)?.into_int_value()?;
    let record_problem_block = cg.context.append_basic_block(function, "record_problem");
    let next_block = cg.context.append_basic_block(function, "next");
    cg.builder.build_conditional_branch(cmp, next_block, record_problem_block);
    
    cg.builder.position_at_end(record_problem_block);
    let status_code = cg.builder.build_load(status_code_alloca, "status_code").into_int_value();
    let field_idx = module.idx_for_field(field).unwrap();
    let update = cg.uint_const(1 << field_idx);
    let status_code_update = cg.builder.build_or(status_code, update, "status_code_update");
    cg.builder.build_store(status_code_alloca, status_code_update);
    cg.builder.build_unconditional_branch(next_block);

    cg.builder.position_at_end(next_block);
  }


  let status_code = cg.builder.build_load(status_code_alloca, "status_code").into_int_value();

  let test = cg.builder.build_int_compare(IntPredicate::EQ, status_code, cg.uint_const(0), "test");
  let non_zero_status = append_new_block(cg, "non_zero_status")?;
  let finally = append_new_block(cg, "finally")?;

  cg.builder.build_conditional_branch(test, finally, non_zero_status);
  
  cg.builder.position_at_end(non_zero_status);

  let printf = get_printf(cg);
  let format = cg.global_string("\nUnexpected result for example %d. Output of this example:\n\n");
  cg.builder.build_call(printf, &[format.into(), cg.uint32_const(idx as u32).into()], "_");


  let dump = cg.module.get_function(&(module.name.clone() + "__dump")).unwrap();
  cg.builder.build_call(dump, &[state_ptr.into()], "_");

  let format = cg.global_string("\nerror bitfield: %ld\n\n");
  cg.builder.build_call(printf, &[format.into(), status_code.into()], "_");

  cg.builder.build_unconditional_branch(finally);

  cg.builder.position_at_end(finally);
  let state_ptr_as_char_ptr = cg.builder.build_bitcast(state_ptr, cg.context.i8_type().ptr_type(AddressSpace::Generic), "state_ptr_as_char_ptr").into_pointer_value();
  free(cg, state_ptr_as_char_ptr);

  cg.builder.build_return(Some(&status_code));
  Ok(function)
}

// A main function for examples. This runs all examples in all provided modules. Note that it doesn't take
// a CodegenState because generating this isn't part of the main codegen flow - instead, it's an optional
// extra triggered from main.rs.
pub fn main_for_examples<'ctx>(context: &'ctx Context, target_machine: &TargetMachine, target_triple: &TargetTriple, modules: &Vec<Module<'ctx>>) -> CodegenResult<Module<'ctx>> {

  let mut cg = CodegenState::new(context, target_machine, target_triple, "main");
  let function = cg.module.add_function("main", context.i32_type().fn_type(&[], false), None);

  let entry = cg.context.append_basic_block(function, "entry");
  cg.builder.position_at_end(entry);

  for submodule in modules {
    let name = submodule.get_name().to_str().unwrap().to_string();
    let fn_name = format!("{}_run_examples", name);
    if let Some(sub_fn) = submodule.get_function(&fn_name) {
      let sub_fn = cg.module.add_function(&fn_name, sub_fn.get_type(), None);
      let result = cg.builder.build_call(sub_fn, &[], "result").try_as_basic_value().left().unwrap().into_int_value();
      let bad_result = context.append_basic_block(function, &format!("{}_bad", fn_name));
      let next = context.append_basic_block(function, &(fn_name + "_next"));

      let test = cg.builder.build_int_compare(IntPredicate::EQ, result, context.i64_type().const_zero(), "test");
      cg.builder.build_conditional_branch(test, next, bad_result);

      cg.builder.position_at_end(bad_result);
      cg.builder.build_return(Some(&context.i32_type().const_int(-(1 as i32) as u64, true)));
      
      cg.builder.position_at_end(next);

      let example_count_fn = submodule.get_function(&format!("{}__get_example_count", name)).unwrap();
      let count = cg.builder.build_call(example_count_fn, &[], "count").try_as_basic_value().left().unwrap().into_int_value();
      printf(&mut cg, &(name + ": %d examples succeeded\n"), vec!(count.into()));
    }
  }

  cg.builder.build_return(Some(&context.i32_type().const_zero()));

  Ok(cg.module)
}