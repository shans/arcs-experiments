use super::*;

use inkwell::types::PointerType;

pub struct DebugState<'ctx> {
  state_ptr: PointerValue<'ctx>,
  explode: bool,
  current_indent: usize,
  at_line_start: bool,
}

// struct DebugState {
//    char* buffer;
//    size_t allocated;
//    size_t write_position; 
// }
impl <'ctx> DebugState<'ctx> {
  fn llvm_type(cg: &CodegenState<'ctx>) -> PointerType<'ctx> {
    let struct_type = cg.context.struct_type(&[cg.context.i8_type().ptr_type(AddressSpace::Generic).into(), cg.context.i32_type().into(), cg.context.i32_type().into()], false);
    struct_type.ptr_type(AddressSpace::Generic)
  }
  pub fn new(cg: &mut CodegenState<'ctx>, explode: bool) -> Self {
    // Allocate struct in memory & bitcast to appropriate type
    let ds_type = DebugState::llvm_type(cg);
    // let debug_size = ds_type.get_element_type().size_of().unwrap();
    // let debug_size32 = cg.builder.build_int_cast(debug_size, cg.context.i32_type(), "debug_size32");
    let debug_size32 = cg.uint32_const(24);
    let raw_mem = malloc(cg, debug_size32.into(), "raw_alloc");
    let ptr = cg.builder.build_bitcast(raw_mem, ds_type, "state_ptr").into_pointer_value();
    
    // Allocate buffer in memory (500 bytes to start with) and set struct values appropriately
    let buffer = malloc(cg, cg.uint32_const(500).into(), "buffer").into_pointer_value();
    let me = Self { state_ptr: ptr, explode, current_indent: 0, at_line_start: true };
    DebugState::set_buffer(&me, cg, buffer);
    DebugState::set_allocated(&me, cg, cg.uint32_const(500));
    DebugState::set_position(&me, cg, cg.uint32_const(0));
    me
  }

  fn set_buffer(&self, cg: &CodegenState<'ctx>, buffer: PointerValue<'ctx>) {
    let buffer_ptr = cg.builder.build_struct_gep(self.state_ptr, 0, "buffer_ptr").unwrap();
    cg.builder.build_store(buffer_ptr, buffer);
  }
  fn get_buffer(&self, cg: &CodegenState<'ctx>) -> PointerValue<'ctx> {
    let buffer_ptr = cg.builder.build_struct_gep(self.state_ptr, 0, "buffer_ptr").unwrap();
    cg.builder.build_load(buffer_ptr, "buffer").into_pointer_value()
  }
  
  fn set_allocated(&self, cg: &CodegenState<'ctx>, allocated: IntValue<'ctx>) {
    let allocated_ptr = cg.builder.build_struct_gep(self.state_ptr, 1, "allocated_ptr").unwrap();
    cg.builder.build_store(allocated_ptr, allocated);
  }
  fn get_allocated(&self, cg: &CodegenState<'ctx>) -> IntValue<'ctx> {
    let allocated_ptr = cg.builder.build_struct_gep(self.state_ptr, 1, "allocated_ptr").unwrap();
    cg.builder.build_load(allocated_ptr, "allocated").into_int_value()
  }
  
  fn set_position(&self, cg: &CodegenState<'ctx>, position: IntValue<'ctx>) {
    let position_ptr = cg.builder.build_struct_gep(self.state_ptr, 2, "position_ptr").unwrap();
    cg.builder.build_store(position_ptr, position);
  }
  fn get_position(&self, cg: &CodegenState<'ctx>) -> IntValue<'ctx> {
    let position_ptr = cg.builder.build_struct_gep(self.state_ptr, 2, "position_ptr").unwrap();
    cg.builder.build_load(position_ptr, "position").into_int_value()
  }
  
  pub fn print_to_console(self, cg: &CodegenState<'ctx>) {
    let printf = get_printf(cg);
    let buffer = self.get_buffer(cg);
    cg.builder.build_call(printf, &[buffer.into()], "_");
    free(cg, self.get_buffer(cg));
    let raw_mem = cg.builder.build_bitcast(self.state_ptr, cg.char_ptr_type(), "raw_mem").into_pointer_value();
    free(cg, raw_mem);
  }

  fn get_printf(&self, cg: &mut CodegenState<'ctx>) -> FunctionValue<'ctx> {
    let name = format!("_{}_printf", cg.module.get_name().to_str().unwrap());
    cg.module.get_function(&name).or_else(|| {
      let current_pos = cg.builder.get_insert_block();
      let printf = self.generate_printf(cg).unwrap();
      if let Some(block) = current_pos {
        cg.builder.position_at_end(block);
      }
      Some(printf)
    }).unwrap()
  }

  // let size = allocated - position;
  // let potentially_written = snprintf(buffer + position, size, format, args..);
  // if (potentially_written > size) {
  //   let new_allocated = potentially_written + allocated * 2;
  //   let buffer = realloc(buffer, new_allocated);
  //   !! store buffer and new_allocated in struct
  //   let potentially_written = snprintf(buffer + position, size, format, args..);
  // }
  // position = position + potentially_written
  // !! store position in struct
  //
  // TODO: make this take the rest of the state as arguments and make it generic across modules.
  // It's quite wrong at the moment..
  fn generate_printf(&self, cg: &mut CodegenState<'ctx>) -> CodegenResult<FunctionValue<'ctx>> {
    let function_type = cg.context.void_type().fn_type(&[DebugState::llvm_type(cg).into(), cg.char_ptr_type().into()], true);
    let name = format!("_{}_printf", cg.module.get_name().to_str().unwrap());
    let function = cg.module.add_function(&name, function_type, None);
    let block = cg.context.append_basic_block(function, "entry");
    cg.builder.position_at_end(block);

    let va_list_alloca = cg.builder.build_alloca(va_list_type(cg), "va_list_alloca");
    let va_list_as_char_ptr = cg.builder.build_bitcast(va_list_alloca, cg.char_ptr_type(), "va_list_as_char_ptr");

    let vsnprintf = get_vsnprintf(cg);
    let ptr = function.get_first_param().unwrap().into_pointer_value();
    let me = DebugState { state_ptr: ptr, explode: self.explode, current_indent: self.current_indent, at_line_start: self.at_line_start };
    let allocated = DebugState::get_allocated(&me, cg);
    let position = DebugState::get_position(&me, cg);
    let position64 = cg.builder.build_int_cast(position, cg.context.i64_type(), "position64");
    let size = cg.builder.build_int_sub(allocated, position, "size");
    let buffer = DebugState::get_buffer(&me, cg);
    let buffer_as_int = cg.builder.build_ptr_to_int(buffer, cg.context.i64_type(), "buffer_as_int");
    let write_pos_as_int = cg.builder.build_int_add(buffer_as_int, position64, "write_pos_as_int");
    let write_pos = cg.builder.build_int_to_ptr(write_pos_as_int, cg.char_ptr_type(), "write_pos");
    let format_str = function.get_nth_param(1).unwrap();

    cg.builder.build_call(get_va_start(cg), &[va_list_as_char_ptr.into()], "va_start");
    let potentially_written = cg.builder.build_call(vsnprintf, &[write_pos.into(), size.into(), format_str.into(), va_list_alloca.into()], "potentially_written").try_as_basic_value().left().unwrap().into_int_value();
    cg.builder.build_call(get_va_end(cg), &[va_list_as_char_ptr.into()], "va_end");

    let test = cg.builder.build_int_compare(IntPredicate::UGT, potentially_written, size, "test");
    let too_big = append_new_block(cg, "too_big")?;
    let ok = append_new_block(cg, "ok")?;
    cg.builder.build_conditional_branch(test, too_big, ok);
    
    cg.builder.position_at_end(too_big);
    let double_alloc = cg.builder.build_int_mul(allocated, cg.uint32_const(2), "double_alloc");
    let new_allocated = cg.builder.build_int_add(double_alloc, potentially_written, "new_allocated");
    let realloc = get_realloc(cg);
    let new_buffer = cg.builder.build_call(realloc, &[buffer.into(), new_allocated.into()], "new_buffer").try_as_basic_value().left().unwrap().into_pointer_value();
    DebugState::set_allocated(&me, cg, new_allocated);
    DebugState::set_buffer(&me, cg, new_buffer);
    let new_buffer_as_int = cg.builder.build_ptr_to_int(new_buffer, cg.context.i64_type(), "new_buffer_as_int");
    let new_write_pos_as_int = cg.builder.build_int_add(new_buffer_as_int, position64, "new_write_pos_as_int");
    let new_write_pos = cg.builder.build_int_to_ptr(new_write_pos_as_int, cg.char_ptr_type(), "new_write_pos");
    let new_size = cg.builder.build_int_sub(new_allocated, position, "new_size");

    cg.builder.build_call(get_va_start(cg), &[va_list_as_char_ptr.into()], "va_start");
    let written = cg.builder.build_call(vsnprintf, &[new_write_pos.into(), new_size.into(), format_str.into(), va_list_alloca.into()], "written").try_as_basic_value().left().unwrap().into_int_value();
    cg.builder.build_call(get_va_end(cg), &[va_list_as_char_ptr.into()], "va_end");

    cg.builder.build_unconditional_branch(ok);

    cg.builder.position_at_end(ok);
    let written_phi = cg.builder.build_phi(cg.context.i32_type(), "actual_written");
    written_phi.add_incoming(&[(&potentially_written, block), (&written, too_big)]);
    let new_position = cg.builder.build_int_add(position, written_phi.as_basic_value().into_int_value(), "new_position");
    DebugState::set_position(&me, cg, new_position);


    cg.builder.build_return(None);
    Ok(function)
  }
}

// TODO: Move indent/line-start handling into codegen'd code so that it works with if/else cases like null handling
pub trait Printer<'ctx> {
  fn printf(&mut self, cg: &mut CodegenState<'ctx>, format: &str, args: &[BasicValueEnum<'ctx>]) -> CodegenStatus;
  fn sep(&mut self, cg: &mut CodegenState<'ctx>, sep: &str) -> CodegenStatus;
  fn open_bracket(&mut self, cg: &mut CodegenState<'ctx>, bracket: &str) -> CodegenStatus;
  fn close_bracket(&mut self, cg: &mut CodegenState<'ctx>, bracket: &str) -> CodegenStatus;
}

impl <'ctx> Printer<'ctx> for DebugState<'ctx> {
  fn printf(&mut self, cg: &mut CodegenState<'ctx>, format: &str, args: &[BasicValueEnum<'ctx>]) -> CodegenStatus {
    let format_str = cg.global_string(&format);
    let printf = self.get_printf(cg);
    if self.at_line_start {
      let padding = cg.uint32_const(self.current_indent as u32);
      let empty = cg.global_string("");
      let format = cg.global_string("%*s");
      cg.builder.build_call(printf, &[self.state_ptr.into(), format.into(), padding.into(), empty.into()], "_");
      self.at_line_start = false;
    }
    cg.builder.build_call(printf, &[&[self.state_ptr.into(), format_str.into()], args].concat(), "_");
    Ok(())
  }


  fn sep(&mut self, cg: &mut CodegenState<'ctx>, sep: &str) -> CodegenStatus {
    if self.explode {
      self.printf(cg, &(sep.to_string() + "\n"), &[])?;
      self.at_line_start = true;
      Ok(())
    } else {
      self.printf(cg, &(sep.to_string() + " "), &[])
    }
  }
  fn open_bracket(&mut self, cg: &mut CodegenState<'ctx>, bracket: &str) -> CodegenStatus {
    if self.explode {
      self.current_indent += 2;
      self.printf(cg, &(bracket.to_string() + "\n"), &[])?;
      self.at_line_start = true;
    } else {
      self.printf(cg, bracket, &[])?;
    }
    Ok(())
  }
  fn close_bracket(&mut self, cg: &mut CodegenState<'ctx>, bracket: &str) -> CodegenStatus {
    if self.explode {
      self.current_indent -= 2;
      self.printf(cg, &(bracket.to_string() + "\n"), &[])?;
      self.at_line_start = true;
      Ok(())
    } else {
      self.printf(cg, bracket, &[])
    }
  }
}

pub fn print_if_not_null<'ctx>(cg: &mut CodegenState<'ctx>, printer: &mut dyn Printer<'ctx>, value: PointerValue<'ctx>, null_action: &dyn Fn(&mut CodegenState<'ctx>, &mut dyn Printer<'ctx>) -> CodegenStatus, action: &dyn Fn(&mut CodegenState<'ctx>, &mut dyn Printer<'ctx>) -> CodegenStatus) -> CodegenStatus {
  let value_as_int = cg.builder.build_ptr_to_int(value, cg.context.i64_type(), "value_as_int");
  let is_null = append_new_block(cg, "is_null")?;
  let not_null = append_new_block(cg, "not_null")?;
  let finally = append_new_block(cg, "finally")?;
  let test = cg.builder.build_int_compare(IntPredicate::EQ, value_as_int, cg.uint_const(0), "test");
  cg.builder.build_conditional_branch(test, is_null, not_null);
  cg.builder.position_at_end(is_null);
  null_action(cg, printer)?;
  cg.builder.build_unconditional_branch(finally);
  cg.builder.position_at_end(not_null);
  action(cg, printer)?;
  cg.builder.build_unconditional_branch(finally);
  cg.builder.position_at_end(finally);
  Ok(())
}

// Generate a <module>__dump(state) function.
pub fn debug_codegen<'ctx>(cg: &mut CodegenState<'ctx>, module: &'ctx ast::Module) -> CodegenStatus {

  let module_type = module.ir_type(cg).into_struct_type();
  let module_ptr_type = module_type.ptr_type(AddressSpace::Generic);
  let dump_function_type = cg.context.void_type().fn_type(&[module_ptr_type.into()], false);
  let dump_function_name = module.name.clone() + "__dump";
  let function = cg.module.add_function(&dump_function_name, dump_function_type, None);
  let entry_block = cg.context.append_basic_block(function, "entry");
  cg.builder.position_at_end(entry_block);
  let mut printer = DebugState::new(cg, true);

  printer.printf(cg, &(module.name.to_string() + " "), &[])?;
  printer.open_bracket(cg, "{")?;

  let state_ptr = function.get_first_param().unwrap().into_pointer_value();
  for handle in &module.handles {
    printer.printf(cg, &(handle.name.clone() + ": "), &[])?;
    let field_ptr = cg.read_ptr_for_field(module, state_ptr, &handle.name)?;
    let field_value = field_ptr.load(cg, "field")?;
    field_value.debug(cg, &mut printer)?;
    printer.sep(cg, ",")?;
  }

  for param in &module.value_params {
    printer.printf(cg, &(param.name.clone() + ": "), &[])?;
    let param_ptr = cg.read_ptr_for_field(module, state_ptr, &param.name)?;
    let param_value = param_ptr.load(cg, "param")?;
    param_value.debug(cg, &mut printer)?;
    printer.sep(cg, ", ")?;
  }

  printer.close_bracket(cg, "}")?;
  printer.printf(cg, "\n", &[])?;
  
  printer.print_to_console(cg);
  cg.builder.build_return(None);
  Ok(())
}