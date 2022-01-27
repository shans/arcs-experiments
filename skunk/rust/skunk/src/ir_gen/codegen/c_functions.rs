use super::*;

pub fn malloc<'ctx>(cg: &CodegenState<'ctx>, size: IntValue<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
  let malloc = get_malloc(cg);
  if size.get_type().get_bit_width() != 32 {
    panic!("malloc called with non-32 bit size");
  }
  cg.builder.build_call(malloc, &[size.into()], name).try_as_basic_value().left().unwrap()
}

fn get_malloc<'ctx>(cg: &CodegenState<'ctx>) -> FunctionValue<'ctx> {
  cg.module.get_function("malloc").or_else(|| {
    let function_type = cg.char_ptr_type().fn_type(&[cg.context.i32_type().into()], false);
    Some(cg.module.add_function("malloc", function_type, None))
  }).unwrap()
}

pub fn free<'ctx>(cg: &CodegenState<'ctx>, ptr: PointerValue<'ctx>) {
  let free = get_free(cg);
  cg.builder.build_call(free, &[ptr.into()], "");
}

fn get_free<'ctx>(cg: &CodegenState<'ctx>) -> FunctionValue<'ctx> {
  cg.module.get_function("free").or_else(|| {
    let function_type = cg.context.void_type().fn_type(&[cg.char_ptr_type().into()], false);
    Some(cg.module.add_function("free", function_type, None))
  }).unwrap()
}

pub fn memcmp<'ctx>(cg: &CodegenState<'ctx>, lhs: PointerValue<'ctx>, rhs: PointerValue<'ctx>, size: IntValue<'ctx>) -> IntValue<'ctx> {
  let memcmp = get_memcmp(cg);
  cg.builder.build_call(memcmp, &[lhs.into(), rhs.into(), size.into()], "memcmp_result").try_as_basic_value().left().unwrap().into_int_value()
}

fn get_memcmp<'ctx>(cg: &CodegenState<'ctx>) -> FunctionValue<'ctx> {
  cg.module.get_function("memcmp").or_else(|| {
    let function_type = cg.context.i64_type().fn_type(&[
      cg.char_ptr_type().into(), 
      cg.char_ptr_type().into(), 
      cg.context.i64_type().into()
    ], false);
    Some(cg.module.add_function("memcmp", function_type, None))
  }).unwrap()
}

#[allow(dead_code)]
pub fn get_snprintf<'ctx>(cg: &mut CodegenState<'ctx>) -> FunctionValue<'ctx> {
  cg.module.get_function("snprintf").or_else(|| {
    let function_type = cg.context.i32_type().fn_type(&[cg.char_ptr_type().into(), cg.context.i32_type().into(), cg.char_ptr_type().into()], true);
    Some(cg.module.add_function("snprintf", function_type, None))
  }).unwrap()
}

pub fn va_list_type<'ctx>(cg: &CodegenState<'ctx>) -> StructType<'ctx> {
  // NOTE: THis is the correct va_list struct type for X86 64-bit linux platforms. YMMV for other platforms.
  cg.context.struct_type(&[cg.context.i32_type().into(), cg.context.i32_type().into(), cg.char_ptr_type().into(), cg.char_ptr_type().into()], false)
}

pub fn get_vsnprintf<'ctx>(cg: &mut CodegenState<'ctx>) -> FunctionValue<'ctx> {
  cg.module.get_function("vsnprintf").or_else(|| {
    let function_type = cg.context.i32_type().fn_type(&[cg.char_ptr_type().into(), cg.context.i32_type().into(), cg.char_ptr_type().into(), va_list_type(cg).ptr_type(AddressSpace::Generic).into()], false);
    Some(cg.module.add_function("vsnprintf", function_type, None))
  }).unwrap()
}

pub fn get_printf<'ctx>(cg: &CodegenState<'ctx>) -> FunctionValue<'ctx> {
  cg.module.get_function("printf").or_else(|| {
    let function_type = cg.context.i32_type().fn_type(&[cg.char_ptr_type().into()], true);
    Some(cg.module.add_function("printf", function_type, None))
  }).unwrap()
}

pub fn get_realloc<'ctx>(cg: &CodegenState<'ctx>) -> FunctionValue<'ctx> {
  cg.module.get_function("realloc").or_else(|| {
    let function_type = cg.char_ptr_type().fn_type(&[cg.char_ptr_type().into(), cg.context.i32_type().into()], false);
    Some(cg.module.add_function("realloc", function_type, None))
  }).unwrap()
}

pub fn get_va_start<'ctx>(cg: &CodegenState<'ctx>) -> FunctionValue<'ctx> {
  cg.module.get_function("llvm.va_start").or_else(|| Some(cg.module.add_function("llvm.va_start", cg.context.void_type().fn_type(&[cg.char_ptr_type().into()], false), None))).unwrap()
}

pub fn get_va_end<'ctx>(cg: &CodegenState<'ctx>) -> FunctionValue<'ctx> {
  cg.module.get_function("llvm.va_end").or_else(|| Some(cg.module.add_function("llvm.va_end", cg.context.void_type().fn_type(&[cg.char_ptr_type().into()], false), None))).unwrap()
}

pub fn memset<'ctx>(cg: &CodegenState<'ctx>, ptr: PointerValue<'ctx>, c: IntValue<'ctx>, n: IntValue<'ctx>) -> PointerValue<'ctx> {
  let memset = get_memset(cg);
  cg.builder.build_call(memset, &[ptr.into(), c.into(), n.into()], "memset_result").try_as_basic_value().left().unwrap().into_pointer_value()
}

pub fn get_memset<'ctx>(cg: &CodegenState<'ctx>) -> FunctionValue<'ctx> {
  cg.module.get_function("memset").or_else(|| {
    let function_type = cg.char_ptr_type().fn_type(&[cg.char_ptr_type().into(), cg.context.i8_type().into(), cg.context.i32_type().into()], false);
    Some(cg.module.add_function("memset", function_type, None))
  }).unwrap()
}