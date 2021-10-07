use inkwell::AddressSpace;
use inkwell::types::{StructType};
use inkwell::values::{BasicValueEnum, IntValue, PointerValue};

use super::codegen_state::*;
use super::ast;

use std::convert::TryInto;

//
// Each field in a module's state has a type (currently SingleWordPrimitive or MemRegion).
// 
// The simplest encoding of a state field is a pointer to that field's first word.
// Methods like update_ptr_for_field, read_ptr_for_field, etc. generate the 
// appropriate pointer given a module and a field name.
//
// Generic pointers can be used directly to read single-word values (e.g. SingleWordPrimitives).
// They can also be used to construct more complex pointers (e.g. MemRegionPointer)
// for fine-grained access to multi-word values.
//
// StatePointers are enums that wrap up pointer with pointer type. They can be
// loaded and stored safely, with errors generated on mismatch.
//
// StateValues are immediates that encode a value from/for a field. In particular,
// you can store a StateValue in a field, and read a StateValue out of a field.

pub fn memregion_ir_type<'ctx>(cg: &CodegenState<'ctx>) -> StructType<'ctx> {
  cg.context.struct_type(&[
    cg.context.i8_type().ptr_type(AddressSpace::Generic).into(),
    cg.context.i64_type().into()
  ], false)
}

pub fn memregion_data_ptr<'ctx>(cg: &CodegenState<'ctx>, ptr: PointerValue<'ctx>) -> CodegenResult<PointerValue<'ctx>> {
  cg.builder.build_struct_gep(ptr, 0, "data").or(Err(CodegenError::InvalidStructPointer("MemRegion::data_ptr given bad state pointer".to_string())))
}

pub fn memregion_size_ptr<'ctx>(cg: &CodegenState<'ctx>, ptr: PointerValue<'ctx>) -> CodegenResult<PointerValue<'ctx>> {
  cg.builder.build_struct_gep(ptr, 1, "size").or(Err(CodegenError::InvalidStructPointer("MemRegion::size_ptr given bad state pointer".to_string())))
}

#[derive(PartialEq)]
pub enum PointerKind {
  SingleWordPrimitive, DynamicMemRegion, StaticMemRegion
}

pub trait Loadable<'ctx> {
  fn load(&self, cg: &CodegenState<'ctx>, name: &str) -> CodegenResult<StateValue<'ctx>>;
  fn clear(&self, cg: &CodegenState<'ctx>) -> CodegenStatus;
}

// The compiler views state field types as an array of type primitives. These will eventually
// be canonicalized into a known order, to maximize compatibility.
#[derive(Clone, Debug, PartialEq)]
pub enum TypePrimitive {
  Int, Char, MemRegion, PointerTo(Box<Vec<TypePrimitive>>), FixedArrayOf(Box<Vec<TypePrimitive>>, u64), DynamicArrayOf(Box<Vec<TypePrimitive>>)
}

fn type_size(type_vec: &Vec<TypePrimitive>) -> CodegenResult<u64> {
  panic!("Not implemented yet");
}

pub struct StatePointer<'ctx> {
  pub pointer_kind: PointerKind,
  pub pointer: PointerValue<'ctx>,
  pub pointer_type: Vec<TypePrimitive> // this is a pointer *to* the type primitives, i.e. these aren't necessarily all PointerTo.
}

impl <'ctx> StatePointer<'ctx> {
  pub fn new(ptr: PointerValue<'ctx>, h_type: ast::TypePrimitive) -> Self {
    // h_type "should" match ptr.get_element_type() as the element type is derive (via the state struct)
    // from h_type.
    let (pointer_kind, pointer_type) = match h_type {
      ast::TypePrimitive::Int => (PointerKind::SingleWordPrimitive, vec!(TypePrimitive::Int)),
      ast::TypePrimitive::Char => (PointerKind::SingleWordPrimitive, vec!(TypePrimitive::Char)),
      ast::TypePrimitive::MemRegion => (PointerKind::DynamicMemRegion, vec!(TypePrimitive::MemRegion)),
      ast::TypePrimitive::String => (PointerKind::DynamicMemRegion, vec!(TypePrimitive::DynamicArrayOf(Box::new(vec!(TypePrimitive::Char))))),
    };
    StatePointer { pointer_kind, pointer: ptr, pointer_type } 
  }
  pub fn load(&self, cg: &CodegenState<'ctx>, name: &str) -> CodegenResult<StateValue<'ctx>> {
    match self.pointer_kind {
      PointerKind::SingleWordPrimitive => {
        let value = cg.builder.build_load(self.pointer, name);
        Ok(StateValue { value: ValueParts::SingleWordPrimitive(value), value_type: self.pointer_type.clone() })
      }
      PointerKind::StaticMemRegion => {
        let value = cg.builder.build_load(self.pointer, name);
        Ok(StateValue { value: ValueParts::StaticMemRegion(value.into_pointer_value()), value_type: self.pointer_type.clone() })
      }
      PointerKind::DynamicMemRegion => {
        let data = cg.builder.build_load(memregion_data_ptr(cg, self.pointer)?, &(name.to_string() + "_data")).into_pointer_value();
        let size = cg.builder.build_load(memregion_size_ptr(cg, self.pointer)?, &(name.to_string() + "_size")).into_int_value();
        Ok(StateValue { value: ValueParts::DynamicMemRegion(data, size), value_type: self.pointer_type.clone() })
      }
    }
  }
  pub fn clear(&self, cg: &CodegenState<'ctx>) -> CodegenStatus {
    match self.pointer_kind {
      PointerKind::SingleWordPrimitive => {
        cg.builder.build_store(self.pointer, cg.uint_const(0));
        Ok(())
      }
      PointerKind::StaticMemRegion => {
        panic!("Don't know what to do here");
      }
      PointerKind::DynamicMemRegion => {
        cg.builder.build_store(memregion_data_ptr(cg, self.pointer)?, cg.uint_const(0));
        cg.builder.build_store(memregion_size_ptr(cg, self.pointer)?, cg.uint_const(0));
        Ok(())
      }
    }
  }
}

#[derive(Debug)]
pub enum ValueParts<'ctx> {
  None,
  SingleWordPrimitive(BasicValueEnum<'ctx>),
  DynamicMemRegion(PointerValue<'ctx>, IntValue<'ctx>),
  StaticMemRegion(PointerValue<'ctx>)
}

#[derive(Debug)]
pub struct StateValue<'ctx> {
  value: ValueParts<'ctx>,
  value_type: Vec<TypePrimitive>
}

impl <'ctx> StateValue<'ctx> {
  pub fn new_int(value: IntValue<'ctx>) -> Self {
    StateValue::new_prim_of_type(value.into(), vec!(TypePrimitive::Int))
  }
  pub fn new_prim_of_type(value: BasicValueEnum<'ctx>, value_type: Vec<TypePrimitive>) -> Self {
    StateValue { value: ValueParts::SingleWordPrimitive(value), value_type }
  }
  pub fn new_dynamic_mem_region_of_type(data: PointerValue<'ctx>, size: IntValue<'ctx>, value_type: Vec<TypePrimitive>) -> Self {
    StateValue { value: ValueParts::DynamicMemRegion(data, size), value_type }
  }
  pub fn new_none() -> Self {
    StateValue { value: ValueParts::None, value_type: Vec::new() }
  }
  pub fn into_int_value(&self) -> CodegenResult<IntValue<'ctx>> {
    if let ValueParts::SingleWordPrimitive(v) = self.value {
      Ok(v.into_int_value())
    } else {
      Err(CodegenError::TypeMismatch(std::format!("can't into_int_value() on ${:?}", self)))
    }
  }
  pub fn into_pointer_value(&self) -> CodegenResult<PointerValue<'ctx>> {
    if let ValueParts::DynamicMemRegion(ptr, _size) = self.value {
      Ok(ptr)
    } else {
      Err(CodegenError::TypeMismatch(std::format!("Can't into_pointer_value on ${:?}", self)))
    }
  }
  pub fn store(&self, cg: &CodegenState<'ctx>, ptr: StatePointer<'ctx>) -> CodegenStatus {
    // TODO: more complex type comparison?
    if !(ptr.pointer_type == self.value_type) {
      return Err(CodegenError::TypeMismatch("Attempt to store value into mismatched pointer".to_string()));
    }
    match self.value {
      ValueParts::SingleWordPrimitive(v) => {
        if ptr.pointer_kind == PointerKind::SingleWordPrimitive {
          cg.builder.build_store(ptr.pointer, v);
          Ok(())
        } else {
          Err(CodegenError::TypeMismatch("Attempt to store integer into non-integer pointer".to_string()))
        }
      }
      ValueParts::DynamicMemRegion(data, size) => {
        if ptr.pointer_kind == PointerKind::DynamicMemRegion {
          cg.builder.build_store(memregion_data_ptr(cg, ptr.pointer)?, data);
          cg.builder.build_store(memregion_size_ptr(cg, ptr.pointer)?, size);
          Ok(())
        } else {
          Err(CodegenError::TypeMismatch("Attempt to store memrange into non-memrange pointer".to_string()))
        }
      }
      ValueParts::StaticMemRegion(data) => {
        if ptr.pointer_kind == PointerKind::StaticMemRegion {
          cg.builder.build_store(ptr.pointer, data);
          Ok(())
        } else if ptr.pointer_kind == PointerKind::DynamicMemRegion {
          cg.builder.build_store(memregion_data_ptr(cg, ptr.pointer)?, data);
          cg.builder.build_store(memregion_size_ptr(cg, ptr.pointer)?, cg.uint_const(type_size(&ptr.pointer_type)?));
          Ok(())
        } else {
          Err(CodegenError::TypeMismatch("Attempt to store static-size region into non-region pointer".to_string()))
        }
      }
      ValueParts::None => Ok(())
    }
  }
  pub fn size(&self, cg: &CodegenState<'ctx>) -> CodegenResult<Self> {
    // TODO: size should (maybe?) work on both state pointers to allocated memranges, and
    // directly computed memrange values. Probably requires being able to represent pointers
    // as values, and may not be useful.
    match self.value {
      ValueParts::SingleWordPrimitive(_v) => Err(CodegenError::InvalidFunctionArgument("Can't call size() on SingleWordPrimitive result".to_string())),
      ValueParts::DynamicMemRegion(_data, size) => {
        Ok(StateValue::new_int(size))
      }
      ValueParts::StaticMemRegion(_data) => Ok(StateValue::new_int(cg.uint_const(type_size(&self.value_type)?))),
      ValueParts::None => Err(CodegenError::InvalidFunctionArgument("Can't call size() on None result".to_string()))
    }
  }
  pub fn array_lookup(&self, cg: &CodegenState<'ctx>, index: StateValue<'ctx>) -> CodegenResult<Self> {
    // TODO: bounds checking when necessary
    if self.value_type.len() != 1 {
      return Err(CodegenError::TypeMismatch("Can't perform array lookup on this".to_string()));
    }
    if let TypePrimitive::DynamicArrayOf(element_type) = &self.value_type[0] {
      let value_ptr;
      unsafe {
        value_ptr = cg.builder.build_gep(self.into_pointer_value()?, &[index.into_int_value()?], "lookup");
      }
      let value = cg.builder.build_load(value_ptr, "value");
      Ok(StateValue::new_prim_of_type(value, (**element_type).clone()))
    } else {
      Err(CodegenError::TypeMismatch("Can't perform array lookup on this".to_string()))
    }
  } 
}

pub enum UpdatePtrPurpose {
  ReadAndClear,
  ReadWithoutClearing,
  WriteAndSet
}

impl <'ctx> CodegenState<'ctx> {
  pub fn read_ptr_for_field(&self, module: &ast::Module, state: PointerValue<'ctx>, name: &str) -> CodegenResult<StatePointer<'ctx>> {
    let idx = module.idx_for_field(name).ok_or(CodegenError::BadReadFieldName)?;
    let h_type = module.type_for_field(name).ok_or(CodegenError::BadReadFieldName)?;
    let struct_idx = (2 * idx).try_into().or(Err(CodegenError::InvalidIndex))?;
    let ptr = self.builder.build_struct_gep(state, struct_idx, &("read_ptr_to".to_string() + name)).or(Err(CodegenError::InvalidStructPointer("read_ptr_for_field given bad state pointer".to_string())))?;
    Ok(StatePointer::new(ptr, h_type))
  }

  pub fn update_ptr_for_field(&self, module: &ast::Module, state: PointerValue<'ctx>, name: &str, purpose: UpdatePtrPurpose) -> CodegenResult<StatePointer<'ctx>> {
    let idx = module.idx_for_field(name).ok_or(CodegenError::BadUpdateFieldName)?;
    let h_type = module.type_for_field(name).ok_or(CodegenError::BadReadFieldName)?;
    let struct_idx = (2 * idx + 1).try_into().or(Err(CodegenError::InvalidIndex))?;

    let bitfield_ptr = self.module_bitfield_ptr(module, state)?;
    let bitfield = self.builder.build_load(bitfield_ptr, "bitfield").into_int_value();

    match purpose {
      UpdatePtrPurpose::ReadAndClear => {
        let new_clear = self.uint_const(!(1 << idx));
        let new_bitfield = self.builder.build_and(bitfield, new_clear, "new_bitfield");
        self.builder.build_store(bitfield_ptr, new_bitfield);
      },
      UpdatePtrPurpose::WriteAndSet => {
        let new_write = self.uint_const(1 << idx);
        let new_bitfield = self.builder.build_or(bitfield, new_write, "new_bitfield");
        self.builder.build_store(bitfield_ptr, new_bitfield);
      },
      UpdatePtrPurpose::ReadWithoutClearing => ()
    }

    let ptr = self.builder.build_struct_gep(state, struct_idx, &(String::from("update_ptr_to_") + name)).or(Err(CodegenError::InvalidStructPointer("update_ptr_for_field given bad state pointer".to_string())))?;
    Ok(StatePointer::new(ptr, h_type))
  }

  pub fn module_bitfield_ptr(&self, module: &ast::Module, state: PointerValue<'ctx>) -> CodegenResult<PointerValue<'ctx>> {
    let bitfield_idx = module.idx_for_bitfield().try_into().unwrap();
    self.builder.build_struct_gep(state, bitfield_idx, "bitfield_ptr").or(Err(CodegenError::InvalidStructPointer("module_bitfield_ptr given bad state pointer".to_string())))
  }

  pub fn submodule_ptr(&self, module: &ast::Module, state: PointerValue<'ctx>, index: usize) -> CodegenResult<PointerValue<'ctx>> {
    let mut base_idx: u32 = module.idx_for_bitfield().try_into().unwrap();
    base_idx += 1;
    let offset: u32 = index.try_into().unwrap();
    self.builder.build_struct_gep(state, base_idx + offset, &(module.submodules[index].module.name.clone() + "_ptr")).or(Err(CodegenError::InvalidStructPointer("submodule_ptr given bad state pointer".to_string())))
  }
}