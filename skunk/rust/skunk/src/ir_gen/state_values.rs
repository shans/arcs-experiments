use inkwell::AddressSpace;
use inkwell::types::{StructType};
use inkwell::values::{BasicValueEnum, IntValue, PointerValue};

use super::codegen_state::*;
use super::ast;

use std::convert::TryInto;

//
// Each field in a module's state has a type (currently Integer or MemRegion).
// 
// The simplest encoding of a state field is a pointer to that field's first word.
// Methods like update_ptr_for_field, read_ptr_for_field, etc. generate the 
// appropriate pointer given a module and a field name.
//
// Generic pointers can be used directly to read single-word values (e.g. Integers).
// They can also be used to construct more complex pointers (e.g. MemRegionPointer)
// for fine-grained access to multi-word values.
//
// StatePointers are enums that wrap up pointer with pointer type. They can be
// loaded and stored safely, with errors generated on mismatch.
//
// StateValues are immediates that encode a value from/for a field. In particular,
// you can store a StateValue in a field, and read a StateValue out of a field.

pub struct MemRegionPointer<'ctx> {
  ptr: PointerValue<'ctx>,
}

impl <'ctx> MemRegionPointer<'ctx> {
  pub fn struct_ir_type(cg: &CodegenState<'ctx>) -> StructType<'ctx> {
    cg.context.struct_type(&[
      cg.context.i8_type().ptr_type(AddressSpace::Generic).into(),
      cg.context.i64_type().into()
    ], false)
  }
  pub fn new(cg: &CodegenState<'ctx>, ptr: PointerValue<'ctx>) -> MemRegionPointer<'ctx> {
    let pair_ty = MemRegionPointer::struct_ir_type(cg).ptr_type(AddressSpace::Generic);
    MemRegionPointer { ptr: cg.builder.build_bitcast(ptr, pair_ty, "cast_pointer").into_pointer_value() }
  }
  pub fn data_ptr(&self, cg: &CodegenState<'ctx>) -> CodegenResult<PointerValue<'ctx>> {
    cg.builder.build_struct_gep(self.ptr, 0, "data").or(Err(CodegenError::InvalidStructPointer("MemRegion::data_ptr given bad state pointer".to_string())))
  }
  pub fn size_ptr(&self, cg: &CodegenState<'ctx>) -> CodegenResult<PointerValue<'ctx>> {
    cg.builder.build_struct_gep(self.ptr, 1, "size").or(Err(CodegenError::InvalidStructPointer("MemRegion::size_ptr given bad state pointer".to_string())))
  }
}

pub enum StatePointer<'ctx> {
  Integer(PointerValue<'ctx>),
  MemRegion(MemRegionPointer<'ctx>),
}

impl <'ctx> StatePointer<'ctx> {
  fn new(cg: &CodegenState<'ctx>, ptr: PointerValue<'ctx>, h_type: ast::TypePrimitive) -> Self {
    match h_type {
      ast::TypePrimitive::Int => StatePointer::Integer(ptr),
      ast::TypePrimitive::MemRegion => StatePointer::MemRegion(MemRegionPointer::new(cg, ptr)),
      ast::TypePrimitive::String => panic!("Strings not yet implemented")
    }
  }
}

pub trait Loadable<'ctx> {
  fn load(&self, cg: &CodegenState<'ctx>, name: &str) -> CodegenResult<StateValue<'ctx>>;
  fn clear(&self, cg: &CodegenState<'ctx>) -> CodegenStatus;
}

impl <'ctx> Loadable<'ctx> for PointerValue<'ctx> {
  fn load(&self, cg: &CodegenState<'ctx>, name: &str) -> CodegenResult<StateValue<'ctx>> {
    let value = cg.builder.build_load(*self, name);
    Ok(StateValue::Integer(value))
  }
  fn clear(&self, cg: &CodegenState<'ctx>) -> CodegenStatus {
    cg.builder.build_store(*self, cg.uint_const(0));
    Ok(())
  }
}

impl <'ctx> Loadable<'ctx> for MemRegionPointer<'ctx> {
  fn load(&self, cg: &CodegenState<'ctx>, name: &str) -> CodegenResult<StateValue<'ctx>> {
    let data = cg.builder.build_load(self.data_ptr(cg)?, &(name.to_string() + "_data"));
    let size = cg.builder.build_load(self.size_ptr(cg)?, &(name.to_string() + "_size"));
    Ok(StateValue::MemRegion(data, size))
  }
  fn clear(&self, cg: &CodegenState<'ctx>) -> CodegenStatus {
    cg.builder.build_store(self.data_ptr(cg)?, cg.uint_const(0));
    cg.builder.build_store(self.size_ptr(cg)?, cg.uint_const(0));
    Ok(())
  }
}

impl <'ctx> Loadable<'ctx> for StatePointer<'ctx> {
  fn load(&self, cg: &CodegenState<'ctx>, name: &str) -> CodegenResult<StateValue<'ctx>> {
    match self {
      StatePointer::Integer(ptr) => ptr.load(cg, name),
      StatePointer::MemRegion(ptr) => ptr.load(cg, name)
    }
  }
  fn clear(&self, cg: &CodegenState<'ctx>) -> CodegenStatus {
    match self {
      StatePointer::Integer(ptr) => ptr.clear(cg),
      StatePointer::MemRegion(ptr) => ptr.clear(cg)
    }
  }
}

#[derive(Debug)]
pub enum StateValue<'ctx> {
  None,
  Integer(BasicValueEnum<'ctx>),
  MemRegion(BasicValueEnum<'ctx>, BasicValueEnum<'ctx>),
}

impl <'ctx> StateValue<'ctx> {
  pub fn into_int_value(&self) -> IntValue {
    if let StateValue::Integer(v) = self {
      v.into_int_value()
    } else {
      panic!("can't to_int_value() on ${:?}", self)
    }
  }
  pub fn store_unsafe(&self, cg: &CodegenState<'ctx>, ptr: PointerValue<'ctx>) -> CodegenStatus {
    match self {
      StateValue::Integer(v) => { cg.builder.build_store(ptr, *v); },
      StateValue::MemRegion(data, size) => {
        let struct_ptr = MemRegionPointer::new(cg, ptr);
        cg.builder.build_store(struct_ptr.data_ptr(cg)?, *data);
        cg.builder.build_store(struct_ptr.size_ptr(cg)?, *size);
      },
      StateValue::None => {}
    }
    Ok(())
  }
  pub fn store(&self, cg: &CodegenState<'ctx>, ptr: StatePointer<'ctx>) -> CodegenStatus {
    match self {
      StateValue::Integer(v) => { 
        if let StatePointer::Integer(ptr) = ptr {
          cg.builder.build_store(ptr, *v);
          Ok(())
        } else {
          Err(CodegenError::TypeMismatch("Attempt to store integer into non-integer pointer".to_string()))
        }
      }
      StateValue::MemRegion(data, size) => {
        if let StatePointer::MemRegion(ptr) = ptr {
          cg.builder.build_store(ptr.data_ptr(cg)?, *data);
          cg.builder.build_store(ptr.size_ptr(cg)?, *size);
          Ok(())
        } else {
          Err(CodegenError::TypeMismatch("Attempt to store memrange into non-memrange pointer".to_string()))
        }
      }
      StateValue::None => Ok(())
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
    Ok(StatePointer::new(self, ptr, h_type))
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
    Ok(StatePointer::new(self, ptr, h_type))
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

impl <'ctx> From<StateValue<'ctx>> for BasicValueEnum<'ctx> {
  fn from(item: StateValue<'ctx>) -> Self {    
    if let StateValue::Integer(v) = item {
      v
    } else {
      panic!("can't convert ${:?} into BasicValueEnum")
    }
  }
}