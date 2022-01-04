use inkwell::{AddressSpace, IntPredicate};
use inkwell::basic_block::BasicBlock;
use inkwell::types::{StructType, BasicTypeEnum, BasicType};
use inkwell::values::{BasicValueEnum, IntValue, PointerValue, PhiValue, FunctionValue};

use super::codegen_state::*;
use super::ast;

use super::codegen::*;

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

pub fn dptr_ir_type<'ctx>(cg: &CodegenState<'ctx>, primitive: BasicTypeEnum<'ctx>) -> StructType<'ctx> {
  cg.context.struct_type(&[
    primitive.ptr_type(AddressSpace::Generic).into(),
    cg.context.i64_type().into()
  ], false)
}

pub fn memregion_data_ptr<'ctx>(cg: &CodegenState<'ctx>, ptr: PointerValue<'ctx>) -> CodegenResult<PointerValue<'ctx>> {
  cg.builder.build_struct_gep(ptr, 0, "data").or(Err(CodegenError::InvalidStructPointer("MemRegion::data_ptr given bad state pointer".to_string())))
}

pub fn memregion_size_ptr<'ctx>(cg: &CodegenState<'ctx>, ptr: PointerValue<'ctx>) -> CodegenResult<PointerValue<'ctx>> {
  cg.builder.build_struct_gep(ptr, 1, "size").or(Err(CodegenError::InvalidStructPointer("MemRegion::size_ptr given bad state pointer".to_string())))
}

// The compiler views state field types as an array of type primitives. These will eventually
// be canonicalized into a known order, to maximize compatibility.
//
// What do we want to represent?
// (1) values that are directly inline. This should just be a Vec<TypePrimitive>
//    i.e. vec!(Int, Char, Char) represents 2 contiguous words in memory
// (2) pointers to regions of a known size. These are themselves a known size (1 word) and so can be directly inlined.
//      (a) directly inlined values.
//      (b) arrays of known size of direcly inlined values.
// (3) pointers to runtime-sized regions. These are also a known size (2 words) and so can be directly inlined.
// (4) MemRegions (as a special bottoming-out pointer-to-region-of-known-size primitive where more type information isn't available).
//
// So (Int, String) is vec!(Int, DynamicArrayOf(vec!(Char)))
#[derive(Clone, Debug, PartialEq)]
pub enum TypePrimitive {
  Int, Char, Bool, MemRegion, PointerTo(Vec<TypePrimitive>), FixedArrayOf(Vec<TypePrimitive>, u64), DynamicArrayOf(Vec<TypePrimitive>)
}

// PointerKind describes operationally how a pointer should be treated. This includes an understanding of how to move values
// between pointers, and how to inflate from a pointer to a StateValue
#[derive(PartialEq, Debug)]
pub enum PointerKind {
  // Pointers 1 word or smaller; the actual size is given by the LLVM pointer type used
  SingleWordPrimitive,
  // A dynamically (runtime) sized region of heap (or whatever). The value is 2 words big: (data_ptr, size)
  DynamicMemRegion,
  // A statically sized region of heap (or whatever). Size is given by type.
  StaticMemRegion,
  ConstPointer
}

pub fn type_size(type_vec: &Vec<TypePrimitive>) -> u64 {
  let mut size = 0;
  for h_type in type_vec {
    size += match h_type {
      TypePrimitive::Int => 8,
      TypePrimitive::Char | TypePrimitive::Bool => 1,
      TypePrimitive::MemRegion => 16,
      TypePrimitive::PointerTo(_x) => 8,
      TypePrimitive::FixedArrayOf(_x, _s) => 8,
      TypePrimitive::DynamicArrayOf(_x) => 16
    };
  }
  size
}

#[derive(Debug)]
pub struct StatePointer<'ctx> {
  pub pointer_kind: PointerKind,
  pub pointer: BasicValueEnum<'ctx>,
  pub pointer_type: Vec<TypePrimitive> // this is a pointer *to* the type primitives, i.e. these aren't necessarily all PointerTo.
}

pub fn type_primitive_for_type(h_type: &ast::Type) -> Vec<TypePrimitive> {
  match h_type {
    ast::Type::Int => vec!(TypePrimitive::Int),
    ast::Type::Char => vec!(TypePrimitive::Char),
    ast::Type::Bool => vec!(TypePrimitive::Bool),
    ast::Type::MemRegion => vec!(TypePrimitive::MemRegion),
    ast::Type::String => vec!(TypePrimitive::DynamicArrayOf(vec!(TypePrimitive::Char))),
    ast::Type::Tuple(members) => {
      let member_vec = members.iter().map(|t| type_primitive_for_type(t)).flatten().collect();
      if type_size(&member_vec) <= 16 {
        member_vec
      } else {
        vec!(TypePrimitive::PointerTo(member_vec))
      }
    }
  }
}

pub fn pointer_kind_for_type_primitive(primitive: &Vec<TypePrimitive>) -> PointerKind {
  if primitive.len() != 1 {
    panic!("Don't yet know how to deal with aggregate toplevel type primitives");
  }
  match &primitive[0] {
    TypePrimitive::Int | TypePrimitive::Char | TypePrimitive::Bool => PointerKind::SingleWordPrimitive,
    TypePrimitive::MemRegion => PointerKind::DynamicMemRegion,
    TypePrimitive::DynamicArrayOf(_x) => PointerKind::DynamicMemRegion,
    TypePrimitive::FixedArrayOf(_x, _s) => PointerKind::StaticMemRegion,
    TypePrimitive::PointerTo(_x) => PointerKind::StaticMemRegion
  }
}

impl <'ctx> StatePointer<'ctx> {
  pub fn new(ptr: PointerValue<'ctx>, h_type: ast::Type) -> Self {
    // h_type "should" match ptr.get_element_type() as the element type is derive (via the state struct)
    // from h_type.
    let pointer_type = type_primitive_for_type(&h_type);
    StatePointer::new_from_type_primitive(ptr, pointer_type)
  }
  pub fn new_from_type_primitive(ptr: PointerValue<'ctx>, pointer_type: Vec<TypePrimitive>) -> Self {
    let pointer_kind = pointer_kind_for_type_primitive(&pointer_type);
    StatePointer { pointer_kind, pointer: ptr.into(), pointer_type } 
  }
  pub fn load(&self, cg: &CodegenState<'ctx>, name: &str) -> CodegenResult<StateValue<'ctx>> {
    match self.pointer_kind {
      PointerKind::SingleWordPrimitive => {
        let value = cg.builder.build_load(self.pointer.into_pointer_value(), name);
        Ok(StateValue { value: ValueParts::SingleWordPrimitive(value), value_type: self.pointer_type.clone() })
      }
      PointerKind::StaticMemRegion => {
        let value = cg.builder.build_load(self.pointer.into_pointer_value(), name);
        Ok(StateValue { value: ValueParts::StaticMemRegion(value.into_pointer_value()), value_type: self.pointer_type.clone() })
      }
      PointerKind::DynamicMemRegion => {
        let data = cg.builder.build_load(memregion_data_ptr(cg, self.pointer.into_pointer_value())?, &(name.to_string() + "_data")).into_pointer_value();
        let size = cg.builder.build_load(memregion_size_ptr(cg, self.pointer.into_pointer_value())?, &(name.to_string() + "_size")).into_int_value();
        Ok(StateValue { value: ValueParts::DynamicMemRegion(data, size), value_type: self.pointer_type.clone() })
      }
      PointerKind::ConstPointer => {
        Ok(StateValue { value: ValueParts::SingleWordPrimitive(self.pointer), value_type: self.pointer_type.clone() })
      }
    }
  }
  // NOTE: This is *not* a general-purpose clear! It is not intended to set the pointed-at value
  // back to some default state; it's used to null out the update pointer (or zero out an
  // immediate value) after an update has been applied.
  pub fn clear_update_pointer(&self, cg: &CodegenState<'ctx>) -> CodegenStatus {
    match self.pointer_kind {
      PointerKind::SingleWordPrimitive => {
        cg.builder.build_store(self.pointer.into_pointer_value(), cg.uint_const(0));
        Ok(())
      }
      PointerKind::StaticMemRegion => {
        // TODO this probably doesn't work in all cases
        cg.builder.build_store(self.pointer.into_pointer_value(), self.pointer.into_pointer_value().get_type().get_element_type().into_pointer_type().const_null());
        Ok(())
      }
      PointerKind::DynamicMemRegion => {
        cg.builder.build_store(memregion_data_ptr(cg, self.pointer.into_pointer_value())?, cg.uint_const(0));
        cg.builder.build_store(memregion_size_ptr(cg, self.pointer.into_pointer_value())?, cg.uint_const(0));
        Ok(())
      }
      PointerKind::ConstPointer => panic!("Can't clear a const pointer")
    }
  }

  pub fn default_value(&self, cg: &CodegenState<'ctx>) -> CodegenResult<StateValue> {
    if self.pointer_type.len() != 1 {
      todo!("handle default inline aggregate values");
    }
    match self.pointer_type[0] {
      TypePrimitive::Int => Ok(StateValue::new_int(cg.uint_const(0))),
      _ => todo!("handle default of this type")
    }
  }
}

#[derive(Debug, Clone)]
pub enum ValueParts<'ctx> {
  None,
  Suppress, // used to indicate a known terminated code-flow
  SingleWordPrimitive(BasicValueEnum<'ctx>),
  DynamicMemRegion(PointerValue<'ctx>, IntValue<'ctx>),
  StaticMemRegion(PointerValue<'ctx>)
}

#[derive(Debug, Clone)]
pub struct StateValue<'ctx> {
  pub value: ValueParts<'ctx>,
  pub value_type: Vec<TypePrimitive>
}

impl <'ctx> StateValue<'ctx> {
  pub fn new_int(value: IntValue<'ctx>) -> Self {
    StateValue::new_prim_of_type(value.into(), vec!(TypePrimitive::Int))
  }
  pub fn new_bool(value: IntValue<'ctx>) -> Self {
    StateValue::new_prim_of_type(value.into(), vec!(TypePrimitive::Bool))
  }
  pub fn new_true(cg: &CodegenState<'ctx>) -> Self {
    StateValue::new_bool(cg.context.bool_type().const_int(1, false))
  }
  pub fn new_false(cg: &CodegenState<'ctx>) -> Self {
    StateValue::new_bool(cg.context.bool_type().const_int(0, false))
  }
  pub fn new_char(value: IntValue<'ctx>) -> Self {
    StateValue::new_prim_of_type(value.into(), vec!(TypePrimitive::Char))
  }
  pub fn new_prim_of_type(value: BasicValueEnum<'ctx>, value_type: Vec<TypePrimitive>) -> Self {
    StateValue { value: ValueParts::SingleWordPrimitive(value), value_type }
  }
  pub fn new_dynamic_mem_region_of_type(data: PointerValue<'ctx>, size: IntValue<'ctx>, value_type: Vec<TypePrimitive>) -> Self {
    StateValue { value: ValueParts::DynamicMemRegion(data, size), value_type }
  }
  pub fn new_static_mem_region_of_type(data: PointerValue<'ctx>, value_type: Vec<TypePrimitive>) -> Self {
    StateValue { value: ValueParts::StaticMemRegion(data), value_type}
  }
  pub fn new_tuple(cg: &CodegenState<'ctx>, tuple_type: Vec<TypePrimitive>) -> CodegenResult<Self> {
    if let TypePrimitive::PointerTo(members) = &tuple_type[0] {
      let tuple_size = type_size(&members);
      let tuple_ptr = super::malloc(cg, cg.uint32_const(tuple_size as u32).into(), "tuple_memory").into_pointer_value();
      let tuple_llvm_type = super::llvm_type_for_primitive(cg, &tuple_type);
      let typed_tuple_ptr = cg.builder.build_bitcast(tuple_ptr, tuple_llvm_type, "ptr_as_struct_ptr").into_pointer_value();
      Ok(StateValue::new_static_mem_region_of_type(typed_tuple_ptr, tuple_type))
    } else {
      Err(CodegenError::TypeMismatch("Attempt to construct tuple from non-tuple type".to_string()))
    }
  }
  pub fn new_none() -> Self {
    StateValue { value: ValueParts::None, value_type: Vec::new() }
  }
  pub fn new_suppress() -> Self {
    StateValue { value: ValueParts::Suppress, value_type: Vec::new() }
  }

  pub fn into_int_value(&self) -> CodegenResult<IntValue<'ctx>> {
    if let ValueParts::SingleWordPrimitive(v) = self.value {
      Ok(v.into_int_value())
    } else {
      Err(CodegenError::TypeMismatch(std::format!("can't into_int_value() on ${:?}", self)))
    }
  }
  pub fn into_pointer_value(&self) -> CodegenResult<PointerValue<'ctx>> {
    if let ValueParts::DynamicMemRegion(ptr, _) | ValueParts::StaticMemRegion(ptr) = self.value {
      Ok(ptr)
    } else {
      Err(CodegenError::TypeMismatch(std::format!("Can't into_pointer_value on ${:?}", self)))
    }
  }

  pub fn llvm_type(&self) -> BasicTypeEnum<'ctx> {
    match self.value {
      ValueParts::SingleWordPrimitive(value) => value.get_type(),
      ValueParts::DynamicMemRegion(ptr, _) | ValueParts::StaticMemRegion(ptr) => ptr.get_type().into(),
      ValueParts::None | ValueParts::Suppress => panic!("dunno!"),
    }
  }
  pub fn is_none(&self) -> bool {
    if let ValueParts::None = self.value {
      true
    } else {
      false
    }
  }

  pub fn is_suppress(&self) -> bool {
    if let ValueParts::Suppress = self.value {
      true
    } else {
      false
    }
  }

  fn only_value_type(&self) -> CodegenResult<&TypePrimitive> {
    if self.value_type.len() != 1 {
      return Err(CodegenError::TypeMismatch("Attempt to treat inline aggregate value as atomic value".to_string()));
    } 
    Ok(&self.value_type[0])
  }

  fn static_mem_region_members(&self) -> CodegenResult<&Vec<TypePrimitive>> {
    let value_type = self.only_value_type()?;
    if let TypePrimitive::PointerTo(elements) = value_type {
      Ok(elements)
    } else {
      Err(CodegenError::TypeMismatch("Attempt to treat non-pointer type as pointer".to_string()))
    }
    
  }

  pub fn debug(&self, cg: &mut CodegenState<'ctx>, printer: &mut dyn Printer<'ctx>) -> CodegenStatus {
    match self.value {
      ValueParts::SingleWordPrimitive(v) => {
        let value_type = self.only_value_type()?;
        match value_type {
          TypePrimitive::Int => {
            printer.printf(cg, "[Int %ld]", &[v])
          }
          TypePrimitive::Bool => {
            printer.printf(cg, "[Bool %d]", &[v])
          }
          TypePrimitive::Char => {
            // TODO: Printing %c isn't safe if the char isn't a sub-range of ASCII
            printer.printf(cg, "[Char %d]", &[v])
          }
          _ => todo!("Implement debug for {:?}", value_type)
        }
      }
      ValueParts::DynamicMemRegion(data, size) => {
        // TODO: this is only correct if an array of Char
        let size32 = cg.builder.build_int_cast(size, cg.context.i32_type(), "size32");
        printer.printf(cg, "[DynString %.*s]", &[size32.into(), data.into()])
      }
      ValueParts::StaticMemRegion(data) => {
        if let TypePrimitive::PointerTo(type_list) = self.only_value_type()? {
          print_if_not_null(cg, printer, data, 
            &|cg, printer| printer.printf(cg, "[NULL_PTR]", &[]),
            &|cg, printer| {
              printer.open_bracket(cg, "(")?;
              for type_idx in 0..type_list.len() {
                let value = self.get_tuple_index(cg, type_idx as u32)?;
                value.debug(cg, printer)?;
                printer.sep(cg, ",")?;
              }
              printer.close_bracket(cg, ")")
            }
           )
        } else {
          todo!("Implement debug for static mem region which is not a PointerTo");
        }
      }
      _ => todo!("Implement debug for {:?} {:?}", self.value, self.value_type)
    }
  }

  pub fn store(&self, cg: &CodegenState<'ctx>, ptr: &StatePointer<'ctx>) -> CodegenStatus {
    // TODO: more complex type comparison?
    if !(ptr.pointer_type == self.value_type) {
      return Err(CodegenError::TypeMismatch("Attempt to store value into mismatched pointer".to_string()));
    }
    match self.value {
      ValueParts::SingleWordPrimitive(v) => {
        if ptr.pointer_kind == PointerKind::SingleWordPrimitive {
          cg.builder.build_store(ptr.pointer.into_pointer_value(), v);
          Ok(())
        } else {
          Err(CodegenError::TypeMismatch("Attempt to store integer into non-integer pointer".to_string()))
        }
      }
      ValueParts::DynamicMemRegion(data, size) => {
        if ptr.pointer_kind == PointerKind::DynamicMemRegion {
          cg.builder.build_store(memregion_data_ptr(cg, ptr.pointer.into_pointer_value())?, data);
          cg.builder.build_store(memregion_size_ptr(cg, ptr.pointer.into_pointer_value())?, size);
          Ok(())
        } else {
          Err(CodegenError::TypeMismatch("Attempt to store memrange into non-memrange pointer".to_string()))
        }
      }
      ValueParts::StaticMemRegion(data) => {
        if ptr.pointer_kind == PointerKind::StaticMemRegion {
          cg.builder.build_store(ptr.pointer.into_pointer_value(), data);
          Ok(())
        } else if ptr.pointer_kind == PointerKind::DynamicMemRegion {
          cg.builder.build_store(memregion_data_ptr(cg, ptr.pointer.into_pointer_value())?, data);
          cg.builder.build_store(memregion_size_ptr(cg, ptr.pointer.into_pointer_value())?, cg.uint_const(type_size(&ptr.pointer_type)));
          Ok(())
        } else {
          Err(CodegenError::TypeMismatch("Attempt to store static-size region into non-region pointer".to_string()))
        }
      }
      ValueParts::None => Ok(()),
      ValueParts::Suppress => panic!("Attempting to store in known-terminated code flow"),
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
      ValueParts::StaticMemRegion(_data) => Ok(StateValue::new_int(cg.uint_const(type_size(&self.value_type)))),
      ValueParts::None => Err(CodegenError::InvalidFunctionArgument("Can't call size() on None result".to_string())),
      ValueParts::Suppress => panic!("Attempting to size a known-terminated code flow"),
    }
  }
  pub fn array_lookup(&self, cg: &CodegenState<'ctx>, index: StateValue<'ctx>) -> CodegenResult<Self> {
    // TODO: bounds checking when necessary
    let value_type = self.only_value_type()?;
    match value_type {
      TypePrimitive::DynamicArrayOf(element_type) => {
        let value_ptr;
        unsafe {
          value_ptr = cg.builder.build_gep(self.into_pointer_value()?, &[index.into_int_value()?], "lookup");
        }
        let value = cg.builder.build_load(value_ptr, "value");
        Ok(StateValue::new_prim_of_type(value, element_type.clone()))
      }
      _otherwise => Err(CodegenError::TypeMismatch(format!("Can't perform array lookup on {:?}", self.value_type)))
    }
  } 
  fn tuple_index_ptr(&self, cg: &CodegenState<'ctx>, index: u32) -> CodegenResult<StatePointer<'ctx>> {
    let ptr = cg.builder.build_struct_gep(self.into_pointer_value()?, index as u32, "tuple_entry").or(Err(CodegenError::InvalidStructPointer("Bad tuple pointer".to_string())))?;

    let members = self.static_mem_region_members()?;
    let ptr_type = vec!(members[index as usize].clone());
    Ok(StatePointer::new_from_type_primitive(ptr, ptr_type))
  }
  pub fn set_tuple_index(&self, cg: &CodegenState<'ctx>, index: u32, value: StateValue<'ctx>) -> CodegenStatus {
    // TODO: we should probably check if this type matches the expression type.
    let typed_ptr = self.tuple_index_ptr(cg, index)?;
    value.store(cg, &typed_ptr)
  }
  pub fn get_tuple_index(&self, cg: &CodegenState<'ctx>, index: u32) -> CodegenResult<StateValue<'ctx>> {
    let typed_ptr = self.tuple_index_ptr(cg, index)?;
    typed_ptr.load(cg, "tuple_at_idx")
  }
  pub fn add_to_phi_node(&self, node: PhiValue, block: BasicBlock<'ctx>) -> CodegenStatus {
    // TODO: non-unitary types
    let value_type = self.only_value_type()?;
    match value_type {
      TypePrimitive::Char | TypePrimitive::Int | TypePrimitive::Bool => node.add_incoming(&[(&self.into_int_value()?, block)]),
      _ => todo!("phi node processing for non-primitive types")
    }
    Ok(())
  }
  pub fn equals(&self, cg: &mut CodegenState<'ctx>, other: &StateValue<'ctx>) -> CodegenResult<StateValue<'ctx>> {
    let value_type = self.only_value_type()?;
    match value_type {
      TypePrimitive::Char | TypePrimitive::Int | TypePrimitive::Bool => self.apply_int_predicate(cg, IntPredicate::EQ, other),
      TypePrimitive::PointerTo(struct_type) => {
        // TODO: need to check whether self and/or other is NULL.
        let self_as_int = cg.builder.build_ptr_to_int(self.into_pointer_value()?, cg.context.i64_type(), "self_as_int");
        let self_is_null = cg.builder.build_int_compare(IntPredicate::EQ, self_as_int, cg.uint_const(0), "self_is_null");

        let other_as_int = cg.builder.build_ptr_to_int(other.into_pointer_value()?, cg.context.i64_type(), "other_as_int");
        let other_is_null = cg.builder.build_int_compare(IntPredicate::EQ, other_as_int, cg.uint_const(0), "other_is_null");

        // if (self_is_null) {
        //   return other_is_null;
        // } else {
        //   if (other_is_null) {
        //     return false;
        //   } else {
        //     .. do normal check
        //   }
        // }


        if_else_expression(cg, self_is_null,
          |_cg| Ok(StateValue::new_bool(other_is_null)),
          |cg| if_else_expression(cg, other_is_null,
            |cg| Ok(StateValue::new_false(cg)),
            |cg| {
              let mut result = StateValue::new_bool(cg.context.bool_type().const_int(1, false));
              for (idx, _member_type) in struct_type.iter().enumerate() {
                let lhs = self.get_tuple_index(cg, idx as u32)?;
                let rhs = other.get_tuple_index(cg, idx as u32)?;
                // TODO: this clone of result is unpleasant
                result = expression_logical_and(cg, move |_cg| Ok(result.clone()), |cg| lhs.equals(cg, &rhs))?
              }
              Ok(result)
            }
          )
        )

      }
      TypePrimitive::DynamicArrayOf(array_member_type) => {
        // specialization for array of single type is to use memcmp
        if array_member_type.len() == 1 {
          let arr_type = &array_member_type[0];
          match arr_type {
            TypePrimitive::Char | TypePrimitive::Int | TypePrimitive::Bool => {
              expression_logical_and(cg,
                |cg| self.size(cg)?.equals(cg, &other.size(cg)?),
                |cg| StateValue::new_int(cg.uint_const(0)).equals(cg, &StateValue::new_int(memcmp(cg, self.into_pointer_value()?, other.into_pointer_value()?, self.size(cg)?.into_int_value()?)))
              )
            }
            _ => todo!("")
          }
        } else {
          todo!("")
        }
      }
      _ => todo!("equality for {:?}", value_type)
    }
  }

  pub fn not_equals(&self, cg: &mut CodegenState<'ctx>, other: &StateValue<'ctx>) -> CodegenResult<StateValue<'ctx>> {
    self.equals(cg, other)?.not(cg)
  }

  pub fn numeric_or(&self, cg: &CodegenState<'ctx>, other: &StateValue<'ctx>) -> CodegenResult<StateValue<'ctx>> {
    let value_type = self.only_value_type()?;
    let other_value_type = other.only_value_type()?;
    if *value_type != TypePrimitive::Bool || *other_value_type != TypePrimitive::Bool {
      Err(CodegenError::TypeMismatch("Can't use logical or on non-bools".to_string()))
    } else {
      let result = cg.builder.build_or(self.into_int_value()?, other.into_int_value()?, "logical-or");
      Ok(StateValue::new_bool(result))
    }
  }

  pub fn less_than(&self, cg: &CodegenState<'ctx>, other: &StateValue<'ctx>) -> CodegenResult<StateValue<'ctx>> {
    // TODO: This isn't right, need to take signedness into account
    self.apply_int_predicate(cg, IntPredicate::SLT, other)
  }

  pub fn less_than_or_equal(&self, cg: &CodegenState<'ctx>, other: &StateValue<'ctx>) -> CodegenResult<StateValue<'ctx>> {
    // TODO: This isn't right, need to take signedness into account
    self.apply_int_predicate(cg, IntPredicate::SLE, other)
  }

  pub fn greater_than(&self, cg: &CodegenState<'ctx>, other: &StateValue<'ctx>) -> CodegenResult<StateValue<'ctx>> {
    // TODO: This isn't right, need to take signedness into account
    self.apply_int_predicate(cg, IntPredicate::SGT, other)
  }

  pub fn greater_than_or_equal(&self, cg: &CodegenState<'ctx>, other: &StateValue<'ctx>) -> CodegenResult<StateValue<'ctx>> {
    // TODO: This isn't right, need to take signedness into account
    self.apply_int_predicate(cg, IntPredicate::SGE, other)
  }

  fn bitsize_adjusted(cg: &CodegenState<'ctx>, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> (IntValue<'ctx>, IntValue<'ctx>) {
    let lhs_bitsize = lhs.get_type().get_bit_width();
    let rhs_bitsize = rhs.get_type().get_bit_width();
    if lhs_bitsize > rhs_bitsize {
      (lhs, cg.builder.build_int_cast(rhs, lhs.get_type(), "sext_rhs"))
    } else if rhs_bitsize > lhs_bitsize {
      (cg.builder.build_int_cast(lhs, rhs.get_type(), "sext_lhs"), rhs)
    } else {
      (lhs, rhs)
    }
  }

  fn apply_int_predicate(&self, cg: &CodegenState<'ctx>, predicate: IntPredicate, other: &StateValue<'ctx>) -> CodegenResult<StateValue<'ctx>> {
    // TODO: compound type equality
    let value_type = self.only_value_type()?;
    match value_type {
      TypePrimitive::Char | TypePrimitive::Int | TypePrimitive::Bool => {
        let (lhs, rhs) = StateValue::bitsize_adjusted(cg, self.into_int_value()?, other.into_int_value()?);
        let result = cg.builder.build_int_compare(predicate, lhs, rhs, "eq");
        Ok(StateValue::new_bool(result))
      }
      _ => todo!("IntPredicate for {:?}", value_type)
    }
  }

  pub fn multiply(&self, cg: &CodegenState<'ctx>, other: &StateValue<'ctx>) -> CodegenResult<StateValue<'ctx>> {
    let lhs = self.into_int_value()?;
    let rhs = other.into_int_value()?;
    Ok(StateValue::new_int(cg.builder.build_int_mul(lhs, rhs, "multiply")))
  }

  pub fn add(&self, cg: &CodegenState<'ctx>, other: &StateValue<'ctx>) -> CodegenResult<StateValue<'ctx>> {
    let (lhs, rhs) = StateValue::bitsize_adjusted(cg, self.into_int_value()?, other.into_int_value()?);
    Ok(StateValue::new_int(cg.builder.build_int_add(lhs, rhs, "add")))
  }

  pub fn subtract(&self, cg: &CodegenState<'ctx>, other: &StateValue<'ctx>) -> CodegenResult<StateValue<'ctx>> {
    let (lhs, rhs) = StateValue::bitsize_adjusted(cg, self.into_int_value()?, other.into_int_value()?);
    Ok(StateValue::new_int(cg.builder.build_int_sub(lhs, rhs, "subtract")))
  }

  pub fn not(&self, cg: &CodegenState<'ctx>) -> CodegenResult<StateValue<'ctx>> {
    let value_type = self.only_value_type()?;
    match value_type {
      TypePrimitive::Bool => {
        Ok(StateValue::new_bool(cg.builder.build_not(self.into_int_value()?, "not")))
      }
      _ => Err(CodegenError::TypeMismatch("Can't execute not on non-bool types".to_string()))
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
    let h_type = module.type_for_field(name).ok_or(CodegenError::BadReadFieldName(name.to_string()))?;
    if let Some(idx) = module.idx_for_field(name) {
      let struct_idx = (2 * idx).try_into().or(Err(CodegenError::InvalidIndex))?;
      let ptr = self.builder.build_struct_gep(state, struct_idx, &("read_ptr_to".to_string() + name)).or(Err(CodegenError::InvalidStructPointer("read_ptr_for_field given bad state pointer".to_string())))?;
      Ok(StatePointer::new(ptr, h_type))
    } else if let Some(idx) = module.value_param_idx_for_field(name) {
      dbg!(idx);
      dbg!(state.get_type());
      let param_idx = module.idx_for_bitfield() + idx + 1;
      dbg!(param_idx);
      dbg!(&h_type);
      let ptr = self.builder.build_struct_gep(state, param_idx as u32, &("read_param_ptr_to".to_string() + name)).or(Err(CodegenError::InvalidStructPointer("read_ptr_for_field given bad state pointer".to_string())))?;
      Ok(StatePointer::new(ptr, h_type))
    } else {
      Err(CodegenError::BadReadFieldName(name.to_string()))
    }
  }

  pub fn update_ptr_for_field(&self, module: &ast::Module, state: PointerValue<'ctx>, name: &str, purpose: UpdatePtrPurpose) -> CodegenResult<StatePointer<'ctx>> {
    let idx = module.idx_for_field(name).ok_or(CodegenError::BadUpdateFieldName)?;
    let h_type = module.type_for_field(name).ok_or(CodegenError::BadReadFieldName(name.to_string()))?;
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