pub mod codegen;
pub mod codegen_state;
mod state_values;

use super::ast;
use codegen::{malloc, llvm_type_for_primitive};

pub use codegen::{codegen, main_for_examples};