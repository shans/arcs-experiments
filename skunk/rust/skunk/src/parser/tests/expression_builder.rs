
use super::super::ast::{ExpressionValue, ExpressionValueEnum, Operator, OutputExpression, SafeSpan};

// TODO: consider putting offset, line arguments for functions below into a struct.
pub struct ExpressionBuilder<'a> {
  offset: usize,
  line: u32,
  expr: Box<dyn 'a + FnOnce(usize, u32) -> ExpressionValueEnum>,
}

impl <'a> ExpressionBuilder<'a> {
  fn mk(self, offset: usize, line: u32) -> ExpressionValue {
    let offset = offset + self.offset;
    let line = line + self.line;
    ExpressionValue { 
      position: SafeSpan { offset, line },
      info: (self.expr)(offset, line),
    }
  }
  pub fn block(offset: usize, line: u32, mut body: Vec<ExpressionBuilder<'a>>) -> Self {
    ExpressionBuilder { expr: Box::new(move |offset, line| ExpressionValueEnum::Block(body.drain(..).map(|e| e.mk(offset, line)).collect())), offset, line }
  }
  pub fn output(offset: usize, line: u32, name: &'a str, expr: ExpressionBuilder<'a>) -> Self {
    ExpressionBuilder { expr: Box::new(move |offset, line| ExpressionValueEnum::Output(OutputExpression { output: name.to_string(), expression: Box::new(expr.mk(offset, line)), and_return: false })), offset, line }
  }
  pub fn sref(offset: usize, line: u32, name: &'a str) -> Self {
    ExpressionBuilder { expr: Box::new(move |_, _| ExpressionValueEnum::ReferenceToState(name.to_string())), offset, line }
  }
  pub fn empty(offset: usize, line: u32) -> Self {
    ExpressionBuilder { expr: Box::new(move |_, _| ExpressionValueEnum::Empty), offset, line }
  }
  pub fn fun(offset: usize, line: u32, name: &'a str, expr: ExpressionBuilder<'a>) -> Self {
    ExpressionBuilder { expr: Box::new(move |offset, line| ExpressionValueEnum::FunctionCall(name.to_string(), Box::new(expr.mk(offset, line)))), offset, line }
  }
  pub fn op(self, offset: usize, line: u32, op: Operator, rhs: ExpressionBuilder<'a>) -> Self {
    let offset = offset + self.offset;
    let line = line + self.line;
    ExpressionBuilder { expr: Box::new(move |prev_offset, prev_line| {
      let lhs = self.mk(prev_offset - offset, prev_line - line);
      let rhs = rhs.mk(prev_offset, prev_line);
      ExpressionValueEnum::BinaryOperator(Box::new(lhs), op, Box::new(rhs))
    }), offset, line }
  }
  pub fn tuple_ref(self, offset: usize, line: u32, idx: i64) -> Self {
    let offset = offset + self.offset;
    let line = line + self.line;
    ExpressionBuilder { expr: Box::new(move |prev_offset, prev_line| {
      let lhs = self.mk(prev_offset - offset, prev_line - line);
      ExpressionValueEnum::TupleLookup(Box::new(lhs), idx)
    }), offset, line }
  }
  pub fn tuple(offset: usize, line: u32, mut exprs: Vec<ExpressionBuilder<'a>>) -> Self {
    ExpressionBuilder { expr: Box::new(move |offset, line| ExpressionValueEnum::Tuple(exprs.drain(..).map(|e| e.mk(offset, line)).collect())), offset, line }
  }
  pub fn array_index(self, offset: usize, line: u32, expr: ExpressionBuilder<'a>) -> Self {
    let offset = offset + self.offset;
    let line = line + self.line;
    ExpressionBuilder { expr: Box::new(move |prev_offset, prev_line| {
      let lhs = self.mk(prev_offset - offset, prev_line - line);
      let rhs = expr.mk(prev_offset, prev_line);
      ExpressionValueEnum::ArrayLookup(Box::new(lhs), Box::new(rhs))
    }), offset, line }
  }
  pub fn char(offset: usize, line: u32, const_char: char) -> Self {
    ExpressionBuilder { expr: Box::new(move |_, _| ExpressionValueEnum::CharLiteral(const_char as u8)), offset, line }
  }
  pub fn build(self) -> ExpressionValue {
    self.mk(0, 1)
  }
}

