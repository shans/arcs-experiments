extern crate nom_locate;

use std::collections::hash_map::HashMap;
use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Usage {
  Read,
  Write,
}

// TODO: consider making references Rc<Vec<Type>> so this is copiable.
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
  Int,
  String,
  Char,
  Bool,
  MemRegion,
  Tuple(Vec<Type>)
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ListenerKind {
  OnChange,
  OnWrite
}

impl ListenerKind {
  pub fn to_string(&self) -> &str {
    match self {
      ListenerKind::OnChange => "OnChange",
      ListenerKind::OnWrite => "OnWrite"
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Handle<'a> {
  pub position: Span<'a>,
  pub name: Span<'a>,
  pub usages: Vec<Usage>,
  pub h_type: Type,
}

impl <'a> Handle<'a> {
  pub fn is_input(&self) -> bool {
    self.usages.iter().any(|usage| *usage == Usage::Read)
  }

  pub fn is_output(&self) -> bool {
    self.usages.iter().any(|usage| *usage == Usage::Write)
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CopyTo {
  pub state: String,
  pub submodule_index: usize,
  pub submodule_state: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
  Equality,
  LogicalOr,
  LogicalAnd,
  LessThan,
  LessThanOrEqual,
  GreaterThan,
  GreaterThanOrEqual,
  Multiply,
  Divide,
  Add,
  Subtract,
}

impl Operator {
  pub fn precedence(&self) -> usize {
    match self {
      Operator::Multiply | Operator::Divide => 120,
      Operator::Add | Operator::Subtract => 100,
      Operator::LogicalOr => 40,
      Operator::LogicalAnd => 50,
      Operator::LessThan | Operator::LessThanOrEqual | Operator::GreaterThanOrEqual | Operator::GreaterThan | Operator::Equality => 60,
    }
  }
  pub fn is_logical(&self) -> bool {
    match self {
      Operator::LogicalOr => true,
      Operator::LogicalAnd => true,
      _ => false,
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression<'a> {
  pub value: ExpressionValue<'a>,
  pub is_terminated: bool,
  pub precedence: usize,
}

impl <'a> Expression<'a> {
  pub fn terminated(value: ExpressionValue<'a>) -> Self {
    Expression { value, is_terminated: true, precedence: 0 }
  }
  pub fn unterminated(value: ExpressionValue<'a>) -> Self {
    Expression { value, is_terminated: false, precedence: 0 }
  }
  pub fn binary_operator(position: Span<'a>, lhs: Expression<'a>, op: Operator, rhs: Expression<'a>) -> Self {
    let precedence = op.precedence();
    Expression { value: ExpressionValue { info: ExpressionValueEnum::BinaryOperator(Box::new(lhs.value), op, Box::new(rhs.value)), position }, precedence, is_terminated: rhs.is_terminated }
  }
  pub fn empty(position: Span<'a>) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::Empty, position })
  }
  pub fn state_reference(position: Span<'a>, state: &str) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::ReferenceToState(state.to_string()), position })
  }
  pub fn int_literal(position: Span<'a>, literal: i64) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::IntLiteral(literal), position })
  }
  pub fn string_literal(position: Span<'a>, literal: &str) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::StringLiteral(literal.to_string()), position })
  }
  pub fn char_literal(position: Span<'a>, literal: u8) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::CharLiteral(literal), position })
  }
  pub fn tuple(position: Span<'a>, mut members: Vec<Expression<'a>>) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::Tuple(members.drain(..).map(|a| a.value).collect()), position })
  }
  pub fn function_call(position: Span<'a>, name: &str, argument: Expression<'a>) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::FunctionCall(name.to_string(), Box::new(argument.into())), position })
  }
  pub fn array_lookup(position: Span<'a>, array: Expression<'a>, lookup: Expression<'a>) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::ArrayLookup(Box::new(array.into()), Box::new(lookup.into())), position })
  }
  pub fn tuple_lookup(position: Span<'a>, tuple: Expression<'a>, literal: i64) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::TupleLookup(Box::new(tuple.into()), literal), position })
  }
  pub fn block(position: Span<'a>, mut expressions: Vec<Expression<'a>>) -> Self {
    Expression::terminated(ExpressionValue { info: ExpressionValueEnum::Block(expressions.drain(..).map(|a| a.value).collect()), position })
  }
  pub fn break_expression(position: Span<'a>) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::Break, position })
  }
  pub fn output(position: Span<'a>, name: &str, expression: Expression<'a>, and_return: bool) -> Self {
    let output = OutputExpression { output: name.to_string(), expression: Box::new(expression.into()), and_return };
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::Output(output), position })
  }
  pub fn let_expression(position: Span<'a>, name: &str, expression: Expression<'a>, is_update: bool) -> Self {
    let let_expression = LetExpression { var_name: name.to_string(), expression: Box::new(expression.into()), is_update };
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::Let(let_expression), position })
  }
  pub fn if_expression(position: Span<'a>, test: Expression<'a>, if_true: Expression<'a>, if_false: Expression<'a>) -> Self {
    let if_expression = IfExpression { test: Box::new(test.into()), if_true: Box::new(if_true.into()), if_false: Box::new(if_false.into()) };
    Expression::terminated(ExpressionValue { info: ExpressionValueEnum::If(if_expression), position })
  }
  pub fn while_expression(position: Span<'a>, test: Expression<'a>, body: Expression<'a>) -> Self {
    let while_expression = WhileExpression { test: Box::new(test.into()), body: Box::new(body.into()) };
    Expression::terminated(ExpressionValue { info: ExpressionValueEnum::While(while_expression), position })
  }
  pub fn copy_to_submodule(position: Span<'a>, state: &str, submodule_index: usize, submodule_state: &str) -> Self {
    let copy_to = CopyTo { state: state.to_string(), submodule_index, submodule_state: submodule_state.to_string() };
    Expression::terminated(ExpressionValue { info: ExpressionValueEnum::CopyToSubModule(copy_to), position })
  }
}

pub struct Expr<'a> {
  offset: usize,
  line: u32,
  expr: Box<dyn 'a + FnOnce(usize, u32) -> ExpressionValueEnum<'a>>,
}

impl <'a> Expr<'a> {
  fn mk(self, offset: usize, line: u32) -> ExpressionValue<'a> {
    let offset = offset + self.offset;
    let line = line + self.line;
    ExpressionValue { 
      position: unsafe { Span::new_from_raw_offset(offset, line, "", ()) },
      info: (self.expr)(offset, line),
    }
  }
  pub fn block(offset: usize, line: u32, mut body: Vec<Expr<'a>>) -> Self {
    Expr { expr: Box::new(move |offset, line| ExpressionValueEnum::Block(body.drain(..).map(|e| e.mk(offset, line)).collect())), offset, line }
  }
  pub fn output(offset: usize, line: u32, name: &'a str, expr: Expr<'a>) -> Self {
    Expr { expr: Box::new(move |offset, line| ExpressionValueEnum::Output(OutputExpression { output: name.to_string(), expression: Box::new(expr.mk(offset, line)), and_return: false })), offset, line }
  }
  pub fn sref(offset: usize, line: u32, name: &'a str) -> Self {
    Expr { expr: Box::new(move |_, _| ExpressionValueEnum::ReferenceToState(name.to_string())), offset, line }
  }
  pub fn empty(offset: usize, line: u32) -> Self {
    Expr { expr: Box::new(move |_, _| ExpressionValueEnum::Empty), offset, line }
  }
  pub fn fun(offset: usize, line: u32, name: &'a str, expr: Expr<'a>) -> Self {
    Expr { expr: Box::new(move |offset, line| ExpressionValueEnum::FunctionCall(name.to_string(), Box::new(expr.mk(offset, line)))), offset, line }
  }
  pub fn op(self, offset: usize, line: u32, op: Operator, rhs: Expr<'a>) -> Self {
    let offset = offset + self.offset;
    let line = line + self.line;
    Expr { expr: Box::new(move |prev_offset, prev_line| {
      let lhs = self.mk(prev_offset - offset, prev_line - line);
      let rhs = rhs.mk(prev_offset, prev_line);
      ExpressionValueEnum::BinaryOperator(Box::new(lhs), op, Box::new(rhs))
    }), offset, line }
  }
  pub fn tuple_ref(self, offset: usize, line: u32, idx: i64) -> Self {
    let offset = offset + self.offset;
    let line = line + self.line;
    Expr { expr: Box::new(move |prev_offset, prev_line| {
      let lhs = self.mk(prev_offset - offset, prev_line - line);
      ExpressionValueEnum::TupleLookup(Box::new(lhs), idx)
    }), offset, line }
  }
  pub fn tuple(offset: usize, line: u32, mut exprs: Vec<Expr<'a>>) -> Self {
    Expr { expr: Box::new(move |offset, line| ExpressionValueEnum::Tuple(exprs.drain(..).map(|e| e.mk(offset, line)).collect())), offset, line }
  }
  pub fn array_index(self, offset: usize, line: u32, expr: Expr<'a>) -> Self {
    let offset = offset + self.offset;
    let line = line + self.line;
    Expr { expr: Box::new(move |prev_offset, prev_line| {
      let lhs = self.mk(prev_offset - offset, prev_line - line);
      let rhs = expr.mk(prev_offset, prev_line);
      ExpressionValueEnum::ArrayLookup(Box::new(lhs), Box::new(rhs))
    }), offset, line }
  }
  pub fn char(offset: usize, line: u32, const_char: char) -> Self {
    Expr { expr: Box::new(move |_, _| ExpressionValueEnum::CharLiteral(const_char as u8)), offset, line }
  }
  pub fn build(self) -> ExpressionValue<'a> {
    self.mk(0, 1)
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionValue<'a> {
  pub info: ExpressionValueEnum<'a>,
  pub position: Span<'a>
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionValueEnum<'a> {
  // statement-like
  Output(OutputExpression<'a>),
  Block(Vec<ExpressionValue<'a>>),
  Let(LetExpression<'a>),
  If(IfExpression<'a>),
  While(WhileExpression<'a>),

  Empty, // used to terminate Blocks with no return
  Break,

  // expression-like
  // TODO: ArrayLookup and maybe TupleLookup should probably be examples of BinaryOperators.
  ArrayLookup(Box<ExpressionValue<'a>>, Box<ExpressionValue<'a>>),
  ReferenceToState(String),
  CopyToSubModule(CopyTo),
  FunctionCall(String, Box<ExpressionValue<'a>>),
  StringLiteral(String),
  IntLiteral(i64),
  CharLiteral(u8),
  Tuple(Vec<ExpressionValue<'a>>),
  TupleLookup(Box<ExpressionValue<'a>>, i64),
  BinaryOperator(Box<ExpressionValue<'a>>, Operator, Box<ExpressionValue<'a>>)
}

impl <'a> From<Expression<'a>> for ExpressionValue<'a> {
  fn from(expr: Expression<'a>) -> ExpressionValue<'a> {
    expr.value
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct OutputExpression<'a> {
  pub output: String,
  pub expression: Box<ExpressionValue<'a>>,
  pub and_return: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetExpression<'a> {
  pub var_name: String,
  pub expression: Box<ExpressionValue<'a>>,
  pub is_update: bool
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression<'a> {
  pub test: Box<ExpressionValue<'a>>,
  pub if_true: Box<ExpressionValue<'a>>,
  pub if_false: Box<ExpressionValue<'a>>
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileExpression<'a> {
  pub test: Box<ExpressionValue<'a>>,
  pub body: Box<ExpressionValue<'a>>
}

#[derive(Debug, PartialEq, Clone)]
pub struct Listener<'a> {
  pub trigger: String,
  pub kind: ListenerKind,
  pub implementation: ExpressionValue<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ModuleInfo<'a> {
  pub module: Module<'a>,
  pub handle_map: HashMap<String, String>
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExampleInfo<'a> {
  pub value: Expression<'a>,
  pub is_update: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Example<'a> {
  pub inputs: HashMap<String, ExampleInfo<'a>>,
  pub expected: HashMap<String, ExampleInfo<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Examples<'a> {
  pub examples: Vec<Example<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Module<'a> {
  pub name: String,
  pub handles: Vec<Handle<'a>>,
  pub listeners: Vec<Listener<'a>>,
  pub submodules: Vec<ModuleInfo<'a>>,
  pub examples: Examples<'a>,
}

impl <'a> Module<'a> {
  pub fn idx_for_field(&self, field: &str) -> Option<usize> {
    self.handles.iter().position(|handle| *handle.name.fragment() == field)
  }

  pub fn idx_for_bitfield(&self) -> usize {
    self.handles.len() * 2
  }

  pub fn outputs(&self) -> Vec<&Handle> {
    self.handles.iter().filter(|&handle| handle.is_output()).collect()
  }

  pub fn inputs(&self) -> Vec<&Handle> {
    self.handles.iter().filter(|&handle| handle.is_input()).collect()
  }

  pub fn handle_for_field(&self, field: &str) -> Option<&Handle> {
    self.handles.iter().find(|handle| *handle.name.fragment() == field)
  }

  pub fn type_for_field(&self, field: &str) -> Option<Type> {
    Some(self.handle_for_field(field)?.h_type.clone())
  }
}

#[derive(Debug, PartialEq)]
pub struct Graph {
  pub modules: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub enum TopLevel<'a> {
  Module(Module<'a>),
  Graph(Graph),
}

pub fn modules<'a>(ast: &'a Vec<TopLevel<'a>>) -> Vec<&'a Module<'a>> {
  ast.iter().filter_map(|top_level| {
    match top_level {
      TopLevel::Module(m) => Some(m),
      TopLevel::Graph(_g) => None
    }
  }).collect()
}

pub fn graphs<'a>(ast: &'a Vec<TopLevel>) -> Vec<&'a Graph> {
  ast.iter().filter_map(|top_level| {
    match top_level {
      TopLevel::Module(_m) => None,
      TopLevel::Graph(g) => Some(g)
    }
  }).collect()
}