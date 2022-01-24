extern crate nom_locate;

use std::collections::hash_map::HashMap;
use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;
#[derive(Debug, PartialEq, Clone)]
pub struct SafeSpan { pub offset: usize, pub line: u32 }

pub trait Safe {
  fn safe(&self) -> SafeSpan;
}

impl <'a> Safe for Span<'a> {
  fn safe(&self) -> SafeSpan {
    SafeSpan { offset: self.location_offset(), line: self.location_line() }
  }
}

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
  Tuple(Vec<Type>),
  Unresolved,
  TypeName(String),
  NewType(String, Box<Type>)
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
pub struct Handle {
  pub position: SafeSpan,
  pub name: String,
  pub usages: Vec<Usage>,
  pub h_type: Type,
}

impl Handle {
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
pub struct WriteToTuple {
  pub state: String,
  pub tuple_fields: Vec<String>,
  pub tuple_id: usize,
  pub tuple_index: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
  Equality,
  Inequality,
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
  // precedence copied from https://doc.rust-lang.org/reference/expressions.html
  pub fn precedence(&self) -> usize {
    match self {
      Operator::Multiply | Operator::Divide => 120,
      Operator::Add | Operator::Subtract => 100,
      Operator::LogicalOr => 40,
      Operator::LogicalAnd => 50,
      Operator::LessThan | Operator::LessThanOrEqual | Operator::GreaterThanOrEqual | Operator::GreaterThan | Operator::Equality | Operator::Inequality => 60,
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
pub struct Expression {
  pub value: ExpressionValue,
  pub is_terminated: bool,
  pub precedence: usize,
}

impl Expression {
  pub fn terminated(value: ExpressionValue) -> Self {
    Expression { value, is_terminated: true, precedence: 0 }
  }
  pub fn unterminated(value: ExpressionValue) -> Self {
    Expression { value, is_terminated: false, precedence: 0 }
  }
  pub fn binary_operator(position: SafeSpan, lhs: Expression, op: Operator, rhs: Expression) -> Self {
    let precedence = op.precedence();
    Expression { value: ExpressionValue { info: ExpressionValueEnum::BinaryOperator(Box::new(lhs.value), op, Box::new(rhs.value)), position }, precedence, is_terminated: rhs.is_terminated }
  }
  pub fn empty(position: SafeSpan) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::Empty, position })
  }
  pub fn state_reference(position: SafeSpan, state: &str) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::ReferenceToState(state.to_string()), position })
  }
  pub fn int_literal(position: SafeSpan, literal: i64) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::IntLiteral(literal), position })
  }
  pub fn string_literal(position: SafeSpan, literal: &str) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::StringLiteral(literal.to_string()), position })
  }
  pub fn char_literal(position: SafeSpan, literal: u8) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::CharLiteral(literal), position })
  }
  pub fn tuple(position: SafeSpan, mut members: Vec<Expression>) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::Tuple(members.drain(..).map(|a| a.value).collect()), position })
  }
  pub fn function_call(position: SafeSpan, name: &str, argument: Expression) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::FunctionCall(name.to_string(), Box::new(argument.into())), position })
  }
  pub fn array_lookup(position: SafeSpan, array: Expression, lookup: Expression) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::ArrayLookup(Box::new(array.into()), Box::new(lookup.into())), position })
  }
  pub fn tuple_lookup(position: SafeSpan, tuple: Expression, literal: i64) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::TupleLookup(Box::new(tuple.into()), literal), position })
  }
  pub fn block(position: SafeSpan, mut expressions: Vec<Expression>) -> Self {
    Expression::terminated(ExpressionValue { info: ExpressionValueEnum::Block(expressions.drain(..).map(|a| a.value).collect()), position })
  }
  pub fn break_expression(position: SafeSpan) -> Self {
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::Break, position })
  }
  pub fn output(position: SafeSpan, name: &str, expression: Expression, and_return: bool) -> Self {
    let output = OutputExpression { output: name.to_string(), expression: Box::new(expression.into()), and_return };
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::Output(output), position })
  }
  pub fn let_expression(position: SafeSpan, name: &str, expression: Expression, is_update: bool) -> Self {
    let let_expression = LetExpression { var_name: name.to_string(), expression: Box::new(expression.into()), is_update };
    Expression::unterminated(ExpressionValue { info: ExpressionValueEnum::Let(let_expression), position })
  }
  pub fn if_expression(position: SafeSpan, test: Expression, if_true: Expression, if_false: Expression) -> Self {
    let if_expression = IfExpression { test: Box::new(test.into()), if_true: Box::new(if_true.into()), if_false: Box::new(if_false.into()) };
    Expression::terminated(ExpressionValue { info: ExpressionValueEnum::If(if_expression), position })
  }
  pub fn while_expression(position: SafeSpan, test: Expression, body: Expression) -> Self {
    let while_expression = WhileExpression { test: Box::new(test.into()), body: Box::new(body.into()) };
    Expression::terminated(ExpressionValue { info: ExpressionValueEnum::While(while_expression), position })
  }
  pub fn copy_to_submodule(position: SafeSpan, state: &str, submodule_index: usize, submodule_state: &str) -> Self {
    let copy_to = CopyTo { state: state.to_string(), submodule_index, submodule_state: submodule_state.to_string() };
    Expression::terminated(ExpressionValue { info: ExpressionValueEnum::CopyToSubModule(copy_to), position })
  }
  pub fn write_to_tuple(position: SafeSpan, state: &str, tuple_fields: &Vec<String>, tuple_id: usize, tuple_index: usize) -> Self {
    let write_to_tuple = WriteToTuple { state: state.to_string(), tuple_fields: tuple_fields.clone(), tuple_id, tuple_index };
    Expression::terminated(ExpressionValue { info: ExpressionValueEnum::WriteToTuple(write_to_tuple), position })
  }
}

pub struct Expr<'a> {
  offset: usize,
  line: u32,
  expr: Box<dyn 'a + FnOnce(usize, u32) -> ExpressionValueEnum>,
}

impl <'a> Expr<'a> {
  fn mk(self, offset: usize, line: u32) -> ExpressionValue {
    let offset = offset + self.offset;
    let line = line + self.line;
    ExpressionValue { 
      position: SafeSpan { offset, line },
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
  pub fn build(self) -> ExpressionValue {
    self.mk(0, 1)
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionValue {
  pub info: ExpressionValueEnum,
  pub position: SafeSpan
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionValueEnum {
  // statement-like
  Output(OutputExpression),
  Block(Vec<ExpressionValue>),
  Let(LetExpression),
  If(IfExpression),
  While(WhileExpression),

  Empty, // used to terminate Blocks with no return
  Break,

  // expression-like
  // TODO: ArrayLookup and maybe TupleLookup should probably be examples of BinaryOperators.
  ArrayLookup(Box<ExpressionValue>, Box<ExpressionValue>),
  ReferenceToState(String),
  FunctionCall(String, Box<ExpressionValue>),
  StringLiteral(String),
  IntLiteral(i64),
  CharLiteral(u8),
  Tuple(Vec<ExpressionValue>),
  TupleLookup(Box<ExpressionValue>, i64),
  BinaryOperator(Box<ExpressionValue>, Operator, Box<ExpressionValue>),

  CopyToSubModule(CopyTo),
  WriteToTuple(WriteToTuple),
}

impl From<Expression> for ExpressionValue {
  fn from(expr: Expression) -> ExpressionValue {
    expr.value
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct OutputExpression {
  pub output: String,
  pub expression: Box<ExpressionValue>,
  pub and_return: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetExpression {
  pub var_name: String,
  pub expression: Box<ExpressionValue>,
  pub is_update: bool
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
  pub test: Box<ExpressionValue>,
  pub if_true: Box<ExpressionValue>,
  pub if_false: Box<ExpressionValue>
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileExpression {
  pub test: Box<ExpressionValue>,
  pub body: Box<ExpressionValue>
}

#[derive(Debug, PartialEq, Clone)]
pub struct Listener {
  pub trigger: String,
  pub kind: ListenerKind,
  pub implementation: ExpressionValue,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ModuleInfo {
  pub module: Module,
  pub handle_map: HashMap<String, String>
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExampleInfo {
  pub value: Expression,
  pub is_update: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Example {
  pub inputs: HashMap<String, ExampleInfo>,
  pub expected: HashMap<String, ExampleInfo>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Examples {
  pub examples: Vec<Example>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ValueParam {
  pub name: String,
  pub vp_type: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
  pub name: String,
  pub handles: Vec<Handle>,
  pub listeners: Vec<Listener>,
  pub submodules: Vec<ModuleInfo>,
  pub examples: Examples,
  pub value_params: Vec<ValueParam>,
  pub graph: Vec<GraphDirective>,
  pub tuples: HashMap<usize, usize>
}

/**
 * layout:
 *  [ field_value, field_update ]
 *  current updates bitmap
 *  (optional) tuple initialization bitmap
 *  [ param_value ]
 *  [ submodule pointer ]
 */
impl Module {
  pub fn minidump(&self) -> String {
    let mut s = format!("module {} {{\n", self.name);
    for handle in &self.handles {
      s += &format!("  {}: {:?};\n", handle.name, handle.h_type);
    }

    s += &"|\n";
    s
  }
  pub fn idx_for_field(&self, field: &str) -> Option<usize> {
    self.handles.iter().position(|handle| handle.name == field)
  }

  pub fn value_param_idx_for_field(&self, field: &str) -> Option<usize> {
    self.value_params.iter().position(|param| param.name == field)
  }

  pub fn idx_for_bitfield(&self) -> usize {
    self.handles.len() * 2
  }

  pub fn idx_for_tuple_field(&self) -> usize {
    if self.tuples.len() == 0 {
      panic!("Should not be retrieving tuple field idx for modules with no tuples");
    }
    self.idx_for_bitfield() + 1
  }

  pub fn offset_for_tuple(&self, uid: usize) -> Option<usize> {    
    let mut offset = 0;
    for i in 0..uid {
      let increment = self.tuples.get(&i)?;
      offset += increment;
    }
    Some(offset)
  }

  pub fn offset_for_value_params(&self) -> usize {
    if self.tuples.len() > 0 {
      self.idx_for_tuple_field() + 1
    } else {
      self.idx_for_bitfield() + 1
    }
  }

  pub fn offset_for_submodules(&self) -> usize {
    self.offset_for_value_params() + self.value_params.len()
  }

  pub fn outputs(&self) -> Vec<&Handle> {
    self.handles.iter().filter(|&handle| handle.is_output()).collect()
  }

  pub fn inputs(&self) -> Vec<&Handle> {
    self.handles.iter().filter(|&handle| handle.is_input()).collect()
  }

  pub fn handle_for_field(&self, field: &str) -> Option<&Handle> {
    self.handles.iter().find(|handle| handle.name == field)
  }

  pub fn value_param_for_field(&self, field: &str) -> Option<&ValueParam> {
    self.value_params.iter().find(|param| param.name == field)
  }

  pub fn type_for_field(&self, field: &str) -> Option<Type> {
    if let Some(handle) = self.handle_for_field(field) {
      Some(handle.h_type.clone())
    } else if let Some(param) = self.value_param_for_field(field) {
      Some(param.vp_type.clone())
    } else {
      None
    } 
  }

  pub fn resolve_types(&mut self, newtypes: &Vec<NewType>) {
    for idx in 0..self.handles.len() {
      if let Type::TypeName(name) = &self.handles[idx].h_type {
        let underlying = newtypes.iter().find(|nt| nt.name == *name).unwrap();
        self.handles[idx].h_type = Type::NewType(name.clone(), Box::new(underlying.nt_type.clone()));
      }
    }
  }

  pub fn create(
    name: &str, 
    handles: Vec<Handle>, 
    listeners: Vec<Listener>, 
    submodules: Vec<ModuleInfo>, 
    examples: Examples, 
    value_params: Vec<ValueParam>,
    graph: Vec<GraphDirective>
  ) -> Self {
    Module { name: name.to_string(), handles, listeners, submodules, examples, value_params, graph, tuples: HashMap::new() }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ModuleSpecifier {
  This,
  Module(String),
  // local_name first, then module_name
  NamedModule(String, String),
  Name(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum GraphModuleInfo {
  Module(ModuleSpecifier),
  Field(ModuleSpecifier, String),
  Tuple(Vec<GraphModuleInfo>),
}

impl GraphModuleInfo {
  pub fn module(name: &str, local_name: Option<&str>) -> GraphModuleInfo {
    match local_name {
      None => GraphModuleInfo::Module(ModuleSpecifier::Module(name.to_string())),
      Some(local_name) => GraphModuleInfo::Module(ModuleSpecifier::NamedModule(local_name.to_string(), name.to_string()))
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum GraphDirective {
  Chain(Vec<GraphModuleInfo>),
}

#[derive(Debug, PartialEq)]
pub struct Use {
  pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct NewType {
  pub name: String,
  pub nt_type: Type,
}

#[derive(Debug, PartialEq)]
pub enum TopLevel {
  Module(Module),
  Graph(GraphDirective),
  Use(Use),
  NewType(NewType),
}

pub fn uses(ast: &Vec<TopLevel>) -> Vec<&Use> {
  ast.iter().filter_map(|top_level| {
    if let TopLevel::Use(use_statement) = top_level {
      Some(use_statement)
    } else {
      None
    }
  }).collect()
}

pub fn modules(ast: &Vec<TopLevel>) -> Vec<&Module> {
  ast.iter().filter_map(|top_level| {
    match top_level {
      TopLevel::Module(m) => Some(m),
      _ => None
    }
  }).collect()
}

pub fn modules_mut(ast: &mut Vec<TopLevel>) -> Vec<&mut Module> {
  ast.iter_mut().filter_map(|top_level| {
    match top_level {
      TopLevel::Module(m) => Some(m),
      _ => None
    }
  }).collect()
}

pub fn graphs(ast: &Vec<TopLevel>) -> Vec<&GraphDirective> {
  ast.iter().filter_map(|top_level| {
    match top_level {
      TopLevel::Graph(g) => Some(g),
      _ => None
    }
  }).collect()
}

pub fn newtypes(ast: &Vec<TopLevel>) -> Vec<&NewType> {
  ast.iter().filter_map(|top_level| {
    match top_level {
      TopLevel::NewType(t) => Some(t),
      _ => None
    }
  }).collect()
}