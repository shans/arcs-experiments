use std::collections::hash_map::HashMap;

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
pub struct Handle {
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
pub enum Operator {
  Equality,
  LogicalOr,
  LessThan,
  GreaterThan
}

impl Operator {
  pub fn precedence(&self) -> usize {
    match self {
      Operator::LogicalOr => 40,
      Operator::LessThan | Operator::GreaterThan | Operator::Equality => 60,
    }
  }
  pub fn is_logical(&self) -> bool {
    match self {
      Operator::LogicalOr => true,
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
  pub fn binary_operator(lhs: Expression, op: Operator, rhs: Expression) -> Self {
    let precedence = op.precedence();
    Expression { value: ExpressionValue::BinaryOperator(Box::new(lhs.value), op, Box::new(rhs.value)), precedence, is_terminated: rhs.is_terminated }
  }
  pub fn empty() -> Self {
    Expression::unterminated(ExpressionValue::Empty)
  }
  pub fn state_reference(state: &str) -> Self {
    Expression::unterminated(ExpressionValue::ReferenceToState(state.to_string()))
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionValue {
  // statement-like
  Output(OutputExpression),
  Block(Vec<ExpressionValue>),
  Let(LetExpression),
  If(IfExpression),

  Empty, // used to terminate Blocks with no return

  // expression-like
  ArrayLookup(Box<ExpressionValue>, Box<ExpressionValue>),
  ReferenceToState(String),
  CopyToSubModule(CopyTo),
  FunctionCall(String, Box<ExpressionValue>),
  StringLiteral(String),
  IntLiteral(i64),
  CharLiteral(u8),
  Tuple(Vec<ExpressionValue>),
  TupleLookup(Box<ExpressionValue>, i64),
  BinaryOperator(Box<ExpressionValue>, Operator, Box<ExpressionValue>)
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
  pub expression: Box<ExpressionValue>
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
  pub test: Box<ExpressionValue>,
  pub if_true: Box<ExpressionValue>,
  pub if_false: Box<ExpressionValue>
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
pub struct Module {
  pub name: String,
  pub handles: Vec<Handle>,
  pub listeners: Vec<Listener>,
  pub submodules: Vec<ModuleInfo>,
}

impl Module {
  pub fn idx_for_field(&self, field: &str) -> Option<usize> {
    self.handles.iter().position(|handle| handle.name == field)
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
    self.handles.iter().find(|handle| handle.name == field)
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
pub enum TopLevel {
  Module(Module),
  Graph(Graph),
}

pub fn modules<'a>(ast: &'a Vec<TopLevel>) -> Vec<&'a Module> {
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