
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Usage {
  Read,
  Write,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TypePrimitive {
  Int,
  String
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ListenerKind {
  OnChange,
  OnWrite
}

#[derive(Debug, PartialEq)]
pub struct Handle {
  pub name: String,
  pub usages: Vec<Usage>,
  pub h_type: TypePrimitive,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
  ReferenceToState(String),
}

#[derive(Debug, PartialEq)]
pub struct Statement {
  pub output: String,
  pub expression: Expression,
}

#[derive(Debug, PartialEq)]
pub struct Listener {
  pub trigger: String,
  pub kind: ListenerKind,
  pub statement: Statement,
}

#[derive(Debug, PartialEq)]
pub struct Module {
  pub name: String,
  pub handles: Vec<Handle>,
  pub listeners: Vec<Listener>,
}

#[derive(Debug, PartialEq)]
pub struct Graph {
  pub modules: Vec<String>,
}