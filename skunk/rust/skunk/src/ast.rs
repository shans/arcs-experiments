
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

impl ListenerKind {
  pub fn to_string(&self) -> &str {
    match self {
      ListenerKind::OnChange => "OnChange",
      ListenerKind::OnWrite => "OnWrite"
    }
  }
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

impl Module {
  pub fn idx_for_field(&self, field: &str) -> Option<usize> {
    self.handles.iter().position(|handle| handle.name == field)
  }

  pub fn idx_for_bitfield(&self) -> usize {
    self.handles.len() * 2
  }

  pub fn outputs(&self) -> Vec<&Handle> {
    self.handles.iter().filter(|&handle| handle.usages.iter().any(|usage| *usage == Usage::Write)).collect()
  }

  pub fn inputs(&self) -> Vec<&Handle> {
    self.handles.iter().filter(|&handle| handle.usages.iter().any(|usage| *usage == Usage::Read)).collect()
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