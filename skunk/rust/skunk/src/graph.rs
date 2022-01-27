
use super::ast::{Type, ModuleSpecifier, ParamAssignment};

use std::collections::hash_map::HashMap;
use std::slice::from_ref;

#[derive(Debug)]
pub struct Handle {
  pub name: String,
  pub h_type: Type
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum SimpleEndpoint {
  Module(usize),
  Connection(usize),
  Handle(usize),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Endpoint {
  Simple(SimpleEndpoint),
  Tuple(Vec<SimpleEndpoint>),
}

#[derive(Copy, Clone)]
pub enum EndpointSpec {
  Specific(SimpleEndpoint),
  AnyModule,
  AnyConnection,
  AnyHandle
}

impl SimpleEndpoint {
  fn matches_spec(&self, spec: EndpointSpec) -> bool {
    match spec {
      EndpointSpec::AnyModule => if let SimpleEndpoint::Module(_idx) = self { true } else { false }
      EndpointSpec::AnyConnection => if let SimpleEndpoint::Connection(_idx) = self { true } else { false }
      EndpointSpec::AnyHandle => if let SimpleEndpoint::Handle(_idx) = self { true } else { false }
      EndpointSpec::Specific(endpoint) => *self == endpoint
    }
  }

  pub fn module_idx(&self) -> Option<usize> {
    if let SimpleEndpoint::Module(idx) = self { Some(*idx) } else { None }
  }

  pub fn connection_idx(&self) -> Option<usize> {
    if let SimpleEndpoint::Connection(idx) = self { Some(*idx) } else { None }
  }

  pub fn handle_idx(&self) -> Option<usize> {
    if let SimpleEndpoint::Handle(idx) = self { Some(*idx) } else { None }
  }
}

impl Endpoint {
  pub fn all(&self) -> &[SimpleEndpoint] {
    match self {
      Endpoint::Simple(e) => from_ref(e),
      Endpoint::Tuple(es) => &es,
    }
  }
  fn matches_spec(&self, spec: EndpointSpec) -> bool {
    self.all().iter().any(|e| e.matches_spec(spec))
  }

  pub fn simple_endpoint(&self) -> Option<SimpleEndpoint> {
    if let Endpoint::Simple(e) = self { Some(*e) } else { None }
  }

  pub fn module_idx(&self) -> Option<usize> {
    self.simple_endpoint()?.module_idx()
  }

  pub fn connection_idx(&self) -> Option<usize> {
    self.simple_endpoint()?.connection_idx()
  }

  pub fn handle_idx(&self) -> Option<usize> {
    self.simple_endpoint()?.handle_idx()
  }
}

#[derive(Debug, PartialEq)]
pub enum ArrowInfo {
  None,
  // ArrowInfo::TupleConstructor is used after a tuple arrow is deconstructed
  // into simple arrows - each simple arrow is tagged with TupleConstructor
  // to indicate its role.
  // Need a unique ID that characterizes the original arrow (so that if there
  // are multiple TupleConstructors pointing to the same tuple, they can be
  // distinguished), and the constructing index (i.e. which part of the tuple
  // is built by this arrow)
  TupleConstructor(usize, usize)
}

#[derive(Debug, PartialEq)]
pub struct Arrow {
  pub from: Endpoint,
  pub to: Endpoint,
  pub info: ArrowInfo
}

#[derive(Debug)]
pub struct GraphModule {
  pub name: String,
  pub params: ParamAssignment
}

impl GraphModule {
  pub fn create(name: &str, params: &ParamAssignment) -> Self { 
    GraphModule { name: name.to_string(), params: params.clone() }
  }
}

#[derive(Debug)]
pub struct Graph {
  pub modules: Vec<GraphModule>,
  pub connections: Vec<String>,
  pub handles: Vec<Handle>,
  arrows: Vec<Arrow>,
  pub names: HashMap<String, usize>
}

impl Graph {
  pub fn new() -> Graph {
    Graph { modules: Vec::new(), connections: Vec::new(), handles: Vec::new(), arrows: Vec::new(), names: HashMap::new() }
  }

  pub fn add_module(&mut self, module_name: &ModuleSpecifier, params: &ParamAssignment) -> Endpoint {
    let mut idx = self.modules.len();
    match module_name {
      ModuleSpecifier::This => todo!("Probably this doesn't make sense?"),
      ModuleSpecifier::Module(name) => self.modules.push(GraphModule::create(name, params)),
      ModuleSpecifier::NamedModule(local_name, name) => {
        self.modules.push(GraphModule::create(name, params));
        self.names.insert(local_name.clone(), idx);
      }
      ModuleSpecifier::Name(local_name) => idx = *self.names.get(local_name).unwrap()
    }
    Endpoint::Simple(SimpleEndpoint::Module(idx))
  }

  pub fn add_connection(&mut self, connection_name: &str) -> Endpoint {
    self.connections.push(connection_name.to_string());
    Endpoint::Simple(SimpleEndpoint::Connection(self.connections.len() - 1))
  }

  pub fn add_handle(&mut self, handle_name: &str, handle_type: Type) -> Endpoint {
    self.handles.push(Handle { name: handle_name.to_string(), h_type: handle_type });
    Endpoint::Simple(SimpleEndpoint::Handle(self.handles.len() - 1))
  }

  pub fn connect(&mut self, from: &Endpoint, to: &Endpoint) -> usize {
    assert!(self.endpoint_is_valid(from));
    assert!(self.endpoint_is_valid(to));
    self.arrows.push(Arrow::new(from, to));
    self.arrows.len() - 1
  }

  pub fn connect_tuple_constructor(&mut self, from: &Endpoint, to: &Endpoint, uid: usize, idx: usize) -> usize {
    assert!(self.endpoint_is_valid(from));
    assert!(self.endpoint_is_valid(to));
    self.arrows.push(Arrow::new_with_info(from, to, ArrowInfo::TupleConstructor(uid, idx)));
    self.arrows.len() - 1
  }

  fn simple_endpoint_is_valid(&self, endpoint: SimpleEndpoint) -> bool {
    match endpoint {
      SimpleEndpoint::Module(idx) => idx < self.modules.len(),
      SimpleEndpoint::Connection(idx) => idx < self.connections.len(),
      SimpleEndpoint::Handle(idx) => idx < self.handles.len()
    }
  }

  fn endpoint_is_valid(&self, endpoint: &Endpoint) -> bool {
    endpoint.all().iter().all(|e| self.simple_endpoint_is_valid(*e))
  }

  // Remove and return any arrows matching (from_spec -> to_spec).
  // Note that this will pick up structure/destructure arrows where one endpoint matches from_spec or to_spec.
  fn filter_arrows(&mut self, from_spec: EndpointSpec, to_spec: EndpointSpec) -> Vec<Arrow> {
    let mut remaining = Vec::new();
    let mut returning = Vec::new();
    self.arrows.drain(..).for_each(|arrow| {
      if arrow.from.matches_spec(from_spec) && arrow.to.matches_spec(to_spec) {
        returning.push(arrow);
      } else {
        remaining.push(arrow);
      }
    });
    self.arrows = remaining;
    returning
  }

  // Return any arrows matching (from_spec -> to_spec).
  // Note that this will pick up structure/destructure arrows where one endpoint matches from_spec or to_spec.
  pub fn arrows_matching(&self, from_spec: EndpointSpec, to_spec: EndpointSpec) -> Vec<&Arrow> {
    self.arrows.iter().filter(|arrow| arrow.from.matches_spec(from_spec) && arrow.to.matches_spec(to_spec)).collect()
  }

  // Return any arrows matching (endpoint -> spec) or (spec -> endpoint).
  pub fn arrows_involving_endpoint(&self, endpoint: SimpleEndpoint, spec: EndpointSpec) -> Vec<&Arrow> {
    let mut lhs = self.arrows_matching(EndpointSpec::Specific(endpoint), spec);
    lhs.append(&mut self.arrows_matching(spec, EndpointSpec::Specific(endpoint)));
    lhs
  }

  /** 
   * Return all endpoints associated with spec matching (endpoing -> spec) or (spec -> endpoint)
   */
  pub fn endpoints_associated_with_endpoint(&self, endpoint: SimpleEndpoint, spec: EndpointSpec) -> Vec<(&Endpoint, &Arrow)> {
    let lhs = self.arrows_matching(EndpointSpec::Specific(endpoint), spec);
    let lhs = lhs.iter().map(|endpoint| (&endpoint.to, *endpoint));
    lhs.chain(&mut self.arrows_matching(spec, EndpointSpec::Specific(endpoint)).iter().map(|endpoint| (&endpoint.from, *endpoint))).collect()
  }

  pub fn filter_module_to_module_connections(&mut self) -> Vec<Arrow> {
    self.filter_arrows(EndpointSpec::AnyModule, EndpointSpec::AnyModule)
  }

  pub fn filter_module_to_handle_connections(&mut self) -> Vec<Arrow> {
    self.filter_arrows(EndpointSpec::AnyModule, EndpointSpec::AnyHandle)
  }
}

impl Arrow {
  pub fn new(from: &Endpoint, to: &Endpoint) -> Arrow {
    Arrow { from: from.clone(), to: to.clone(), info: ArrowInfo::None }
  }
  pub fn new_with_info(from: &Endpoint, to: &Endpoint, info: ArrowInfo) -> Arrow {
    Arrow { from: from.clone(), to: to.clone(), info }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn filter_module_to_module_connections_works() {
    let mut graph = Graph::new();
    let m0 = graph.add_module(&ModuleSpecifier::Module("mod1".to_string()), &ParamAssignment::empty());
    let m1 = graph.add_module(&ModuleSpecifier::Module("mod2".to_string()), &ParamAssignment::empty());
    let c0 = graph.add_connection("c0");
    graph.connect(&m0, &m1);
    graph.connect(&m0, &c0);
    graph.connect(&c0, &m1);
    
    let m2m = graph.filter_module_to_module_connections();

    assert_eq!(m2m, vec!(Arrow::new(&m0, &m1)));
    assert_eq!(graph.arrows, vec!(Arrow::new(&m0, &c0), Arrow::new(&c0, &m1)));
  }

  #[test]
  fn module_spec_matches_any_module() {
    assert_eq!(SimpleEndpoint::Module(0).matches_spec(EndpointSpec::AnyModule), true);
    assert_eq!(SimpleEndpoint::Module(10).matches_spec(EndpointSpec::AnyModule), true);
    assert_eq!(SimpleEndpoint::Handle(0).matches_spec(EndpointSpec::AnyModule), false);
    assert_eq!(SimpleEndpoint::Connection(0).matches_spec(EndpointSpec::AnyModule), false);
  }

  #[test]
  fn handle_spec_matches_any_handle() {
    assert_eq!(SimpleEndpoint::Handle(0).matches_spec(EndpointSpec::AnyHandle), true);
    assert_eq!(SimpleEndpoint::Handle(10).matches_spec(EndpointSpec::AnyHandle), true);
    assert_eq!(SimpleEndpoint::Module(0).matches_spec(EndpointSpec::AnyHandle), false);
    assert_eq!(SimpleEndpoint::Connection(0).matches_spec(EndpointSpec::AnyHandle), false);
  }
} 