
use super::ast::TypePrimitive;

#[derive(Debug)]
pub struct Handle {
  name: String,
  h_type: TypePrimitive
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Endpoint {
  Module(usize),
  Connection(usize),
  Handle(usize)
}

#[derive(Clone, Copy)]
pub enum EndpointSpec {
  Specific(Endpoint),
  AnyModule,
  AnyConnection,
  AnyHandle
}

impl Endpoint {
  fn matches_spec(&self, spec: EndpointSpec) -> bool {
    match spec {
      EndpointSpec::AnyModule => if let Endpoint::Module(_idx) = self { true } else { false }
      EndpointSpec::AnyConnection => if let Endpoint::Connection(_idx) = self { true } else { false }
      EndpointSpec::AnyHandle => if let Endpoint::Handle(_idx) = self { true } else { false }
      EndpointSpec::Specific(endpoint) => *self == endpoint
    }
  }

  pub fn module_idx(&self) -> Option<usize> {
    if let Endpoint::Module(idx) = self { Some(*idx) } else { None }
  }
}

#[derive(Debug, PartialEq)]
pub struct Arrow {
  pub from: Endpoint,
  pub to: Endpoint,
}

#[derive(Debug)]
pub struct Graph {
  pub modules: Vec<String>,
  pub connections: Vec<String>,
  pub handles: Vec<Handle>,
  arrows: Vec<Arrow>
}

impl Graph {
  pub fn new() -> Graph {
    Graph { modules: Vec::new(), connections: Vec::new(), handles: Vec::new(), arrows: Vec::new() }
  }

  pub fn add_module(&mut self, module_name: &str) -> Endpoint {
    self.modules.push(module_name.to_string());
    Endpoint::Module(self.modules.len() - 1)
  }

  pub fn add_connection(&mut self, connection_name: &str) -> Endpoint {
    self.connections.push(connection_name.to_string());
    Endpoint::Connection(self.connections.len() - 1)
  }

  pub fn add_handle(&mut self, handle_name: &str, handle_type: TypePrimitive) -> Endpoint {
    self.handles.push(Handle { name: handle_name.to_string(), h_type: handle_type });
    Endpoint::Handle(self.handles.len() - 1)
  }

  pub fn connect(&mut self, from: Endpoint, to: Endpoint) -> usize {
    self.assert_endpoint_is_valid(from);
    self.assert_endpoint_is_valid(to);
    self.arrows.push(Arrow::new(from, to));
    self.arrows.len() - 1
  }

  fn assert_endpoint_is_valid(&self, endpoint: Endpoint) {
    match endpoint {
      Endpoint::Module(idx) => assert!(idx < self.modules.len()),
      Endpoint::Connection(idx) => assert!(idx < self.connections.len()),
      Endpoint::Handle(idx) => assert!(idx < self.handles.len())
    }
  }

  fn filter_module_arrows(&mut self, from_spec: EndpointSpec, to_spec: EndpointSpec) -> Vec<Arrow> {
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

  pub fn filter_module_to_module_connections(&mut self) -> Vec<Arrow> {
    self.filter_module_arrows(EndpointSpec::AnyModule, EndpointSpec::AnyModule)
  }
}

impl Arrow {
  pub fn new(from: Endpoint, to: Endpoint) -> Arrow {
    Arrow { from, to }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn filter_module_to_module_connections_works() {
    let mut graph = Graph::new();
    let m0 = graph.add_module("mod1");
    let m1 = graph.add_module("mod2");
    let c0 = graph.add_connection("c0");
    graph.connect(m0, m1);
    graph.connect(m0, c0);
    graph.connect(c0, m1);
    
    let m2m = graph.filter_module_to_module_connections();

    assert_eq!(m2m, vec!(Arrow::new(m0, m1)));
    assert_eq!(graph.arrows, vec!(Arrow::new(m0, c0), Arrow::new(c0, m1)));
  }
} 