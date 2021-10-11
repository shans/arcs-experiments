
use super::ast::Type;

#[derive(Debug)]
pub struct Handle {
  pub name: String,
  pub h_type: Type
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

  pub fn connection_idx(&self) -> Option<usize> {
    if let Endpoint::Connection(idx) = self { Some(*idx) } else { None }
  }

  pub fn handle_idx(&self) -> Option<usize> {
    if let Endpoint::Handle(idx) = self { Some(*idx) } else { None }
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

  pub fn add_handle(&mut self, handle_name: &str, handle_type: Type) -> Endpoint {
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

  pub fn arrows_matching(&self, from_spec: EndpointSpec, to_spec: EndpointSpec) -> Vec<&Arrow> {
    self.arrows.iter().filter(|arrow| arrow.from.matches_spec(from_spec) && arrow.to.matches_spec(to_spec)).collect()
  }

  pub fn arrows_involving_endpoint(&self, endpoint: Endpoint, spec: EndpointSpec) -> Vec<&Arrow> {
    let mut lhs = self.arrows_matching(EndpointSpec::Specific(endpoint), spec);
    lhs.append(&mut self.arrows_matching(spec, EndpointSpec::Specific(endpoint)));
    lhs
  }

  pub fn endpoints_associated_with_endpoint(&self, endpoint: Endpoint, spec: EndpointSpec) -> Vec<Endpoint> {
    let lhs = self.arrows_matching(EndpointSpec::Specific(endpoint), spec);
    let lhs = lhs.iter().map(|endpoint| endpoint.to);
    lhs.chain(&mut self.arrows_matching(spec, EndpointSpec::Specific(endpoint)).iter().map(|endpoint| endpoint.from)).collect()
  }

  pub fn filter_module_to_module_connections(&mut self) -> Vec<Arrow> {
    self.filter_arrows(EndpointSpec::AnyModule, EndpointSpec::AnyModule)
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

  #[test]
  fn module_spec_matches_any_module() {
    assert_eq!(Endpoint::Module(0).matches_spec(EndpointSpec::AnyModule), true);
    assert_eq!(Endpoint::Module(10).matches_spec(EndpointSpec::AnyModule), true);
    assert_eq!(Endpoint::Handle(0).matches_spec(EndpointSpec::AnyModule), false);
    assert_eq!(Endpoint::Connection(0).matches_spec(EndpointSpec::AnyModule), false);
  }

  #[test]
  fn handle_spec_matches_any_handle() {
    assert_eq!(Endpoint::Handle(0).matches_spec(EndpointSpec::AnyHandle), true);
    assert_eq!(Endpoint::Handle(10).matches_spec(EndpointSpec::AnyHandle), true);
    assert_eq!(Endpoint::Module(0).matches_spec(EndpointSpec::AnyHandle), false);
    assert_eq!(Endpoint::Connection(0).matches_spec(EndpointSpec::AnyHandle), false);
  }
} 