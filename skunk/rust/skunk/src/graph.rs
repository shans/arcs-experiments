
use super::ast::TypePrimitive;

#[derive(Debug)]
pub struct Handle {
  name: String,
  h_type: TypePrimitive
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum EndpointType {
  ModuleEndpoint,
  ConnectionEndpoint,
  HandleEndpoint,
}

#[derive(Debug, PartialEq)]
pub struct Endpoint {
  pub endpoint_type: EndpointType,
  pub endpoint_index: usize,
}

// TODO: Endpoints should be an enum directly and this is what the connect function should return
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

  pub fn add_module(&mut self, module_name: &str) -> usize {
    self.modules.push(module_name.to_string());
    self.modules.len() - 1
  }

  pub fn add_connection(&mut self, connection_name: &str) -> usize {
    self.connections.push(connection_name.to_string());
    self.connections.len() - 1
  }

  pub fn add_handle(&mut self, handle_name: &str, handle_type: TypePrimitive) -> usize {
    self.handles.push(Handle { name: handle_name.to_string(), h_type: handle_type });
    self.handles.len() - 1
  }

  pub fn connect_module_to_module(&mut self, from_idx: usize, to_idx: usize) -> usize {
    self.connect(from_idx, EndpointType::ModuleEndpoint, to_idx, EndpointType::ModuleEndpoint)
  }

  pub fn connect_module_to_connection(&mut self, from_idx: usize, to_idx: usize) -> usize {
    self.connect(from_idx, EndpointType::ModuleEndpoint, to_idx, EndpointType::ConnectionEndpoint)
  }

  pub fn connect_connection_to_module(&mut self, from_idx: usize, to_idx: usize) -> usize {
    self.connect(from_idx, EndpointType::ConnectionEndpoint, to_idx, EndpointType::ModuleEndpoint)
  }

  pub fn connect_connection_to_handle(&mut self, from_idx: usize, to_idx: usize) -> usize {
    self.connect(from_idx, EndpointType::ConnectionEndpoint, to_idx, EndpointType::HandleEndpoint)
  }

  pub fn connect_handle_to_connection(&mut self, from_idx: usize, to_idx: usize) -> usize {
    self.connect(from_idx, EndpointType::HandleEndpoint, to_idx, EndpointType::ConnectionEndpoint)
  }

  fn connect(&mut self, from_idx: usize, from_type: EndpointType, to_idx: usize, to_type: EndpointType) -> usize {
    self.assert_endpoint_is_valid(from_idx, from_type);
    self.assert_endpoint_is_valid(to_idx, to_type);
    self.arrows.push(Arrow::new(from_type, from_idx, to_type, to_idx));
    self.arrows.len() - 1
  }

  fn assert_endpoint_is_valid(&self, idx: usize, endpoint_type: EndpointType) {
    match endpoint_type {
      EndpointType::ModuleEndpoint => assert!(idx < self.modules.len()),
      EndpointType::ConnectionEndpoint => assert!(idx < self.connections.len()),
      EndpointType::HandleEndpoint => assert!(idx < self.handles.len())
    }
  }

  fn filter_module_arrows(&mut self, from_type: EndpointType, to_type: EndpointType) -> Vec<Arrow> {
    let mut remaining = Vec::new();
    let mut returning = Vec::new();
    self.arrows.drain(..).for_each(|arrow| {
      if arrow.from.endpoint_type == from_type && arrow.to.endpoint_type == to_type {
        returning.push(arrow);
      } else {
        remaining.push(arrow);
      }
    });
    self.arrows = remaining;
    returning
  }

  pub fn filter_module_to_module_connections(&mut self) -> Vec<Arrow> {
    self.filter_module_arrows(EndpointType::ModuleEndpoint, EndpointType::ModuleEndpoint)
  }
}

impl Arrow {
  pub fn new(from_type: EndpointType, from_idx: usize, to_type: EndpointType, to_idx: usize) -> Arrow {
    Arrow { from: Endpoint { endpoint_type: from_type, endpoint_index: from_idx }, to: Endpoint { endpoint_type: to_type, endpoint_index: to_idx } }
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
    graph.connect_module_to_module(m0, m1);
    graph.connect_module_to_connection(m0, c0);
    graph.connect_connection_to_module(c0, m1);
    
    let m2m = graph.filter_module_to_module_connections();

    assert_eq!(m2m, vec!(Arrow::new(EndpointType::ModuleEndpoint, m0, EndpointType::ModuleEndpoint, m1)));
    assert_eq!(graph.arrows, vec!(
      Arrow::new(EndpointType::ModuleEndpoint, m0, EndpointType::ConnectionEndpoint, c0),
      Arrow::new(EndpointType::ConnectionEndpoint, c0, EndpointType::ModuleEndpoint, m1)
    ));
  }

} 