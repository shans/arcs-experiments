use super::graph;
use super::ast;

#[derive(Debug)]
pub enum GraphBuilderError {
  NotModuleEndpoint(graph::Endpoint),
  NotHandleEndpoint(graph::Endpoint),
  ModuleNotFound(String),
  HandleNotFound(String),
  MismatchedTypes,
}

pub fn make_graph(ast: Vec<&ast::GraphDirective>) -> graph::Graph {
  let mut graph = graph::Graph::new();
  ast.iter().for_each(|g| add_graph(&mut graph, g));
  graph
}


pub fn add_graph(graph: &mut graph::Graph, ast: &ast::GraphDirective) {
  match ast {
    ast::GraphDirective::Chain(modules) => {
      if modules.len() == 0 {
        return;
      }

      let mut prev_idx = add_graph_module_info(graph, &modules[0]);
  
      for i in 1..modules.len() {
        let idx = add_graph_module_info(graph, &modules[i]);
        graph.connect(&prev_idx, &idx);
        prev_idx = idx;
      }
    }
  }
}

pub fn add_graph_module_info(graph: &mut graph::Graph, info: &ast::GraphModuleInfo) -> graph::Endpoint {
  match info {
    ast::GraphModuleInfo::Module(specifier) => graph.add_module(specifier),
    ast::GraphModuleInfo::Tuple(specifiers) => {
      let endpoints = specifiers.iter().map(|s| add_graph_module_info(graph, s));
      let simple_endpoints = endpoints.map(|e| {
        if let graph::Endpoint::Simple(se) = e {
          se
        } else {
          todo!("can a tuple contain complex endpoints?");
        }
      }).collect();
      graph::Endpoint::Tuple(simple_endpoints)
    }
    ast::GraphModuleInfo::Field(specifier, h_name) => {
      if specifier == &ast::ModuleSpecifier::This {
        graph.add_handle(h_name, ast::Type::Unresolved)
      } else {
        todo!("can't add fields that aren't This.fields yet")
      }
    }
    _ => todo!("can't add {:?} to a graph yet", info)
  }
}

pub fn resolve_graph(module: &ast::Module, sub_modules: &Vec<&ast::Module>, graph: &mut graph::Graph) -> Result<(), GraphBuilderError> {
  resolve_handles(module, graph)?;
  let mut uid = 0;
  let connections = graph.filter_module_to_module_connections();
  let success: Result<Vec<_>, _> = connections.iter().map(|connection| expand_to_full_connection(sub_modules, graph, connection, &mut uid)).collect();
  success?;
  let m2h_connections = graph.filter_module_to_handle_connections();
  for connection in m2h_connections {
    expand_to_full_connection(sub_modules, graph, &connection, &mut uid)?;
  }
  dbg!(&graph);
  Ok(())
}

fn resolve_handles(module: &ast::Module, graph: &mut graph::Graph) -> Result<(), GraphBuilderError> {
  let unresolved_handles: Vec<_> = graph.handles.iter_mut().filter(|handle| handle.h_type == ast::Type::Unresolved).collect();
  for mut handle in unresolved_handles {
    let module_handle = module.handle_for_field(&handle.name).ok_or(GraphBuilderError::HandleNotFound(handle.name.clone()))?;
    handle.h_type = module_handle.h_type.clone();
  }
  Ok(())
}

pub fn find_module_by_name<'a>(modules: &Vec<&'a ast::Module>, name: &str) -> Option<&'a ast::Module> {
  modules.iter().find(|&&module| module.name == name).map(|module| *module)
}

// Eventually this should compare a 'from' type to a 'to' type - i.e. write -> read
fn types_match(left: &ast::Handle, right: &ast::Handle) -> bool {
  left.h_type == right.h_type
}

// Eventually this should indicate if the super type encapsulates the sub_type (e.g. if the super_type is a more
// general type that can be used in place of the sub_type)
pub fn type_encapsulates(super_type: &ast::Handle, sub_type: &ast::Handle) -> bool {
  super_type.h_type == sub_type.h_type
}

fn matching_connections<'a, 'b>(from_module: &'a ast::Module, to_module: &'b ast::Module) -> Vec<(&'a str, &'b str, ast::Type)> {
  let from_connections = from_module.outputs();
  let to_connections = to_module.inputs();
  from_connections.iter().map(|from_con| {
    to_connections.iter().filter_map(move |to_con| {
      if types_match(from_con, to_con) {
        Some((from_con.name.as_str(), to_con.name.as_str(), from_con.h_type.clone()))
      } else {
        None
      }
    })
  }).flatten().collect()
}

fn only_matching_connection<'a, 'b>(from_module: &'a ast::Module, to_module: &'b ast::Module) -> (&'a str, &'b str, ast::Type) {
  let matches = matching_connections(from_module, to_module);
  assert!(matches.len() == 1);
  matches[0].clone()
}

fn connections_matching_type<'a>(from_module: &'a ast::Module, to_type: &ast::Type) -> Vec<(&'a str, ast::Type)> {
  let from_connections = from_module.outputs();
  from_connections.iter().filter_map(|from_con| {
    // TODO: Better type checking here
    if from_con.h_type == *to_type {
      Some((from_con.name.as_str(), from_con.h_type.clone()))
    } else {
      None
    }
  }).collect()
}

fn only_connection_matching_type<'a>(from_module: &'a ast::Module, to_type: &ast::Type) -> (&'a str, ast::Type) {
  let matches = connections_matching_type(from_module, to_type);
  assert!(matches.len() == 1);
  matches[0].clone()
}

fn expand_to_full_connection(modules: &Vec<&ast::Module>, graph: &mut graph::Graph, connection: &graph::Arrow, uid: &mut usize) -> Result<(), GraphBuilderError> {
  let conn_from = &connection.from;
  let conn_to = &connection.to;
  if let graph::Endpoint::Tuple(connections) = conn_from {
    // TODO: can only deal with tuples to handles for now.
    let to_handle_idx = conn_to.handle_idx().ok_or(GraphBuilderError::NotHandleEndpoint(conn_to.clone()))?;
    let to_type = graph.handles[to_handle_idx].h_type.clone();
    if let ast::Type::Tuple(sub_types) = to_type {
      assert!(sub_types.len() == connections.len());
      for idx in 0..connections.len() {
        let from_module_idx = connections[idx].module_idx().ok_or(GraphBuilderError::NotModuleEndpoint(conn_from.clone()))?;
        let from_module = find_module_by_name(modules, &graph.modules[from_module_idx]).ok_or(
          GraphBuilderError::ModuleNotFound(graph.modules[from_module_idx].clone()))?;
        let (from_name, _compatible_type) = only_connection_matching_type(from_module, &sub_types[idx]);
        let from_connection = graph.add_connection(from_name);
        let conn_from = graph::Endpoint::Simple(connections[idx]);
        graph.connect(&conn_from, &from_connection);
        graph.connect_tuple_constructor(&from_connection, conn_to, *uid, idx);
      }
      *uid += 1;
      return Ok(());
    } else {
      return Err(GraphBuilderError::MismatchedTypes);
    }
  }

  let from_module_idx = conn_from.module_idx().ok_or(GraphBuilderError::NotModuleEndpoint(conn_from.clone()))?;
  let to_module_idx = conn_to.module_idx().ok_or(GraphBuilderError::NotModuleEndpoint(conn_to.clone()))?;
  let from_module = find_module_by_name(modules, &graph.modules[from_module_idx]).ok_or(
    GraphBuilderError::ModuleNotFound(graph.modules[from_module_idx].clone()))?;
  let to_module = find_module_by_name(modules, &graph.modules[to_module_idx]).ok_or(
    GraphBuilderError::ModuleNotFound(graph.modules[to_module_idx].clone()))?;
  let (from_name, to_name, compatible_type) = only_matching_connection(from_module, to_module);
  let from_connection = graph.add_connection(from_name);
  graph.connect(conn_from, &from_connection);
  let handle = graph.add_handle(&(from_module.name.to_string() + "-" + &from_name + "-" + &to_name + "-" + &to_module.name), compatible_type);
  graph.connect(&from_connection, &handle);
  let to_connection = graph.add_connection(to_name);
  graph.connect(&handle, &to_connection);
  graph.connect(&to_connection, conn_to);
  Ok(())
}
