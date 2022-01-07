use super::graph;
use super::ast;

#[derive(Debug)]
pub enum GraphBuilderError {
  NotModuleEndpoint(graph::Endpoint),
  ModuleNotFound(String),
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
        graph.connect(prev_idx, idx);
        prev_idx = idx;
      }
    }
  }
}

pub fn add_graph_module_info(graph: &mut graph::Graph, info: &ast::GraphModuleInfo) -> graph::Endpoint {
  match info {
    ast::GraphModuleInfo::Module(specifier) => graph.add_module(specifier),
    _ => todo!("can't add {:?} to a graph yet", info)
  }
}

pub fn resolve_graph(modules: &Vec<&ast::Module>, graph: &mut graph::Graph) -> Result<(), GraphBuilderError> {
  let connections = graph.filter_module_to_module_connections();
  let success: Result<Vec<_>, _> = connections.iter().map(|connection| expand_to_full_connection(modules, graph, connection)).collect();
  success?; Ok(())
}

pub fn find_module_by_name<'a>(modules: &Vec<&'a ast::Module>, name: &str) -> Option<&'a ast::Module> {
  modules.iter().find(|&&module| module.name == name).map(|module| *module)
}

fn types_match(left: &ast::Handle, right: &ast::Handle) -> bool {
  left.h_type == right.h_type
}

fn matching_connections<'a, 'b>(from_module: &'a ast::Module, to_module: &'b ast::Module) -> Vec<(&'a str, &'b str, ast::Type)> {
  dbg!(from_module);
  dbg!(to_module);
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
  dbg!(&matches);
  assert!(matches.len() == 1);
  matches[0].clone()
}

fn only_from(arrow: &graph::Arrow) -> Result<&graph::Endpoint, GraphBuilderError> {
  let froms = arrow.from();
  if froms.len() == 1 {
    Ok(&froms[0])
  } else {
    todo!("Need to consider how to consider multiple endpoints when expanding to full connection")
  }
}

fn only_to(arrow: &graph::Arrow) -> Result<&graph::Endpoint, GraphBuilderError> {
  let tos = arrow.to();
  if tos.len() == 1 {
    Ok(&tos[0])
  } else {
    todo!("Need to consider how to consider multiple endpoints when expanding to full connection")
  }
}

fn expand_to_full_connection(modules: &Vec<&ast::Module>, graph: &mut graph::Graph, connection: &graph::Arrow) -> Result<(), GraphBuilderError> {
  let conn_from = only_from(connection)?;
  let conn_to = only_to(connection)?;
  let from_module_idx = conn_from.module_idx().ok_or(GraphBuilderError::NotModuleEndpoint(*conn_from))?;
  let to_module_idx = conn_to.module_idx().ok_or(GraphBuilderError::NotModuleEndpoint(*conn_to))?;
  let from_module = find_module_by_name(modules, &graph.modules[from_module_idx]).ok_or(
    GraphBuilderError::ModuleNotFound(graph.modules[from_module_idx].clone()))?;
  let to_module = find_module_by_name(modules, &graph.modules[to_module_idx]).ok_or(
    GraphBuilderError::ModuleNotFound(graph.modules[to_module_idx].clone()))?;
  let (from_name, to_name, compatible_type) = only_matching_connection(from_module, to_module);
  let from_connection = graph.add_connection(from_name);
  graph.connect(*conn_from, from_connection);
  let handle = graph.add_handle(&(from_module.name.to_string() + "-" + &from_name + "-" + &to_name + "-" + &to_module.name), compatible_type);
  graph.connect(from_connection, handle);
  let to_connection = graph.add_connection(to_name);
  graph.connect(handle, to_connection);
  graph.connect(to_connection, *conn_to);
  Ok(())
}