use super::graph;
use super::ast;

pub fn make_graph(ast: Vec<&ast::Graph>) -> graph::Graph {
  let mut graph = graph::Graph::new();
  ast.iter().for_each(|g| add_graph(&mut graph, g));
  graph
}


pub fn add_graph(graph: &mut graph::Graph, ast: &ast::Graph) {
  if ast.modules.len() == 0 {
    return;
  }

  let mut prev_idx = graph.add_module(&ast.modules[0]);
  
  for i in 1..ast.modules.len() {
    let idx = graph.add_module(&ast.modules[i]);
    graph.connect(prev_idx, idx);
    prev_idx = idx;
  }
}

pub fn resolve_graph(modules: &Vec<&ast::Module>, graph: &mut graph::Graph) {
  let connections = graph.filter_module_to_module_connections();
  connections.iter().for_each(|connection| expand_to_full_connection(modules, graph, connection));
}

fn find_module_by_name<'a>(modules: &'a Vec<&ast::Module>, name: &str) -> Option<&'a ast::Module> {
  modules.iter().find(|&&module| module.name == name).map(|module| *module)
}

fn types_match(left: &ast::Handle, right: &ast::Handle) -> bool {
  left.h_type == right.h_type
}

fn matching_connections<'a, 'b>(from_module: &'a ast::Module, to_module: &'b ast::Module) -> Vec<(&'a str, &'b str, ast::TypePrimitive)> {
  let from_connections = from_module.outputs();
  let to_connections = to_module.inputs();
  from_connections.iter().map(|from_con| {
    to_connections.iter().filter_map(move |to_con| {
      if types_match(from_con, to_con) {
        Some((from_con.name.as_str(), to_con.name.as_str(), from_con.h_type))
      } else {
        None
      }
    })
  }).flatten().collect()
}

fn only_matching_connection<'a, 'b>(from_module: &'a ast::Module, to_module: &'b ast::Module) -> (&'a str, &'b str, ast::TypePrimitive) {
  let matches = matching_connections(from_module, to_module);
  assert!(matches.len() == 1);
  matches[0]
}

// TODO: upgrade to Result types and signal error conditions
fn expand_to_full_connection(modules: &Vec<&ast::Module>, graph: &mut graph::Graph, connection: &graph::Arrow) {
  let from_module_idx = connection.from.module_idx().unwrap();
  let to_module_idx = connection.to.module_idx().unwrap();
  let from_module = find_module_by_name(modules, &graph.modules[from_module_idx]).unwrap();
  let to_module = find_module_by_name(modules, &graph.modules[to_module_idx]).unwrap();
  let (from_name, to_name, compatible_type) = only_matching_connection(from_module, to_module);
  let from_connection = graph.add_connection(from_name);
  graph.connect(connection.from, from_connection);
  // XXX FIXME XXX
  let handle = graph.add_handle("h", compatible_type);
  graph.connect(from_connection, handle);
  let to_connection = graph.add_connection(to_name);
  graph.connect(handle, to_connection);
  graph.connect(to_connection, connection.to);
}