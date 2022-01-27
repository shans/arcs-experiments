use std::collections::hash_map::HashMap;

use super::*;

#[derive(Debug, Clone)]
pub enum GraphToModuleError {
  NameNotInModuleList(String),
  NameNotInHandleList(ast::Module, String),
  MultipleModulesForConnection(String),
  InvalidHandleType(String),
}

#[derive(Debug)]
pub struct ModuleInfo<'a> {
  pub index: usize,
  pub module: &'a ast::Module,
  pub params: ast::ParamAssignment
}

impl <'a> ModuleInfo<'a> {
  fn new(graph: &graph::Graph, modules: &Vec<&'a ast::Module>, index: usize) -> Result<ModuleInfo<'a>, GraphToModuleError> {
    let module_name = &graph.modules[index].name;
    let params = &graph.modules[index].params;
    let module = graph_builder::find_module_by_name(modules, module_name).ok_or_else(|| GraphToModuleError::NameNotInModuleList(module_name.clone()))?;
    let result = ModuleInfo { index, module, params: params.clone() };
    Ok(result)
  }
}

#[derive(Debug)]
pub struct HandleMappingInfo {
  pub submodule_idx: usize,
  pub submodule_handle: String
}

#[derive(Debug)]
pub enum WriteBehaviour {
  WritesToSubmodule(usize, String),
  // (handle to write to, handles to read from, uid for this constructor, index into read handles)
  WritesToTupleHandle(String, Vec<String>, usize, usize),
  None
}

// Information about a new handle that will exist on the constructed module.
#[derive(Debug)]
pub struct HandleInfo {
  pub handle: ast::Handle,
  pub mapped_for_submodules: Vec<HandleMappingInfo>,
  pub write_behaviour: WriteBehaviour
}

pub fn graph_to_module(module: &mut ast::Module, graph: graph::Graph, modules: Vec<&ast::Module>) -> Result<(), GraphToModuleError> {
  let module_context = ModuleContext::new(graph, modules)?;
  let mut handle_infos = module_context.generate_handles(&mut module.tuples)?;
  let mut submodules: Vec<ast::ModuleInfo> = 
    module_context.submodules().drain(..).map(|(module, params)| ast::ModuleInfo { module, handle_map: HashMap::new(), params }).collect();
  for handle_info in &handle_infos {
    for idx in &handle_info.mapped_for_submodules {
      submodules[idx.submodule_idx].handle_map.insert(idx.submodule_handle.clone(), handle_info.handle.name.clone());
    }
  }
  let mut listeners = module_context.generate_listeners(&handle_infos);
  let mut error: Option<GraphToModuleError> = None;
  handle_infos.drain(..).for_each(|info| {
    let existing_handle = module.handle_for_field(&info.handle.name);
    if let Some(handle) = existing_handle {
      // This handle name matches one on the public interface.
      if !graph_builder::type_encapsulates(handle, &info.handle) {
        dbg!(&handle, &info.handle);
        error = Some(GraphToModuleError::InvalidHandleType(info.handle.name.clone()))
      }
    } else {
      module.handles.push(info.handle);
    }
  });
  if let Some(err) = error {
    return Err(err);
  }
  module.listeners.append(&mut listeners);
  module.submodules.append(&mut submodules);
  Ok(())
}

pub struct ModuleContext<'a> {
  pub graph: graph::Graph,
  pub modules: Vec<&'a ast::Module>,
  pub module_infos: Vec<ModuleInfo<'a>>
}

impl <'a> ModuleContext<'a> {
  fn new(graph: graph::Graph, modules: Vec<&'a ast::Module>) -> Result<Self, GraphToModuleError> {
    let module_infos = (0..graph.modules.len()).map(|idx| ModuleInfo::new(&graph, &modules, idx)).collect::<Result<Vec<_>, _>>()?;
    Ok(Self { graph, modules, module_infos })
  }

  /** 
   * Generate handles required on the outer module, and capture associated information (connection & submodule that the handle feeds into)
   * so that we can also generate listeners.
   */
  fn generate_handles(&self, tuples: &mut HashMap<usize, usize>) -> Result<Vec<HandleInfo>, GraphToModuleError> {
    let mut result: Vec<HandleInfo> = Vec::new();

    // Any submodule connections that aren't in the graph need to be added to the top-level module. These form the interface of the outer module.
    let connections = self.free_connections();
    for (info, name) in connections {
      let handle = info.module.handle_for_field(&name).ok_or_else(|| GraphToModuleError::NameNotInHandleList(info.module.clone(), name.clone()))?;
      let mapping_info = HandleMappingInfo { submodule_idx: info.index, submodule_handle: name.clone() };
      let write_behaviour = if handle.is_input() {
        WriteBehaviour::WritesToSubmodule(info.index, name.clone())
      } else {
        WriteBehaviour::None
      };
      result.push(HandleInfo { handle: handle.clone(), write_behaviour, mapped_for_submodules: vec!(mapping_info) });
    }

    // Additionally, any handles in the graph need to be represented as connections in the top-level module so that the value of the handle can be tracked
    // and shared between sub-modules. These aren't part of the public interface but they're accessible from outside the outer module.
    let mut index = 0;
    for handle in &self.graph.handles {
      let candidate_connections = self.graph.endpoints_associated_with_endpoint(graph::SimpleEndpoint::Handle(index), graph::EndpointSpec::AnyConnection);
      let mut candidate_found = false;

      let mut mapped_for_submodules = Vec::new();
      let mut writes_to_submodule = 0;
      let mut submodule_handle = &"".to_string();
      for connection in candidate_connections {
        // We have a connection that connects to the constructed handle. From that we need to fetch the connected module..
        let connection_idx = connection.0.connection_idx().unwrap();
        let candidate_modules = self.graph.endpoints_associated_with_endpoint(connection.0.simple_endpoint().unwrap(), graph::EndpointSpec::AnyModule);
        if candidate_modules.len() != 1 {
          return Err(GraphToModuleError::MultipleModulesForConnection(self.graph.connections[connection_idx].clone()));
        }
        let module = candidate_modules[0].0;
        // .. and retrieve the actual module info from the modules list
        let candidate_writes_to_submodule = module.module_idx().unwrap();
        let submodule_name = &self.graph.modules[candidate_writes_to_submodule].name;
        let submodule = self.find_module_by_name(&submodule_name).ok_or_else(|| GraphToModuleError::NameNotInModuleList(submodule_name.clone()))?;
        let connection_name = &self.graph.connections[connection_idx];
        let submodule_connection = submodule.handle_for_field(connection_name).ok_or_else(|| GraphToModuleError::NameNotInHandleList(submodule.clone(), connection_name.clone()))?;

        if let graph::ArrowInfo::TupleConstructor(u, n) = connection.1.info {
          // if the arrow is a tuple construction arrow, then we need to create a toplevel handle
          // for the output of the module connection on the LHS. Then we need to register a handler for that handle
          // which copies updates into the RHS and checks the initialization tags. We also need to make
          // sure the module knows to reserve initialization tags for the components of the RHS.
          let new_handle = ast::Handle { 
            position: ast::SafeSpan { offset: 0, line: 1}, 
            name: format!("{}.{}", handle.name, n), 
            h_type: submodule_connection.h_type.clone(),
            usages: vec!(ast::Usage::Read, ast::Usage::Write)
          };
          result.push(HandleInfo {
            handle: new_handle, 
            mapped_for_submodules: vec!(HandleMappingInfo { submodule_idx: candidate_writes_to_submodule, submodule_handle: connection_name.clone() }),
            write_behaviour: WriteBehaviour::WritesToTupleHandle(handle.name.clone(), Vec::new(), u, n)
          });
          match tuples.get(&u) {
            None => { tuples.insert(u, n + 1); },
            Some(v) => if (n + 1) > *v { tuples.insert(u, n + 1); }
          }

          continue;
        }

        // all modules connected in this way participate in the map, collect them here.
        mapped_for_submodules.push(HandleMappingInfo { submodule_idx: candidate_writes_to_submodule, submodule_handle: connection_name.clone() });

        // make sure that this connection inputs into the module. If so, then the connection we found is going to be
        // triggered by a listener associated with this handle.
        if !submodule_connection.is_input() {
          continue;
        }
        if candidate_found {
          // TODO: we don't currently deal with a handle having multiple readers
          panic!("Handle has multiple readers, can't cope");
        }
        writes_to_submodule = candidate_writes_to_submodule;
        submodule_handle = connection_name;
        candidate_found = true;
      }

      if !candidate_found {
        // There's no read candidate for this handle
        let new_handle = ast::Handle { position: ast::SafeSpan { offset: 0, line: 1 }, name: handle.name.clone(), h_type: handle.h_type.clone(), usages: vec!(ast::Usage::Write) };
        result.push(HandleInfo { handle: new_handle, write_behaviour: WriteBehaviour::None, mapped_for_submodules });
      } else {
        // TODO: What's the right place to position these synthetic handles?
        let new_handle = ast::Handle { position: ast::SafeSpan { offset: 0, line: 1 }, name: handle.name.clone(), h_type: handle.h_type.clone(), usages: vec!(ast::Usage::Read, ast::Usage::Write) };
        let write_behaviour = WriteBehaviour::WritesToSubmodule(writes_to_submodule, submodule_handle.clone());
        result.push(HandleInfo { handle: new_handle, write_behaviour, mapped_for_submodules });
      }
      index += 1;
    }

    let mut all_handle_names = HashMap::<usize, Vec<String>>::new();
    for handle_info in &result {
      if let WriteBehaviour::WritesToTupleHandle(_, _, u, n) = &handle_info.write_behaviour {
        if !all_handle_names.contains_key(u) {
          all_handle_names.insert(*u, Vec::<String>::new());
        }
        all_handle_names.get_mut(u).unwrap().push(handle_info.handle.name.clone());
      }
    }

    let mut output = vec!();
    for mut handle_info in result {
      if let WriteBehaviour::WritesToTupleHandle(name, _, u, n) = &handle_info.write_behaviour {
        handle_info.write_behaviour = WriteBehaviour::WritesToTupleHandle(name.to_string(), all_handle_names.get(u).unwrap().clone(), *u, *n);
      }
      output.push(handle_info);
    }

    Ok(output)
  }

  fn find_module_by_name(&self, name: &str) -> Option<&ast::Module> {
    graph_builder::find_module_by_name(&self.modules, name)
  }

  fn free_connections(&self) -> Vec<(&ModuleInfo<'a>, String)> {
    let mut result = Vec::new();

    for idx in 0..self.graph.modules.len() {
      let info = &self.module_infos[idx];
      let mut free_connections = self.free_connections_for_module(info);
      free_connections.drain(..).for_each(|handle_name| result.push((info, handle_name)))
    }
    result
  }

  fn free_connections_for_module(&self, module_info: &ModuleInfo<'a>) -> Vec<String> {
    // "connected connections" are all the connections which the graph indicates are connected (i.e. there's an arrow from the module to them).
    // We are relying on the lack of blind connections - i.e. we're assuming that if there's an arrow between a module M and a connection C,
    // then C will also have an arrow to a handle H  -   M -> C -> H or H -> C -> M.
    let connected_connections: Vec<&str> = self.graph.endpoints_associated_with_endpoint(graph::SimpleEndpoint::Module(module_info.index), graph::EndpointSpec::AnyConnection)
      .iter().map(|endpoint| {
        match endpoint.0.simple_endpoint().unwrap() {
          graph::SimpleEndpoint::Connection(idx) => self.graph.connections[idx].as_str(),
          _ => panic!("Shouldn't be possible to have non-connection endpoints here")
        }
      }).collect();
    module_info.module.handles.iter().map(|handle| handle.name.clone())
                              .filter(|this_name| !connected_connections.iter().any(|handle_name| handle_name == this_name))
                              .collect()
  }

  fn generate_listeners(&self, handle_infos: &Vec<HandleInfo>) -> Vec<ast::Listener> {
    // all handles with read permissions need listeners
    // TODO: Appropriate span locations for generated code.
    handle_infos.iter().filter(|handle_info| handle_info.handle.is_input())
                       .map(|handle_info| {
                          match &handle_info.write_behaviour {
                            WriteBehaviour::WritesToSubmodule(submodule, sub_handle_name) =>
                              ast::Listener {
                                trigger: handle_info.handle.name.clone(),
                                kind: ast::ListenerKind::OnWrite,
                                implementation: ast::Expression::output(ast::SafeSpan { offset: 0, line: 1 }, "", 
                                  ast::Expression::copy_to_submodule(ast::SafeSpan { offset: 0, line: 1 }, &handle_info.handle.name, *submodule, sub_handle_name), 
                                  false).value
                              },
                            WriteBehaviour::WritesToTupleHandle(name, handles, uid, idx) =>
                              ast::Listener {
                                trigger: handle_info.handle.name.clone(),
                                kind: ast::ListenerKind::OnWrite,
                                implementation: ast::Expression::output(ast::SafeSpan { offset: 0, line: 1}, "",
                                  ast::Expression::write_to_tuple(ast::SafeSpan { offset: 0, line: 1}, name, handles, *uid, *idx), false).value
                              },
                            WriteBehaviour::None => panic!("Shouldn't be possible")
                          }
                        }).collect()
}

  fn submodules(&self) -> Vec<(ast::Module, ast::ParamAssignment)> {
    // clone modules so the final data structure doesn't refer into the provided module list.
    self.module_infos.iter().map(|info| (info.module.clone(), info.params.clone())).collect()
  }
}