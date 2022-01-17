use std::collections::hash_map::HashMap;

use super::*;

#[derive(Debug, Clone)]
pub enum GraphToModuleError {
  NameNotInModuleList(String),
  NameNotInHandleList(ast::Module, String),
  MultipleModulesForConnection(String)
}

#[derive(Debug)]
pub struct ModuleInfo<'a> {
  pub index: usize,
  pub module: &'a ast::Module
}

impl <'a> ModuleInfo<'a> {
  fn new(graph: &graph::Graph, modules: &Vec<&'a ast::Module>, index: usize) -> Result<ModuleInfo<'a>, GraphToModuleError> {
    let module_name = &graph.modules[index];
    let module = graph_builder::find_module_by_name(modules, module_name).ok_or_else(|| GraphToModuleError::NameNotInModuleList(module_name.clone()))?;
    let result = ModuleInfo { index, module };
    Ok(result)
  }
}

#[derive(Debug)]
pub struct HandleMappingInfo {
  pub submodule_idx: usize,
  pub submodule_handle: String
}

// Information about a new handle that will exist on the constructed module.
#[derive(Debug)]
pub struct HandleInfo {
  pub handle: ast::Handle,
  pub writes_to_submodule: usize,
  pub mapped_for_submodules: Vec<HandleMappingInfo>,
  pub submodule_handle: String
}

pub fn graph_to_module(module: &mut ast::Module, graph: graph::Graph, modules: Vec<&ast::Module>, name: &str) -> Result<(), GraphToModuleError> {
  let module_context = ModuleContext::new(graph, modules)?;
  let mut handle_infos = module_context.generate_handles()?;
  let mut submodules: Vec<ast::ModuleInfo> = module_context.submodules().drain(..).map(|module| ast::ModuleInfo { module, handle_map: HashMap::new() }).collect();
  for handle_info in &handle_infos {
    for idx in &handle_info.mapped_for_submodules {
      submodules[idx.submodule_idx].handle_map.insert(idx.submodule_handle.clone(), handle_info.handle.name.clone());
    }
  }
  let listeners = module_context.generate_listeners(&handle_infos);
  let handles = handle_infos.drain(..).map(|info| info.handle).collect();
  module.handles = handles;
  module.listeners = listeners;
  module.submodules = submodules;
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

  // Generate handles required on the outer module, and capture associated information (connection & submodule that the handle feeds into)
  // so that we can also generate listeners.
  fn generate_handles(&self) -> Result<Vec<HandleInfo>, GraphToModuleError> {
    let mut result: Vec<HandleInfo> = Vec::new();

    // Any submodule connections that aren't in the graph need to be added to the top-level module. These form the interface of the outer module.
    let connections = self.free_connections();
    for (info, name) in connections {
      let handle = info.module.handle_for_field(&name).ok_or_else(|| GraphToModuleError::NameNotInHandleList(info.module.clone(), name.clone()))?;
      let mapping_info = HandleMappingInfo { submodule_idx: info.index, submodule_handle: name.clone() };
      let writes_to_submodule = info.index;
      let submodule_handle = name.clone();
      result.push(HandleInfo { handle: handle.clone(), writes_to_submodule, mapped_for_submodules: vec!(mapping_info), submodule_handle });
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
        let connection_idx = connection.connection_idx().unwrap();
        let candidate_modules = self.graph.endpoints_associated_with_endpoint(connection.simple_endpoint().unwrap(), graph::EndpointSpec::AnyModule);
        if candidate_modules.len() != 1 {
          return Err(GraphToModuleError::MultipleModulesForConnection(self.graph.connections[connection_idx].clone()));
        }
        let module = candidate_modules[0];
        // .. retrieve the actual module info from the modules list ..
        let candidate_writes_to_submodule = module.module_idx().unwrap();
        let candidate_submodule_handle = &self.graph.connections[connection_idx];
        // all modules connected in this way participate in the map, collect them here.
        mapped_for_submodules.push(HandleMappingInfo { submodule_idx: candidate_writes_to_submodule, submodule_handle: candidate_submodule_handle.clone() });
        let submodule_name = &self.graph.modules[candidate_writes_to_submodule];
        let submodule = self.find_module_by_name(&submodule_name).ok_or_else(|| GraphToModuleError::NameNotInModuleList(submodule_name.clone()))?;
        let connection_name = &self.graph.connections[connection_idx];
        // .. and make sure that this connection inputs into the module. If so, then the connection we found is going to be
        // triggered by a listener associated with this handle.
        let submodule_connection = submodule.handle_for_field(connection_name).ok_or_else(|| GraphToModuleError::NameNotInHandleList(submodule.clone(), connection_name.clone()))?;
        if !submodule_connection.is_input() {
          continue;
        }
        if candidate_found {
          // TODO: we don't currently deal with a handle having multiple readers
          panic!("Handle has multiple readers, can't cope");
        }
        writes_to_submodule = candidate_writes_to_submodule;
        submodule_handle = candidate_submodule_handle;
        candidate_found = true;
      }
      if !candidate_found {
        panic!("Handle has no readers, can't cope");
      }
      // TODO: What's the right place to position these synthetic handles?
      let new_handle = ast::Handle { position: ast::SafeSpan { offset: 0, line: 1 }, name: handle.name.clone(), h_type: handle.h_type.clone(), usages: vec!(ast::Usage::Read, ast::Usage::Write) };
      result.push(HandleInfo { handle: new_handle, writes_to_submodule, mapped_for_submodules, submodule_handle: submodule_handle.clone() });
      // TODO: it's probably an error if there are no readers?
      index += 1;
    }

    Ok(result)
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
        match endpoint.simple_endpoint().unwrap() {
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
                       .map(|handle_info| ast::Listener {
                         trigger: handle_info.handle.name.clone(),
                         kind: ast::ListenerKind::OnWrite,
                         implementation: ast::Expression::output(ast::SafeSpan { offset: 0, line: 1 }, "", 
                           ast::Expression::copy_to_submodule(ast::SafeSpan { offset: 0, line: 1 }, &handle_info.handle.name, handle_info.writes_to_submodule, &handle_info.submodule_handle), 
                           false).value
                       }).collect()
}

  fn submodules(&self) -> Vec<ast::Module> {
    // clone modules so the final data structure doesn't refer into the provided module list.
    self.module_infos.iter().map(|info| info.module.clone()).collect()
  }
}