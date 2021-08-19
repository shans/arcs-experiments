#ifndef _GRAPH_H
#define _GRAPH_H

#include "skunk_ast.h"

#include <string>
#include <vector>

namespace skunk {

struct ModuleInfo {
  std::string name;
};

struct ConnectionInfo {
  std::string name;
};

struct HandleInfo {
  std::string name;
};

class Graph {
  public:
    size_t addModule(std::string name);
    void connectModuleToModule(size_t module1, size_t module2, std::vector<ast::Usage> usages);
    //size_t addConnection(std::string name);
    //size_t addHandle(std::string name);
    //void connectModuleToConnection(size_t module, size_t connection, std::vector<ast::Usage> usages);
    //void connectConnectionToHandle(size_t connection, size_t handle, std::vector<ast::Usage> usages);
  
    std::vector<ModuleInfo> modules;
    std::vector<ConnectionInfo> connections;
    std::vector<HandleInfo> handles;
    std::vector<std::pair<size_t, size_t>> module_connections;
    std::vector<std::pair<size_t, size_t>> connection_modules;
    std::vector<std::pair<size_t, size_t>> connection_handles;
    std::vector<std::pair<size_t, size_t>> handle_connections;

    std::vector<std::pair<size_t, size_t>> module_modules;
};

}
#endif