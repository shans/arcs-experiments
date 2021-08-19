#include "graph.h"

namespace skunk {

size_t Graph::addModule(std::string name) {
  struct ModuleInfo i = { name };
  modules.push_back(i);
  return modules.size() - 1;
}

void Graph::connectModuleToModule(size_t module1, size_t module2, std::vector<ast::Usage> usages) {
  module_modules.push_back(std::make_pair(module1, module2));
}

}