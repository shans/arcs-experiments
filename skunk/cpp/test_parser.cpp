#define BOOST_TEST_MODULE Parser Tests
#include <boost/test/included/unit_test.hpp>
#include "parser.h"
#include "skunk_ast.h"

using namespace skunk;
using namespace tao::pegtl;

#define PARSE_TEST(name, rule, string) BOOST_AUTO_TEST_CASE(name) { \
  memory_input<> in(string, __FUNCTION__); \
  BOOST_TEST(parse<rule>(in)); \
}

PARSE_TEST(parses_empty_module, module, "module MyModule { }\n")

PARSE_TEST(parses_module_with_one_handle_definition, module, 
R"(module MyModule {
  foo: reads Int;
})")

PARSE_TEST(parses_handle, handle, "foo: reads Int")

PARSE_TEST(parses_module_with_handles_and_listeners, module, 
R"(module MyModule {
  foo: reads Int;
  bar: writes Int;

  foo.onChange: bar <- foo;
})")

PARSE_TEST(parses_listener, listener, "foo.onChange: bar <- foo")

PARSE_TEST(parses_graph, graph, "Foo -> Bar")

PARSE_TEST(parses_multiple_toplevels, toplevels, 
R"(module MyModule {
  foo: reads Int;
  bar: writes Int;

  foo.onChange: bar <- foo;
}

MyModule -> MyModule2
)")

BOOST_AUTO_TEST_CASE(extracts_moduleName) {
  memory_input<> in("module MyModule { }\n", __FUNCTION__);
  SkunkState state;
  parse< module, skunk_action >(in, state);
  BOOST_TEST(state.module->name == "MyModule");
}

void assert_handle(ast::Handle& handle, std::string name, ast::PrimitiveType::Name typeName) {
  BOOST_TEST(handle.name == name);
  auto type = dynamic_cast<ast::PrimitiveType*>(handle.type);
  BOOST_TEST(type->name == typeName);

}

void assert_listener(ast::Listener& listener, std::string handle, std::string kind, std::string output) {
  BOOST_TEST(listener.handle == handle);
  BOOST_TEST(listener.kind == kind);
  BOOST_TEST(listener.statement->output->name == output);
}

BOOST_AUTO_TEST_CASE(extracts_handles_and_listeners) {
  memory_input<> in(
R"(module MyModule {
  foo: reads Int;
  bar: writes Int;

  foo.onChange: bar <- foo;
}
)", __FUNCTION__);
  SkunkState state;
  parse< module, skunk_action >(in, state);
  BOOST_TEST(state.module->name == "MyModule");
  BOOST_TEST(state.module->handles.size() == 2);
  assert_handle(state.module->handles.at(0), "foo", ast::PrimitiveType::Name::intType);
  assert_handle(state.module->handles.at(1), "bar", ast::PrimitiveType::intType);
  BOOST_TEST(state.module->listeners.size() == 1);
  assert_listener(state.module->listeners.at(0), "foo", "onChange", "bar");
}

BOOST_AUTO_TEST_CASE(extracts_graph) {
  memory_input<> in("MyModule -> MyModule2\n", __FUNCTION__);
  SkunkState state;
  parse< graph, skunk_action >(in, state);
  BOOST_TEST(state.graph->modules.size() == 2);
  BOOST_TEST(state.graph->modules.at(0).name == "MyModule");
  BOOST_TEST(state.graph->modules.at(1).name == "MyModule2");
  BOOST_TEST(state.graph->module_modules.size() == 1);
  BOOST_TEST(state.graph->module_modules.at(0).first == 0);
  BOOST_TEST(state.graph->module_modules.at(0).second == 1);
}

BOOST_AUTO_TEST_CASE(extracts_multiple) {
  memory_input<> in(
R"(module MyModule {
  foo: reads Int;
  bar: writes Int;

  foo.onChange: bar <- foo;
}

MyModule -> MyModule2
)", __FUNCTION__);  

  SkunkState state;
  parse< toplevels, skunk_action >(in, state);
  BOOST_TEST(state.modules.size() == 1);
  auto module = state.modules[0];
  BOOST_TEST(module->name == "MyModule");
  BOOST_TEST(module->handles.size() == 2);
  assert_handle(module->handles.at(0), "foo", ast::PrimitiveType::Name::intType);
  assert_handle(module->handles.at(1), "bar", ast::PrimitiveType::intType);
  BOOST_TEST(module->listeners.size() == 1);
  assert_listener(module->listeners.at(0), "foo", "onChange", "bar");

  BOOST_TEST(state.graphs.size() == 1);
  auto graph = state.graphs[0];
  BOOST_TEST(graph->modules.size() == 2);
  BOOST_TEST(graph->modules.at(0).name == "MyModule");
  BOOST_TEST(graph->modules.at(1).name == "MyModule2");
  BOOST_TEST(graph->module_modules.size() == 1);
  BOOST_TEST(graph->module_modules.at(0).first == 0);
  BOOST_TEST(graph->module_modules.at(0).second == 1);
}