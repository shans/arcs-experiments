#ifndef _PARSER_H
#define _PARSER_H

#include <string>
#include <iostream>
#include <vector>

#include <tao/pegtl.hpp>

#include "skunk_ast.h"
#include "graph.h"

using namespace tao::pegtl;


/*
module ModuleName {
  input: reads Int;
  output: writes Int;

  input.onChange: output <- input;
}
*/

namespace skunk {

class SkunkState {
public:
  std::string ident = "";
  ast::Type* type = nullptr;
  ast::Module* module = nullptr;
  ast::PrimitiveType::Name typeToken = ast::PrimitiveType::Name::intType;
  ast::Expression* expression = nullptr;
  std::vector<ast::Usage> usages = {};
  std::vector<ast::Handle> handles = {};
  ast::StateReference* statement_output = nullptr;
  ast::Statement* statement = nullptr;
  std::vector<ast::Listener> listeners = {};
  std::string listener_handle = "";
  std::string listener_kind = "";
  std::string module_name = "";
  std::vector<std::string> graph_segments = {};
  Graph* graph = nullptr;
  std::vector<ast::Module*> modules = {};
  std::vector<Graph*> graphs = {};
};

enum ParseOutputError {
  OK, CouldNotOpenFile, ParseError
};

class ParseOutput {
public:
  ParseOutput(std::vector<ast::Module*> modules, std::vector<Graph*> graphs) : modules(std::move(modules)), graphs(std::move(graphs)) { }
  ParseOutput(ParseOutputError error, std::string message) : errorMessage(std::move(message)), error(error) { }
  std::vector<ast::Module*> modules = {};
  std::vector<Graph*> graphs = {};
  std::string errorMessage = "";
  ParseOutputError error = OK;
};

struct kw_module : TAO_PEGTL_KEYWORD( "module" ) {};

struct ident_module : seq< upper, star< identifier_other > > {};
struct ident_handle : seq< lower, star< identifier_other > > {};
struct ident_listener : seq< TAO_PEGTL_STRING( "on" ), upper, star < identifier_other > > {};

struct open_brace : one< '{' > {};
struct close_brace : one< '}' > {};
struct colon: one< ':' > {};
struct semicolon: one< ';' > {};
struct dot: one< '.' > {};
struct left_arrow : string< 0x3C, '-' > {}; // Matches "<-" - using '<' in a string makes the VisualStudio Code C++ parser sad...
struct right_arrow: string< '-', '>' > {};

struct spaces : star< space > {};
struct spaces_required : plus< space > {};

struct kw_reads : TAO_PEGTL_KEYWORD( "reads" ) {};
struct kw_writes : TAO_PEGTL_KEYWORD( "writes" ) {};

struct kw_usage : sor< kw_reads, kw_writes > {};

struct usages : list< kw_usage, spaces_required > {};

struct kw_intType : TAO_PEGTL_KEYWORD( "Int" ) {};
struct kw_stringType : TAO_PEGTL_KEYWORD( "String" ) {};

struct type : sor< kw_intType, kw_stringType > {};

struct handle : seq< ident_handle, spaces, colon, spaces, usages, spaces, type > {};

struct handles : opt< list< seq< handle, spaces, semicolon >, spaces > > {};

struct expression : seq< ident_handle > {};

struct statement_output: seq< ident_handle > {};

struct statement : seq< statement_output, spaces, left_arrow, spaces, expression > {};

struct listener_handle: seq< ident_handle > {};
struct listener_kind: seq< ident_listener > {};

struct listener : seq< listener_handle, dot, listener_kind, spaces, colon, spaces, statement > {};

struct listeners : opt< list< seq< listener, spaces, semicolon >, spaces > > {};

struct module_name: seq< ident_module > {};

struct module : seq< kw_module, spaces_required, module_name, spaces, open_brace, spaces, handles, spaces, listeners, spaces, close_brace > {};

struct ident_graph_module_init: seq< ident_module > {};
struct ident_graph_module: seq< ident_module > {};

struct graph_segment: seq< spaces, right_arrow, spaces, ident_graph_module > {};
struct graph: seq< ident_graph_module_init, plus< seq< graph_segment > > > {};

struct toplevel: sor< graph, module > {};

struct toplevels: list< toplevel, spaces_required > {};

template< typename Rule >
struct skunk_action : nothing< Rule > {};

#define CAPTURE_IDENT(ident_rule) \
template<> \
struct skunk_action< ident_rule > { \
  template< typename ActionInput > \
  static void apply( const ActionInput& in, SkunkState& out) { \
    out.ident = std::move(in.string()); \
  } \
};

#define CAPTURE_TOKEN(token_rule, token_field, token_type) \
template<> \
struct skunk_action< token_rule > { \
  static void apply0(SkunkState& out) { \
    out.token_field = token_type; \
  } \
};

#define ACCUMULATE_TOKEN(token_rule, token_field, token_type) \
template<> \
struct skunk_action< token_rule > { \
  static void apply0(SkunkState& out) { \
    out.token_field.push_back(token_type); \
  } \
};

#define MOVE_IDENT(rule, location) \
template<> \
struct skunk_action< rule > {\
  static void apply0(SkunkState& out) { \
    out.location = std::move(out.ident); \
  } \
};

#define ACCUMULATE_IDENT(rule, location) \
template<> \
struct skunk_action< rule > {\
  static void apply0(SkunkState& out) { \
    out.location.push_back(std::move(out.ident)); \
  } \
};

CAPTURE_IDENT(ident_module)
CAPTURE_IDENT(ident_handle)
CAPTURE_IDENT(ident_listener)

CAPTURE_TOKEN(kw_intType, typeToken, ast::PrimitiveType::Name::intType)
CAPTURE_TOKEN(kw_stringType, typeToken, ast::PrimitiveType::Name::stringType)

ACCUMULATE_TOKEN(kw_reads, usages, ast::Usage::reads)
ACCUMULATE_TOKEN(kw_writes, usages, ast::Usage::writes)

MOVE_IDENT(listener_handle, listener_handle)
MOVE_IDENT(listener_kind, listener_kind)

MOVE_IDENT(module_name, module_name)

ACCUMULATE_IDENT(ident_graph_module, graph_segments)
ACCUMULATE_IDENT(ident_graph_module_init, graph_segments)

template<>
struct skunk_action< module > {
  static void apply0(SkunkState& out) {
    out.module = new ast::Module(std::move(out.module_name), std::move(out.handles), std::move(out.listeners));
  }
};

template<>
struct skunk_action< type > {
  template< typename ActionInput >
  static void apply( const ActionInput& in, SkunkState& out) {
    out.type = new ast::PrimitiveType(out.typeToken);
  }
};

template<>
struct skunk_action< handle > {
  static void apply0(SkunkState& out) {
    out.handles.push_back(std::move(ast::Handle(std::move(out.ident), out.type, std::move(out.usages))));
    out.type = nullptr;
  }
};

template<>
struct skunk_action< expression > {
  static void apply0(SkunkState& out) {
    // only references supported at the moment..
    out.expression = new ast::StateReference(std::move(out.ident));
  }
};

template<>
struct skunk_action< statement_output > {
  static void apply0(SkunkState& out) {
    out.statement_output = new ast::StateReference(std::move(out.ident));
  }
};

template<>
struct skunk_action< statement > {
  static void apply0(SkunkState& out) {
    out.statement = new ast::Statement(out.statement_output, out.expression);
    out.statement_output = nullptr;
    out.expression = nullptr;
  }
};

template<>
struct skunk_action< listener > {
  static void apply0(SkunkState& out) {
    out.listeners.push_back(std::move(ast::Listener(std::move(out.listener_handle), std::move(out.listener_kind), out.statement)));
    out.statement = nullptr;
  }
};

template<>
struct skunk_action< graph > {
  static void apply0(SkunkState& out) {
    out.graph = new Graph();
    auto prev = -1;
    std::vector<ast::Usage> writes = {ast::Usage::writes};
    for(size_t i = 0; i < out.graph_segments.size(); i++) {
      auto current = out.graph->addModule(out.graph_segments[i]);
      if (prev >= 0) {
        out.graph->connectModuleToModule(prev, current, writes);
      }
      prev = current;
    }
  }
};

template<>
struct skunk_action< toplevel > {
  static void apply0(SkunkState& out) {
    if (out.graph != nullptr) {
      out.graphs.push_back(out.graph);
      out.graph = nullptr;
    }
    if (out.module != nullptr) {
      out.modules.push_back(out.module);
      out.module = nullptr;
    }
  }
};

ParseOutput parseString(std::string string);

ParseOutput parseFile(std::string fileName);

}
#endif