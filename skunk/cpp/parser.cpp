#include "parser.h"

#include <fstream>

namespace skunk {

ParseOutput parseFile(std::string fileName) {
  std::filebuf fb;
  if (!fb.open(fileName, std::ios::in)) {
    std::string errorMessage = "Could not open input file " + fileName;
    return ParseOutput(CouldNotOpenFile, errorMessage);
  }
  std::istream is(&fb);
  // FIXME: Provide reasonable buffer size and implement
  // discard() calling: https://github.com/taocpp/PEGTL/blob/master/doc/Inputs-and-Parsing.md#incremental-input
  istream_input<eol::cr_crlf> input(is, 10000, fileName);
  
  skunk::SkunkState state;
  if (!parse< skunk::toplevels, skunk::skunk_action >(input, state)) {
    std::string errorMessage = "Parse failed for " + fileName;
    return ParseOutput(ParseError, errorMessage);
  }

  return ParseOutput(std::move(state.modules), std::move(state.graphs));
}

ParseOutput parseString(std::string string) {
  memory_input<> input(string, __FUNCTION__);
  skunk::SkunkState state;
  if (!parse< skunk::toplevels, skunk::skunk_action >(input, state)) {
    return ParseOutput(ParseError, "Parse failed for provided string");
  }
  
  return ParseOutput(std::move(state.modules), std::move(state.graphs));
}

}

