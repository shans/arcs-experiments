CPP = clang++
CC = clang

CPP_ARGS = --std=c++17
LLVM_ARGS = `llvm-config --cxxflags --ldflags --system-libs --libs all`

%.o: %.cpp
	$(CPP) $(CPP_ARGS) -c $< -o $@

skunk: parser.o skunk.o skunk_ast.o graph.o
	$(CPP) $(CPP_ARGS) $(LLVM_ARGS) $^ -o $@

test_parser: test_parser.o skunk_ast.o graph.o
	$(CPP) $(CPP_ARGS) $(LLVM_ARGS) $^ -o $@
