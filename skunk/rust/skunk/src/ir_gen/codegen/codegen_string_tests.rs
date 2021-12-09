use super::*;
use super::super::super::parser;
use super::tests::test_module;

use inkwell::execution_engine::{JitFunction, ExecutionEngine};
use super::super::super::graph_builder;
use super::super::super::graph_to_module;
use super::super::codegen_state::tests::*;
use paste::paste;
use std::ptr;

fn ee_for_string<F>(module: &str, func: F) -> CodegenStatus
    where F: FnOnce(ExecutionEngine, &Module) -> () {
  let context = Context::create();
  let (_, ast) = parser::parse(module).unwrap();
  
  if ast::modules(&ast).len() == 1 && ast::graphs(&ast).len() == 0 {
    let mut jit_info = JitInfo::new();
    let modules = codegen(&context, &mut jit_info, &ast::modules(&ast)[0])?;
    let ee = jit_info.execution_engine.unwrap();
    func(ee, &modules[0]);
  } else {
    let mut graph = graph_builder::make_graph(ast::graphs(&ast));
    graph_builder::resolve_graph(&ast::modules(&ast), &mut graph).unwrap();
    let modules = ast::modules(&ast);
    let main = graph_to_module::graph_to_module(graph, modules, "Main").unwrap();
    let mut jit_info = JitInfo::new();
    let cg_modules = codegen(&context, &mut jit_info, &main)?;
    let ee = jit_info.execution_engine.unwrap();
    func(ee, &cg_modules[0]);
  }
  Ok(())
}

#[derive(Debug)]
#[repr(C)]
pub struct MemRegion {
  data: u64, // TODO is there a better representation than this?
  size: u64,
}

impl MemRegion {
  fn empty() -> Self {
    MemRegion { data: 0, size: 0 }
  }
  fn from_str(data: &'static str) -> Self {
    let size = data.len() as u64;
    MemRegion { data: data.as_ptr() as u64, size }
  }
}

#[macro_export]
macro_rules! state_struct {
  ( $name:ident, $($field_name:ident: $field_type:ty),* $(| $($module_field:ident: $module_type:ty),*)? ) => {
    paste! {
      #[derive(Debug)]
      #[repr(C)]
      pub struct [<$name State>] {
        $(
          pub $field_name: $field_type,
          pub [<$field_name _upd>]: $field_type,
        )*
        pub bitfield: u64,
        $( $(
          pub $module_field: $module_type,
        )* )?
      } 

      type [<$name Func>] = unsafe extern "C" fn(*mut [<$name State>]) -> ();
      type [<$name PrepFunc>] = unsafe extern "C" fn() -> *mut [<$name State>];
      type [<$name PrepVectorFunc>] = unsafe extern "C" fn(u64) -> *mut [<$name State>];
      type [<$name CheckFunc>] = unsafe extern "C" fn(*mut [<$name State>]) -> u64;
      type [<$name CheckVectorFunc>] = unsafe extern "C" fn(u64, *mut [<$name State>]) -> u64;
      type [<$name RunFunc>] = unsafe extern "C" fn(u64) -> u64;
    }
  }
}

type u64Func = unsafe extern "C" fn() -> u64;

#[macro_export]
macro_rules! check_examples {
  ($name:ident, $defn:ident) => {
    paste! {
      #[test]
      fn [<check_examples_for_ $name>]() -> CodegenStatus {
        ee_for_string($defn, |ee: ExecutionEngine, _m| {
          unsafe {
            //_m.print_to_stderr();
            let count_func = concat!(stringify!($name), "__get_example_count");
            let count_function: JitFunction<u64Func> = ee.get_function(count_func).unwrap();
            let count = count_function.call();

            let run_func = concat!(stringify!($name), "__run_example");
            let run_function: JitFunction<[<$name RunFunc>]> = ee.get_function(run_func).unwrap();

            for i in (0..count) {
              let r= run_function.call(i);
              assert_eq!(r, 0);
            }
          }
        })
      }
    }
  }
}

state_struct!(TestModule, foo: u64, far: u64, bar: u64);

#[test]
fn jit_module_codegen_runs() -> CodegenStatus {
  let context = Context::create();
  let module = test_module();
  let mut jit_info = JitInfo::new();
  let mut cg = jit_info.construct(&context, "TestModule");
  let ee = jit_info.execution_engine.unwrap();

  module_codegen(&mut cg, &module)?;
  unsafe {
    let function: JitFunction<TestModuleFunc> = ee.get_function("TestModule_update").unwrap();
    let mut state = TestModuleState { foo: 0, foo_upd: 10, bar: 0, bar_upd: 0, far: 20, far_upd: 0, bitfield: 0x1 };
    function.call(&mut state);
    assert_eq!(state.bitfield, 0x4);
    assert_eq!(state.bar_upd, 20);
  }

  Ok(())
}

static MULTI_MODULE_STRING: &str = "
module MyModule {
  foo: reads Int;
  bar: writes Int;

  foo.onChange: bar <- foo;
}

module MyModule2 {
  foo: reads Int;
  bar: writes Int;

  foo.onChange: bar <- foo;
}

MyModule -> MyModule2;
";

state_struct!(NestedModule, foo: u64, bar: u64);
state_struct!(MultiModule, foo: u64, bar: u64, h0: u64 | my_module:NestedModuleState, my_module2:NestedModuleState);


#[test]
fn jit_multi_module_codegen_runs() -> CodegenStatus {
  ee_for_string(MULTI_MODULE_STRING, |ee: ExecutionEngine, _| {
    unsafe {
      let function: JitFunction<MultiModuleFunc> = ee.get_function("Main_update").unwrap();
      let mut state = MultiModuleState { foo: 0, foo_upd: 10, h0: 0, h0_upd: 0, bar: 0, bar_upd: 0, bitfield: 0x1, 
                        my_module: NestedModuleState { foo: 0, foo_upd: 0, bar: 0, bar_upd: 0, bitfield: 0 },
                        my_module2: NestedModuleState { foo: 0, foo_upd: 0, bar: 0, bar_upd: 0, bitfield: 0 },
                      };

      // After a single call to update, foo_upd has been copied into my_module.foo_upd, then my_module has been
      // iterated until stable (foo_upd -> foo & bar_upd, bar_upd -> bar); new state has been copied back (h0_upd).
      // my_module2 has not been changed.
      function.call(&mut state);
      assert_eq!(state.bitfield, 0x4);
      assert_eq!(state.foo, 10);
      assert_eq!(state.foo_upd, 0);
      assert_eq!(state.h0_upd, 10);
      assert_eq!(state.my_module.foo, 10);
      assert_eq!(state.my_module.bar, 10);
      assert_eq!(state.my_module2.foo, 0);
      assert_eq!(state.my_module2.bar, 0);

      // After a second call to update, h0_upd has been copied into my_module2.foo_upd, then my_module2 has been
      // iterated until stable (foo_upd -> foo & bar_upd, bar_upd -> bar); new state has been copied back (bar_upd).
      // my_module has not been changed.
      function.call(&mut state);
      assert_eq!(state.bitfield, 0x2);
      assert_eq!(state.h0, 10);
      assert_eq!(state.h0_upd, 0);
      assert_eq!(state.bar_upd, 10);
      assert_eq!(state.my_module.foo, 10);
      assert_eq!(state.my_module.bar, 10);
      assert_eq!(state.my_module2.foo, 10);
      assert_eq!(state.my_module2.bar, 10);
    }
  })
}

static NEW_MEMREGION_TEST_STRING: &str = "
module ModuleWithNew {
  bar: writes MemRegion;
  foo: reads Int;

  foo.onChange: bar <- new(foo);
}
";

state_struct!(ModuleWithNew, bar: MemRegion, foo: u64);

#[test]
fn jit_new_memregion_codegen_runs() -> CodegenStatus {
  ee_for_string(NEW_MEMREGION_TEST_STRING, |ee: ExecutionEngine, _| {
    unsafe {
      let function: JitFunction<ModuleWithNewFunc> = ee.get_function("ModuleWithNew_update").unwrap();
      let mut state = ModuleWithNewState { bar: MemRegion::empty(), bar_upd: MemRegion::empty(), foo: 0, foo_upd: 10, bitfield: 0x2 };
      function.call(&mut state);
      assert_eq!(state.bitfield, 0x1);
      assert_eq!(state.foo, 10);
      assert_eq!(state.foo_upd, 0);
      assert_eq!(state.bar_upd.size, 10);
      assert_ne!(state.bar_upd.data, 0);
    }
  })
}

static COPY_MEMREGION_TEST_STRING: &str = "
module Writer {
  size_in: reads Int;
  region: writes MemRegion;
  
  size_in.onChange: region <- new(size_in);
}

module Reader {
  region: reads MemRegion;
  size_out: writes Int;

  region.onChange: size_out <- size(region);
}

Writer -> Reader;
";

state_struct!(Writer, size_in: u64, region: MemRegion);
state_struct!(Reader, region: MemRegion, size_out: u64);
state_struct!(CopyMemRegionMain, size_in: u64, size_out: u64, h0: MemRegion | writer: WriterState, reader: ReaderState);

#[test]
fn jit_copy_memregion_codegen_runs() -> CodegenStatus {
  ee_for_string(COPY_MEMREGION_TEST_STRING, |ee: ExecutionEngine, _| {
    unsafe {
      let function: JitFunction<CopyMemRegionMainFunc> = ee.get_function("Main_update").unwrap();  
      let mut state = CopyMemRegionMainState {
        size_in: 0, size_in_upd: 50, size_out: 0, size_out_upd: 0, h0: MemRegion::empty(), h0_upd: MemRegion::empty(), bitfield: 0x1,
        writer: WriterState { size_in: 0, size_in_upd: 0, region: MemRegion::empty(), region_upd: MemRegion::empty(), bitfield: 0},
        reader: ReaderState { size_out: 0, size_out_upd: 0, region: MemRegion::empty(), region_upd: MemRegion::empty(), bitfield: 0},
      };

      function.call(&mut state);
      assert_eq!(state.bitfield, 0x4);
      assert_eq!(state.h0_upd.size, 50);
      assert_ne!(state.h0_upd.data, 0);

      function.call(&mut state);
      assert_eq!(state.bitfield, 0x2);
      assert_eq!(state.size_out_upd, 50);
    }
  })
}

static STRING_TEST_STRING: &str = "
module CreateString {
  inp: reads Int;
  out: writes String;

  inp.onChange: out <- \"static string\";
}

module CharFromString {
  inp: reads String;
  out: writes Char;
  
  inp.onChange: out <- inp[5];
}

CreateString -> CharFromString;
";

state_struct!(CreateString, inp: u64, out: MemRegion);
state_struct!(CharFromString, inp: MemRegion, out: u8);
state_struct!(StringTestMain, inp: u64, out: u8, h0: MemRegion | writer: CreateStringState, reader: CharFromStringState);

#[test]
fn jit_create_string_codegen_runs() -> CodegenStatus {
  ee_for_string(STRING_TEST_STRING, |ee: ExecutionEngine, _| {
    unsafe {
      let function: JitFunction<StringTestMainFunc> = ee.get_function("Main_update").unwrap();
      let mut state = StringTestMainState {
        inp: 0, inp_upd: 10, h0: MemRegion::empty(), h0_upd: MemRegion::empty(), out: 0, out_upd: 0, bitfield: 0x1,
        writer: CreateStringState { inp: 0, inp_upd: 0, out: MemRegion::empty(), out_upd: MemRegion::empty(), bitfield: 0x0},
        reader: CharFromStringState { inp: MemRegion::empty(), inp_upd: MemRegion::empty(), out: 0, out_upd: 0, bitfield: 0x0}
      };

      function.call(&mut state);
      function.call(&mut state);
      assert_eq!(state.out_upd, 'c' as u8);
    }
  })
}

static TUPLE_TEST_STRING: &str = "
module CreateTuple {
  inp: reads Int;
  out: writes (Int, String);

  inp.onChange: out <- (inp, \"static string\");
}

module ReadTuple {
  inp: reads (Int, String);
  out1: writes Int;
  out2: writes String;

  inp.onChange: {
    out1 <- inp.0;
    out2 <- inp.1;
  }
}

CreateTuple -> ReadTuple;
";

state_struct!(CreateTuple, inp: u64, out: *const (u64, MemRegion));
state_struct!(ReadTuple, inp: *const (u64, MemRegion), out1: u64, out2: MemRegion);
state_struct!(TupleMain, inp: u64, out1: u64, out2: MemRegion, h0: *const (u64, MemRegion) | writer: CreateTupleState, reader: ReadTupleState);

#[test]
fn jit_create_tuple_codegen_runs() -> CodegenStatus {
  ee_for_string(TUPLE_TEST_STRING, |ee: ExecutionEngine, _| {
    unsafe {
      let function: JitFunction<TupleMainFunc> = ee.get_function("Main_update").unwrap();
      let mut state = TupleMainState {
        inp: 0, inp_upd: 10, out1: 0, out1_upd: 0, out2: MemRegion::empty(), out2_upd: MemRegion::empty(), 
        h0: ptr::null(), h0_upd: ptr::null(),
        bitfield: 0x1,
        writer: CreateTupleState { inp: 0, inp_upd: 0, out: ptr::null(), out_upd: ptr::null(), bitfield: 0x0 },
        reader: ReadTupleState { inp: ptr::null(), inp_upd: ptr::null(), out1: 0, out1_upd: 0, out2: MemRegion::empty(), out2_upd: MemRegion::empty(), bitfield: 0x0}
      };

      function.call(&mut state);
      function.call(&mut state);
      assert_eq!(state.out1_upd, 10);
      assert_eq!(state.out2_upd.size, 13);
    }
  })
}

static SYNTAX_TEST_STRING: &str = "
module SyntaxTest {
  input: reads (String, Int);
  output: writes (String, Int);
  result: writes Int;
  error: writes Int;

  input.onChange: {
    let offset = input.1;
    if offset == size(input.0) || input.0[offset] < '0' || input.0[offset] > '9' {
      error <!- 1;
    }
    
    let result = 0;
    while input.0[offset] >= '0' && input.0[offset] <= '9' {
      result = result * 10 + input.0[offset] - '0';
      offset = offset + 1;
      if offset == size(input.0) {
        break;
      }
    }

    result <- result;
    output <- (input.0, offset);
  }

  examples {
    !input: (\"420e\", 0) -> result: 420;
    !input: (\"420e\", 1) -> result: 20;
    !input: (\"420e\", 2) -> result: 0;
    !input: (\"420e\", 3) -> error: 1;

    !input: (\"in text 151 end text\", 4) -> error: 1;
    !input: (\"in text 151 end text\", 7) -> error: 1;
    !input: (\"in text 151 end text\", 8) -> result: 151, output: (\"in text 151 end text\", 11);

    !input: (\"54 is a number\", 0) -> result: 54, output: (\"54 is a number\", 2);
  }
}
";

state_struct!(SyntaxTest, inp: *const (MemRegion, u64), out: *const(MemRegion, u64), result: u64, error: u64);

check_examples!(SyntaxTest, SYNTAX_TEST_STRING);

#[test]
fn jit_syntax_test_codegen_runs() -> CodegenStatus {
  ee_for_string(SYNTAX_TEST_STRING, |ee: ExecutionEngine, _m| {
    unsafe {
      //_m.print_to_stderr();
      let function: JitFunction<SyntaxTestFunc> = ee.get_function("SyntaxTest_update").unwrap();
      let mut state = SyntaxTestState { inp: ptr::null(), inp_upd: &(MemRegion::from_str("Delicious boots"), 15), out: ptr::null(), out_upd: ptr::null(), result: 0, result_upd: 0, error: 0, error_upd: 0, bitfield: 0x1 };
      function.call(&mut state);
      assert_eq!(state.bitfield, 0x8); // error updated
      let mut state = SyntaxTestState { inp: ptr::null(), inp_upd: &(MemRegion::from_str("15 Delicious boots"), 0), out: ptr::null(), out_upd: ptr::null(), result: 0, result_upd: 0, error: 0, error_upd: 0, bitfield: 0x1 };
      function.call(&mut state);
      assert_eq!(state.bitfield, 0x6); // result, output updated
      assert_eq!((*state.out_upd).1, 2); // offset advanced by 2
      assert_eq!(state.result_upd, 15); // result parsed from string
      let mut state = SyntaxTestState { inp: ptr::null(), inp_upd: &(MemRegion::from_str("Delicious boots"), 0), out: ptr::null(), out_upd: ptr::null(), result: 0, result_upd: 0, error: 0, error_upd: 0, bitfield: 0x1 };
      function.call(&mut state);
      assert_eq!(state.bitfield, 0x8); // error updated
    }
  }) 
}

static WHILE_TEST_STRING: &str = "
module WhileTest {
  len: reads Int;
  string: reads String;
  result: writes Int;

  len.onChange: {
    let x = 0;
    let counter = 0;
    while string[counter] >= '0' && string[counter] <= '9' {
      x = x * 10 + string[counter] - '0';
      counter = counter + 1;
      if counter == len {
        break;
      }
    }

    result <- x;
  }
}  
";

state_struct!(WhileTest, len: u64, string: MemRegion, result: u64);

#[test]
fn jit_while_test_codegen_runs() -> CodegenStatus {
  ee_for_string(WHILE_TEST_STRING, |ee: ExecutionEngine, _| {
    unsafe {
      let function: JitFunction<WhileTestFunc> = ee.get_function("WhileTest_update").unwrap();
      let mut state = WhileTestState { len: 0, len_upd: 5, string: MemRegion::from_str("42360 "), string_upd: MemRegion::empty(), result: 0, result_upd: 0, bitfield: 0x1 };
      function.call(&mut state);
      dbg!(state);
    }
  })
}

static EXAMPLES_TEST_STRING: &str = "
module ExampleTest {
  a: reads Int;
  b: reads Int;
  c: writes Int;
  a.onChange: c <- a + b;
  b.onChange: c <- a + b;

  examples {
    a: 4, !b: 5 -> c: 9;
    !a: 10, b: 1 -> c: 15;
  }
}
";

state_struct!(ExampleTest, a: u64, b: u64, c: u64);

#[test]
fn compile_and_run_examples() -> CodegenStatus {
  ee_for_string(EXAMPLES_TEST_STRING, |ee: ExecutionEngine, _| {
    unsafe {
      let function: JitFunction<ExampleTestPrepFunc> = ee.get_function("ExampleTest__example_prep_0").unwrap();
      let state = function.call();
      assert_eq!((&*state).a, 4);
      assert_eq!((&*state).b_upd, 5);
      assert_eq!((&*state).bitfield, 0x2);

      let update: JitFunction<ExampleTestFunc> = ee.get_function("ExampleTest_update").unwrap();
      while (&*state).bitfield > 0 {
        update.call(state);
      }
      assert_eq!((&*state).c, 9);

      let check: JitFunction<ExampleTestCheckFunc> = ee.get_function("ExampleTest__example_check_0").unwrap();
      let r = check.call(state);
      assert_eq!(r, 0);
    }

    unsafe {
      let function: JitFunction<ExampleTestPrepFunc> = ee.get_function("ExampleTest__example_prep_1").unwrap();
      let state = function.call();
      assert_eq!((&*state).a_upd, 10);
      assert_eq!((&*state).b, 1);
      assert_eq!((&*state).bitfield, 0x1);

      let update: JitFunction<ExampleTestFunc> = ee.get_function("ExampleTest_update").unwrap();
      while (&*state).bitfield > 0 {
        update.call(state);
      }
      assert_eq!((&*state).c, 11);

      let check: JitFunction<ExampleTestCheckFunc> = ee.get_function("ExampleTest__example_check_1").unwrap();
      let r = check.call(state);
      assert_eq!(r, 0x4);
    }
  })
}

