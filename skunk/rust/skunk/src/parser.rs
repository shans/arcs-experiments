extern crate nom;

use super::ast;

use nom::{
  IResult,
  branch::alt,
  bytes::complete::tag,
  character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1}, 
  combinator::{verify, map_res},
  error::{Error, ErrorKind},
  multi::{separated_list0, separated_list1},
  sequence::tuple,
};

#[inline]
pub fn is_upper_alphabetic(chr: char) -> bool {
  chr >= 'A' && chr <= 'Z'
}

fn uppercase_name(i: &str) -> IResult<&str, &str> {
  verify(alphanumeric1, |s: &str| s.len() > 0 && is_upper_alphabetic(s.chars().nth(0).unwrap()))(i)
}

fn name(i: &str) -> IResult<&str, &str> {
  alpha1(i)
}

fn token<T : Copy>(text: &'static str, result: T) -> impl Fn(&str) -> IResult<&str, T> {
  move |i: &str| {
    let (input, _) = tag(text)(i)?;
    Ok((input, result))
  }
}

fn usage_token(i: &str) -> IResult<&str, ast::Usage> {
  let reads = token("reads", ast::Usage::Read);
  let writes = token("writes", ast::Usage::Write);
  alt((reads, writes))(i)
}

fn usages(i: &str) -> IResult<&str, Vec<ast::Usage>> {
  separated_list1(multispace1, usage_token)(i)
}

fn type_primitive_token(i: &str) -> IResult<&str, ast::TypePrimitive> {
  let int = token("Int", ast::TypePrimitive::Int);
  let string = token("String", ast::TypePrimitive::String);
  alt((int, string))(i)
}

fn handle(i: &str) -> IResult<&str, ast::Handle> {
  let (input, (h_name, _, _, _, h_usages, _, h_type, _, _))
    = tuple((name, multispace0, char(':'), multispace0, usages, multispace1, type_primitive_token, multispace0, char(';')))(i)?;
  Ok((
    input, 
    ast::Handle {
      name: String::from(h_name),
      usages: h_usages.clone(),
      h_type
    }
  ))
}

fn handles(i: &str) -> IResult<&str, Vec<ast::Handle>> {
  separated_list0(multispace1, handle)(i)
}

fn kind_token(i: &str) -> IResult<&str, ast::ListenerKind> {
  alt((
    token("onChange", ast::ListenerKind::OnChange),
    token("onWrite", ast::ListenerKind::OnWrite),
  ))(i)
}

fn expression(i: &str) -> IResult<&str, ast::Expression> {
  name(i).map(|(rest, state_elt)| (rest, ast::Expression::ReferenceToState(state_elt.to_string())))
}

fn statement(i: &str) -> IResult<&str, ast::Statement> {
  let (input, (output_name, _, _, _, expr)) = tuple((name, multispace0, tag("<-"), multispace0, expression))(i)?;
  Ok((
    input,
    ast::Statement { output: output_name.to_string(), expression: expr }
  ))
}

fn listener(i: &str) -> IResult<&str, ast::Listener> {
  let (input, (trigger, _, kind, _, _, _, statement, _, _)) 
    = tuple((name, char('.'), kind_token, multispace0, char(':'), multispace0, statement, multispace0, char(';')))(i)?;
  Ok((
    input,
    ast::Listener {
      trigger: trigger.to_string(),
      kind,
      statement,
    }
  ))
}

fn listeners(i: &str) -> IResult<&str, Vec<ast::Listener>> {
  separated_list0(multispace1, listener)(i)
}

fn module(i: &str) -> IResult<&str, ast::Module> {
  let (input, (_, _, name, _, _, _, handles, _, listeners, _, _))
    = tuple((tag("module"), multispace1, uppercase_name, multispace0, char('{'), multispace0, handles, multispace0, listeners, multispace0, char('}')))(i)?;
  Ok((
    input,
    ast::Module {
      name: name.to_string(),
      handles,
      listeners,
    }
  ))
}

fn graph(i: &str) -> IResult<&str, ast::Graph> {
  let (input, names) = separated_list1(tuple((multispace0, tag("->"), multispace0)), uppercase_name)(i)?;
  Ok((
    input,
    ast::Graph { modules: names.iter().map(|s| s.to_string()).collect() }
  ))
}

fn graph_top_level(i: &str) -> IResult<&str, ast::TopLevel> {
  let (input, graph) = graph(i)?;
  Ok((input, ast::TopLevel::Graph(graph)))
}

fn module_top_level(i: &str) -> IResult<&str, ast::TopLevel> {
  let (input, module) = module(i)?;
  Ok((input, ast::TopLevel::Module(module)))
}

fn top_level(i: &str) -> IResult<&str, ast::TopLevel> {
  alt((graph_top_level, module_top_level))(i)
}

fn top_levels(i: &str) -> IResult<&str, Vec<ast::TopLevel>> {
  separated_list0(multispace1, top_level)(i)
}

#[cfg(test)]
mod tests {
  use super::*;

  fn mk_error<I, O>(msg: I, code: ErrorKind) -> IResult<I, O> {
    Err(nom::Err::Error(Error { input: msg, code }))
  }

  #[test]
  fn parse_uppercase_names() {
    assert_eq!(uppercase_name("Hello"), Ok(("", "Hello")));
    assert_eq!(uppercase_name("Hello2"), Ok(("", "Hello2")));
    assert_eq!(uppercase_name("TesT other"), Ok((" other", "TesT")));
    assert_eq!(uppercase_name("NAmE!other"), Ok(("!other", "NAmE")));
    assert_eq!(uppercase_name("hello"), mk_error("hello", ErrorKind::Verify));
  }

  #[test]
  fn parse_usage_token() {
    assert_eq!(usage_token("reads"), Ok(("", ast::Usage::Read)));
  }

  #[test]
  fn parse_usages() {
    assert_eq!(usages("reads writes"), Ok(("", vec!(ast::Usage::Read, ast::Usage::Write))));
  }

  #[test]
  fn parse_type_primitive() {
    assert_eq!(type_primitive_token("Int"), Ok(("", ast::TypePrimitive::Int)));
  }

  #[test]
  fn parse_handle() {
    assert_eq!(
      handle("foo: reads writes Int;"),
      Ok((
        "",
        ast::Handle { name: String::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::TypePrimitive::Int }
      ))
    );
    assert_eq!(
      handle("bar: writes String;"),
      Ok((
        "",
        ast::Handle { name: String::from("bar"), usages: vec!(ast::Usage::Write), h_type: ast::TypePrimitive::String }
      ))
    );
    assert_eq!(
      handle("foo: reads writes Int; bar: writes String;"),
      Ok((
        " bar: writes String;",
        ast::Handle { name: String::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::TypePrimitive::Int }
      ))
    )
  }

  #[test]
  fn parse_handles() {
    assert_eq!(
      handles("foo: reads writes Int;
               bar: writes String;"),
      Ok((
        "",
        vec!(
          ast::Handle { name: String::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::TypePrimitive::Int },
          ast::Handle { name: String::from("bar"), usages: vec!(ast::Usage::Write), h_type: ast::TypePrimitive::String },
        )
      ))
    )
  }

  #[test]
  fn parse_kind_token() {
    assert_eq!(kind_token("onChange"), Ok(("", ast::ListenerKind::OnChange)));
    assert_eq!(kind_token("onWrite"), Ok(("", ast::ListenerKind::OnWrite)));
  }

  #[test]
  fn parse_listener() {
    assert_eq!(
      listener("foo.onChange: bar <- far;"),
      Ok((
        "",
        ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, statement: ast::Statement {
          output: String::from("bar"), expression: ast::Expression::ReferenceToState(String:: from("far"))
        }}
      ))
    )
  }

  #[test]
  fn parse_listeners() {
    assert_eq!(
      listeners("foo.onChange: bar <- far;
                 far.onWrite: bax <- fax;"),
      Ok((
        "",
        vec!(
          ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, statement: ast::Statement {
            output: String::from("bar"), expression: ast::Expression::ReferenceToState(String:: from("far"))
          }},
          ast::Listener { trigger: String::from("far"), kind: ast::ListenerKind::OnWrite, statement: ast::Statement {
            output: String::from("bax"), expression: ast::Expression::ReferenceToState(String:: from("fax"))
          }}
        )

      ))
    )
  }

  static TEST_MODULE_STRING : &str = 
"module TestModule {
  foo: reads writes Int;

  foo.onChange: bar <- far;
}";

  fn test_module_result() -> ast::Module {
    ast::Module {
      name: String::from("TestModule"),
      handles: vec!(ast::Handle { name: String::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::TypePrimitive::Int }),
      listeners: vec!(ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, statement: ast::Statement {
        output: String::from("bar"), expression: ast::Expression::ReferenceToState(String::from("far"))
      }})
    }
  }

  static TEST_GRAPH_STRING : &str = "MyModule -> MyModule2 -> AnotherModule";
  
  fn test_graph_result() -> ast::Graph {
    ast::Graph { modules: vec!(String::from("MyModule"), String::from("MyModule2"), String::from("AnotherModule")) }
  }

  #[test]
  fn parse_module() {
    assert_eq!(
      module(TEST_MODULE_STRING),
      Ok((
        "",
        test_module_result()
      ))
    )
  }

  #[test]
  fn parse_graph() {
    assert_eq!(
      graph(TEST_GRAPH_STRING),
      Ok((
        "",
        test_graph_result()
      ))
    )
  }

  #[test]
  fn parse_top_level() {
    assert_eq!(
      top_level(TEST_MODULE_STRING),
      Ok(( "", ast::TopLevel::Module(test_module_result()) ))   
    );
    assert_eq!(
      top_level(TEST_GRAPH_STRING),
      Ok(( "", ast::TopLevel::Graph(test_graph_result()) ))   
    );
  }

  #[test]
  fn parse_top_levels() {
    let test_str = TEST_MODULE_STRING.to_string() + "\n\n" + TEST_GRAPH_STRING;
    assert_eq!(
      top_levels(&test_str),
      Ok((
        "",
        vec!(ast::TopLevel::Module(test_module_result()), ast::TopLevel::Graph(test_graph_result()))
      ))
    )
  }
}