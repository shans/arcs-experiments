extern crate nom;

use super::ast;

use nom::{
  IResult,
  branch::alt,
  bytes::complete::{tag, is_a, take_until},
  character::complete::{alpha1, char, multispace0, multispace1, digit1}, 
  combinator::{verify, eof, cut},
  error::{Error, ErrorKind, VerboseError, VerboseErrorKind},
  multi::{separated_list0, separated_list1, many0, many_till},
  sequence::{tuple, delimited, terminated, preceded},
};

// use nom_supreme::error::ErrorTree;

#[inline]
pub fn is_upper_alphabetic(chr: char) -> bool {
  chr >= 'A' && chr <= 'Z'
}

#[inline]
pub fn is_lower_alphabetic(chr: char) -> bool {
  chr >= 'a' && chr <= 'z'
}

static ALLOWED_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";

type ParseResult<'a, T> = IResult<&'a str,T, VerboseError<&'a str>>;

fn uppercase_name(i: &str) -> ParseResult<&str> {
  verify(is_a(ALLOWED_CHARS), |s: &str| s.len() > 0 && is_upper_alphabetic(s.chars().nth(0).unwrap()))(i)
}

fn name(i: &str) -> ParseResult<&str> {
  verify(is_a(ALLOWED_CHARS), |s: &str| s.len() > 0 && is_lower_alphabetic(s.chars().nth(0).unwrap()))(i)
}

fn token<T: Clone>(text: &'static str, result: T) -> impl Fn(&str) -> ParseResult<T> {
  move |i: &str| {
    let (input, _) = tag(text)(i)?;
    Ok((input, result.clone()))
  }
}

fn usage_token(i: &str) -> ParseResult<ast::Usage> {
  let reads = token("reads", ast::Usage::Read);
  let writes = token("writes", ast::Usage::Write);
  alt((reads, writes))(i)
}

fn usages(i: &str) -> ParseResult<Vec<ast::Usage>> {
  cut(separated_list1(multispace1, usage_token))(i)
}

fn type_primitive_token(i: &str) -> ParseResult<ast::Type> {
  alt((
    token("Int", ast::Type::Int),
    token("String", ast::Type::String),
    token("MemRegion", ast::Type::MemRegion),
    token("Char", ast::Type::Char),
  ))(i)
}

fn tuple_type(i: &str) -> ParseResult<ast::Type> {
  let (input, members) = delimited(char('('), separated_list1(tuple((multispace0, char(','), multispace0)), handle_type), char(')'))(i)?;
  Ok((input, ast::Type::Tuple(members)))
}

fn handle_type(i: &str) -> ParseResult<ast::Type> {
  cut(alt((type_primitive_token, tuple_type)))(i)
}

fn handle(i: &str) -> ParseResult<ast::Handle> {
  let (input, (h_name, _, _, _, h_usages, _, h_type, _, _))
    = tuple((name, multispace0, char(':'), multispace0, usages, multispace1, handle_type, multispace0, char(';')))(i)?;
  Ok((
    input, 
    ast::Handle {
      name: String::from(h_name),
      usages: h_usages.clone(),
      h_type
    }
  ))
}

fn handles(i: &str) -> ParseResult<Vec<ast::Handle>> {
  separated_list0(multispace1, handle)(i)
}

fn kind_token(i: &str) -> ParseResult<ast::ListenerKind> {
  alt((
    token("onChange", ast::ListenerKind::OnChange),
    token("onWrite", ast::ListenerKind::OnWrite),
  ))(i)
}

fn state_reference(i: &str) -> ParseResult<ast::Expression> {
  name(i).map(|(rest, state_elt)| (rest, ast::Expression::ReferenceToState(state_elt.to_string())))
}

fn int_literal(i: &str) -> ParseResult<ast::Expression> {
  let (input, const_int) = digit1(i)?;
  Ok((input, ast::Expression::IntLiteral(const_int.parse::<i64>().unwrap())))
}

fn string_literal(i: &str) -> ParseResult<ast::Expression> {
  let (input, (_, literal, _)) = tuple((char('"'), take_until("\""), char('"')))(i)?;
  Ok((input, ast::Expression::StringLiteral(literal.to_string())))
}

fn tuple_expression(i: &str) -> ParseResult<ast::Expression> {
  let (input, members) = delimited(char('('), separated_list1(tuple((multispace0, char(','), multispace0)), expression), char(')'))(i)?;
  Ok((input, ast::Expression::Tuple(members)))
}

fn function(i: &str) -> ParseResult<ast::Expression> {
  // TODO: multiple arguments
  let (input, (f_name, _, _, _, f_arg, _, _)) = tuple((name, multispace0, char('('), multispace0, expression, multispace0, char(')')))(i)?;
  Ok((
    input,
    ast::Expression::Function(f_name.to_string(), Box::new(f_arg))
  ))
}

fn expression(i: &str) -> ParseResult<ast::Expression> {
  let (input, expr) = alt((
    int_literal,
    string_literal, 
    tuple_expression,
    function, 
    state_reference
  ))(i)?;
  let mut result = expr;
  let mut remainder = input;
  while true {
    let test = delimited(char('['), expression, char(']'))(remainder);
    if let Ok((input, expr)) = test {
      result = ast::Expression::ArrayLookup(Box::new(result), Box::new(expr));
      remainder = input;
    } else {
      let test = preceded(char('.'), int_literal)(remainder);
      if let Ok((input, ast::Expression::IntLiteral(literal))) = test {
        result = ast::Expression::TupleLookup(Box::new(result), literal);
        remainder = input
      } else {
        break;
      }
    }
  }
  return Ok((remainder, result));
}

fn output_statement(i: &str) -> ParseResult<ast::Statement> {
  let (input, (output_name, _, _, _, expr, _, _)) 
    = cut(tuple((name, multispace0, tag("<-"), multispace0, expression, multispace0, char(';'))))(i)?;
  Ok((
    input,
    ast::Statement::Output(ast::OutputStatement { output: output_name.to_string(), expression: expr })
  ))
}

fn block_statement(i: &str) -> ParseResult<ast::Statement> {
  let (input, (_, _, (statements, _))) = tuple((
    char('{'),
    multispace0,
    many_till(terminated(statement, multispace0), char('}'))
  ))(i)?;
  Ok((input, ast::Statement::Block(statements)))
}

fn statement(i: &str) -> ParseResult<ast::Statement> {
  alt((block_statement, output_statement))(i)
}

fn listener(i: &str) -> ParseResult<ast::Listener> {
  let (input, (trigger, _, kind, _, (_, _, statement))) 
    = tuple((name, char('.'), kind_token, multispace0, 
        cut(tuple((char(':'), multispace0, statement)))))(i)?;
  Ok((
    input,
    ast::Listener {
      trigger: trigger.to_string(),
      kind,
      statement,
    }
  ))
}

fn listeners(i: &str) -> ParseResult<Vec<ast::Listener>> {
  separated_list0(multispace1, listener)(i)
}

fn module(i: &str) -> ParseResult<ast::Module> {
  let (input, (_, _, name, _, _, _, handles, _, listeners, _, _))
    = tuple((tag("module"), multispace1, uppercase_name, multispace0, char('{'), multispace0, handles, multispace0, listeners, multispace0, char('}')))(i)?;
  Ok((
    input,
    ast::Module {
      name: name.to_string(),
      handles,
      listeners,
      submodules: Vec::new(),
    }
  ))
}

fn graph(i: &str) -> ParseResult<ast::Graph> {
  let (input, names) = terminated(
    separated_list1(tuple((multispace0, tag("->"), multispace0)), uppercase_name), 
    tuple((multispace0, char(';')))
  )(i)?;
  Ok((
    input,
    ast::Graph { modules: names.iter().map(|s| s.to_string()).collect() }
  ))
}

fn graph_top_level(i: &str) -> ParseResult<ast::TopLevel> {
  let (input, graph) = graph(i)?;
  Ok((input, ast::TopLevel::Graph(graph)))
}

fn module_top_level(i: &str) -> ParseResult<ast::TopLevel> {
  let (input, module) = module(i)?;
  Ok((input, ast::TopLevel::Module(module)))
}

fn top_level(i: &str) -> ParseResult<ast::TopLevel> {
  alt((graph_top_level, module_top_level))(i)
}

// TODO: Make this private, and provide a public wrapper that is nicer
fn top_levels(i: &str) -> ParseResult<Vec<ast::TopLevel>> {
  separated_list0(multispace1, top_level)(i)
}

pub fn parse(i: &str) -> ParseResult<Vec<ast::TopLevel>> {
  let (input, (result, _)) = tuple((
    delimited(multispace0, top_levels, multispace0),
    eof
  ))(i)?;
  Ok((input, result))
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
    // assert_eq!(uppercase_name("hello"), mk_error("hello", ErrorKind::Verify));
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
    assert_eq!(type_primitive_token("Int"), Ok(("", ast::Type::Int)));
  }

  #[test]
  fn parse_function() {
    assert_eq!(
      function("foo(bar)"), 
      Ok(("", ast::Expression::Function("foo".to_string(), Box::new(ast::Expression::ReferenceToState("bar".to_string())))))
    );
  }

  #[test]
  fn parse_handle() {
    assert_eq!(
      handle("foo: reads writes Int;"),
      Ok((
        "",
        ast::Handle { name: String::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::Type::Int }
      ))
    );
    assert_eq!(
      handle("bar: writes String;"),
      Ok((
        "",
        ast::Handle { name: String::from("bar"), usages: vec!(ast::Usage::Write), h_type: ast::Type::String }
      ))
    );
    assert_eq!(
      handle("foo: reads writes Int; bar: writes String;"),
      Ok((
        " bar: writes String;",
        ast::Handle { name: String::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::Type::Int }
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
          ast::Handle { name: String::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::Type::Int },
          ast::Handle { name: String::from("bar"), usages: vec!(ast::Usage::Write), h_type: ast::Type::String },
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
        ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, statement: ast::Statement::Output (
          ast::OutputStatement {
            output: String::from("bar"), expression: ast::Expression::ReferenceToState(String:: from("far"))
          }
        )}
      ))
    )
  }

  #[test]
  fn parse_block_listener() {
    assert_eq!(
      listener("foo.onChange: {\n  bar <- far;\n  }"),
      Ok((
        "",
        ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, statement: ast::Statement::Block (
          vec!(ast::Statement::Output ( ast::OutputStatement {
            output: String::from("bar"), expression: ast::Expression::ReferenceToState(String:: from("far"))
          }))
        )}
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
          ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, statement: ast::Statement::Output (
            ast::OutputStatement {
              output: String::from("bar"), expression: ast::Expression::ReferenceToState(String:: from("far"))
            }
          )},
          ast::Listener { trigger: String::from("far"), kind: ast::ListenerKind::OnWrite, statement: ast::Statement::Output (
            ast::OutputStatement {
              output: String::from("bax"), expression: ast::Expression::ReferenceToState(String:: from("fax"))
            }
          )}
        )

      ))
    )
  }

  static TEST_MODULE_STRING : &str = 
"module TestModule {
  foo: reads writes Int;

  foo.onChange: bar <- far(la);
}";

  fn test_module_result() -> ast::Module {
    ast::Module {
      name: String::from("TestModule"),
      handles: vec!(ast::Handle { name: String::from("foo"), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::Type::Int }),
      listeners: vec!(ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, statement: ast::Statement::Output (
        ast::OutputStatement {
          output: String::from("bar"),
          expression: ast::Expression::Function("far".to_string(), Box::new(ast::Expression::ReferenceToState(String::from("la"))))
        }
      )}),
      submodules: Vec::new(),
    }
  }

  static TEST_GRAPH_STRING : &str = "MyModule -> MyModule2 -> AnotherModule;";
  
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