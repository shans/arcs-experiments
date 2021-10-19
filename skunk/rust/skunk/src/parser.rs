extern crate nom;

use super::ast;

use nom::{
  IResult,
  branch::alt,
  bytes::complete::{tag, is_a, take, take_until},
  character::complete::{alpha1, char, multispace0, multispace1, digit1}, 
  combinator::{verify, eof, cut, opt},
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
    token("Bool", ast::Type::Bool),
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
  name(i).map(|(rest, state_elt)| (rest, ast::Expression::unterminated(ast::ExpressionValue::ReferenceToState(state_elt.to_string()))))
}

fn int_literal(i: &str) -> ParseResult<ast::Expression> {
  let (input, const_int) = digit1(i)?;
  Ok((input, ast::Expression::unterminated(ast::ExpressionValue::IntLiteral(const_int.parse::<i64>().unwrap()))))
}

fn string_literal(i: &str) -> ParseResult<ast::Expression> {
  let (input, (_, literal, _)) = tuple((char('"'), cut(take_until("\"")), char('"')))(i)?;
  Ok((input, ast::Expression::unterminated(ast::ExpressionValue::StringLiteral(literal.to_string()))))
}

fn char_literal(i: &str) -> ParseResult<ast::Expression> {
  let (input, literal) = delimited(char('\''), cut(take(1usize)), char('\''))(i)?;
  Ok((input, ast::Expression::unterminated(ast::ExpressionValue::CharLiteral(literal.chars().nth(0).unwrap() as u8))))

}

fn tuple_expression(i: &str) -> ParseResult<ast::Expression> {
  let (input, mut members) = delimited(char('('), separated_list1(tuple((multispace0, char(','), multispace0)), expression(0)), char(')'))(i)?;
  Ok((input, ast::Expression::unterminated(ast::ExpressionValue::Tuple(members.drain(..).map(|a| a.value).collect()))))
}

fn function(i: &str) -> ParseResult<ast::Expression> {
  // TODO: multiple arguments
  let (input, (f_name, _, _, _, f_arg, _, _)) = tuple((name, multispace0, char('('), multispace0, expression(0), multispace0, char(')')))(i)?;
  Ok((
    input,
    ast::Expression::unterminated(ast::ExpressionValue::FunctionCall(f_name.to_string(), Box::new(f_arg.into())))
  ))
}

// Expression cases
// some places require a terminated expression (either a block or an expression with a trailing ';')
//  - listeners
//  - function bodies
//  - non-terminal expressions in blocks
//
//  - it's OK to terminate a block but not required. This is best viewed as a block expression followed by an empty (terminated) expression
//
// so a terminated expression is a block, or a non-block expression with a ';'
// a non-block expression is (everything except block), or a block with trailing components

fn operator(precedence: usize) -> impl Fn(&str) -> ParseResult<ast::Operator> {
  move |i: &str| verify(alt((
    token("==", ast::Operator::Equality),
    token("||", ast::Operator::LogicalOr),
    token("<", ast::Operator::LessThan),
    token(">", ast::Operator::GreaterThan)
  )), |op| op.precedence() > precedence)(i)
}

fn expression_modifier(i: &str, expr: ast::Expression, precedence: usize) -> ParseResult<ast::Expression> {
  let test = delimited(char('['), expression(0), char(']'))(i);
  if let Ok((input, value)) = test {
    Ok((input, ast::Expression::unterminated(ast::ExpressionValue::ArrayLookup(Box::new(expr.into()), Box::new(value.into())))))
  } else {
    let test = preceded(char('.'), int_literal)(i);
    if let Ok((input, ast::Expression { value: ast::ExpressionValue::IntLiteral(literal), is_terminated: _, precedence: _ })) = test {
      Ok((input, ast::Expression::unterminated(ast::ExpressionValue::TupleLookup(Box::new(expr.into()), literal))))
    } else {
      let op_test = preceded(multispace0, operator(precedence))(i)?;
      let exp_test = preceded(multispace0, expression(op_test.1.precedence()))(op_test.0)?;
      Ok((exp_test.0, ast::Expression::binary_operator(expr, op_test.1, exp_test.1)))   
    }
  }
}

fn consume_modifiers(i: &str, expr: ast::Expression, precedence: usize) -> ParseResult<ast::Expression> {
  let mut r = (i, expr);
  while true {
    // TODO: cloning here isn't really OK .. 
    let candidate = expression_modifier(r.0, r.1.clone(), precedence);
    if let Ok(result) = candidate {
      r = result;
    } else {
      break;
    }
  }
  Ok(r)
}

fn block_expression(i: &str) -> ParseResult<ast::Expression> {
  let (input, (_, _, mut expressions, _, unterm, _, _)) = tuple((
    char('{'),
    multispace0,
    many0(terminated(terminated_expression, multispace0)),
    multispace0,
    opt(unterminated_expression),
    multispace0,
    cut(char('}'))
  ))(i)?;
  if let Some(expr) = unterm {
    expressions.push(expr);
  } else {
    expressions.push(ast::Expression::unterminated(ast::ExpressionValue::Empty));
  }
  Ok((input, ast::Expression::terminated(ast::ExpressionValue::Block(expressions.drain(..).map(|a| a.value).collect()))))
}

fn expression(precedence: usize) -> impl Fn(&str) -> ParseResult<ast::Expression> {
  move |i: &str| {
    let r = alt((
      block_expression, // { ...
      let_expression, // let x = ...
      output_expression, // x <- ...
      output_return_expression, // x <!- ...
      if_expression, // if ...
      int_literal, // 0-9...
      string_literal,  // "...
      char_literal, // '...
      tuple_expression, // (...
      function, // known name set
      state_reference // known name set
    ))(i)?;
    consume_modifiers(r.0, r.1, precedence)
  }
}

fn terminated_expression(i: &str) -> ParseResult<ast::Expression> {
  let (input, expr) = expression(0)(i)?;
  if expr.is_terminated {
    Ok((input, expr))
  } else {
    let (input, _) = preceded(multispace0, char(';'))(input)?;
    Ok((input, ast::Expression::terminated(expr.value)))
  }
}

fn unterminated_expression(i: &str) -> ParseResult<ast::Expression> {
  verify(expression(0), |result| !result.is_terminated)(i)
}

fn output_expression(i: &str) -> ParseResult<ast::Expression> {
  let (input, (output_name, _, _, _, expr)) 
    = tuple((name, multispace0, tag("<-"), multispace0, expression(0)))(i)?;
  Ok((
    input,
    ast::Expression::unterminated(ast::ExpressionValue::Output(ast::OutputExpression { output: output_name.to_string(), expression: Box::new(expr.into()), and_return: false }))
  ))
}

fn output_return_expression(i: &str) -> ParseResult<ast::Expression> {
  let (input, (output_name, _, _, _, expr)) 
    = tuple((name, multispace0, tag("<!-"), multispace0, expression(0)))(i)?;
  Ok((
    input,
    ast::Expression::unterminated(ast::ExpressionValue::Output(ast::OutputExpression { output: output_name.to_string(), expression: Box::new(expr.into()), and_return: true }))
  ))
}

fn let_expression(i: &str) -> ParseResult<ast::Expression> {
  let (input, (_, _, var_name, _, _, _, expr))
    = tuple((tag("let"), multispace1, name, multispace0, char('='), multispace0, expression(0)))(i)?;
  Ok((
    input,
    ast::Expression::unterminated(ast::ExpressionValue::Let(ast::LetExpression { var_name: var_name.to_string(), expression: Box::new(expr.into()) }))
  ))
}

fn if_expression(i: &str) -> ParseResult<ast::Expression> {
  let (input, (_, _, test, _, if_true, else_clause)) = tuple((
    tag("if"), multispace0, expression(0), multispace0, block_expression, 
    opt(tuple((multispace0, tag("else"), multispace0, block_expression)))
  ))(i)?;
  if let Some((_, _, _, if_false)) = else_clause {
    Ok((input, ast::Expression::terminated(ast::ExpressionValue::If(ast::IfExpression { test: Box::new(test.into()), if_true: Box::new(if_true.into()), if_false: Box::new(if_false.into()) }))))
  } else {
    Ok((input, ast::Expression::terminated(ast::ExpressionValue::If(ast::IfExpression { test: Box::new(test.into()), if_true: Box::new(if_true.into()), if_false: Box::new(ast::ExpressionValue::Empty) }))))
  }
}

fn listener(i: &str) -> ParseResult<ast::Listener> {
  let (input, (trigger, _, kind, _, (_, _, expression))) 
    = tuple((name, char('.'), kind_token, multispace0, 
        cut(tuple((char(':'), multispace0, terminated_expression)))))(i)?;
  Ok((
    input,
    ast::Listener {
      trigger: trigger.to_string(),
      kind,
      implementation: expression.into(),
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
      Ok(("", ast::Expression::unterminated(ast::ExpressionValue::FunctionCall("foo".to_string(), Box::new(ast::ExpressionValue::ReferenceToState("bar".to_string()))))))
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
        ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, implementation: ast::ExpressionValue::Output (
          ast::OutputExpression {
            output: String::from("bar"), expression: Box::new(ast::ExpressionValue::ReferenceToState("far".to_string())), and_return: false
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
        ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, implementation: ast::ExpressionValue::Block(
          vec!(ast::ExpressionValue::Output ( ast::OutputExpression {
            output: String::from("bar"), expression: Box::new(ast::ExpressionValue::ReferenceToState("far".to_string())), and_return: false
          }), ast::ExpressionValue::Empty)
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
          ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, implementation: ast::ExpressionValue::Output (
            ast::OutputExpression {
              output: String::from("bar"), expression: Box::new(ast::ExpressionValue::ReferenceToState("far".to_string())), and_return: false
            }
          )},
          ast::Listener { trigger: String::from("far"), kind: ast::ListenerKind::OnWrite, implementation: ast::ExpressionValue::Output (
            ast::OutputExpression {
              output: String::from("bax"), expression: Box::new(ast::ExpressionValue::ReferenceToState("fax".to_string())), and_return: false
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
      listeners: vec!(ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, implementation: ast::ExpressionValue::Output (
        ast::OutputExpression {
          output: String::from("bar"),
          expression: Box::new(ast::ExpressionValue::FunctionCall("far".to_string(), Box::new(ast::ExpressionValue::ReferenceToState(String::from("la"))))),
          and_return: false
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
  
  #[test]
  fn parse_expression_test() {
    let test_str = "foo || bar == far > baz";
    dbg!(expression(0)(test_str));
    let test_str = "foo < boo || far > baz";
    dbg!(expression(0)(test_str));
    let test_str = "offset == size(input.0) || input.0[offset] < '0' || input.0[offset] > '9'";
    dbg!(expression(0)(test_str));
  }
}