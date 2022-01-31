extern crate nom;
extern crate nom_locate;

use super::ast;

use nom::{
  IResult,
  branch::alt,
  bytes::complete::{tag, is_a, take, take_until},
  character::complete::{char, multispace0, multispace1, digit1}, 
  combinator::{verify, eof, cut, opt},
  error::{VerboseError},
  multi::{separated_list0, separated_list1, many0},
  sequence::{tuple, delimited, terminated, preceded},
};

use nom_locate::{position, LocatedSpan};

type Span<'a> = LocatedSpan<&'a str>;

use ast::Safe;

#[inline]
pub fn is_upper_alphabetic(chr: char) -> bool {
  chr >= 'A' && chr <= 'Z'
}

#[inline]
pub fn is_lower_alphabetic(chr: char) -> bool {
  chr >= 'a' && chr <= 'z'
}

static ALLOWED_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";

type ParseResult<'a, T> = IResult<Span<'a>,T, VerboseError<Span<'a>>>;

fn uppercase_name(i: Span) -> ParseResult<Span> {
  verify(is_a(ALLOWED_CHARS), |s: &Span| s.len() > 0 && is_upper_alphabetic(s.chars().nth(0).unwrap()))(i)
}

fn name(i: Span) -> ParseResult<Span> {
  verify(is_a(ALLOWED_CHARS), |s: &Span| s.len() > 0 && is_lower_alphabetic(s.chars().nth(0).unwrap()))(i)
}

fn token<T: Clone>(text: &'static str, result: T) -> impl Fn(Span) -> ParseResult<T> {
  move |i: Span| {
    let (input, _) = tag(text)(i)?;
    Ok((input, result.clone()))
  }
}

fn usage_token(i: Span) -> ParseResult<ast::Usage> {
  let reads = token("reads", ast::Usage::Read);
  let writes = token("writes", ast::Usage::Write);
  alt((reads, writes))(i)
}

fn usages(i: Span) -> ParseResult<Vec<ast::Usage>> {
  cut(separated_list1(multispace1, usage_token))(i)
}

fn type_primitive_token(i: Span) -> ParseResult<ast::Type> {
  alt((
    token("Int", ast::Type::Int),
    token("String", ast::Type::String),
    token("MemRegion", ast::Type::MemRegion),
    token("Char", ast::Type::Char),
    token("Bool", ast::Type::Bool),
  ))(i)
}

fn tuple_type(i: Span) -> ParseResult<ast::Type> {
  let (input, members) = delimited(char('('), separated_list1(tuple((multispace0, char(','), multispace0)), handle_type), char(')'))(i)?;
  Ok((input, ast::Type::Tuple(members)))
}

fn named_type(i: Span) -> ParseResult<ast::Type> {
  let (i, uname) = uppercase_name(i)?;
  Ok((i, ast::Type::TypeName(uname.to_string())))
}

fn handle_type(i: Span) -> ParseResult<ast::Type> {
  cut(alt((type_primitive_token, tuple_type, named_type)))(i)
}

fn handle(i: Span) -> ParseResult<ast::Handle> {
  let (i, position) = position(i)?;
  let (i, (h_name, _, _, _, h_usages, _, h_type, _, _))
    = tuple((name, multispace0, char(':'), multispace0, usages, multispace1, handle_type, multispace0, char(';')))(i)?;
  Ok((
    i, 
    ast::Handle {
      position: position.safe(),
      name: h_name.fragment().to_string(),
      usages: h_usages.clone(),
      h_type
    }
  ))
}

fn handles(i: Span) -> ParseResult<Vec<ast::Handle>> {
  separated_list0(multispace1, handle)(i)
}

fn kind_token(i: Span) -> ParseResult<ast::ListenerKind> {
  alt((
    token("onChange", ast::ListenerKind::OnChange),
    token("onWrite", ast::ListenerKind::OnWrite),
  ))(i)
}

fn state_reference(i: Span) -> ParseResult<ast::Expression> {
  let (i, position) = position(i)?;
  name(i).map(|(rest, state_elt)| (rest, ast::Expression::state_reference(position.safe(), state_elt.fragment())))
}

fn int_literal(i: Span) -> ParseResult<ast::Expression> {
  let (i, position) = position(i)?;
  let (i, const_int) = digit1(i)?;
  Ok((i, ast::Expression::int_literal(position.safe(), const_int.parse::<i64>().unwrap())))
}

fn string_literal(i: Span) -> ParseResult<ast::Expression> {
  let (i, position) = position(i)?;
  let (i, (_, literal, _)) = tuple((char('"'), cut(take_until("\"")), char('"')))(i)?;
  Ok((i, ast::Expression::string_literal(position.safe(), literal.fragment())))
}

fn char_literal(i: Span) -> ParseResult<ast::Expression> {
  let (i, position) = position(i)?;
  let (i, literal) = delimited(char('\''), cut(take(1usize)), char('\''))(i)?;
  Ok((i, ast::Expression::char_literal(position.safe(), literal.chars().nth(0).unwrap() as u8)))

}

fn tuple_expression(i: Span) -> ParseResult<ast::Expression> {
  let (i, position) = position(i)?;
  let (i, members) = delimited(char('('), separated_list1(tuple((multispace0, char(','), multispace0)), expression(0)), char(')'))(i)?;
  Ok((i, ast::Expression::tuple(position.safe(), members)))
}

fn function(i: Span) -> ParseResult<ast::Expression> {
  // TODO: multiple arguments
  let (i, position) = position(i)?;
  let (i, (f_name, _, _, _, f_arg, _, _)) = tuple((name, multispace0, char('('), multispace0, expression(0), multispace0, char(')')))(i)?;
  Ok((i, ast::Expression::function_call(position.safe(), f_name.fragment(), f_arg)))
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

fn operator(precedence: usize) -> impl Fn(Span) -> ParseResult<ast::Operator> {
  move |i: Span| verify(alt((
    token("==", ast::Operator::Equality),
    token("!=", ast::Operator::Inequality),
    token("||", ast::Operator::LogicalOr),
    token("&&", ast::Operator::LogicalAnd),
    token("<=", ast::Operator::LessThanOrEqual),
    token("<", ast::Operator::LessThan),
    token(">=", ast::Operator::GreaterThanOrEqual),
    token(">", ast::Operator::GreaterThan),
    token("*", ast::Operator::Multiply),
    token("/", ast::Operator::Divide),
    token("+", ast::Operator::Add),
    token("-", ast::Operator::Subtract),
  )), |op| op.precedence() > precedence)(i)
}

fn expression_modifier<'a>(i: Span<'a>, expr: ast::Expression, precedence: usize) -> ParseResult<'a, ast::Expression> {
  let test = delimited(char('['), expression(0), char(']'))(i);
  let (i, _) = multispace0(i)?;
  let (i, position) = position(i)?;
  if let Ok((i, value)) = test {
    Ok((i, ast::Expression::array_lookup(position.safe(), expr, value)))
  } else {
    let test = preceded(char('.'), int_literal)(i);
    if let Ok((i, ast::Expression {
      value: ast::ExpressionValue {
        info: ast::ExpressionValueEnum::IntLiteral(literal),
        position: _
      }, is_terminated: _, precedence: _ })) = test {
      Ok((i, ast::Expression::tuple_lookup(position.safe(), expr, literal)))
    } else {
      let op_test = operator(precedence)(i)?;
      let exp_test = preceded(multispace0, expression(op_test.1.precedence()))(op_test.0)?;
      Ok((exp_test.0, ast::Expression::binary_operator(position.safe(), expr, op_test.1, exp_test.1)))   
    }
  }
}

fn consume_modifiers<'a>(i: Span<'a>, expr: ast::Expression, precedence: usize) -> ParseResult<'a, ast::Expression> {
  let mut r = (i, expr);
  loop {
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

fn block_expression(i: Span) -> ParseResult<ast::Expression> {
  let (i, block_position) = position(i)?;
  let (i, (_, _, mut expressions, _, unterm, _, _)) = tuple((
    char('{'),
    multispace0,
    many0(terminated(terminated_expression, multispace0)),
    multispace0,
    opt(unterminated_expression),
    multispace0,
    cut(char('}'))
  ))(i)?;
  let i = if let Some(expr) = unterm {
      expressions.push(expr);
      i
    } else {
      let (i, position) = position(i)?;
      expressions.push(ast::Expression::empty(position.safe()));
      i
    };
  Ok((i, ast::Expression::block(block_position.safe(), expressions)))
}

fn expression(precedence: usize) -> impl Fn(Span) -> ParseResult<ast::Expression> {
  move |i: Span| {
    let r = alt((
      block_expression, // { ...
      let_expression, // let x = ...
      update_expression, // x = ...
      output_expression, // x <- ...
      output_return_expression, // x <!- ...
      if_expression, // if ...
      while_expression, // while ...
      break_expression, // break
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

fn break_expression(i: Span) -> ParseResult<ast::Expression> {
  let (i, position) = position(i)?;
  let (i, _) = tag("break")(i)?;
  Ok((i, ast::Expression::break_expression(position.safe())))
}

fn terminated_expression(i: Span) -> ParseResult<ast::Expression> {
  let (input, expr) = expression(0)(i)?;
  if expr.is_terminated {
    Ok((input, expr))
  } else {
    let (input, _) = preceded(multispace0, char(';'))(input)?;
    Ok((input, ast::Expression::terminated(expr.value)))
  }
}

fn unterminated_expression(i: Span) -> ParseResult<ast::Expression> {
  verify(expression(0), |result| !result.is_terminated)(i)
}

fn output_expression(i: Span) -> ParseResult<ast::Expression> {
  let (i, position) = position(i)?;
  let (i, (output_name, _, _, _, expr)) 
    = tuple((name, multispace0, tag("<-"), multispace0, expression(0)))(i)?;
  Ok((i, ast::Expression::output(position.safe(), output_name.fragment(), expr, false)))
}

fn output_return_expression(i: Span) -> ParseResult<ast::Expression> {
  let (i, position) = position(i)?;
  let (i, (output_name, _, _, _, expr)) 
    = tuple((name, multispace0, tag("<!-"), multispace0, expression(0)))(i)?;
  Ok((i, ast::Expression::output(position.safe(), output_name.fragment(), expr, true)))
}

fn let_expression(i: Span) -> ParseResult<ast::Expression> {
  let (i, position) = position(i)?;
  let (i, (_, _, var_name, _, _, _, expr))
    = tuple((tag("let"), multispace1, name, multispace0, char('='), multispace0, expression(0)))(i)?;
  Ok((i, ast::Expression::let_expression(position.safe(), var_name.fragment(), expr, false)))
}

fn update_expression(i: Span) -> ParseResult<ast::Expression> {
  let (i, position) = position(i)?;
  let (i, (var_name, _, _, _, expr))
    = tuple((name, multispace0, char('='), multispace0, expression(0)))(i)?;
  Ok((i, ast::Expression::let_expression(position.safe(), var_name.fragment(), expr, true)))
}

fn if_expression(i: Span) -> ParseResult<ast::Expression> {
  let (i, position) = position(i)?;
  let (i, (_, _, test, _, if_true, else_clause)) = tuple((
    tag("if"), multispace1, expression(0), multispace0, block_expression, 
    opt(tuple((multispace0, tag("else"), multispace0, block_expression)))
  ))(i)?;
  if let Some((_, _, _, if_false)) = else_clause {
    Ok((i, ast::Expression::if_expression(position.safe(), test, if_true, if_false)))
  } else {
    Ok((i, ast::Expression::if_expression(position.safe(), test, if_true, ast::Expression::empty(position.safe()))))
  }
}

fn while_expression(i: Span) -> ParseResult<ast::Expression> {
  let (i, position) = position(i)?;
  let (i, (_, _, expr, _, block)) = tuple((
    tag("while"), multispace1, cut(expression(0)), multispace0, cut(block_expression)
  ))(i)?;
  Ok((i, ast::Expression::while_expression(position.safe(), expr, block)))
}

fn listener(i: Span) -> ParseResult<ast::Listener> {
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

fn listeners(i: Span) -> ParseResult<Vec<ast::Listener>> {
  separated_list0(multispace1, listener)(i)
}

fn example_value(i: Span) -> ParseResult<(String, ast::ExampleInfo)> {
  let (i, (is_update, name, expr)) = tuple((opt(char('!')), name, preceded(tuple((multispace0, char(':'), multispace0)), expression(0))))(i)?;
  Ok((i, (name.fragment().to_string(), ast::ExampleInfo { value: expr, is_update: is_update.is_some() })))
}

fn example_values(i: Span) -> ParseResult<Vec<(String, ast::ExampleInfo)>> {
  separated_list0(tuple((multispace0, char(','), multispace0)), example_value)(i)
}

fn example(i: Span) -> ParseResult<ast::Example> {
  let (i, (mut inputs, mut expected)) = tuple((
    example_values, preceded(tuple((multispace0, tag("->"), multispace0)), example_values)
  ))(i)?;
  Ok((i, ast::Example { inputs: inputs.drain(..).collect(), expected: expected.drain(..).collect() }))
}

fn examples(i: Span) -> ParseResult<ast::Examples> {
  let (i, _) = tag("examples")(i)?;
  let (i, examples) = cut(
    preceded(multispace0, delimited(
      terminated(char('{'), multispace0),
      separated_list0(multispace0, terminated(example, tuple((multispace0, char(';'))))),
      preceded(multispace0, char('}'))
    ))
  )(i)?;
  Ok((i, ast::Examples { examples }))
}

fn module_param(i: Span) -> ParseResult<ast::ValueParam> {
  let (i, (name, vp_type)) = tuple((delimited(multispace0, name, multispace0), delimited(terminated(char(':'), multispace0), handle_type, multispace0)))(i)?;
  Ok((i, ast::ValueParam { name: name.fragment().to_string(), vp_type }))
}

fn module_params(i: Span) -> ParseResult<Vec<ast::ValueParam>> {
  delimited(char('<'), separated_list1(tuple((multispace0, char(','), multispace0)), module_param), char('>'))(i)
}

fn module(i: Span) -> ParseResult<ast::Module> {
  let (input, (_, name, params, _, handles, listeners, graphs, examples, _))
    = tuple((tag("module"),
            delimited(multispace1, uppercase_name, multispace0), 
            opt(terminated(module_params, multispace0)),
            char('{'),
            preceded(multispace0, handles),
            preceded(multispace0, listeners),
            preceded(multispace0, module_graphs),
            delimited(multispace0, opt(examples), multispace0),
            char('}')
          ))(i)?;
  let examples = match examples {
    None => ast::Examples { examples: Vec::new() },
    Some(e) => e
  };
  let params = match params {
    None => Vec::new(),
    Some(p) => p
  };
  Ok((
    input,
    ast::Module::create(name.fragment(), handles, listeners, Vec::new(), examples, params, graphs)
  ))
}

fn module_graphs(i: Span) -> ParseResult<Vec<ast::GraphDirective>> {
  separated_list0(multispace1, graph)(i)
}

fn graph_module_specifier(i: Span) -> ParseResult<ast::GraphModuleInfo> {
  let (i, (local_name, module_name, params)) =
  tuple((
    opt(delimited(char('$'), name, tuple((multispace0, char(':'), multispace0)))),
    uppercase_name,
    opt(delimited(
      tuple((char('<'), multispace0)),
      separated_list1(tuple((multispace0, char(','), multispace0)), expression(0)),
      tuple((multispace0, char('>')))
    ))
  ))(i)?;
  let params = params.unwrap_or(vec!());
  Ok((i, ast::GraphModuleInfo::module(module_name.fragment(), local_name.map(|name| *name.fragment()), params)))
}

fn graph_module_tuple(i: Span) -> ParseResult<ast::GraphModuleInfo> {
  let (i, gmi_list) = delimited(
    tuple((char('('), multispace0)),
    separated_list1(tuple((multispace0, char(','), multispace0)), graph_module_info),
    tuple((multispace0, char(')')))
  )(i)?;
  Ok((i, ast::GraphModuleInfo::Tuple(gmi_list)))
}

fn graph_name_specifier(i: Span) -> ParseResult<ast::GraphModuleInfo> {
  let (i, name) = preceded(char('$'), name)(i)?;
  Ok((i, ast::GraphModuleInfo::module_name(name.fragment())))
}

fn field_specifier(i: Span) -> ParseResult<ast::GraphModuleInfo> {
  let (i, name) = name(i)?;
  Ok((i, ast::GraphModuleInfo::Field(ast::ModuleSpecifier::This, name.to_string())))
}

fn graph_module_info(i: Span) -> ParseResult<ast::GraphModuleInfo> {
  alt((graph_module_tuple, graph_module_specifier, graph_name_specifier, field_specifier))(i)
}

fn graph(i: Span) -> ParseResult<ast::GraphDirective> {
  let (input, names) = terminated(
    separated_list1(tuple((multispace0, tag("->"), multispace0)), graph_module_info), 
    tuple((multispace0, char(';')))
  )(i)?;
  Ok((
    input,
    ast::GraphDirective::Chain(names)
  ))
}

fn use_component_simple(i: Span) -> ParseResult<ast::Use> {
  let (i, name) = alt((name, uppercase_name))(i)?;
  Ok((i, ast::Use { name: name.to_string(), children: vec!() }))
}

fn use_component_compound(i: Span) -> ParseResult<ast::Use> {
  let (i, (mut component, children)) = tuple((
    terminated(use_component_simple, tag("::")),
    delimited(
      tuple((char('{'), multispace0)), 
      separated_list1(tuple((multispace0, char(','), multispace0)), use_component_recursive),
      tuple((multispace0, char('}')))
    ))
  )(i)?;
  component.children = children;
  Ok((i, component))
}

fn use_component(i: Span) -> ParseResult<ast::Use> {
  alt((use_component_compound, use_component_simple))(i)
}

fn use_component_recursive(i: Span) -> ParseResult<ast::Use> {
  let (i, (mut component, rest)) = tuple((
    use_component,
    opt(preceded(tag("::"), use_component_recursive))
  ))(i)?;
  match rest {
    None => Ok((i, component)),
    Some (sub_use) => {
      if component.children.len() > 0 {
        panic!("Shouldn't be able to do 'uses foo::{}::far'. Ideally this would be a nom error.", "{bar}");
      }
      component.children = vec!(sub_use);
      Ok((i, component))
    }
  }
}

fn use_statement(i: Span) -> ParseResult<ast::Use> {
  delimited(
    tuple((tag("uses"), multispace1)), 
    use_component_recursive, 
    tuple((multispace0, char(';')))
  )(i)
}

fn newtype(i: Span) -> ParseResult<ast::NewType> {
  let (i, (name, nt_type, _)) = tuple((
    preceded(tuple((tag("type"), multispace1)), uppercase_name),
    preceded(tuple((multispace0, char('='), multispace0)), handle_type),
    preceded(multispace0, char(';'))
  ))(i)?;
  Ok((i, ast::NewType { name: name.to_string(), nt_type }))
}

fn graph_top_level(i: Span) -> ParseResult<ast::TopLevel> {
  let (input, graph) = graph(i)?;
  Ok((input, ast::TopLevel::Graph(graph)))
}

fn module_top_level(i: Span) -> ParseResult<ast::TopLevel> {
  let (input, module) = module(i)?;
  Ok((input, ast::TopLevel::Module(module)))
}

fn use_top_level(i: Span) -> ParseResult<ast::TopLevel> {
  let (input, use_statement) = use_statement(i)?;
  Ok((input, ast::TopLevel::Use(use_statement)))
}

fn newtype_top_level(i: Span) -> ParseResult<ast::TopLevel> {
  let (i, newtype) = newtype(i)?;
  Ok((i, ast::TopLevel::NewType(newtype)))
}

fn top_level(i: Span) -> ParseResult<ast::TopLevel> {
  alt((graph_top_level, module_top_level, use_top_level, newtype_top_level))(i)
}

// TODO: Make this private, and provide a public wrapper that is nicer
fn top_levels(i: Span) -> ParseResult<Vec<ast::TopLevel>> {
  separated_list0(multispace1, top_level)(i)
}

pub fn parse<'a>(i: &'a str) -> ParseResult<Vec<ast::TopLevel>> {
  let (input, (result, _)) = tuple((
    delimited(multispace0, top_levels, multispace0),
    eof
  ))(Span::new(i))?;
  Ok((input, result))
}

#[cfg(test)]
pub mod tests {
  pub mod expression_builder;

  use super::*;
  use expression_builder::ExpressionBuilder;

  #[test]
  fn parse_uppercase_names() {
    assert_eq!(uppercase_name(Span::new("Hello")).unwrap().1.fragment(), &"Hello");
    assert_eq!(uppercase_name(Span::new("Hello2")).unwrap().1.fragment(), &"Hello2");
    assert_eq!(uppercase_name(Span::new("TesT other")).unwrap().1.fragment(), &"TesT");
    assert_eq!(uppercase_name(Span::new("NAmE!other")).unwrap().1.fragment(), &"NAmE");
  }

  #[test]
  fn parse_usage_token() {
    assert_eq!(usage_token(Span::new("reads")).unwrap().1, ast::Usage::Read);
  }

  #[test]
  fn parse_usages() {
    assert_eq!(usages(Span::new("reads writes")).unwrap().1, vec!(ast::Usage::Read, ast::Usage::Write));
  }

  #[test]
  fn parse_type_primitive() {
    assert_eq!(type_primitive_token(Span::new("Int")).unwrap().1, ast::Type::Int);
  }

  #[test]
  fn parse_function() {
    assert_eq!(
      function(Span::new("foo(bar)")).unwrap().1, 
      ast::Expression::unterminated(ExpressionBuilder::fun(0, 0, "foo", ExpressionBuilder::sref(4, 0, "bar")).build())
    );
  }

  #[test]
  fn parse_handle() {
    assert_eq!(
      handle(Span::new("foo: reads writes Int;")).unwrap().1,
      ast::Handle { position: ast::SafeSpan{ offset: 0, line: 1 }, name: "foo".to_string(), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::Type::Int }
    );
    assert_eq!(
      handle(Span::new("bar: writes String;")).unwrap().1,
      ast::Handle { position: ast::SafeSpan{ offset: 0, line: 1 }, name: "bar".to_string(), usages: vec!(ast::Usage::Write), h_type: ast::Type::String }
    );
    assert_eq!(
      handle(Span::new("foo: reads writes Int; bar: writes String;")).unwrap().1,
      ast::Handle { position: ast::SafeSpan{ offset: 0, line: 1 }, name: "foo".to_string(), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::Type::Int }
    )
  }

  #[test]
  fn parse_handles() {
    assert_eq!(
      handles(Span::new("foo: reads writes Int;
                         bar: writes String;")).unwrap().1,
      vec!(
        ast::Handle { position: ast::SafeSpan{ offset: 0, line: 1 }, name: "foo".to_string(), usages: vec!(ast::Usage::Read, ast::Usage::Write), h_type: ast::Type::Int },
        ast::Handle { position: ast::SafeSpan{ offset: 48, line: 2 }, name: "bar".to_string(), usages: vec!(ast::Usage::Write), h_type: ast::Type::String },
      )
    )
  }

  #[test]
  fn parse_kind_token() {
    assert_eq!(kind_token(Span::new("onChange")).unwrap().1, ast::ListenerKind::OnChange);
    assert_eq!(kind_token(Span::new("onWrite")).unwrap().1, ast::ListenerKind::OnWrite);
  }

  #[test]
  fn parse_listener() {
    assert_eq!(
      listener(Span::new("foo.onChange: bar <- far;")).unwrap().1,
      ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, implementation: 
        ExpressionBuilder::output(14, 0, "bar", ExpressionBuilder::sref(7, 0, "far")).build()
      }
    )
  }

  #[test]
  fn parse_block_listener() {
    assert_eq!(
      listener(Span::new("foo.onChange: {\n  bar <- far;\n  }")).unwrap().1,

      ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, implementation: 
        ExpressionBuilder::block(14, 0, vec!(ExpressionBuilder::output(4, 1, "bar", ExpressionBuilder::sref(7, 0, "far")), ExpressionBuilder::empty(19, 2))).build()
      }
    )
  }


  #[test]
  fn parse_listeners() {
    assert_eq!(
      listeners(Span::new("foo.onChange: bar <- far;
                           far.onWrite: bax <- fax;")).unwrap().1,
      vec!(
        ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, implementation: 
          ExpressionBuilder::output(14, 0, "bar", ExpressionBuilder::sref(7, 0, "far")).build()
        },
        ast::Listener { trigger: String::from("far"), kind: ast::ListenerKind::OnWrite, implementation:
          ExpressionBuilder::output(66, 1, "bax", ExpressionBuilder::sref(7, 0, "fax")).build()
        }
      )
    )
  }

  static TEST_MODULE_STRING : &str = 
"module TestModule {
  foo: reads writes Int;

  foo.onChange: bar <- far(la);
}";

  fn test_module_result<'a>(offset: usize, line: u32) -> ast::Module {
    ast::Module::create(
      "TestModule", 
      vec!(
        ast::Handle {
          position: ast::SafeSpan { offset: 22 + offset, line: 1 + line },
          name: "foo".to_string(), 
          usages: vec!(ast::Usage::Read, ast::Usage::Write), 
          h_type: ast::Type::Int 
        }
      ),
      vec!(ast::Listener { trigger: String::from("foo"), kind: ast::ListenerKind::OnChange, implementation:
        ExpressionBuilder::output(offset + 62, line + 2, "bar", ExpressionBuilder::fun(7, 0, "far", ExpressionBuilder::sref(4, 0, "la"))).build()
      }), 
      Vec::new(),
      ast::Examples { examples: Vec::new() },
      Vec::new(),
      Vec::new(),
    )
  }

  static TEST_GRAPH_STRING : &str = "MyModule -> MyModule2 -> AnotherModule;";
 
  fn gmi(name: &str) -> ast::GraphModuleInfo {
    ast::GraphModuleInfo::module(name, None, vec!())
  }

  fn test_graph_result() -> ast::GraphDirective {
    ast::GraphDirective::Chain(vec!(gmi("MyModule"), gmi("MyModule2"), gmi("AnotherModule")))
  }

  #[test]
  fn parse_module() {
    assert_eq!(
      module(Span::new(TEST_MODULE_STRING)).unwrap().1,
      test_module_result(0, 1)
    )
  }

  #[test]
  fn parse_graph() {
    assert_eq!(
      graph(Span::new(TEST_GRAPH_STRING)).unwrap().1,
      test_graph_result()
    )
  }

  #[test]
  fn parse_top_level() {
    assert_eq!(
      top_level(Span::new(TEST_MODULE_STRING)).unwrap().1,
      ast::TopLevel::Module(test_module_result(0, 1))    
    );
    assert_eq!(
      top_level(Span::new(TEST_GRAPH_STRING)).unwrap().1,
      ast::TopLevel::Graph(test_graph_result())   
    );
  }

  #[test]
  fn parse_top_levels() {
    let test_str = TEST_MODULE_STRING.to_string() + "\n\n" + TEST_GRAPH_STRING;
    assert_eq!(
      top_levels(Span::new(&test_str)).unwrap().1,
      vec!(ast::TopLevel::Module(test_module_result(0, 1)), ast::TopLevel::Graph(test_graph_result()))
    )
  }
  
  #[test]
  fn parse_simple_use_statement() {
    let test_str = "uses simple;";
    assert_eq!(
      use_statement(Span::new(&test_str)).unwrap().1,
      ast::Use { name: "simple".to_string(), children: vec!() }
    )
  }

  #[test]
  fn parse_compound_use_satement() {
    let test_str = "uses simple::Thing;";
    assert_eq!(
      use_statement(Span::new(&test_str)).unwrap().1,
      ast::Use { name: "simple".to_string(), children: vec!(ast::Use { name: "Thing".to_string(), children: vec!() }) }
    )
  }

  #[test]
  fn parse_graph_tuple() {
    let test_str = "(Module1, Module2)";
    assert_eq!(
      graph_module_tuple(Span::new(&test_str)).unwrap().1,
      ast::GraphModuleInfo::Tuple(vec!(
        ast::GraphModuleInfo::module("Module1", None, vec!()),
        ast::GraphModuleInfo::module("Module2", None, vec!()),
      ))
    );
  }

  #[test]
  fn parse_expression_test() {
    assert_eq!(
      expression(0)(Span::new("foo || bar")).unwrap().1.value,
      ExpressionBuilder::sref(0, 0, "foo").op(4, 0, ast::Operator::LogicalOr, ExpressionBuilder::sref(3, 0, "bar")).build()
    );
    assert_eq!(
      expression(0)(Span::new("foo || bar == far > baz")).unwrap().1.value,
      ExpressionBuilder::sref(0, 0, "foo").op(4, 0, ast::Operator::LogicalOr, 
        ExpressionBuilder::sref(3, 0, "bar").op(4, 0, ast::Operator::Equality,
          ExpressionBuilder::sref(3, 0, "far")
        ).op(7, 0, ast::Operator::GreaterThan, 
          ExpressionBuilder::sref(2, 0, "baz")
        )
      ).build()
    );
    assert_eq!(
      expression(0)(Span::new("foo < boo || far > baz")).unwrap().1.value,
      ExpressionBuilder::sref(0, 0, "foo").op(4, 0, ast::Operator::LessThan,
        ExpressionBuilder::sref(2, 0, "boo")
      ).op(6, 0, ast::Operator::LogicalOr,
        ExpressionBuilder::sref(3, 0, "far").op(4, 0, ast::Operator::GreaterThan,
          ExpressionBuilder::sref(2, 0, "baz")
        )
      ).build()
    );
    assert_eq!(
      expression(0)(Span::new("offset == size(input.0) || input.0[offset] < '0' || input.0[offset] > '9'")).unwrap().1.value,
      ExpressionBuilder::sref(0, 0, "offset").op(7, 0, ast::Operator::Equality,
        ExpressionBuilder::fun(3, 0, "size", ExpressionBuilder::sref(5, 0, "input").tuple_ref(5, 0, 0))
      ).op(17, 0, ast::Operator::LogicalOr, 
        ExpressionBuilder::sref(3, 0, "input").tuple_ref(5, 0, 0).array_index(2, 0, ExpressionBuilder::sref(1, 0, "offset")).op(9, 0, ast::Operator::LessThan,
          ExpressionBuilder::char(2, 0, '0')
        )
      ).op(25, 0, ast::Operator::LogicalOr,
        ExpressionBuilder::sref(3, 0, "input").tuple_ref(5, 0, 0).array_index(2, 0, ExpressionBuilder::sref(1, 0, "offset")).op(9, 0, ast::Operator::GreaterThan,
          ExpressionBuilder::char(2, 0, '9')
        )
      ).build()
    );
  }
}