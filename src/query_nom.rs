use nom::{self, {types::CompleteStr as NS}, IResult};

use lex::Lex;
use json::{self, Json};
use query::{Thunk};

named!(nom_dot(NS) -> NS, ws!(tag!(".")));
named!(nom_dotdot(NS) -> NS, ws!(tag!("..")));
named!(nom_colon(NS) -> NS, ws!(tag!(":")));
named!(nom_comma(NS) -> NS, ws!(tag!(",")));
named!(nom_equal(NS) -> NS, ws!(tag!("=")));
named!(nom_open_sqr(NS) -> NS, ws!(tag!("[")));
named!(nom_clos_sqr(NS) -> NS, ws!(tag!("]")));
named!(nom_open_brace(NS) -> NS, ws!(tag!("{")));
named!(nom_clos_brace(NS) -> NS, ws!(tag!("}")));
named!(nom_open_paran(NS) -> NS, ws!(tag!("(")));
named!(nom_clos_paran(NS) -> NS, ws!(tag!(")")));
named!(nom_opt(NS) -> Option<NS>, opt!(ws!(tag!("?"))));
named!(nom_identifier(NS) -> NS, ws!(re_find!(r"^[A-Za-z_][0-9A-Za-z_]*")));

named!(nom_null(NS) -> NS, ws!(tag!("null")));
named!(nom_true(NS) -> NS, ws!(tag!("true")));
named!(nom_false(NS) -> NS, ws!(tag!("false")));
named!(nom_isize(NS) -> isize,
    flat_map!(ws!(re_find!(r#"^[+-]?\d+"#)), parse_to!(isize))
);
// TODO: number syntax have conflict with `dot` syntax, for example
// number should start with 0.1 instead of .1
named!(nom_number(NS) -> NS,
    ws!(re_find!(r#"^[-+]?[0-9]+\.?[0-9]*([eE][-+]?[0-9]+)?"#))
);
fn nom_json_string(text: NS) -> nom::IResult<NS, String> {
    check_next_byte(text, b'"')?;
    let mut lex = Lex::new(0, 1, 1);
    match json::parse_string(&text, &mut lex) {
        Ok(Json::String(s)) => Ok((NS(&text[lex.off..]), s)),
        _ => {
            let kind = nom::ErrorKind::Custom(lex.off as u32);
            return Err(nom::Err::Error(nom::Context::Code(text, kind)));
        }
    }
}
//#[allow(dead_code)] // TODO: Can be removed.
//fn nom_json_array(text: NS) -> nom::IResult<NS, query::Scalar> {
//    check_next_byte(text, b'[')?;
//    //println!("array {}", &text);
//    let mut lex = Lex::new(0, 1, 1);
//    match json::parse_array(&text, &mut lex) {
//        Ok(a) => Ok((NS(&text[lex.off..]), From::from(a))),
//        _ => {
//            let kind = nom::ErrorKind::Custom(lex.off as u32);
//            return Err(nom::Err::Error(nom::Context::Code(text, kind)));
//        }
//    }
//}
//#[allow(dead_code)] // TODO: Can be removed.
//fn nom_json_object(text: NS) -> nom::IResult<NS, Json> {
//    check_next_byte(text, b'{')?;
//    let mut lex = Lex::new(0, 1, 1);
//    match json::parse_object(&text, &mut lex) {
//        Ok(o) => Ok((NS(&text[lex.off..]), o)),
//        _ => {
//            let kind = nom::ErrorKind::Custom(lex.off as u32);
//            return Err(nom::Err::Error(nom::Context::Code(text, kind)));
//        }
//    }
//}
//

//---- Expression Operations, in PEG grammar
//
//    Expr       <- Or PipeExpr*
//    PipeExpr   <- ('|') Or
//    Or         <- And OrExpr*
//    OrExpr     <- ('||') And
//    And        <- Compar AndExpr*
//    AndExpr    <- ('&&') Compar
//    Compar     <- BitOr ComparExpr*
//    ComparExpr <- ('==' | '!=' | '<' | '<=' | '>' | '>=' ) BitOr
//    BitOr      <- BitXor BitOrExpr*
//    BitOrExpr  <- ('bor') BitXor
//    BitXor     <- BitAnd BitXorExpr*
//    BitXorExpr <- ('^') BitAnd
//    BitAnd     <- Shift BitAndExpr*
//    BitAndExpr <- ('&') Shift
//    Shift      <- Add ShiftExpr*
//    ShiftExpr  <- ('<<' | '>>') Add
//    Add        <- Mult AddExpr*
//    AddExpr    <- ('+' | '-') Mult
//    Mult       <- Primary MulExpr*
//    MulExpr    <- ('*'|'/'|'%') Primary
//    Primary    <- '(' Expr ')'
//                | nom_literal
//                | nom_slice
//                | nom_iterate
//                | nom_collection
//                | nom_index_short
//                | nom_dot
//                | '-' Primary
//                | '!' Primary
//                | <and many more can be added>

named!(nom_expr(NS) -> Thunk,
    map!(
        do_parse!(
               lhs: nom_or >>
            thunks: many0!(nom_pipe_expr) >>
            (lhs, thunks)
        ),
        |(mut lhs, thunks)| {
            //println!("nom_expr-eee, {:?} {:?}", lhs, thunks);
            for rhs in thunks {
                lhs = Thunk::Pipe(Box::new(lhs), Box::new(rhs));
            }
            //println!("nom_expr-xxx, {:?}", lhs);
            lhs
        }
    )
);
named!(nom_pipe_expr(NS) -> Thunk,
    do_parse!(
              ws!(opt!(tag!("|"))) >>
        expr: nom_or >>
        (expr)
    )
);

named!(nom_or(NS) -> Thunk,
    map!(
        do_parse!(
             thunk: nom_and >>
            thunks: many0!(nom_or_expr) >>
            (thunk, thunks)
        ),
        |(mut lhs, thunks)| {
            for rhs in thunks {
                lhs = Thunk::Or(Box::new(lhs), Box::new(rhs));
            }
            lhs
        }
    )
);
named!(nom_or_expr(NS) -> Thunk,
    do_parse!(
              ws!(tag!("||")) >>
        expr: nom_and >>
        (expr)
    )
);

named!(nom_and(NS) -> Thunk,
    map!(
        do_parse!(
             thunk: nom_compare >>
            thunks: many0!(nom_and_expr) >>
            (thunk, thunks)
        ),
        |(mut lhs, thunks)| {
            for rhs in thunks {
                lhs = Thunk::And(Box::new(lhs), Box::new(rhs));
            }
            lhs
        }
    )
);
named!(nom_and_expr(NS) -> Thunk,
    do_parse!(
              ws!(tag!("&&")) >>
        expr: nom_compare >>
        (expr)
    )
);

named!(nom_compare(NS) -> Thunk,
    map!(
        do_parse!(
                thunk: nom_bitor >>
            op_thunks: many0!(nom_compare_expr) >>
            (thunk, op_thunks)
        ),
        |(mut lhs, op_thunks)| {
            for (op, rhs) in op_thunks {
                lhs = match *op {
                    "==" => Thunk::Eq(Box::new(lhs), Box::new(rhs)),
                    "!=" => Thunk::Ne(Box::new(lhs), Box::new(rhs)),
                    "<" => Thunk::Lt(Box::new(lhs), Box::new(rhs)),
                    "<=" => Thunk::Le(Box::new(lhs), Box::new(rhs)),
                    ">" => Thunk::Gt(Box::new(lhs), Box::new(rhs)),
                    ">=" => Thunk::Ge(Box::new(lhs), Box::new(rhs)),
                    _ => unreachable!(),
                }
            }
            lhs
        }
    )
);
named!(nom_compare_expr(NS) -> (NS, Thunk),
    do_parse!(
          op: ws!(alt!(
                tag!("==") | tag!("!=") | tag!("<") |
                tag!("<=") | tag!(">") | tag!(">=")
              )) >>
        expr: nom_bitor >>
        (op, expr)
    )
);

named!(nom_bitor(NS) -> Thunk,
    map!(
        do_parse!(
             thunk: nom_bitxor >>
            thunks: many0!(nom_bitor_expr) >>
            (thunk, thunks)
        ),
        |(mut lhs, thunks)| {
            for rhs in thunks {
                lhs = Thunk::BitOr(Box::new(lhs), Box::new(rhs));
            }
            lhs
        }
    )
);
named!(nom_bitor_expr(NS) -> Thunk,
    do_parse!(
              ws!(tag!("bor")) >>
        expr: nom_bitxor >>
        (expr)
    )
);

named!(nom_bitxor(NS) -> Thunk,
    map!(
        do_parse!(
             thunk: nom_bitand >>
            thunks: many0!(nom_bitxor_expr) >>
            (thunk, thunks)
        ),
        |(mut lhs, thunks)| {
            for rhs in thunks {
                lhs = Thunk::BitXor(Box::new(lhs), Box::new(rhs));
            }
            lhs
        }
    )
);
named!(nom_bitxor_expr(NS) -> Thunk,
    do_parse!(
              ws!(tag!("^")) >>
        expr: nom_bitand >>
        (expr)
    )
);

named!(nom_bitand(NS) -> Thunk,
    map!(
        do_parse!(
             thunk: nom_shift >>
            thunks: many0!(nom_bitand_expr) >>
            (thunk, thunks)
        ),
        |(mut lhs, thunks)| {
            for rhs in thunks {
                lhs = Thunk::BitAnd(Box::new(lhs), Box::new(rhs));
            }
            lhs
        }
    )
);
named!(nom_bitand_expr(NS) -> Thunk,
    do_parse!(
              ws!(tag!("&")) >>
        expr: nom_shift >>
        (expr)
    )
);

named!(nom_shift(NS) -> Thunk,
    map!(
        do_parse!(
                thunk: nom_add_sub >>
            op_thunks: many0!(nom_shift_expr) >>
            (thunk, op_thunks)
        ),
        |(mut lhs, op_thunks)| {
            for (op, rhs) in op_thunks {
                lhs = match *op {
                    ">>" => Thunk::Shr(Box::new(lhs), Box::new(rhs)),
                    "<<" => Thunk::Shl(Box::new(lhs), Box::new(rhs)),
                    _ => unreachable!(),
                }
            }
            lhs
        }
    )
);
named!(nom_shift_expr(NS) -> (NS, Thunk),
    do_parse!(
          op: ws!(alt!(tag!("<<") | tag!(">>"))) >>
        expr: nom_add_sub >>
        (op, expr)
    )
);

named!(nom_add_sub(NS) -> Thunk,
    map!(
        do_parse!(
                thunk: nom_mult_div_rem >>
            op_thunks: many0!(nom_add_expr) >>
            (thunk, op_thunks)
        ),
        |(mut lhs, op_thunks)| {
            for (op, rhs) in op_thunks {
                lhs = match *op {
                    "+" => Thunk::Add(Box::new(lhs), Box::new(rhs)),
                    "-" => Thunk::Sub(Box::new(lhs), Box::new(rhs)),
                    _ => unreachable!(),
                }
            }
            lhs
        }
    )
);
named!(nom_add_expr(NS) -> (NS, Thunk),
    do_parse!(
          op: ws!(alt!(tag!("+") | tag!("-"))) >>
        expr: nom_mult_div_rem >>
        (op, expr)
    )
);

named!(nom_mult_div_rem(NS) -> Thunk,
    map!(
        do_parse!(
                thunk: nom_primary_expr >>
            op_thunks: many0!(nom_mult_expr) >>
            (thunk, op_thunks)
        ),
        |(mut lhs, op_thunks)| {
            //println!("............ mult {:?} {:?}", lhs, op_thunks);
            for (op, rhs) in op_thunks {
                lhs = match *op {
                    "*" => Thunk::Mult(Box::new(lhs), Box::new(rhs)),
                    "/" => Thunk::Div(Box::new(lhs), Box::new(rhs)),
                    "%" => Thunk::Rem(Box::new(lhs), Box::new(rhs)),
                    _ => unreachable!(),
                }
            }
            lhs
        }
    )
);
named!(nom_mult_expr(NS) -> (NS, Thunk),
    do_parse!(
          op: ws!(alt!(tag!("*") | tag!("/") | tag!("%"))) >>
        expr: nom_primary_expr >>
        (op, expr)
    )
);


//------------------ PRIMAR EXPRESSIONS ------------------------


named!(nom_key(NS) -> (Option<String>, Option<isize>),
    alt!(
        nom_identifier => { |s: NS| (Some((&s).to_string()), None) } |
        nom_isize => { |i: isize| (None, Some(i)) } |
        nom_json_string => { |s: String| {
            let off = s.parse().map(|x| Some(x)).unwrap_or(None);
            (Some(s), off)
        }}
    )
);
named!(nom_primary_index_short(NS) ->
    ((Option<String>, Option<isize>), Option<NS>),

    do_parse!(
             nom_dot  >>
        key: nom_key  >>
        opt: nom_opt  >>
        (key, opt)
    )
);
named!(nom_primary_slice1(NS) -> (isize, isize, Option<NS>),
    do_parse!(
                nom_dot >>
                nom_open_sqr >>
         start: nom_isize >>
                nom_dotdot    >>
                nom_equal    >>
           end: nom_isize >>
                nom_clos_sqr >>
           opt: nom_opt      >>
        (start, (end+1), opt)
    )
);
named!(nom_primary_slice2(NS) -> (isize, isize, Option<NS>),
    do_parse!(
                nom_dot >>
                nom_open_sqr >>
         start: nom_isize >>
                nom_dotdot    >>
           end: nom_isize >>
                nom_clos_sqr >>
           opt: nom_opt      >>
        (start, end, opt)
    )
);
named!(nom_primary_slice3(NS) -> (isize, isize, Option<NS>),
    do_parse!(
                nom_dot >>
                nom_open_sqr >>
         start: nom_isize >>
                nom_dotdot    >>
                nom_clos_sqr >>
           opt: nom_opt      >>
        (start, isize::max_value(), opt)
    )
);
named!(nom_primary_slice4(NS) -> (isize, isize, Option<NS>),
    do_parse!(
                nom_dot >>
                nom_open_sqr >>
                nom_dotdot    >>
           end: nom_isize >>
                nom_clos_sqr >>
           opt: nom_opt      >>
        (0, end, opt)
    )
);
named!(nom_primary_slice5(NS) -> (isize, isize, Option<NS>),
    do_parse!(
                nom_dot >>
                nom_open_sqr >>
                nom_dotdot    >>
                nom_equal >>
           end: nom_isize >>
                nom_clos_sqr >>
           opt: nom_opt      >>
        (0, (end+1), opt)
    )
);
named!(nom_primary_slice6(NS) -> (isize, isize, Option<NS>),
    do_parse!(
                nom_dot >>
                nom_open_sqr >>
                nom_dotdot    >>
                nom_clos_sqr >>
           opt: nom_opt      >>
        (0, isize::max_value(), opt)
    )
);
named!(nom_iterate_comma_item(NS) -> Thunk,
    do_parse!(
               nom_comma >>
        thunk: nom_expr  >>
        (thunk)
    )
);
named!(nom_iterate_items(NS) -> Vec<Thunk>,
    map!(
        do_parse!(
             item: nom_expr >>
            items: opt!(many1!(nom_iterate_comma_item)) >>
            (item, items)
        ),
        |(item, items)| {
            //println!("........ iterate_items {:?} {:?}", item, items);
            match items {
                Some(mut items) => { items.insert(0, item); items },
                None => vec![item],
            }
        }
    )
);
named!(nom_primary_full_iterate(NS) -> Option<NS>,
    do_parse!(
                nom_dot       >>
                nom_open_sqr  >>
                nom_clos_sqr  >>
           opt: nom_opt       >>
        (opt)
    )
);
named!(nom_primary_iterate(NS) -> (Vec<Thunk>, Option<NS>),
    do_parse!(
                nom_dot       >>
                nom_open_sqr  >>
        thunks: nom_iterate_items >>
                nom_clos_sqr  >>
           opt: nom_opt       >>
        (thunks, opt)
    )
);
named!(nom_primary_collection1(NS) -> (Vec<Thunk>, Option<NS>),
    do_parse!(
        thunks: delimited!(
                    nom_open_sqr,
                    separated_list!(nom_comma, nom_expr),
                    nom_clos_sqr
                )       >>
           opt: nom_opt >>
       (thunks, opt)
    )
);
named!(nom_object_key(NS) -> Thunk,
    alt!(
        nom_identifier => { |s: NS| Thunk::String((&s).to_string(), false) } |
        nom_json_string => { |s| Thunk::String(s, false) } |
        nom_primary_paran_expr
    )
);
named!(nom_collection2_item(NS) -> (Thunk, Option<Thunk>),
    do_parse!(
        key: nom_object_key  >>
             opt!(nom_colon) >>
        val: opt!(nom_expr)  >>
        (key, val)
    )
);
named!(nom_primary_collection2(NS) -> (Vec<(Thunk, Option<Thunk>)>, Option<NS>),
    do_parse!(
        items: delimited!(
                    nom_open_brace,
                    separated_list!(nom_comma, nom_collection2_item),
                    nom_clos_brace
               )        >>
          opt: nom_opt  >>
        (items, opt)
    )
);


named!(nom_primary_paran_expr(NS) -> Thunk,
    do_parse!(
               nom_open_paran >>
        thunk: nom_expr >>
               nom_clos_paran >>
        (thunk)
    )
);
named!(nom_primary_unaryneg_expr(NS) -> Thunk,
    do_parse!(
               ws!(tag!("-")) >>
        thunk: nom_expr >>
        (thunk)
    )
);
named!(nom_primary_unarynot_expr(NS) -> Thunk,
    do_parse!(
               ws!(tag!("!")) >>
        thunk: nom_expr >>
        (thunk)
    )
);
named!(nom_primary_literal(NS) -> (Thunk, Option<NS>),
    do_parse!(
        thunk: alt!(
                nom_null  => { |_| Thunk::Null(false) } |
                nom_true  => { |_| Thunk::Bool(true, false) } |
                nom_false => { |_| Thunk::Bool(false, false) } |
                nom_number => { number_literal } |
                nom_json_string => { |s| Thunk::String(s, false) }
               ) >>
          opt: nom_opt >>
        (thunk, opt)
    )
);
named!(nom_primary_builtins(NS) -> (NS, Vec<Thunk>),
    do_parse!(
        funcname: nom_identifier >>
            args: delimited!(
                    nom_open_paran,
                    separated_list!(nom_comma, nom_expr),
                    nom_clos_paran
                  ) >>
        (funcname, args)
    )
);

named!(nom_primary_identifier_opt(NS) -> (NS, Option<NS>),
    do_parse!(
        thunk: nom_identifier >>
          opt: nom_opt        >>
        (thunk, opt)
    )
);

// NOTE: There is contention between
// slice operation,
// index operation,
// iterate operation,
// array literal
// collection list
named!(nom_primary_expr(NS) -> Thunk,
    alt!(
        nom_primary_slice1 => { slice_to_thunk } |
        nom_primary_slice2 => { slice_to_thunk } |
        nom_primary_slice3 => { slice_to_thunk } |
        nom_primary_slice4 => { slice_to_thunk } |
        nom_primary_slice5 => { slice_to_thunk } |
        nom_primary_slice6 => { slice_to_thunk } |
        nom_primary_full_iterate => { full_iterate_to_thunk } |
        nom_primary_iterate => { iterate_to_thunk } |
        nom_primary_index_short => { index_short_to_thunk } |
        nom_primary_builtins => { builtin_to_thunk } |
        nom_primary_collection1 => { collection1_to_thunk } |
        nom_primary_collection2 => { collection2_to_thunk } |
        // nom_primary_literal should come before identifier
        nom_primary_literal => { literal_to_thunk } |
        nom_primary_identifier_opt => { identifier_to_literal } |
        nom_primary_unarynot_expr => { |thunk| Thunk::Not(Box::new(thunk)) } |
        nom_primary_unaryneg_expr => { |thunk| Thunk::Neg(Box::new(thunk)) } |
        nom_primary_paran_expr |
        nom_dotdot => { |_| Thunk::Recurse } |
        nom_dot => { |_| Thunk::Identity }
    )
);


fn nom_empty_program(text: NS) -> nom::IResult<NS, Thunk> {
    if text.len() == 0 {
        return Ok((NS(&text[..]), Thunk::Empty))
    }
    let ctxt = nom::Context::Code(text, nom::ErrorKind::Custom(0));
    return Err(nom::Err::Error(ctxt))
}
named!(nom_program(NS) -> Thunk,
    ws!(alt!(
        nom_empty_program |
        nom_expr
    ))
);


fn identifier_to_literal((s, opt): (NS, Option<NS>)) -> Thunk {
    Thunk::Identifier((&s).to_string(), opt.map_or(false, |_| true))
}

fn literal_to_thunk((thunk, opt): (Thunk, Option<NS>)) -> Thunk {
    let opt = opt.map_or(false, |_| true);
    match thunk {
        Thunk::Null(_) => Thunk::Null(opt),
        Thunk::Bool(val, _) => Thunk::Bool(val, opt),
        Thunk::Integer(val, _) => Thunk::Integer(val, opt),
        Thunk::Float(val, _) => Thunk::Float(val, opt),
        Thunk::String(val, _) => Thunk::String(val, opt),
        _ => unreachable!(),
    }
}

fn builtin_to_thunk((funcname, args): (NS, Vec<Thunk>)) -> Thunk {
    Thunk::Builtin(funcname.to_string(), args)
}

fn index_short_to_thunk(
    ((key, off), opt): ((Option<String>, Option<isize>), Option<NS>))
    -> Thunk
{
    //println!(".......... index_short_to_thunk {:?}, {:?}", key, off);
    Thunk::IndexShortcut(key, off, opt.map_or(false, |_| true))
}

fn slice_to_thunk((start, end, opt): (isize, isize, Option<NS>))
    -> Thunk
{
    //println!(".......... slice_to_thunk {} {}, {:?}", start, end, opt);
    Thunk::Slice(start, end, opt.map_or(false, |_| true))
}

fn number_literal(s: NS) -> Thunk {
    //println!("number_literal ..... {:?}", s);
    if let Ok(n) = (&s).parse::<i128>() {
        Thunk::Integer(n, false)
    } else if let Ok(f) = (&s).parse::<f64>() {
        Thunk::Float(f, false)
    } else {
        unreachable!()
    }
}

fn full_iterate_to_thunk(opt: Option<NS>) -> Thunk {
    let opt = opt.map_or(false, |_| true);
    //println!("full iterate ........ {:?}", thunks);
    Thunk::IterateValues(opt)
}

fn iterate_to_thunk((thunks, opt): (Vec<Thunk>, Option<NS>)) -> Thunk {
    let opt1 = opt.map_or(false, |_| true);
    //println!("iterate ........ {:?}", thunks);
    let thunks = thunks.into_iter().map(|thunk|
        match thunk {
            Thunk::Integer(n, opt2) => {
                Thunk::IndexShortcut(None, Some(n as isize), opt1 || opt2)
            },
            Thunk::String(s, opt2) => {
                Thunk::IndexShortcut(Some(s), None, opt1 || opt2)
            },
            thunk => thunk,
        }
    ).collect();
    Thunk::Iterate(thunks, opt1)
}

fn collection1_to_thunk((thunks, opt): (Vec<Thunk>, Option<NS>)) -> Thunk {
    Thunk::List(thunks, opt.map_or(false, |_| true))
}

fn collection2_to_thunk((items, opt): (Vec<(Thunk,Option<Thunk>)>, Option<NS>))
    -> Thunk
{
    Thunk::Dict(items, opt.map_or(false, |_| true))
}


fn check_next_byte(text: NS, b: u8) -> nom::IResult<NS, ()> {
    let progbs = text.as_bytes();
    if progbs.len() == 0 {
        let ctxt = nom::Context::Code(text, nom::ErrorKind::Custom(0));
        return Err(nom::Err::Error(ctxt))

    } else if progbs[0] != b {
        let ctxt = nom::Context::Code(text, nom::ErrorKind::Custom(0));
        return Err(nom::Err::Error(ctxt))
    }
    Ok((text, ()))
}

pub fn parse_program_nom<'a>(s: NS<'a>) -> IResult<NS<'a>, Thunk> {
    nom_program(s)
}
