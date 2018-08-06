use nom::{self, {types::CompleteStr as NS}, IResult};

use lex::Lex;
use json::{Json, parse_string, parse_array, parse_object};
use jq::Thunk;

named!(nom_dot(NS) -> NS, tag!("."));
named!(nom_dotdot(NS) -> NS, tag!(".."));
named!(nom_colon(NS) -> NS, tag!(":"));
named!(nom_comma(NS) -> NS, tag!(","));
named!(nom_equal(NS) -> NS, tag!("="));
named!(nom_open_sqr(NS) -> NS, tag!("["));
named!(nom_clos_sqr(NS) -> NS, tag!("]"));
named!(nom_open_brace(NS) -> NS, tag!("{"));
named!(nom_clos_brace(NS) -> NS, tag!("}"));
named!(nom_open_paran(NS) -> NS, tag!(")"));
named!(nom_clos_paran(NS) -> NS, tag!(")"));
named!(nom_opt(NS) -> Option<NS>, opt!(tag!("?")));
named!(nom_identifier(NS) -> NS, re_match!(r#"^[a-zA-Z0-9_]+"#));

named!(nom_null(NS) -> NS, tag!("null"));
named!(nom_true(NS) -> NS, tag!("true"));
named!(nom_false(NS) -> NS, tag!("false"));
named!(nom_int(NS) -> i128,
    flat_map!(re_match!(r#"^[+-]?\d+"#), parse_to!(i128))
);
named!(nom_usize(NS) -> usize,
    flat_map!(re_match!(r#"^[+-]?\d+"#), parse_to!(usize))
);
named!(nom_float(NS) -> f64,
    flat_map!(
        re_match!(r#"^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?"#),
        parse_to!(f64)
    )
);
fn nom_json_string(text: NS) -> nom::IResult<NS, Json> {
    check_next_byte(text, b'"')?;
    let mut lex = Lex::new(0, 1, 1);
    match parse_string(&text, &mut lex) {
        Ok(s) => Ok((NS(&text[lex.off..]), s)),
        _ => {
            let kind = nom::ErrorKind::Custom(lex.off as u32);
            return Err(nom::Err::Failure(nom::Context::Code(text, kind)));
        }
    }
}
fn nom_json_array(text: NS) -> nom::IResult<NS, Json> {
    check_next_byte(text, b'[')?;
    let mut lex = Lex::new(0, 1, 1);
    match parse_array(&text, &mut lex) {
        Ok(a) => Ok((NS(&text[lex.off..]), a)),
        _ => {
            let kind = nom::ErrorKind::Custom(lex.off as u32);
            return Err(nom::Err::Failure(nom::Context::Code(text, kind)));
        }
    }
}
fn nom_json_object(text: NS) -> nom::IResult<NS, Json> {
    check_next_byte(text, b'{')?;
    let mut lex = Lex::new(0, 1, 1);
    match parse_object(&text, &mut lex) {
        Ok(o) => Ok((NS(&text[lex.off..]), o)),
        _ => {
            let kind = nom::ErrorKind::Custom(lex.off as u32);
            return Err(nom::Err::Failure(nom::Context::Code(text, kind)));
        }
    }
}


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
             thunk: nom_or >>
            thunks: many0!(nom_pipe_expr) >>
            (thunk, thunks)
        ),
        |(mut lhs, thunks)| {
            for rhs in thunks {
                lhs = Thunk::Pipe(Box::new(lhs), Box::new(rhs));
            }
            lhs
        }
    )
);
named!(nom_pipe_expr(NS) -> Thunk,
    do_parse!(
              tag!("|") >>
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
              tag!("||") >>
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
              tag!("&&") >>
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
          op: alt!(
                tag!("==") | tag!("!=") | tag!("<") |
                tag!("<=") | tag!(">") | tag!(">=")
              ) >>
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
              tag!("bor") >>
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
              tag!("^") >>
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
              tag!("&") >>
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
          op: alt!(tag!("<<") | tag!(">>")) >>
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
          op: alt!(tag!("+") | tag!("-")) >>
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
          op: alt!(tag!("*") | tag!("/") | tag!("%")) >>
        expr: nom_primary_expr >>
        (op, expr)
    )
);


//------------------ PRIMAR EXPRESSIONS ------------------------


named!(nom_key(NS) -> Json,
    alt!(
        nom_identifier => { |s: NS| Json::String((&s).to_string()) } |
        nom_json_string
    )
);
named!(nom_primary_index_short(NS) -> (Json, Option<NS>),
    do_parse!(
             nom_dot  >>
        key: nom_key  >>
        opt: nom_opt  >>
        (key, opt)
    )
);
named!(nom_primary_slice1(NS) -> (Thunk, usize, usize, Option<NS>),
    do_parse!(
         thunk: nom_primary_expr >>
                nom_open_sqr >>
         start: nom_usize >>
                nom_dotdot    >>
                nom_equal    >>
           end: nom_usize >>
                nom_clos_sqr >>
           opt: nom_opt      >>
        (thunk, start, (end+1), opt)
    )
);
named!(nom_primary_slice2(NS) -> (Thunk, usize, usize, Option<NS>),
    do_parse!(
         thunk: nom_primary_expr >>
                nom_open_sqr >>
         start: nom_usize >>
                nom_dotdot    >>
           end: nom_usize >>
                nom_clos_sqr >>
           opt: nom_opt      >>
        (thunk, start, end, opt)
    )
);
named!(nom_primary_slice3(NS) -> (Thunk, usize, usize, Option<NS>),
    do_parse!(
         thunk: nom_primary_expr >>
                nom_open_sqr >>
         start: nom_usize >>
                nom_dotdot    >>
                nom_clos_sqr >>
           opt: nom_opt      >>
        (thunk, start, usize::max_value(), opt)
    )
);
named!(nom_primary_slice4(NS) -> (Thunk, usize, usize, Option<NS>),
    do_parse!(
         thunk: nom_primary_expr >>
                nom_open_sqr >>
                nom_dotdot    >>
           end: nom_usize >>
                nom_clos_sqr >>
           opt: nom_opt      >>
        (thunk, usize::min_value(), end, opt)
    )
);
named!(nom_primary_slice5(NS) -> (Thunk, usize, usize, Option<NS>),
    do_parse!(
         thunk: nom_primary_expr >>
                nom_open_sqr >>
                nom_dotdot    >>
                nom_equal >>
           end: nom_usize >>
                nom_clos_sqr >>
           opt: nom_opt      >>
        (thunk, usize::min_value(), (end+1), opt)
    )
);
named!(nom_primary_slice6(NS) -> (Thunk, usize, usize, Option<NS>),
    do_parse!(
         thunk: nom_primary_expr >>
                nom_open_sqr >>
                nom_dotdot    >>
                nom_clos_sqr >>
           opt: nom_opt      >>
        (thunk, usize::min_value(), usize::max_value(), opt)
    )
);
named!(nom_iterate_item(NS) -> Thunk,
    map!(
        do_parse!(
        thunk: alt!(
                    nom_expr |
                    nom_identifier => { |s: NS| {
                        Thunk::IndexShortcut((&s).to_string(), None, false)
                    }}
               ) >>
          opt: nom_opt >>
        (thunk, opt)
        ),
        |(thunk, opt_outer)| {
            match thunk {
                Thunk::IndexShortcut(key, None, opt_inner) => {
                    let off: Option<usize> = key.parse()
                        .map(|off| Some(off))
                        .unwrap_or(None);
                    let opt = opt_outer.map_or(opt_inner, |_| true);
                    Thunk::IndexShortcut(key, off, opt)
                },
                thunk => thunk
            }
        }
    )
);
named!(nom_iterate_comma_item(NS) -> Thunk,
    do_parse!(
               nom_comma >>
        thunk: nom_iterate_item >>
        (thunk)
    )
);
named!(nom_iterate_items(NS) -> Vec<Thunk>,
    map!(
        do_parse!(
             item: nom_iterate_item >>
            items: opt!(many1!(nom_iterate_comma_item)) >>
            (item, items)
        ),
        |(item, items)| {
            match items {
                Some(mut items) => { items.insert(0, item); items },
                None => vec![item],
            }
        }
    )
);
named!(nom_primary_iterate(NS) -> (Thunk, Vec<Thunk>, Option<NS>),
    do_parse!(
         thunk: nom_primary_expr >>
                nom_open_sqr  >>
        thunks: nom_iterate_items >>
                nom_clos_sqr  >>
           opt: nom_opt       >>
        (thunk, thunks, opt)
    )
);
named!(nom_primary_collection1(NS) -> (Vec<Thunk>, Option<NS>),
    do_parse!(
                nom_open_sqr  >>
        thunks: nom_expr_list >>
                nom_clos_sqr  >>
           opt: nom_opt       >>
        (thunks, opt)
    )
);
named!(nom_collection2_item(NS) -> (Thunk, Thunk),
    do_parse!(
        key: nom_expr >>
             nom_colon >>
        val: nom_expr >>
        (key,val)
    )
);
named!(nom_collection2_comma_item(NS) -> (Thunk, Thunk),
    do_parse!(
               nom_comma >>
        thunk: nom_collection2_item >>
        (thunk)
    )
);
named!(nom_collection2_items(NS) -> Vec<(Thunk, Thunk)>,
    map!(
        do_parse!(
             item: nom_collection2_item >>
            items: opt!(many1!(nom_collection2_comma_item)) >>
            (item, items)
        ),
        |(item, items)| {
            match items {
                Some(mut items) => { items.insert(0, item); items },
                None => vec![item],
            }
        }
    )
);
named!(nom_primary_collection2(NS) -> (Vec<(Thunk, Thunk)>, Option<NS>),
    do_parse!(
               nom_open_brace  >>
        items: nom_collection2_items >>
               nom_clos_brace  >>
          opt: nom_opt       >>
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
               tag!("-") >>
        thunk: nom_expr >>
        (thunk)
    )
);
named!(nom_primary_unarynot_expr(NS) -> Thunk,
    do_parse!(
               tag!("!") >>
        thunk: nom_expr >>
        (thunk)
    )
);
named!(nom_primary_literal(NS) -> Thunk,
    alt!(
        nom_null  => { |_| Thunk::Literal(Json::Null) } |
        nom_true  => { |_| Thunk::Literal(Json::Bool(true)) } |
        nom_false => { |_| Thunk::Literal(Json::Bool(false)) } |
        nom_int   => { |i| Thunk::Literal(Json::Integer(i)) } |
        nom_float => { |f| Thunk::Literal(Json::Float(f)) } |
        nom_json_string => { |s| Thunk::Literal(s) } |
        nom_json_array => { |a| Thunk::Literal(a) } |
        nom_json_object => { |o| Thunk::Literal(o) }
    )
);
named!(nom_primary_builtins(NS) -> (NS, Vec<Thunk>),
    do_parse!(
        funcname: nom_identifier >>
                  nom_open_paran >>
            args: nom_expr_list  >>
                  nom_clos_paran >>
        (funcname, args)
    )
);

named!(nom_primary_expr(NS) -> Thunk,
    alt!(
        nom_primary_literal | // should come before identifier
        nom_primary_builtins => { builtin_to_thunk } |
        nom_primary_slice1 => { slice_to_thunk } |
        nom_primary_slice2 => { slice_to_thunk } |
        nom_primary_slice3 => { slice_to_thunk } |
        nom_primary_slice4 => { slice_to_thunk } |
        nom_primary_slice5 => { slice_to_thunk } |
        nom_primary_slice6 => { slice_to_thunk } |
        nom_primary_iterate => { iterate_to_thunk } |
        nom_primary_collection1 => { collection1_to_thunk } |
        nom_primary_collection2 => { collection2_to_thunk } |
        nom_primary_index_short => { index_short_to_thunk } |
        nom_primary_unarynot_expr => { |thunk| Thunk::Not(Box::new(thunk)) } |
        nom_primary_unaryneg_expr => { |thunk| Thunk::Neg(Box::new(thunk)) } |
        nom_primary_paran_expr |
        nom_dotdot => { |_| Thunk::Recurse } |
        nom_dot => { |_| Thunk::Identity }
    )
);

named!(nom_comma_expr(NS) -> Thunk,
    do_parse!(
               nom_comma >>
        thunk: nom_expr  >>
        (thunk)
    )
);
named!(nom_expr_list(NS) -> Vec<Thunk>,
    map!(
        do_parse!(
             thunk:  nom_expr                      >>
            thunks: opt!(many1!(nom_comma_expr))  >>
            (thunk, thunks)
        ),
        |(thunk, thunks)| {
            match thunks {
                Some(mut thunks) => { thunks.insert(0, thunk); thunks },
                None => vec![thunk]
            }
        }
    )
);


fn nom_empty_program(text: NS) -> nom::IResult<NS, Thunk> {
    if text.len() == 0 {
        return Ok((NS(&text[..]), Thunk::Empty))
    }
    unreachable!()
}
named!(nom_program(NS) -> Thunk,
    ws!(alt!(
        nom_expr |
        nom_empty_program // should alway be last.
    ))
);


fn builtin_to_thunk((funcname, args): (NS, Vec<Thunk>)) -> Thunk {
    Thunk::Builtin(funcname.to_string(), args)
}

fn index_short_to_thunk((key, opt): (Json, Option<NS>)) -> Thunk {
    let key = match key {Json::String(s) => s, _ => unreachable!()};
    let off: Option<usize> = match key.parse() {
        Ok(off) => Some(off),
        _ => None
    };
    Thunk::IndexShortcut(key, off, opt.map_or(false, |_| true))
}

fn slice_to_thunk((thunk, start, end, opt): (Thunk, usize, usize, Option<NS>))
    -> Thunk
{
    Thunk::Slice(Box::new(thunk), start, end, opt.map_or(false, |_| true))
}

fn iterate_to_thunk((thunk, thunks, opt): (Thunk, Vec<Thunk>, Option<NS>)) -> Thunk {
    let opt = opt.map_or(false, |_| true);
    Thunk::Iterate(Box::new(thunk), thunks, opt)
}

fn collection1_to_thunk((thunks, opt): (Vec<Thunk>, Option<NS>)) -> Thunk {
    Thunk::List(thunks, opt.map_or(false, |_| true))
}

fn collection2_to_thunk((items, opt): (Vec<(Thunk,Thunk)>, Option<NS>)) -> Thunk {
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
