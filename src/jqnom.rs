use nom::{self, {types::CompleteStr as S}, IResult};

use lex::Lex;
use json::{Json, parse_string, parse_array, parse_object};
use jq::Thunk;

named!(nom_dot(S) -> S, tag!("."));
named!(nom_dotdot(S) -> S, tag!(".."));
named!(nom_colon(S) -> S, tag!(":"));
named!(nom_comma(S) -> S, tag!(","));
named!(nom_equal(S) -> S, tag!("="));
named!(nom_open_sqr(S) -> S, tag!("["));
named!(nom_clos_sqr(S) -> S, tag!("]"));
named!(nom_open_brace(S) -> S, tag!("{"));
named!(nom_clos_brace(S) -> S, tag!("}"));
named!(nom_open_paran(S) -> S, tag!(")"));
named!(nom_clos_paran(S) -> S, tag!(")"));
named!(nom_opt(S) -> Option<S>, opt!(tag!("?")));
named!(nom_identifier(S) -> S, re_match!(r#"^[a-zA-Z0-9_]+"#));

named!(nom_null(S) -> S, tag!("null"));
named!(nom_true(S) -> S, tag!("true"));
named!(nom_false(S) -> S, tag!("false"));
named!(nom_int(S) -> i128,
    flat_map!(re_match!(r#"^[+-]?\d+"#), parse_to!(i128))
);
named!(nom_usize(S) -> usize,
    flat_map!(re_match!(r#"^[+-]?\d+"#), parse_to!(usize))
);
named!(nom_float(S) -> f64,
    flat_map!(
        re_match!(r#"^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?"#),
        parse_to!(f64)
    )
);
fn nom_string(text: S) -> nom::IResult<S, Json> {
    check_next_byte(text, b'"')?;
    let mut lex = Lex::new(0, 1, 1);
    match parse_string(&text, &mut lex) {
        Ok(s) => Ok((S(&text[lex.off..]), s)),
        _ => {
            let kind = nom::ErrorKind::Custom(lex.off as u32);
            return Err(nom::Err::Failure(nom::Context::Code(text, kind)));
        }
    }
}
fn nom_array(text: S) -> nom::IResult<S, Json> {
    check_next_byte(text, b'[')?;
    let mut lex = Lex::new(0, 1, 1);
    match parse_array(&text, &mut lex) {
        Ok(a) => Ok((S(&text[lex.off..]), a)),
        _ => {
            let kind = nom::ErrorKind::Custom(lex.off as u32);
            return Err(nom::Err::Failure(nom::Context::Code(text, kind)));
        }
    }
}
fn nom_object(text: S) -> nom::IResult<S, Json> {
    check_next_byte(text, b'{')?;
    let mut lex = Lex::new(0, 1, 1);
    match parse_object(&text, &mut lex) {
        Ok(o) => Ok((S(&text[lex.off..]), o)),
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

named!(nom_expr(S) -> Thunk,
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
named!(nom_pipe_expr(S) -> Thunk,
    do_parse!(
              tag!("|") >>
        expr: nom_or >>
        (expr)
    )
);

named!(nom_or(S) -> Thunk,
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
named!(nom_or_expr(S) -> Thunk,
    do_parse!(
              tag!("||") >>
        expr: nom_and >>
        (expr)
    )
);

named!(nom_and(S) -> Thunk,
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
named!(nom_and_expr(S) -> Thunk,
    do_parse!(
              tag!("&&") >>
        expr: nom_compare >>
        (expr)
    )
);

named!(nom_compare(S) -> Thunk,
    map!(
        do_parse!(
                thunk: nom_bitor >>
            op_thunks: many0!(nom_compare_expr) >>
            (thunk, op_thunks)
        ),
        |(mut lhs, op_thunks)| {
            for (op, rhs) in op_thunks {
                lhs = match *op {
                    "==" => Thunk::Compare(Box::new(lhs), Box::new(rhs)),
                    "!=" => Thunk::Compare(Box::new(lhs), Box::new(rhs)),
                    "<" => Thunk::Compare(Box::new(lhs), Box::new(rhs)),
                    "<=" => Thunk::Compare(Box::new(lhs), Box::new(rhs)),
                    ">" => Thunk::Compare(Box::new(lhs), Box::new(rhs)),
                    ">=" => Thunk::Compare(Box::new(lhs), Box::new(rhs)),
                    _ => unreachable!(),
                }
            }
            lhs
        }
    )
);
named!(nom_compare_expr(S) -> (S, Thunk),
    do_parse!(
          op: alt!(
                tag!("==") | tag!("!=") | tag!("<") |
                tag!("<=") | tag!(">") | tag!(">=")
              ) >>
        expr: nom_bitor >>
        (op, expr)
    )
);

named!(nom_bitor(S) -> Thunk,
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
named!(nom_bitor_expr(S) -> Thunk,
    do_parse!(
              tag!("bor") >>
        expr: nom_bitxor >>
        (expr)
    )
);

named!(nom_bitxor(S) -> Thunk,
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
named!(nom_bitxor_expr(S) -> Thunk,
    do_parse!(
              tag!("^") >>
        expr: nom_bitand >>
        (expr)
    )
);

named!(nom_bitand(S) -> Thunk,
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
named!(nom_bitand_expr(S) -> Thunk,
    do_parse!(
              tag!("&") >>
        expr: nom_shift >>
        (expr)
    )
);

named!(nom_shift(S) -> Thunk,
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
named!(nom_shift_expr(S) -> (S, Thunk),
    do_parse!(
          op: alt!(tag!("<<") | tag!(">>")) >>
        expr: nom_add_sub >>
        (op, expr)
    )
);

named!(nom_add_sub(S) -> Thunk,
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
named!(nom_add_expr(S) -> (S, Thunk),
    do_parse!(
          op: alt!(tag!("+") | tag!("-")) >>
        expr: nom_mult_div_rem >>
        (op, expr)
    )
);

named!(nom_mult_div_rem(S) -> Thunk,
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
named!(nom_mult_expr(S) -> (S, Thunk),
    do_parse!(
          op: alt!(tag!("*") | tag!("/") | tag!("%")) >>
        expr: nom_primary_expr >>
        (op, expr)
    )
);


//------------------ PRIMAR EXPRESSIONS ------------------------


named!(nom_key(S) -> Json,
    alt!(
        nom_identifier => { |s: S| Json::String((&s).to_string()) } |
        nom_string
    )
);
named!(nom_primary_index_short(S) -> (Json, Option<S>),
    do_parse!(
             nom_dot  >>
        key: nom_key  >>
        opt: nom_opt  >>
        (key, opt)
    )
);
named!(nom_primary_slice1(S) -> (Thunk, usize, usize, Option<S>),
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
named!(nom_primary_slice2(S) -> (Thunk, usize, usize, Option<S>),
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
named!(nom_primary_slice3(S) -> (Thunk, usize, usize, Option<S>),
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
named!(nom_primary_slice4(S) -> (Thunk, usize, usize, Option<S>),
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
named!(nom_primary_slice5(S) -> (Thunk, usize, usize, Option<S>),
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
named!(nom_primary_slice6(S) -> (Thunk, usize, usize, Option<S>),
    do_parse!(
         thunk: nom_primary_expr >>
                nom_open_sqr >>
                nom_dotdot    >>
                nom_clos_sqr >>
           opt: nom_opt      >>
        (thunk, usize::min_value(), usize::max_value(), opt)
    )
);
named!(nom_primary_iterate(S) -> (Thunk, Thunk, Option<S>),
    do_parse!(
         thunk: nom_primary_expr >>
                nom_open_sqr  >>
        thunks: nom_expr_list >>
                nom_clos_sqr  >>
           opt: nom_opt       >>
        (thunk, thunks, opt)
    )
);
named!(nom_primary_collection1(S) -> (Thunk, Option<S>),
    do_parse!(
                nom_open_sqr  >>
        thunks: nom_expr_list >>
                nom_clos_sqr  >>
           opt: nom_opt       >>
        (thunks, opt)
    )
);
named!(nom_primary_collection2(S) -> (Thunk, Option<S>),
    do_parse!(
                nom_open_brace  >>
        thunks: nom_expr_list >>
                nom_clos_brace  >>
           opt: nom_opt       >>
        (thunks, opt)
    )
);


named!(nom_primary_paran_expr(S) -> Thunk,
    do_parse!(
               nom_open_paran >>
        thunk: nom_expr >>
               nom_clos_paran >>
        (thunk)
    )
);
named!(nom_primary_unaryneg_expr(S) -> Thunk,
    do_parse!(
               tag!("-") >>
        thunk: nom_expr >>
        (thunk)
    )
);
named!(nom_primary_unarynot_expr(S) -> Thunk,
    do_parse!(
               tag!("!") >>
        thunk: nom_expr >>
        (thunk)
    )
);
named!(nom_primary_literal(S) -> Thunk,
    alt!(
        nom_null  => { |_| Thunk::Literal(Json::Null) } |
        nom_true  => { |_| Thunk::Literal(Json::Bool(true)) } |
        nom_false => { |_| Thunk::Literal(Json::Bool(false)) } |
        nom_int   => { |i| Thunk::Literal(Json::Integer(i)) } |
        nom_float => { |f| Thunk::Literal(Json::Float(f)) } |
        nom_string => { |s| Thunk::Literal(s) } |
        nom_array => { |a| Thunk::Literal(a) } |
        nom_object => { |o| Thunk::Literal(o) }
    )
);
named!(nom_primary_builtins(S) -> (S, Thunk),
    do_parse!(
        funcname: nom_identifier >>
                  nom_open_paran >>
            args: nom_expr_list  >>
                  nom_clos_paran >>
        (funcname, args)
    )
);

named!(nom_primary_expr(S) -> Thunk,
    alt!(
        nom_primary_literal | // should come before identifier
        nom_primary_builtins => { builtin_to_thunk } |
        nom_identifier => { |s: S| Thunk::Identifier(s.to_string()) } |
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

named!(nom_comma_expr(S) -> Thunk,
    do_parse!(
               nom_comma >>
        thunk: nom_expr  >>
        (thunk)
    )
);
named!(nom_expr_list(S) -> Thunk,
    map!(
        do_parse!(
             thunk:  nom_expr                      >>
            thunks: opt!(many1!(nom_comma_expr))  >>
            (thunk, thunks)
        ),
        |(thunk, thunks)| {
            let thunks = match thunks {
                Some(mut thunks) => { thunks.insert(0, thunk); thunks },
                None => vec![thunk]
            };
            Thunk::Thunks(thunks)
        }
    )
);


fn nom_empty_program(text: S) -> nom::IResult<S, Thunk> {
    if text.len() == 0 {
        Ok((S(&text[..]), Thunk::Empty))
    } else {
        panic!("impossible situation")
    }
}
named!(nom_program(S) -> Thunk,
    ws!(alt!(
        nom_expr_list |
        nom_expr |
        nom_empty_program // should alway be last.
    ))
);


fn builtin_to_thunk((funcname, args): (S, Thunk)) -> Thunk {
    Thunk::Builtin(funcname.to_string(), Box::new(args))
}

fn index_short_to_thunk((key, opt): (Json, Option<S>)) -> Thunk {
    let key = match key {Json::String(s) => s, _ => panic!("impossible case")};
    let off: Option<usize> = match key.parse() {
        Ok(off) => Some(off),
        _ => None
    };
    Thunk::IndexShortcut(key, off, opt.map_or(false, |_| true))
}

fn slice_to_thunk((thunk, start, end, opt): (Thunk, usize, usize, Option<S>))
    -> Thunk
{
    Thunk::Slice(Box::new(thunk), start, end, opt.map_or(false, |_| true))
}

fn iterate_to_thunk((thunk, thunks, opt): (Thunk, Thunk, Option<S>))
    -> Thunk
{
    Thunk::Iterate(
        Box::new(thunk), Box::new(thunks), opt.map_or(false, |_| true)
    )
}

fn collection1_to_thunk((thunks, opt): (Thunk, Option<S>)) -> Thunk {
    Thunk::List(Box::new(thunks), opt.map_or(false, |_| true))
}

fn collection2_to_thunk((thunks, opt): (Thunk, Option<S>)) -> Thunk {
    Thunk::Dict(Box::new(thunks), opt.map_or(false, |_| true))
}



fn check_next_byte(text: S, b: u8) -> nom::IResult<S, ()> {
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

pub fn parse_program_nom<'a>(s: S<'a>) -> IResult<S<'a>, Thunk> {
    nom_program(s)
}
