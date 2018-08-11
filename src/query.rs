use std::{result, error, cmp};
use std::fmt::{self};

use nom::{self, {types::CompleteStr as NS}};

use json::{self, Json};
use query_nom::parse_program_nom;
use document::Document;


type Output<D> = Vec<D>;


pub type Result<T> = result::Result<T,Error>;


#[derive(Debug,Eq,PartialEq)]
pub enum Error {
    Parse(String),
    ParseJson(json::Error),
    Op(Option<json::Error>, String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use query::Error::*;

        match self {
            Parse(s) => write!(f, "{}", s),
            ParseJson(err) => write!(f, "{}", err),
            Op(Some(err), s) => write!(f, "{} due to {}", s, err),
            Op(None, s) => write!(f, "{}", s),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&error::Error> { None }
}

impl From<json::Error> for Error {
    fn from(err: json::Error) -> Error {
        use json::Error::{Parse, ParseFloat, ParseInt, NotMyType, KeyMissing};
        use json::Error::{IndexUnbound};

        match err {
            e@Parse(_) | e@ParseFloat(_,_) | e@ParseInt(_,_) => {
                Error::ParseJson(e)
            },
            e@NotMyType(_) | e@KeyMissing(_,_) | e@IndexUnbound(_, _) => {
                Error::Op(Some(e), "json op error".to_string())
            },
        }
    }
}

impl<'a> From<nom::Err<NS<'a>>> for Error {
    fn from(err: nom::Err<NS<'a>>) -> Error {
        use nom::simple_errors::Context as NomContext;

        match err {
            nom::Err::Incomplete(_) => {
                Error::Parse(format!("{}", err))
            },
            nom::Err::Error(NomContext::Code(rem, _)) => {
                let rem_till = cmp::min(5, rem.len());
                Error::Parse(format!("error at {:}", &rem[..rem_till]))
            },
            nom::Err::Failure(NomContext::Code(rem, _)) => {
                let rem_till = cmp::min(5, rem.len());
                Error::Parse(format!("failure at {:}", &rem[..rem_till]))
            },
        }
    }
}


pub fn parse_program(text: &str) -> Result<Box<Thunk>> {
    let (_, thunk) = parse_program_nom(NS(text))?;
    Ok(Box::new(thunk))
}


#[derive(Debug)]
pub enum Thunk where {
    // Primary thunks
    Empty,
    Identity,
    Recurse,
    Literal(Json),
    IndexShortcut(String, Option<usize>, bool),
    Slice(Box<Thunk>, usize, usize, bool),
    Iterate(Box<Thunk>, Vec<Thunk>, bool),
    List(Vec<Thunk>, bool),
    Dict(Vec<(Thunk, Thunk)>, bool),
    // Operations in decreasing precedance
    Neg(Box<Thunk>),
    Not(Box<Thunk>),
    Mult(Box<Thunk>, Box<Thunk>),
    Div(Box<Thunk>, Box<Thunk>),
    Rem(Box<Thunk>, Box<Thunk>),
    Add(Box<Thunk>, Box<Thunk>),
    Sub(Box<Thunk>, Box<Thunk>),
    Eq(Box<Thunk>, Box<Thunk>),
    Ne(Box<Thunk>, Box<Thunk>),
    Lt(Box<Thunk>, Box<Thunk>),
    Le(Box<Thunk>, Box<Thunk>),
    Gt(Box<Thunk>, Box<Thunk>),
    Ge(Box<Thunk>, Box<Thunk>),
    Shr(Box<Thunk>, Box<Thunk>),
    Shl(Box<Thunk>, Box<Thunk>),
    BitAnd(Box<Thunk>, Box<Thunk>),
    BitXor(Box<Thunk>, Box<Thunk>),
    BitOr(Box<Thunk>, Box<Thunk>),
    And(Box<Thunk>, Box<Thunk>),
    Or(Box<Thunk>, Box<Thunk>),
    Pipe(Box<Thunk>, Box<Thunk>),
    // Builtins
    Builtin(String, Vec<Thunk>),
}

impl<D> FnMut<(D,)> for Thunk where D: Document {
    extern "rust-call" fn call_mut(&mut self, args: (D,)) -> Self::Output {
        use query::Thunk::*;

        let doc = args.0;
        match self {
            Empty => { // vector of single item
                Ok(vec![doc])
            },

            Identity => { // vector of single item
                Ok(vec![doc])
            },

            Recurse => { // vector of one or more items
                Ok(doc.recurse())
            },

            Literal(literal) => { // vecot of single item
                Ok(vec![D::from(literal.clone())])
            },

            IndexShortcut(key, off, opt) => { // vector of single item
                let res = do_obj_shortcut(key, doc.clone());
                if res.is_err() {
                    let res = do_index_shortcut(*off, doc);
                    if *opt && res.is_err() { return Ok(vec![]) }
                    Ok(vec![res.unwrap()])
                } else {
                    Ok(vec![res.unwrap()])
                }
            },

            Slice(ref mut thunk, start, end, opt) => { // vector of one or more
                use query::Error::Op;

                let mut outs = Vec::new();
                for item in thunk(doc)?.into_iter() {
                    let mut res = item.slice(*start, *end)
                        .ok_or_else(
                            || Op(None, "json not an array".to_string())
                        );
                    if *opt && res.is_err() { continue }
                    outs.append(&mut res?);
                }
                Ok(outs)
            },

            Iterate(ref mut thunk, ref mut thunks, opt) => {
                let mut outs = Vec::new();
                for item in thunk(doc)?.into_iter() {
                    let mut res = do_iterate(thunks, *opt, item);
                    if *opt && res.is_err() { continue }
                    outs.append(&mut res?);
                }
                Ok(outs)
            },

            List(ref mut thunks, opt) => do_list(thunks, *opt, doc),
            Dict(ref mut kv_thunks, opt) => do_dict(kv_thunks, *opt, doc),

            Neg(ref mut thunk) => do_neg(thunk, doc),
            Not(ref mut thunk) => do_not(thunk, doc),

            Mult(ref mut lthunk, ref mut rthunk) => do_mul(lthunk, rthunk, doc),
            Div(ref mut lthunk, ref mut rthunk) => do_div(lthunk, rthunk, doc),
            Rem(ref mut lthunk, ref mut rthunk) => do_rem(lthunk, rthunk, doc),

            Add(ref mut lthunk, ref mut rthunk) => do_add(lthunk, rthunk, doc),
            Sub(ref mut lthunk, ref mut rthunk) => do_sub(lthunk, rthunk, doc),

            Eq(ref mut lthunk, ref mut rthunk) => do_eq(lthunk, rthunk, doc),
            Ne(ref mut lthunk, ref mut rthunk) => do_ne(lthunk, rthunk, doc),
            Lt(ref mut lthunk, ref mut rthunk) => do_lt(lthunk, rthunk, doc),
            Le(ref mut lthunk, ref mut rthunk) => do_le(lthunk, rthunk, doc),
            Gt(ref mut lthunk, ref mut rthunk) => do_gt(lthunk, rthunk, doc),
            Ge(ref mut lthunk, ref mut rthunk) => do_ge(lthunk, rthunk, doc),

            Shr(ref mut ltnk, ref mut rtnk) => do_shr(ltnk, rtnk, doc),
            Shl(ref mut ltnk, ref mut rtnk) => do_shl(ltnk, rtnk, doc),
            BitAnd(ref mut ltnk, ref mut rtnk) => do_bitand(ltnk, rtnk, doc),
            BitXor(ref mut ltnk, ref mut rtnk) => do_bitxor(ltnk, rtnk, doc),
            BitOr(ref mut ltnk, ref mut rtnk) => do_bitor(ltnk, rtnk, doc),

            And(ref mut ltnk, ref mut rtnk) => do_and(ltnk, rtnk, doc),
            Or(ref mut ltnk, ref mut rtnk) => do_or(ltnk, rtnk, doc),

            Pipe(ref mut lthunk, ref mut rthunk) => do_pipe(lthunk, rthunk, doc),

            Builtin(_,_) => unreachable!(),
        }
    }
}

impl<D> FnOnce<(D,)> for Thunk where D: Document {
    type Output=Result<Output<D>>;

    extern "rust-call" fn call_once(mut self, args: (D,)) -> Self::Output {
        self.call_mut(args)
    }
}

fn do_obj_shortcut<D>(key: &String, doc: D)
    -> Result<D> where D: Document
{
    doc.get(key).map_err(Into::into)
}

fn do_index_shortcut<D>(off: Option<usize>, doc: D)
    -> Result<D> where D: Document
{
    match off {
        Some(off) => doc.index(off).map_err(Into::into),
        None => Err(Error::Op(None, "json not an array".to_string())),
    }
}

fn do_iterate<D>(thunks: &mut Vec<Thunk>, opt: bool, doc: D)
    -> Result<Output<D>> where D: Document
{
    let mut out = Vec::new();
    for thunk in thunks.iter_mut() {
        let mut res = thunk(doc.clone());
        if opt && res.is_err() { continue }
        out.append(&mut res?)
    }
    Ok(out)
}

fn do_list<D>(thunks: &mut Vec<Thunk>, _opt: bool, doc: D)
    -> Result<Output<D>> where D: Document
{
    let iter = thunks.iter_mut().filter_map(|thunk| {
        let res = thunk(doc.clone());
        if res.is_err() { return None }
        Some(res.unwrap())
    });

    let val = { D::list_comprehend(iter) };
    Ok(val)
}

fn do_dict<D>(kv_thunks: &mut Vec<(Thunk, Thunk)>, _opt: bool, doc: D)
    -> Result<Output<D>> where D: Document
{
    let iter = kv_thunks.iter_mut().filter_map(|kv_thunk| {
        let key_res = kv_thunk.0(doc.clone());
        if key_res.is_err() { return None }

        let val_res = kv_thunk.1(doc.clone());
        if val_res.is_err() { return None }

        let keys = key_res.unwrap().into_iter()
            .filter_map(|val| { val.string().ok() })
            .collect();

        Some((keys, val_res.unwrap()))
    });

    let val = D::map_comprehend(iter);
    Ok(val)
}

fn do_neg<D>(thunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(thunk(doc)?.into_iter().map(|x| -x).collect())
}

fn do_not<D>(thunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(thunk(doc)?.into_iter().map(|x| !x).collect())
}

fn do_mul<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())? // need to clone
        .into_iter()
        .zip(rthunk(doc)?.into_iter())
        .map(|(x,y)| x*y)
        .collect()
    )
}

fn do_div<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())? // need to clone
        .into_iter()
        .zip(rthunk(doc)?.into_iter())
        .map(|(x,y)| x/y)
        .collect()
    )
}

fn do_rem<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .into_iter()
        .zip(rthunk(doc)?.into_iter())
        .map(|(x,y)| x%y)
        .collect()
    )
}

fn do_add<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .into_iter()
        .zip(rthunk(doc)?.into_iter())
        .map(|(x,y)| x+y)
        .collect()
    )
}

fn do_sub<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .into_iter()
        .zip(rthunk(doc)?.into_iter())
        .map(|(x,y)| x-y)
        .collect()
    )
}

fn do_eq<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .iter()
        .zip(rthunk(doc)?.iter())
        .map(|(x,y)| D::from(x == y))
        .collect()
    )
}

fn do_ne<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .iter()
        .zip(rthunk(doc)?.iter())
        .map(|(x,y)| D::from(x != y))
        .collect()
    )
}

fn do_lt<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .iter()
        .zip(rthunk(doc)?.iter())
        .map(|(x,y)| D::from(x < y))
        .collect()
    )
}

fn do_le<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .iter()
        .zip(rthunk(doc)?.iter())
        .map(|(x,y)| D::from(x <= y))
        .collect()
    )
}

fn do_gt<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .iter()
        .zip(rthunk(doc)?.iter())
        .map(|(x,y)| D::from(x > y))
        .collect()
    )
}

fn do_ge<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .iter()
        .zip(rthunk(doc)?.iter())
        .map(|(x,y)| D::from(x >= y))
        .collect()
    )
}

fn do_shr<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .into_iter()
        .zip(rthunk(doc)?.into_iter())
        .map(|(x,y)| x>>y)
        .collect()
    )
}

fn do_shl<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .into_iter()
        .zip(rthunk(doc)?.into_iter())
        .map(|(x,y)| x<<y)
        .collect()
    )
}

fn do_bitand<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .into_iter()
        .zip(rthunk(doc)?.into_iter())
        .map(|(x,y)| x&y)
        .collect()
    )
}

fn do_bitxor<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .into_iter()
        .zip(rthunk(doc)?.into_iter())
        .map(|(x,y)| x^y)
        .collect()
    )
}

fn do_bitor<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .into_iter()
        .zip(rthunk(doc)?.into_iter())
        .map(|(x,y)| x|y)
        .collect()
    )
}

fn do_and<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .into_iter()
        .zip(rthunk(doc)?.into_iter())
        .map(|(x,y)| x.and(y))
        .collect()
    )
}

fn do_or<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone())?
        .into_iter()
        .zip(rthunk(doc)?.into_iter())
        .map(|(x,y)| x.or(y))
        .collect()
    )
}


fn do_pipe<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D)
    -> Result<Output<D>> where D: Document
{
    let mut outs = Vec::new();
    for item in lthunk(doc)? {
        let mut out = rthunk(item)?;
        outs.append(&mut out)
    }
    Ok(outs)
}


//#[cfg(test)]
//mod test {
//    use super::*;
//    use json::{JsonBuf, Json::{String as S}};
//    //use std;
//
//    #[test]
//    fn test_jq_empty() {
//        match parse("").unwrap() {
//            mut thunk@box Thunk::Empty => {
//                let mut out = thunk(S("hello".to_string())).unwrap();
//                assert_eq!(Output::One(S("hello".to_string())), out);
//            },
//            _ => {
//                panic!("unexpected thunk")
//            }
//        }
//    }
//
//    #[test]
//    fn test_jq_empty_ws() {
//        match parse("   ").unwrap() {
//            mut thunk@box Thunk::Empty => {
//                let mut out = thunk(S("hello".to_string())).unwrap();
//                assert_eq!(Output::One(S("hello".to_string())), out);
//            },
//            _ => {
//                panic!("unexpected thunk")
//            }
//        }
//    }
//
//    #[test]
//    fn test_expr_null() {
//        let mut thunk = parse("null").unwrap();
//        let doc = JsonBuf::parse_str("[10]").unwrap();
//        let out = thunk(doc.clone()).unwrap();
//        assert_eq!(Output::One(Json::Null), out)
//    }
//
//    #[test]
//    fn test_expr_true() {
//        let mut thunk = parse("true").unwrap();
//        let doc = JsonBuf::parse_str("[10]").unwrap();
//        let out = thunk(doc.clone()).unwrap();
//        assert_eq!(Output::One(Json::Bool(true)), out);
//    }
//
//    #[test]
//    fn test_expr_false() {
//        let mut thunk = parse("false").unwrap();
//        let doc = JsonBuf::parse_str("[10]").unwrap();
//        let out = thunk(doc.clone()).unwrap();
//        assert_eq!(Output::One(Json::Bool(false)), out);
//    }
//
//    #[test]
//    fn test_expr_int() {
//        let mut thunk = parse("12310231231").unwrap();
//        let doc = JsonBuf::parse_str("[10]").unwrap();
//        let out = thunk(doc.clone()).unwrap();
//        assert_eq!(Output::One(Json::Integer(12310231231)), out);
//    }
//
//    #[test]
//    fn test_expr_float() {
//        let doc = JsonBuf::parse_str("[10]").unwrap();
//        let testcases = vec![
//            ("1.2", 1.2), ("0.2", 0.2), ("1.0", 1.0), ("0.0", 0.0),
//            (".2", 0.2), (".0", 0.0),
//            ("1.2e2", 120.0), ("0.2e-2", 0.002), ("1.0e+2", 100.0),
//            (".2e+0", 0.2), (".0e-0", 0.0),
//        ];
//        for tc in testcases {
//            let mut thunk = parse(tc.0).unwrap();
//            let out = thunk(doc.clone()).unwrap();
//            assert_eq!(Output::One(Json::Float(tc.1)), out);
//        }
//    }
//
//    #[test]
//    fn test_expr_string() {
//        let mut thunk = parse(r#""hello world""#).unwrap();
//        let doc = JsonBuf::parse_str("[10]").unwrap();
//        let out = thunk(doc.clone()).unwrap();
//        assert_eq!(
//            Output::One(S("hello world".to_string())),
//            out
//        );
//    }
//
//    #[test]
//    fn test_expr_array() {
//        use self::Json::{Array, Integer};
//
//        let doc = Json::Null;
//        let mut thunk = parse(r#"[10]"#).unwrap();
//        let out = thunk(doc.clone()).unwrap();
//        assert_eq!(Output::One(Array(vec![Integer(10)])), out);
//    }
//
//    #[test]
//    fn test_expr_object() {
//        use self::Json::{Object, Integer};
//
//        let doc = Json::Null;
//        let mut thunk = parse(r#"{"a": 10}"#).unwrap();
//        assert_eq!(
//            Output::One(Object(vec![KeyValue("a".to_string(), Integer(10))])),
//            thunk(doc.clone()).unwrap()
//        );
//    }
//
//    #[test]
//    fn test_expr_object_index() {
//        let doc = JsonBuf::parse_str(r#"{"a":[1,2],"b":[true,1]}"#).unwrap();
//        let refout = Output::One(JsonBuf::parse_str("[true,1]").unwrap());
//
//        let mut thunk = parse(r#".b"#).unwrap();
//        let res = thunk(doc.clone()).unwrap();
//        assert_eq!(refout, res);
//
//        let mut thunk = parse(r#"."b""#).unwrap();
//        let res = thunk(doc.clone()).unwrap();
//        assert_eq!(refout, res);
//
//        let mut thunk = parse(r#".["b"]"#).unwrap();
//        let res = thunk(doc.clone()).unwrap();
//        assert_eq!(refout, res);
//    }
//
//    #[test]
//    fn test_expr_object_index_opt() {
//        let doc1 = JsonBuf::parse_str(r#"{"a":[1,2],"b":[true,1]}"#).unwrap();
//        let doc2 = JsonBuf::parse_str(r#"null"#).unwrap();
//
//        let mut thunk = parse(r#".not"#).unwrap();
//        assert_eq!(
//            Error::Op(format!("missing key not")),
//            thunk(doc1.clone()).err().unwrap()
//        );
//        assert_eq!(
//            Error::Op(format!("not an object Null")),
//            thunk(doc2.clone()).err().unwrap()
//        );
//
//        let mut thunk = parse(r#"."not""#).unwrap();
//        assert_eq!(
//            Error::Op(format!("missing key not")),
//            thunk(doc1.clone()).err().unwrap()
//        );
//        assert_eq!(
//            Error::Op(format!("not an object Null")),
//            thunk(doc2.clone()).err().unwrap()
//        );
//
//        let mut thunk = parse(r#".["not"]"#).unwrap();
//        assert_eq!(
//            Error::Op(format!("missing key not")),
//            thunk(doc1.clone()).err().unwrap()
//        );
//        assert_eq!(
//            Error::Op(format!("not an object Null")),
//            thunk(doc2.clone()).err().unwrap()
//        );
//    }
//}
