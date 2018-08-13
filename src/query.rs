use std::{result, error, cmp};
use std::fmt::{self};
use std::str::FromStr;

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


#[derive(Debug)]
pub enum Thunk where {
    // Primary thunks
    Empty,
    Identity,
    Recurse,
    Literal(Json),
    IndexShortcut(String, Option<usize>, bool),
    Slice(usize, usize, bool),
    Iterate(Vec<Thunk>, bool),
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

impl FromStr for Thunk {
    type Err=Error;

    fn from_str(text: &str) -> Result<Thunk> {
        let (_, thunk) = parse_program_nom(NS(text))?;
        Ok(thunk)
    }
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

            Slice(start, end, opt) => { // vector of one or more
                use query::Error::Op;

                let res = doc
                    .slice(*start, *end)
                    .ok_or_else( || Op(None, "json not an array".to_string()));
                if *opt && res.is_err() { Ok(vec![]) } else { Ok(res?) }
            },

            Iterate(ref mut thunks, opt) => {
                let res = do_iterate(thunks, *opt, doc);
                if *opt && res.is_err() { Ok(vec![]) } else { Ok(res?) }
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


#[cfg(test)]
mod test {
    use super::*;
    use json::Json;
    use json::Json::{Null, Bool, Integer, Float, Array, Object, String as S};

    #[test]
    fn test_query_empty() {
        let mut thunk: Thunk = "".parse().unwrap();
        let out = thunk(S("hello".to_string())).unwrap();
        assert_eq!(1, out.len());
        assert_eq!(S("hello".to_string()), out[0]);

        let mut thunk: Thunk = "   \t \n ".parse().unwrap();
        let out = thunk(Integer(10)).unwrap();
        assert_eq!(1, out.len());
        assert_eq!(Integer(10), out[0]);
    }

    #[test]
    fn test_query_literal() {
        let doc: Json = "[10]".parse().unwrap();

        let mut thunk: Thunk = "null".parse().unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!("[null]", &format!("{:?}", out));

        thunk = "true".parse().unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!("[true]", &format!("{:?}", out));

        thunk = "false".parse().unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!("[false]", &format!("{:?}", out));

        thunk = "10".parse().unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!("[10]", &format!("{:?}", out));

        thunk = "10.2".parse().unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!("[1.02e1]", &format!("{:?}", out));

        thunk = "\"hello\"".parse().unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!("[\"hello\"]", &format!("{:?}", out));

        thunk = "[10.2]".parse().unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!("[[1.02e1]]", &format!("{:?}", out));

        thunk = "{\"x\": 10.2}".parse().unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!("[{\"x\":1.02e1}]", &format!("{:?}", out));
    }

    #[test]
    fn test_query_identity() {
        let mut thunk: Thunk = ".".parse().unwrap();
        let doc: Json = "null".parse().unwrap();
        assert_eq!("[null]", &format!("{:?}", thunk(doc.clone()).unwrap()));
    }
}
