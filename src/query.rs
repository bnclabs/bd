use std::{result, error, cmp, iter};
use std::fmt::{self};
use std::str::FromStr;

use nom::{self, {types::CompleteStr as NS}};

use util;
use json::{self, Json};
use query_nom::parse_program_nom;
use document::{Document,KeyValue};

// TODO: Parametrise Thunk over different types of Document.
// TODO: Better to replace panic with assert!() macro.
// TODO: Don't use vec![] macro, try to use with_capacity.

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
                Error::Parse(format!("failure at {}", &rem[..rem_till]))
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
    Literal(Json, bool),
    IndexShortcut(Option<String>, Option<isize>, bool),
    Slice(isize, isize, bool),
    IterateValues(bool),
    Iterate(Vec<Thunk>, bool),
    List(Vec<Thunk>, bool),
    Dict(Vec<(Thunk, Option<Thunk>)>, bool),
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
        let (_x, thunk) = parse_program_nom(NS(text))?;
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

            Literal(literal, _) => { // vecot of single item
                Ok(vec![D::from(literal.clone())])
            },

            IndexShortcut(key, off, opt) => { // vector of single item
                let res: Result<D>;
                if key.is_some() {
                    res = do_get_shortcut(key.as_ref().unwrap(), doc);
                } else {
                    res = do_index_shortcut(off.unwrap(), doc);
                }
                if *opt && res.is_err() { Ok(vec![]) } else { Ok(vec![res?]) }
            },

            Slice(start, end, opt) => { // vector of one or more
                use query::Error::Op;

                let res = doc
                    .slice(*start, *end)
                    .ok_or_else( || Op(None, "json not an array".to_string()));
                if *opt && res.is_err() { Ok(vec![]) } else { Ok(vec![res?]) }
            },

            IterateValues(opt) => {
                let res = do_iterate_values(doc);
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

fn do_get_shortcut<D>(key: &String, doc: D)
    -> Result<D> where D: Document
{
    doc.get(key).map_err(Into::into)
}

fn do_index_shortcut<D>(off: isize, doc: D)
    -> Result<D> where D: Document
{
    doc.index(off).map_err(Into::into)
}

fn do_iterate_values<D>(doc: D) -> Result<Output<D>> where D: Document {
    if let Some(iter) = doc.values() {
        Ok(iter.collect())
    } else {
        Err(Error::Op(None, "not an iterable".to_string()))
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

// TODO: handle opt.
fn do_list<D>(thunks: &mut Vec<Thunk>, _opt: bool, doc: D)
    -> Result<Output<D>> where D: Document
{
    let mut out: Vec<D> = Vec::with_capacity(thunks.len());

    for thunk in thunks.iter_mut() {
        let mut vals: Vec<D> = thunk(doc.clone())?;
        out.append(&mut vals);
    }
    Ok(vec![Document::new_array(out)])
}

// TODO: handle opt.
fn do_dict<D>(kv_thunks: &mut Vec<(Thunk, Option<Thunk>)>, _opt: bool, doc: D)
    -> Result<Output<D>> where D: Document
{
    let mut dicts: Vec<Vec<KeyValue<D>>> = vec![vec![]];
    let mut keys: Vec<String> = Vec::with_capacity(kv_thunks.len());
    for (kthunk, vthunk) in kv_thunks.iter_mut() {
        keys.clear();
        for key in kthunk(doc.clone())? {
            keys.push(key.string().map_err(Into::into)?)
        }
        let kvss: Vec<Vec<KeyValue<D>>> = match vthunk {
            Some(ref mut vthunk) => {
                vthunk(doc.clone())?.into_iter().map(|val|
                    keys.clone().into_iter()
                    .zip(iter::repeat(val).take(keys.len()))
                    .map(|(k,v)| KeyValue::new(k,v))
                    .collect()
                ).collect()
            },
            None => {
                let mut vals = Vec::with_capacity(keys.len());
                for k in keys.iter() {
                    vals.push(doc.get_ref(k).map_err(Into::into)?.clone());
                }
                vec![
                    keys.clone().into_iter()
                    .zip(vals).map(|(k,v)| KeyValue::new(k,v)).collect()
                ]
            },
        };
        let mut next_dicts: Vec<Vec<KeyValue<D>>> = Vec::new();
        let n = kvss.len();
        let z =  kvss.into_iter().zip(iter::repeat(dicts).take(n).into_iter());
        for (kvs, mut dicts) in z {
            for dict in dicts.iter_mut() {
                kvs.clone().into_iter().for_each(
                    |kv| util::upsert_object_key(dict, kv)
                );
            }
            next_dicts.append(&mut dicts);
            //println!("loop1 {:?} {:?}", dicts, next_dicts);
        }
        dicts = next_dicts
    }
    Ok(dicts.into_iter().map(Document::new_object).collect())
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


// TODO: add the optional variant ``?`` for each test case.
#[cfg(test)]
mod test {
    use super::*;
    use json::Json;
    use json::Json::{Integer, String as S};

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

        thunk = r#"[10.2, true, null, "hello"]"#.parse().unwrap();
        //println!("{:?}", thunk);
        let out = thunk(doc.clone()).unwrap();
        assert_eq!(r#"[[1.02e1,true,null,"hello"]]"#, &format!("{:?}", out));

        thunk = r#"{"x":12, "y":[10,20], "z":{"a":true}}"#.parse().unwrap();
        //println!("{:?}", thunk);
        let out = thunk(doc.clone()).unwrap();
        let refval = r#"[{"x":12,"y":[10,20],"z":{"a":true}}]"#;
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_identity() {
        let mut thunk: Thunk = ".".parse().unwrap();

        let doc: Json = "null".parse().unwrap();
        assert_eq!("[null]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        let doc: Json = "true".parse().unwrap();
        assert_eq!("[true]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        let doc: Json = "false".parse().unwrap();
        assert_eq!("[false]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        let doc: Json = "10".parse().unwrap();
        assert_eq!("[10]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        let doc: Json = "10.2".parse().unwrap();
        assert_eq!("[1.02e1]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        let doc: Json = "\"hello\"".parse().unwrap();
        assert_eq!("[\"hello\"]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        let doc: Json = "[true,10]".parse().unwrap();
        assert_eq!("[[true,10]]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        let doc: Json = "{\"a\": 10}".parse().unwrap();
        assert_eq!("[{\"a\":10}]", &format!("{:?}", thunk(doc.clone()).unwrap()));
    }

    #[test]
    fn test_query_get() {
        let mut thunk: Thunk = ".foo".parse().unwrap();

        let doc: Json = r#"{"foo": 10}"#.parse().unwrap();
        assert_eq!("[10]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        let doc: Json = r#"{"notfoo": 10}"#.parse().unwrap();
        let err = json::Error::KeyMissing(0, "foo".to_string());
        assert_eq!(
            Err(Error::Op(Some(err), "json op error".to_string())),
            thunk(doc.clone())
        );

        thunk = r#".foo?"#.parse().unwrap();
        let doc: Json = r#"{"nonfoo": 10}"#.parse().unwrap();
        assert_eq!("[]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"."foo""#.parse().unwrap();
        let doc: Json = r#"{"foo": 10}"#.parse().unwrap();
        assert_eq!("[10]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#".["foo"]"#.parse().unwrap();
        let doc: Json = r#"{"foo": 10}"#.parse().unwrap();
        assert_eq!("[10]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#".["foo"]?"#.parse().unwrap();
        let doc: Json = r#"{"nonfoo": 10}"#.parse().unwrap();
        assert_eq!("[]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#".[foo]"#.parse().unwrap();
        let doc: Json = r#"{"foo": 10}"#.parse().unwrap();
        assert_eq!("[10]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#".[foo]?"#.parse().unwrap();
        let doc: Json = r#"{"nonfoo": 10}"#.parse().unwrap();
        assert_eq!("[]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"."foo.bar""#.parse().unwrap();
        let doc: Json = r#"{"foo.bar": 10}"#.parse().unwrap();
        assert_eq!("[10]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#".["foo.bar"]"#.parse().unwrap();
        let doc: Json = r#"{"foo.bar": 10}"#.parse().unwrap();
        assert_eq!("[10]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#".["foo.bar"?]"#.parse().unwrap();
        let doc: Json = r#"{"nonfoo.bar": 10}"#.parse().unwrap();
        assert_eq!("[]", &format!("{:?}", thunk(doc.clone()).unwrap()));
    }

    #[test]
    fn test_query_index() {
        let mut thunk: Thunk = ".0".parse().unwrap();

        let doc: Json = r#"[10, true, "hello"]"#.parse().unwrap();
        assert_eq!("[10]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".2".parse().unwrap();
        let doc: Json = r#"[10, true, "hello"]"#.parse().unwrap();
        assert_eq!("[\"hello\"]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".-2".parse().unwrap();
        let doc: Json = r#"[10, true, "hello"]"#.parse().unwrap();
        assert_eq!("[true]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[0]".parse().unwrap();
        let doc: Json = r#"[10, true, "hello"]"#.parse().unwrap();
        assert_eq!("[10]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[2]".parse().unwrap();
        let doc: Json = r#"[10, true, "hello"]"#.parse().unwrap();
        assert_eq!("[\"hello\"]", &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = "-.[2]".parse().unwrap();
        let doc: Json = r#"[10, true, 20]"#.parse().unwrap();
        assert_eq!("[-20]", &format!("{:?}", thunk(doc.clone()).unwrap()));
    }

    #[test]
    fn test_query_slice_array() {
        let mut thunk: Thunk = ".[2..4]".parse().unwrap();
        let doc: Json = r#"["a", "b", "c", "d", "e"]"#.parse().unwrap();

        let refval = r#"[["c","d"]]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[2..=3]".parse().unwrap();
        let refval = r#"[["c","d"]]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[..3]".parse().unwrap();
        let refval = r#"[["a","b","c"]]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[..=3]".parse().unwrap();
        let refval = r#"[["a","b","c","d"]]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[2..]".parse().unwrap();
        let refval = r#"[["c","d","e"]]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[..]".parse().unwrap();
        let refval = r#"[["a","b","c","d","e"]]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));
    }

    #[test]
    fn test_query_slice_string() {
        let mut thunk: Thunk = ".[2..4]".parse().unwrap();
        let doc: Json = r#""abcdefghi""#.parse().unwrap();

        let refval = r#"["cd"]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[2..=3]".parse().unwrap();
        let refval = r#"["cd"]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[..3]".parse().unwrap();
        let refval = r#"["abc"]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[..=3]".parse().unwrap();
        let refval = r#"["abcd"]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[2..]".parse().unwrap();
        let refval = r#"["cdefghi"]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[..]".parse().unwrap();
        let refval = r#"["abcdefghi"]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));
    }

    #[test]
    fn test_query_iterator() {
        let mut thunk: Thunk = ".[]".parse().unwrap();

        let doc: Json = r#"[1,2,3]"#.parse().unwrap();
        let refval = r#"[1, 2, 3]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[]".parse().unwrap();
        let doc: Json = r#"{"a": true, "b": 2, "c": null}"#.parse().unwrap();
        let refval = r#"[true, 2, null]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[]".parse().unwrap();
        let doc = r#"[{"name":"JSON", "good":true}, {"name":"XML", "good":false}]"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[{"good":true,"name":"JSON"}, {"good":false,"name":"XML"}]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[]".parse().unwrap();
        let doc: Json = r#"[]"#.parse().unwrap();
        let refval = r#"[]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[]".parse().unwrap();
        let doc: Json = r#"{"a": 1, "b": 1}"#.parse().unwrap();
        let refval = r#"[1, 1]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[]".parse().unwrap();
        let doc: Json = r#"10"#.parse().unwrap();
        assert_eq!(
            Err(Error::Op(None, "not an iterable".to_string())),
            thunk(doc.clone())
        );

        thunk = ".[]?".parse().unwrap();
        let doc: Json = r#"10"#.parse().unwrap();
        let refval = r#"[]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[foo, bar]".parse().unwrap();
        let doc = r#"{"foo": 42, "bar": "something else", "baz": true}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[42, "something else"]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[user, projects.[]]".parse().unwrap();
        let doc = r#"{"user":"stedolan", "projects": ["jq", "wikiflow"]}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"["stedolan", "jq", "wikiflow"]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));
    }

    #[test]
    fn test_query_pipe() {
        let mut thunk: Thunk = ".[] | foo".parse().unwrap();

        let doc: Json = r#"[{"foo": 10}, {"foo":20}]"#.parse().unwrap();
        let refval = r#"[10, 20]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = ".[] | .foo ".parse().unwrap();
        let doc: Json = r#"[{"foo": 10}, {"foo":20}]"#.parse().unwrap();
        let refval = r#"[10, 20]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));


        thunk = ".a.b.c".parse().unwrap();
        let doc: Json = r#"{"a": {"b": {"c": 100}}}"#.parse().unwrap();
        let refval = r#"[100]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#".a | .b | .c"#.parse().unwrap();
        let doc: Json = r#"{"a": {"b": {"c": 100}}}"#.parse().unwrap();
        let refval = r#"[100]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#".a | . | .b"#.parse().unwrap();
        let doc: Json = r#"{"a": {"b": 100}}"#.parse().unwrap();
        let refval = r#"[100]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));
    }

    #[test]
    fn test_query_paranthesis() {
        let mut thunk: Thunk = "2 + . * 15".parse().unwrap();

        let doc: Json = r#"10"#.parse().unwrap();
        let refval = r#"[152]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"(2 + .) * 15"#.parse().unwrap();
        let doc: Json = r#"10"#.parse().unwrap();
        let refval = r#"[180]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));
    }

    #[test]
    fn test_query_collection_list() {
        let mut thunk: Thunk = r#"[]"#.parse().unwrap();

        let doc: Json = r#"10"#.parse().unwrap();
        let refval = r#"[[]]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"[1,2,3]"#.parse().unwrap();
        let doc: Json = r#"10"#.parse().unwrap();
        let refval = r#"[[1,2,3]]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"[foo, .bar, baz]"#.parse().unwrap();
        let doc: Json = r#"{"foo": 10, "bar":20, "baz":30}"#.parse().unwrap();
        let refval = r#"[[10,20,30]]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"[.0, .4, .2]"#.parse().unwrap();
        let doc: Json = r#"[10,20,30,40,50]"#.parse().unwrap();
        let refval = r#"[[10,50,30]]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"[.items.[].name]"#.parse().unwrap();
        let doc = r#"{"items": [{"name": "x"}, {"name":"y"}]}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[["x","y"]]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"[.user, .projects.[]]"#.parse().unwrap();
        let doc = r#"{"user":"stedolan", "projects": ["jq", "wikiflow"]}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[["stedolan","jq","wikiflow"]]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));
    }

    #[test]
    fn test_query_collection_object() {
        let mut thunk: Thunk = r#"{"a":42,"b":17}"#.parse().unwrap();

        let doc: Json = r#"10"#.parse().unwrap();
        let refval = r#"[{"a":42,"b":17}]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"{a:42, b:17} "#.parse().unwrap();
        let doc: Json = r#"10"#.parse().unwrap();
        let refval = r#"[{"a":42,"b":17}]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"{(."a"+"-"+."b"):59}"#.parse().unwrap();
        let doc: Json = r#"{"a":"firstname","b":"lastname"}"#.parse().unwrap();
        let refval = r#"[{"firstname-lastname":59}]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"{foo:.bar}"#.parse().unwrap();
        let doc: Json = r#"{"bar":10}"#.parse().unwrap();
        let refval = r#"[{"foo":10}]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"{user: .user, title: .title}"#.parse().unwrap();
        let doc = r#"{"user":"prom","title":"testing","age":30,"city":"ax"}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[{"title":"testing","user":"prom"}]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"{user, title}"#.parse().unwrap();
        let doc = r#"{"user":"prom","title":"testing","age":30,"city":"ax"}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[{"title":"testing","user":"prom"}]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"{(ks.[]), title}"#.parse().unwrap();
        let doc = r#"{"ks":["age","city"],"age":30,"city":"ax","title":null}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[{"age":30,"city":"ax","title":null}]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"{user, title: .titles.[]}"#.parse().unwrap();
        let doc = r#"{"user":"sted","titles":["JQ Primer", "More JQ"]}"#;
        let doc: Json = doc.parse().unwrap();
        let mut refval = r#"[{"title":"JQ Primer","user":"sted"}, "#.to_string();
        refval += r#"{"title":"More JQ","user":"sted"}]"#;
        assert_eq!(&refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"{(.user): .titles}"#.parse().unwrap();
        let doc = r#"{"user":"stedolan","titles":["JQ Primer", "More JQ"]}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[{"stedolan":["JQ Primer","More JQ"]}]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));
    }

    #[test]
    fn test_query_recurse() {
        let mut thunk: Thunk = r#"..|.a?"#.parse().unwrap();

        let doc: Json = r#"[[{"a":1}, {"a":2}],{"a":3}]"#.parse().unwrap();
        let refval = r#"[1, 2, 3]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));
    }

    #[test]
    fn test_query_addition() {
        let mut thunk: Thunk = r#"a+b"#.parse().unwrap();

        let doc: Json = r#"{"a":1,"b":2}"#.parse().unwrap();
        let refval = r#"[3]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a+b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":2.3}"#.parse().unwrap();
        let refval = r#"[3.5e0]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a+b+c"#.parse().unwrap();
        let doc: Json = r#"{"a":[1,2],"b":[],"c":[3,4]}"#.parse().unwrap();
        let refval = r#"[[1,2,3,4]]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a+b+c"#.parse().unwrap();
        let doc: Json = r#"{"a":"hello","b":"","c":"world"}"#.parse().unwrap();
        let refval = r#"["helloworld"]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a+b+c"#.parse().unwrap();
        let doc = r#"{"a":{"x":1},"b":{"x":2},"c":{"y":2}}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[{"x":2,"y":2}]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));
    }

    #[test]
    fn test_query_subraction() {
        let mut thunk: Thunk = r#"a-b"#.parse().unwrap();

        let doc: Json = r#"{"a":1,"b":2}"#.parse().unwrap();
        let refval = r#"[-1]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a-b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.3,"b":2.1}"#.parse().unwrap();
        let refval = r#"[-8e-1]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a-b"#.parse().unwrap();
        let doc: Json = r#"{"a":[1,2],"b":[2]}"#.parse().unwrap();
        let refval = r#"[[1]]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));
    }

    #[test]
    fn test_query_multiplication() {
        let mut thunk: Thunk = r#"a*b"#.parse().unwrap();

        let doc: Json = r#"{"a":1,"b":2}"#.parse().unwrap();
        let refval = r#"[2]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a*b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":2}"#.parse().unwrap();
        let refval = r#"[2.4e0]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a*b"#.parse().unwrap();
        let doc: Json = r#"{"a":5,"b":2.5}"#.parse().unwrap();
        let refval = r#"[1.25e1]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a*b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":2.1}"#.parse().unwrap();
        let refval = r#"[2.52e0]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a*b"#.parse().unwrap();
        let doc: Json = r#"{"a":"hello","b":0}"#.parse().unwrap();
        let refval = r#"[null]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a*b"#.parse().unwrap();
        let doc: Json = r#"{"a":"hello","b":2}"#.parse().unwrap();
        let refval = r#"["hellohello"]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a*b"#.parse().unwrap();
        let doc: Json = r#"{"a":{"x":1},"b":{"y":2}}"#.parse().unwrap();
        let refval = r#"[{"x":1,"y":2}]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));
    }

    #[test]
    fn test_query_division() {
        let mut thunk: Thunk = r#"a/b"#.parse().unwrap();

        let doc: Json = r#"{"a":1,"b":2}"#.parse().unwrap();
        let refval = r#"[5e-1]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a/b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":2}"#.parse().unwrap();
        let refval = r#"[6e-1]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a/b"#.parse().unwrap();
        let doc: Json = r#"{"a":5,"b":2.5}"#.parse().unwrap();
        let refval = r#"[2e0]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a/b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":2.1}"#.parse().unwrap();
        let refval = r#"[5.714285714285714e-1]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a/b"#.parse().unwrap();
        let doc: Json = r#"{"a":1,"b":0}"#.parse().unwrap();
        let refval = r#"[null]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a/b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":0}"#.parse().unwrap();
        let refval = r#"[null]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a/b"#.parse().unwrap();
        let doc: Json = r#"{"a":"a,b,c,d","b":","}}"#.parse().unwrap();
        let refval = r#"[["a","b","c","d"]]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));
    }

    #[test]
    fn test_query_rem() {
        let mut thunk: Thunk = r#"a%b"#.parse().unwrap();

        let doc: Json = r#"{"a":1,"b":2}"#.parse().unwrap();
        let refval = r#"[1]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a%b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":2}"#.parse().unwrap();
        let refval = r#"[1.2e0]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a%b"#.parse().unwrap();
        let doc: Json = r#"{"a":5,"b":2.5}"#.parse().unwrap();
        let refval = r#"[0e0]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a%b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":2.1}"#.parse().unwrap();
        let refval = r#"[1.2e0]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a%b"#.parse().unwrap();
        let doc: Json = r#"{"a":1,"b":0}"#.parse().unwrap();
        let refval = r#"[null]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));

        thunk = r#"a%b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":0}"#.parse().unwrap();
        let refval = r#"[null]"#;
        assert_eq!(refval, &format!("{:?}", thunk(doc.clone()).unwrap()));
    }
}
