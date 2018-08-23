use std::{result, error, cmp, iter};
use std::fmt::{self};
use std::str::FromStr;
use std::ops::{IndexMut};
use std::collections::HashMap;

use nom::{self, {types::CompleteStr as NS}};

use json::{self};
use query_nom::parse_program_nom;
use document::{self, Document, Doctype, Docitem, DocIterator};
use document::{ArrayItem, Property};

// TODO: Parametrise Thunk over different types of Document.
// TODO: Better to replace panic with assert!() macro.
// TODO: Don't use vec![] macro, try to use with_capacity.
// TODO: ? operator is not consistent and intuitive, a through review
//       and test cases is required.

type Output<D> = Vec<D>;


pub type Result<T> = result::Result<T,Error>;


#[derive(Debug,Eq,PartialEq)]
pub enum Error {
    InvalidKey,
    Parse(String),
    ParseJson(json::Error),
    Op(Option<json::Error>, String),
    InvalidArg(String),
    InvalidFunction(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use query::Error::*;

        match self {
            InvalidKey => write!(f, "key must be string type"),
            Parse(s) => write!(f, "{}", s),
            ParseJson(err) => write!(f, "parsing json, {}", err),
            Op(Some(err), s) => write!(f, "{} error, {}", s, err),
            Op(None, s) => write!(f, "op error, {}", s),
            InvalidArg(s) => write!(f, "{}", s),
            InvalidFunction(s) => write!(f, "{}", s),
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
                Error::Op(Some(e), "json op".to_string())
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


#[derive(Debug,Clone)]
pub enum Thunk where {
    // Primary thunks
    Empty,
    Identity,
    Recurse,
    // Literals
    Null(bool),
    Bool(bool, bool),
    Integer(i128, bool),
    Float(f64, bool),
    String(String, bool),
    // expressions
    IndexShortcut(Option<String>, Option<isize>, bool),
    Identifier(String, bool),
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

impl<'a, D> FnMut<(D,&'a mut Context<D>)> for Thunk where D: Document {
    extern "rust-call" fn call_mut(&mut self, args: (D, &mut Context<D>))
        -> Self::Output
    {
        use query::Thunk::*;

        let (doc, c) = args;

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

            // literal
            Null(_) => {
                Ok(vec![D::null()])
            },
            Bool(val, _) => {
                Ok(vec![From::from(*val)])
            },
            Integer(val, _) => {
                Ok(vec![From::from(*val)])
            },
            Float(val, _) => {
                Ok(vec![From::from(*val)])
            },
            String(val, _) => {
                Ok(vec![From::from(val.clone())])
            },


            IndexShortcut(key, off, opt) => { // vector of single item
                let res: Result<Output<D>>;
                if key.is_some() {
                    res = do_get_shortcut(key.as_ref().unwrap().as_str(), doc, c);
                } else {
                    res = do_index_shortcut(off.unwrap(), doc, c);
                }
                if *opt && res.is_err() { Ok(vec![]) } else { Ok(res?) }
            },

            Identifier(symbol, opt) => {
                let res = do_identifier(symbol, doc, c);
                if *opt && res.is_err() { Ok(vec![]) } else { Ok(res?) }
            },

            Slice(start, end, opt) => { // vector of one or more
                use query::Error::Op;

                let res = doc
                    .slice(*start, *end)
                    .ok_or_else( || Op(None, "doc not an array".to_string()));
                if *opt && res.is_err() { Ok(vec![]) } else { Ok(vec![res?]) }
            },

            IterateValues(opt) => {
                let res = do_iterate_values(doc, c);
                if *opt && res.is_err() { Ok(vec![]) } else { Ok(res?) }
            },

            Iterate(ref mut thunks, opt) => {
                let res = do_iterate(thunks, *opt, doc, c);
                if *opt && res.is_err() { Ok(vec![]) } else { Ok(res?) }
            },

            List(ref mut thunks, opt) => do_list(thunks, *opt, doc, c),
            Dict(ref mut kv_thunks, opt) => do_dict(kv_thunks, *opt, doc, c),

            Neg(ref mut thunk) => do_neg(thunk, doc, c),
            Not(ref mut thunk) => do_not(thunk, doc, c),

            Mult(ref mut ltnk, ref mut rtnk) => do_mul(ltnk, rtnk, doc, c),
            Div(ref mut ltnk, ref mut rtnk) => do_div(ltnk, rtnk, doc, c),
            Rem(ref mut ltnk, ref mut rtnk) => do_rem(ltnk, rtnk, doc, c),

            Add(ref mut ltnk, ref mut rtnk) => do_add(ltnk, rtnk, doc, c),
            Sub(ref mut ltnk, ref mut rtnk) => do_sub(ltnk, rtnk, doc, c),

            Eq(ref mut ltnk, ref mut rtnk) => do_eq(ltnk, rtnk, doc, c),
            Ne(ref mut ltnk, ref mut rtnk) => do_ne(ltnk, rtnk, doc, c),
            Lt(ref mut ltnk, ref mut rtnk) => do_lt(ltnk, rtnk, doc, c),
            Le(ref mut ltnk, ref mut rtnk) => do_le(ltnk, rtnk, doc, c),
            Gt(ref mut ltnk, ref mut rtnk) => do_gt(ltnk, rtnk, doc, c),
            Ge(ref mut ltnk, ref mut rtnk) => do_ge(ltnk, rtnk, doc, c),

            Shr(ref mut ltnk, ref mut rtnk) => do_shr(ltnk, rtnk, doc, c),
            Shl(ref mut ltnk, ref mut rtnk) => do_shl(ltnk, rtnk, doc, c),
            BitAnd(ref mut ltnk, ref mut rtnk) => do_bitand(ltnk, rtnk, doc, c),
            BitXor(ref mut ltnk, ref mut rtnk) => do_bitxor(ltnk, rtnk, doc, c),
            BitOr(ref mut ltnk, ref mut rtnk) => do_bitor(ltnk, rtnk, doc, c),

            And(ref mut ltnk, ref mut rtnk) => do_and(ltnk, rtnk, doc, c),
            Or(ref mut ltnk, ref mut rtnk) => do_or(ltnk, rtnk, doc, c),

            Pipe(ref mut ltnk, ref mut rtnk) => do_pipe(ltnk, rtnk, doc, c),

            Builtin(funcname, ref mut args) => {
                if let Some(func) = c.get_name(funcname) {
                    func(args, doc, c)
                } else {
                    Err(Error::InvalidFunction(format!("{}", funcname)))
                }
            },
        }
    }
}

impl<'a, D> FnOnce<(D, &'a mut Context<D>)> for Thunk where D:Document {
    type Output=Result<Output<D>>;

    extern "rust-call" fn call_once(mut self, args: (D, &mut Context<D>))
        -> Self::Output
    {
        self.call_mut(args)
    }
}

fn do_get_shortcut<D>(key: &str, doc: D, _: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    if let Some(val) = doc.get_ref(key) {
        Ok(vec![val.clone()])
    } else {
        let dt = doc.doctype();
        Err(Error::Op(None, format!("cannot index {} into {:?}", key, dt)))
    }
}

fn do_index_shortcut<D>(off: isize, doc: D, _: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    if let Some(val) = doc.index_ref(off) {
        Ok(vec![val.clone()])
    } else {
        let dt = doc.doctype();
        Err(Error::Op( None, format!("cannot index {} into {:?}", off, dt)))
    }
}

fn do_identifier<D>(symbol: &str, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    if let Some(func) = c.get_name(symbol) {
        let mut nil_thunks = Vec::new();
        return func(&mut nil_thunks, doc, c)
    }
    do_get_shortcut(symbol, doc, c)
}

fn do_iterate_values<D>(doc: D, _: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    let dt = doc.doctype();
    match docvalues(doc) {
        Some(vals) => Ok(vals),
        None => Err(Error::Op(None, format!("{:?} not an iterable", dt))),
    }
}

fn do_iterate<D>(thunks: &mut Vec<Thunk>, opt: bool, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    let mut out = Vec::new();
    for thunk in thunks.iter_mut() {
        let mut res = thunk(doc.clone(), c);
        if opt && res.is_err() { continue }
        out.append(&mut res?)
    }
    Ok(out)
}

// TODO: handle opt.
fn do_list<D>(thunks: &mut Vec<Thunk>, _opt: bool, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    let mut out: Vec<D> = Vec::with_capacity(thunks.len());

    for thunk in thunks.iter_mut() {
        let mut vals: Vec<D> = thunk(doc.clone(), c)?;
        out.append(&mut vals);
    }
    Ok(vec![From::from(out)])
}

// TODO: handle opt.
fn do_dict<D>(
    kv_thunks: &mut Vec<(Thunk, Option<Thunk>)>, _opt: bool,
    doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    let mut dicts: Vec<Vec<Property<D>>> = vec![vec![]];
    let mut keys: Vec<String> = Vec::with_capacity(kv_thunks.len());
    for (kthunk, vthunk) in kv_thunks.iter_mut() {
        keys.clear();
        for key in kthunk(doc.clone(), c)? {
            keys.push(key.string().ok_or(Error::InvalidKey)?);
        }
        let kvss: Vec<Vec<Property<D>>> = match vthunk {
            Some(ref mut vthunk) => {
                vthunk(doc.clone(), c)?.into_iter().map(|val|
                    keys.clone().into_iter()
                    .zip(iter::repeat(val).take(keys.len()))
                    .map(|(k,v)| Property::new(k,v))
                    .collect()
                ).collect()
            },
            None => {
                let mut vals = Vec::with_capacity(keys.len());
                for k in keys.iter() {
                    match doc.get_ref(k) {
                        Some(val) => vals.push(val.clone()),
                        None => {
                            let dt = doc.doctype();
                            Error::Op(
                                None,
                                format!("cannot index {} into {:?}", k, dt)
                            );
                        },
                    }
                }
                vec![
                    keys.clone().into_iter()
                    .zip(vals).map(|(k,v)| Property::new(k,v)).collect()
                ]
            },
        };
        let mut next_dicts: Vec<Vec<Property<D>>> = Vec::new();
        let n = kvss.len();
        let z =  kvss.into_iter().zip(iter::repeat(dicts).take(n).into_iter());
        for (kvs, mut dicts) in z {
            for dict in dicts.iter_mut() {
                kvs.clone().into_iter().for_each(
                    |kv| document::upsert_object_key(dict, kv)
                );
            }
            next_dicts.append(&mut dicts);
            //println!("loop1 {:?} {:?}", dicts, next_dicts);
        }
        dicts = next_dicts
    }
    Ok(dicts.into_iter().map(From::from).collect())
}

fn do_neg<D>(thunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(thunk(doc, c)?.into_iter().map(|x| -x).collect())
}

fn do_not<D>(thunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(thunk(doc, c)?.into_iter().map(|x| !x).collect())
}

fn do_mul<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone(), c)? // need to clone
        .into_iter()
        .zip(rthunk(doc, c)?.into_iter())
        .map(|(x,y)| x*y)
        .collect()
    )
}

fn do_div<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone(), c)? // need to clone
        .into_iter()
        .zip(rthunk(doc, c)?.into_iter())
        .map(|(x,y)| x/y)
        .collect()
    )
}

fn do_rem<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone(), c)?
        .into_iter()
        .zip(rthunk(doc, c)?.into_iter())
        .map(|(x,y)| x%y)
        .collect()
    )
}

fn do_add<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone(), c)?
        .into_iter()
        .zip(rthunk(doc, c)?.into_iter())
        .map(|(x,y)| x+y)
        .collect()
    )
}

fn do_sub<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone(), c)?
        .into_iter()
        .zip(rthunk(doc, c)?.into_iter())
        .map(|(x,y)| x-y)
        .collect()
    )
}

fn do_eq<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone(), c)?
        .iter()
        .zip(rthunk(doc, c)?.iter())
        .map(|(x,y)| D::from(x == y))
        .collect()
    )
}

fn do_ne<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone(), c)?
        .iter()
        .zip(rthunk(doc, c)?.iter())
        .map(|(x,y)| D::from(x != y))
        .collect()
    )
}

fn do_lt<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone(), c)?
        .iter()
        .zip(rthunk(doc, c)?.iter())
        .map(|(x,y)| D::from(x < y))
        .collect()
    )
}

fn do_le<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone(), c)?
        .iter()
        .zip(rthunk(doc, c)?.iter())
        .map(|(x,y)| D::from(x <= y))
        .collect()
    )
}

fn do_gt<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone(), c)?
        .iter()
        .zip(rthunk(doc, c)?.iter())
        .map(|(x,y)| D::from(x > y))
        .collect()
    )
}

fn do_ge<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone(), c)?
        .iter()
        .zip(rthunk(doc, c)?.iter())
        .map(|(x,y)| D::from(x >= y))
        .collect()
    )
}

fn do_shr<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone(), c)?
        .into_iter()
        .zip(rthunk(doc, c)?.into_iter())
        .map(|(x,y)| x>>y)
        .collect()
    )
}

fn do_shl<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone(), c)?
        .into_iter()
        .zip(rthunk(doc, c)?.into_iter())
        .map(|(x,y)| x<<y)
        .collect()
    )
}

fn do_bitand<D>(ltk: &mut Thunk, rtk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(ltk(doc.clone(), c)?
        .into_iter()
        .zip(rtk(doc, c)?.into_iter())
        .map(|(x,y)| x&y)
        .collect()
    )
}

fn do_bitxor<D>(ltk: &mut Thunk, rtk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(ltk(doc.clone(), c)?
        .into_iter()
        .zip(rtk(doc, c)?.into_iter())
        .map(|(x,y)| x^y)
        .collect()
    )
}

fn do_bitor<D>(ltk: &mut Thunk, rtk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(ltk(doc.clone(), c)?
        .into_iter()
        .zip(rtk(doc, c)?.into_iter())
        .map(|(x,y)| x|y)
        .collect()
    )
}

fn do_and<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone(), c)?
        .into_iter()
        .zip(rthunk(doc, c)?.into_iter())
        .map(|(x,y)| x.and(y))
        .collect()
    )
}

fn do_or<D>(lthunk: &mut Thunk, rthunk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    Ok(lthunk(doc.clone(), c)?
        .into_iter()
        .zip(rthunk(doc, c)?.into_iter())
        .map(|(x,y)| x.or(y))
        .collect()
    )
}


fn do_pipe<D>(ltk: &mut Thunk, rtk: &mut Thunk, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    let mut outs = Vec::new();
    for item in ltk(doc, c)? {
        let mut out = rtk(item, c)?;
        outs.append(&mut out)
    }
    Ok(outs)
}


fn builtin_length<D>(args: &mut Vec<Thunk>, doc: D, _c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    assert_args_len(&args, 0)?;
    let dt = doc.doctype();
    let res = doc.len()
        .ok_or_else( || Error::Op(None, format!("no length for {:?}", dt)));
    Ok(vec![res?])
}

fn builtin_chars<D>(args: &mut Vec<Thunk>, doc: D, _c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    assert_args_len(&args, 0)?;
    let dt = doc.doctype();
    match dt {
        Doctype::String => {
            let out: Vec<ArrayItem<D>> = doc.into_iter().unwrap().collect();
            Ok(vec![From::from(out)])
        },
        _ => Err(Error::Op(None, format!("{:?} not a string", dt)))
    }
}

fn builtin_keys<D>(args: &mut Vec<Thunk>, doc: D, _c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    assert_args_len(&args, 0)?;
    let dt = doc.doctype();
    match dt {
        Doctype::Array => {
            let out: Vec<ArrayItem<D>> = doc.into_iter().unwrap()
                .enumerate()
                .map(
                    |(i, x): (usize, ArrayItem<D>)|
                    ArrayItem::new(i as i32, From::from(x.key() as i128))
                ).collect();
            Ok(vec![From::from(out)])
        },
        Doctype::Object => {
            let out: Vec<ArrayItem<D>> = doc.into_iter().unwrap()
                .enumerate()
                .map(
                    |(i, x): (usize, Property<D>)|
                    ArrayItem::new(i as i32, From::from(x.key()))
                ).collect();
            Ok(vec![From::from(out)])
        },
        _ => Err(Error::Op(None, format!("{:?} not iterable", dt))),
    }
}

fn builtin_has<D>(args: &mut Vec<Thunk>, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    assert_args_len(&args, 1)?;
    let item = args.index_mut(0)(doc.clone(), c)?.remove(0);
    let dt = doc.doctype();
    match dt {
        Doctype::Array => {
            let out = <D as DocIterator<i32,D>>::iter(&doc).unwrap()
                .any(|x: &ArrayItem<D>| {
                    let value = x.value_ref();
                    if value == &item { true } else { false }
                });
            Ok(vec![From::from(out)])
        },
        Doctype::Object => {
            let out = <D as DocIterator<String,D>>::iter(&doc).unwrap()
                .any(|x: &Property<D>| {
                    let key: D = From::from(x.key_ref().clone());
                    if key == item { true } else { false }
                });
            Ok(vec![From::from(out)])
        },
        _ => Err(Error::Op(None, format!("{:?} not iterable", dt))),
    }
}

fn builtin_indoc<D>(args: &mut Vec<Thunk>, item: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    assert_args_len(&args, 1)?;
    let doc = args.index_mut(0)(item.clone(), c)?.remove(0);
    let dt = doc.doctype();
    match dt {
        Doctype::Array => {
            let out = <D as DocIterator<i32,D>>::iter(&doc).unwrap()
                .any(|x| {
                    let value = x.value_ref();
                    if value == &item { true } else { false }
                });
            Ok(vec![From::from(out)])
        },
        Doctype::Object => {
            let out = <D as DocIterator<String,D>>::iter(&doc).unwrap()
                .any(|x| {
                    let key: D = From::from(x.key_ref().clone());
                    if key == item { true } else { false }
                });
            Ok(vec![From::from(out)])
        },
        _ => Err(Error::Op(None, format!("{:?} not iterable", dt))),
    }
}

fn builtin_map<D>(args: &mut Vec<Thunk>, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    assert_args_len(&args, 1)?;
    let thunk = args.index_mut(0);
    let dt = doc.doctype();
    match dt {
        Doctype::String | Doctype::Array => {
            let mut out = Vec::new();
            for item in <D as DocIterator<i32,D>>::into_iter(doc).unwrap() {
                out.push(thunk(item.value(), c)?.remove(0));
            }
            Ok(vec![From::from(out)])
        },
        Doctype::Object => {
            let mut out = Vec::new();
            for item in <D as DocIterator<String,D>>::into_iter(doc).unwrap() {
                let key = item.key_ref().clone();
                out.push(Property::new(key, thunk(item.value(), c)?.remove(0)));
            }
            Ok(vec![From::from(out)])
        },
        _ => Err(Error::Op(None, format!("cannot map over {:?}", dt))),
    }
}

fn builtin_any<D>(args: &mut Vec<Thunk>, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    assert_args_len(&args, 1)?;
    let thunk = args.index_mut(0);
    let dt = doc.doctype();
    match dt {
        Doctype::String | Doctype::Array => {
            for item in <D as DocIterator<i32,D>>::into_iter(doc).unwrap() {
                let out = thunk(item.value(), c)?.remove(0);
                if out.boolean().unwrap_or(false) {
                    return Ok(vec![From::from(true)])
                }
            }
            return Ok(vec![From::from(false)])
        },
        Doctype::Object => {
            for item in <D as DocIterator<String,D>>::into_iter(doc).unwrap() {
                let out = thunk(item.value(), c)?.remove(0);
                if out.boolean().unwrap_or(false) {
                    return Ok(vec![From::from(true)])
                }
            }
            return Ok(vec![From::from(false)])
        },
        _ => Err(Error::Op(None, format!("cannot iterate over {:?}", dt))),
    }
}

fn builtin_all<D>(args: &mut Vec<Thunk>, doc: D, c: &mut Context<D>)
    -> Result<Output<D>> where D: Document
{
    assert_args_len(&args, 1)?;
    let thunk = args.index_mut(0);
    let dt = doc.doctype();
    match dt {
        Doctype::String | Doctype::Array => {
            for item in <D as DocIterator<i32,D>>::into_iter(doc).unwrap() {
                let out = thunk(item.value(), c)?.remove(0);
                if out.boolean().unwrap_or(false) == false {
                    return Ok(vec![From::from(false)])
                }
            }
            return Ok(vec![From::from(true)])
        },
        Doctype::Object => {
            for item in <D as DocIterator<String,D>>::into_iter(doc).unwrap() {
                let out = thunk(item.value(), c)?.remove(0);
                if out.boolean().unwrap_or(false) == false {
                    return Ok(vec![From::from(false)])
                }
            }
            return Ok(vec![From::from(true)])
        },
        _ => Err(Error::Op(None, format!("cannot iterate over {:?}", dt))),
    }
}

fn docvalues<D>(doc: D) -> Option<Vec<D>> where D: Document {
    let dt = doc.doctype();
    match dt {
        Doctype::Array => {
            let iter = <D as DocIterator<i32,D>>::into_iter(doc).unwrap();
            Some(iter.map(|x| x.value()).collect())
        },
        Doctype::Object => {
            let iter = <D as DocIterator<String,D>>::into_iter(doc).unwrap();
            Some(iter.map(|x| x.value()).collect())
        },
        _ => None
    }
}


fn assert_args_len(args: &Vec<Thunk>, n: usize) -> Result<()> {
    if args.len() != n {
        let err = format!("expected {} args, got {}", n, args.len());
        return Err(Error::InvalidArg(err))
    }
    Ok(())
}

#[allow(dead_code)] // TODO: Can be removed.
fn assert_iterator<D>(outs: &Output<D>) -> Result<()> where D: Document {
    if outs.len() < 2 {
        let err = format!("output is not an iterator {}", outs.len());
        return Err(Error::InvalidArg(err))
    }
    Ok(())
}

#[allow(dead_code)]
fn assert_singular<D>(outs: &Output<D>) -> Result<()> where D: Document {
    if outs.len() == 1 {
        let err = format!("output is an iterator {}", outs.len());
        return Err(Error::InvalidArg(err))
    }
    Ok(())
}

pub type Callable<D>
    = fn(&mut Vec<Thunk>, D, &mut Context<D>) -> Result<Output<D>>;

#[derive(Clone)]
pub struct Context<D> where D: Document {
    namespace: HashMap<String, Callable<D>>,
}

impl<D> Context<D> where D: Document {
    pub fn new() -> Context<D> {
        let namespace = HashMap::with_capacity(64);
        let mut c = Context{namespace};
        c.set_name("length", builtin_length);
        c.set_name("chars", builtin_chars);
        c.set_name("keys", builtin_keys);
        c.set_name("has", builtin_has);
        c.set_name("in", builtin_indoc);
        c.set_name("map", builtin_map);
        c.set_name("any", builtin_any);
        c.set_name("all", builtin_all);
        c
    }

    //fn mixin_namespace(&mut self, context: &Context<D>) {
    //    for (k, v) in context.namespace.iter() {
    //        self.namespace.insert(k.to_string(), v.clone());
    //    }
    //}

    fn get_name(&self, name: &str) -> Option<Callable<D>> {
        self.namespace.get(name).cloned()
    }

    pub fn set_name(&mut self, name: &str, value: Callable<D>) {
        self.namespace.insert(name.to_string(), value);
    }

    //fn del_name(&mut self, name: &str) {
    //    self.namespace.remove(name);
    //}

    pub fn eval(&mut self, thunk: &mut Thunk, doc: D)
        -> Result<Output<D>> where D: Document
    {
        thunk(doc, self)
    }
}



// TODO: add the optional variant ``?`` for each test case.
#[cfg(test)]
mod test {
    use super::*;
    use json::Json;
    use json::Json::{Integer, String as S};

    #[test]
    fn test_query_empty() {
        let mut c = Context::new();

        let mut thunk: Thunk = "".parse().unwrap();
        let out = thunk(S("hello".to_string()), &mut c).unwrap();
        assert_eq!(1, out.len());
        assert_eq!(S("hello".to_string()), out[0]);

        let mut thunk: Thunk = "   \t \n ".parse().unwrap();
        let out = thunk(Integer(10), &mut c).unwrap();
        assert_eq!(1, out.len());
        assert_eq!(Integer(10), out[0]);
    }

    #[test]
    fn test_query_literal() {
        let doc: Json = "[10]".parse().unwrap();
        let mut c = Context::new();

        let mut thunk: Thunk = "null".parse().unwrap();
        let out = c.eval(&mut thunk, doc.clone()).unwrap();
        assert_eq!("[null]", &format!("{:?}", out));

        thunk = "true".parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[true]", &format!("{:?}", out));

        thunk = "false".parse().unwrap();
        let out = c.eval(&mut thunk, doc.clone()).unwrap();
        assert_eq!("[false]", &format!("{:?}", out));

        thunk = "10".parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[10]", &format!("{:?}", out));

        thunk = "10.2".parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[1.02e1]", &format!("{:?}", out));

        thunk = "\"hello\"".parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[\"hello\"]", &format!("{:?}", out));

        thunk = r#"[10.2, true, null, "hello"]"#.parse().unwrap();
        //println!("{:?}", thunk);
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(r#"[[1.02e1,true,null,"hello"]]"#, &format!("{:?}", out));

        thunk = r#"{"x":12, "y":[10,20], "z":{"a":true}}"#.parse().unwrap();
        //println!("{:?}", thunk);
        let out = thunk(doc.clone(), &mut c).unwrap();
        let refval = r#"[{"x":12,"y":[10,20],"z":{"a":true}}]"#;
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_identity() {
        let mut thunk: Thunk = ".".parse().unwrap();
        let mut c = Context::new();

        let doc: Json = "null".parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[null]", &format!("{:?}", out));

        let doc: Json = "true".parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[true]", &format!("{:?}", out));

        let doc: Json = "false".parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[false]", &format!("{:?}", out));

        let doc: Json = "10".parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[10]", &format!("{:?}", out));

        let doc: Json = "10.2".parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[1.02e1]", &format!("{:?}", out));

        let doc: Json = "\"hello\"".parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[\"hello\"]", &format!("{:?}", out));

        let doc: Json = "[true,10]".parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[[true,10]]", &format!("{:?}", out));

        let doc: Json = "{\"a\": 10}".parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[{\"a\":10}]", &format!("{:?}", out));
    }

    #[test]
    fn test_query_get() {
        let mut thunk: Thunk = ".foo".parse().unwrap();
        let mut c = Context::new();

        let doc: Json = r#"{"foo": 10}"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[10]", &format!("{:?}", out));

        let doc: Json = r#"{"notfoo": 10}"#.parse().unwrap();
        let err = "cannot index foo into Object".to_string();
        assert_eq!(Err(Error::Op(None, err)), thunk(doc.clone(), &mut c));

        thunk = r#".foo?"#.parse().unwrap();
        let doc: Json = r#"{"nonfoo": 10}"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[]", &format!("{:?}", out));

        thunk = r#"."foo""#.parse().unwrap();
        let doc: Json = r#"{"foo": 10}"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[10]", &format!("{:?}", out));

        thunk = r#".["foo"]"#.parse().unwrap();
        let doc: Json = r#"{"foo": 10}"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[10]", &format!("{:?}", out));

        thunk = r#".["foo"]?"#.parse().unwrap();
        let doc: Json = r#"{"nonfoo": 10}"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[]", &format!("{:?}", out));

        thunk = r#".[foo]"#.parse().unwrap();
        let doc: Json = r#"{"foo": 10}"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[10]", &format!("{:?}", out));

        thunk = r#".[foo]?"#.parse().unwrap();
        let doc: Json = r#"{"nonfoo": 10}"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[]", &format!("{:?}", out));

        thunk = r#"."foo.bar""#.parse().unwrap();
        let doc: Json = r#"{"foo.bar": 10}"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[10]", &format!("{:?}", out));

        thunk = r#".["foo.bar"]"#.parse().unwrap();
        let doc: Json = r#"{"foo.bar": 10}"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[10]", &format!("{:?}", out));

        thunk = r#".["foo.bar"?]"#.parse().unwrap();
        let doc: Json = r#"{"nonfoo.bar": 10}"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[]", &format!("{:?}", out));
    }

    #[test]
    fn test_query_index() {
        let mut thunk: Thunk = ".0".parse().unwrap();
        let mut c = Context::new();

        let doc: Json = r#"[10, true, "hello"]"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[10]", &format!("{:?}", out));

        thunk = ".2".parse().unwrap();
        let doc: Json = r#"[10, true, "hello"]"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[\"hello\"]", &format!("{:?}", out));

        thunk = ".2".parse().unwrap();
        let doc: Json = r#"[10, true, "hello"]"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(r#"["hello"]"#, &format!("{:?}", out));

        thunk = ".[0]".parse().unwrap();
        let doc: Json = r#"[10, true, "hello"]"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[10]", &format!("{:?}", out));

        thunk = ".[2]".parse().unwrap();
        let doc: Json = r#"[10, true, "hello"]"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[\"hello\"]", &format!("{:?}", out));

        thunk = "-.[2]".parse().unwrap();
        let doc: Json = r#"[10, true, 20]"#.parse().unwrap();
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!("[-20]", &format!("{:?}", out));
    }

    #[test]
    fn test_query_slice_array() {
        let mut thunk: Thunk = ".[2..4]".parse().unwrap();
        let doc: Json = r#"["a", "b", "c", "d", "e"]"#.parse().unwrap();
        let mut c = Context::new();

        let refval = r#"[["c","d"]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[2..=3]".parse().unwrap();
        let refval = r#"[["c","d"]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[..3]".parse().unwrap();
        let refval = r#"[["a","b","c"]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[..=3]".parse().unwrap();
        let refval = r#"[["a","b","c","d"]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[2..]".parse().unwrap();
        let refval = r#"[["c","d","e"]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[..]".parse().unwrap();
        let refval = r#"[["a","b","c","d","e"]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_slice_string() {
        let mut thunk: Thunk = ".[2..4]".parse().unwrap();
        let doc: Json = r#""abcdefghi""#.parse().unwrap();
        let mut c = Context::new();

        let refval = r#"["cd"]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[2..=3]".parse().unwrap();
        let refval = r#"["cd"]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[..3]".parse().unwrap();
        let refval = r#"["abc"]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[..=3]".parse().unwrap();
        let refval = r#"["abcd"]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[2..]".parse().unwrap();
        let refval = r#"["cdefghi"]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[..]".parse().unwrap();
        let refval = r#"["abcdefghi"]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_iterator() {
        let mut thunk: Thunk = ".[]".parse().unwrap();
        let mut c = Context::new();

        let doc: Json = r#"[1,2,3]"#.parse().unwrap();
        let refval = r#"[1, 2, 3]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[]".parse().unwrap();
        let doc: Json = r#"{"a": true, "b": 2, "c": null}"#.parse().unwrap();
        let refval = r#"[true, 2, null]"#;
        let out =thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[]".parse().unwrap();
        let doc = r#"[{"na":"JS","go":true},{"name":"XML","good":false}]"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[{"go":true,"na":"JS"}, {"good":false,"name":"XML"}]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[]".parse().unwrap();
        let doc: Json = r#"[]"#.parse().unwrap();
        let refval = r#"[]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[]".parse().unwrap();
        let doc: Json = r#"{"a": 1, "b": 1}"#.parse().unwrap();
        let refval = r#"[1, 1]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[]".parse().unwrap();
        let doc: Json = r#"10"#.parse().unwrap();
        assert_eq!(
            Err(Error::Op(None, "Integer not an iterable".to_string())),
            thunk(doc.clone(), &mut c)
        );

        thunk = ".[]?".parse().unwrap();
        let doc: Json = r#"10"#.parse().unwrap();
        let refval = r#"[]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[foo, bar]".parse().unwrap();
        let doc = r#"{"foo": 42, "bar": "something else", "baz": true}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[42, "something else"]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[user, projects.[]]".parse().unwrap();
        let doc = r#"{"user":"stedolan", "projects": ["jq", "wikiflow"]}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"["stedolan", "jq", "wikiflow"]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_pipe() {
        let mut thunk: Thunk = ".[] | foo".parse().unwrap();
        let mut c = Context::new();

        let doc: Json = r#"[{"foo": 10}, {"foo":20}]"#.parse().unwrap();
        let refval = r#"[10, 20]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = ".[] | .foo ".parse().unwrap();
        let doc: Json = r#"[{"foo": 10}, {"foo":20}]"#.parse().unwrap();
        let refval = r#"[10, 20]"#;
        let out =thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));


        thunk = ".a.b.c".parse().unwrap();
        let doc: Json = r#"{"a": {"b": {"c": 100}}}"#.parse().unwrap();
        let refval = r#"[100]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#".a | .b | .c"#.parse().unwrap();
        let doc: Json = r#"{"a": {"b": {"c": 100}}}"#.parse().unwrap();
        let refval = r#"[100]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#".a | . | .b"#.parse().unwrap();
        let doc: Json = r#"{"a": {"b": 100}}"#.parse().unwrap();
        let refval = r#"[100]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_paranthesis() {
        let mut thunk: Thunk = "2 + . * 15".parse().unwrap();
        let mut c = Context::new();

        let doc: Json = r#"10"#.parse().unwrap();
        let refval = r#"[152]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"(2 + .) * 15"#.parse().unwrap();
        let doc: Json = r#"10"#.parse().unwrap();
        let refval = r#"[180]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_collection_list() {
        let mut thunk: Thunk = r#"[]"#.parse().unwrap();
        let mut c = Context::new();

        let doc: Json = r#"10"#.parse().unwrap();
        let refval = r#"[[]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"[1,2,3]"#.parse().unwrap();
        let doc: Json = r#"10"#.parse().unwrap();
        let refval = r#"[[1,2,3]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"[foo, .bar, baz]"#.parse().unwrap();
        let doc: Json = r#"{"foo": 10, "bar":20, "baz":30}"#.parse().unwrap();
        let refval = r#"[[10,20,30]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"[.0, .4, .2]"#.parse().unwrap();
        let doc: Json = r#"[10,20,30,40,50]"#.parse().unwrap();
        let refval = r#"[[10,50,30]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"[.items.[].name]"#.parse().unwrap();
        let doc = r#"{"items": [{"name": "x"}, {"name":"y"}]}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[["x","y"]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"[.user, .projects.[]]"#.parse().unwrap();
        let doc = r#"{"user":"stedolan", "projects": ["jq", "wikiflow"]}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[["stedolan","jq","wikiflow"]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_collection_object() {
        let mut thunk: Thunk = r#"{"a":42,"b":17}"#.parse().unwrap();
        let mut c = Context::new();

        let doc: Json = r#"10"#.parse().unwrap();
        let refval = r#"[{"a":42,"b":17}]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"{a:42, b:17} "#.parse().unwrap();
        let doc: Json = r#"10"#.parse().unwrap();
        let refval = r#"[{"a":42,"b":17}]"#;
        let out =thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"{(."a"+"-"+."b"):59}"#.parse().unwrap();
        let doc: Json = r#"{"a":"firstname","b":"lastname"}"#.parse().unwrap();
        let refval = r#"[{"firstname-lastname":59}]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"{foo:.bar}"#.parse().unwrap();
        let doc: Json = r#"{"bar":10}"#.parse().unwrap();
        let refval = r#"[{"foo":10}]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"{user: .user, title: .title}"#.parse().unwrap();
        let doc = r#"{"user":"prom","title":"testing","age":30,"city":"ax"}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[{"title":"testing","user":"prom"}]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"{user, title}"#.parse().unwrap();
        let doc = r#"{"user":"prom","title":"testing","age":30,"city":"ax"}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[{"title":"testing","user":"prom"}]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"{(ks.[]), title}"#.parse().unwrap();
        let doc = r#"{"ks":["age","city"],"age":30,"city":"ax","title":null}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[{"age":30,"city":"ax","title":null}]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"{user, title: .titles.[]}"#.parse().unwrap();
        let doc = r#"{"user":"sted","titles":["JQ Primer", "More JQ"]}"#;
        let doc: Json = doc.parse().unwrap();
        let mut refval = r#"[{"title":"JQ Primer","user":"sted"}, "#.to_string();
        refval += r#"{"title":"More JQ","user":"sted"}]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(&refval, &format!("{:?}", out));

        thunk = r#"{(.user): .titles}"#.parse().unwrap();
        let doc = r#"{"user":"stedolan","titles":["JQ Primer", "More JQ"]}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[{"stedolan":["JQ Primer","More JQ"]}]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_recurse() {
        let mut thunk: Thunk = r#"..|.a?"#.parse().unwrap();
        let mut c = Context::new();

        let doc: Json = r#"[[{"a":1}, {"a":2}],{"a":3}]"#.parse().unwrap();
        let refval = r#"[1, 2, 3]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_addition() {
        let mut thunk: Thunk = r#"a+b"#.parse().unwrap();
        let mut c = Context::new();

        let doc: Json = r#"{"a":1,"b":2}"#.parse().unwrap();
        let refval = r#"[3]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a+b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":2.3}"#.parse().unwrap();
        let refval = r#"[3.5e0]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a+b+c"#.parse().unwrap();
        let doc: Json = r#"{"a":[1,2],"b":[],"c":[3,4]}"#.parse().unwrap();
        let refval = r#"[[1,2,3,4]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a+b+c"#.parse().unwrap();
        let doc: Json = r#"{"a":"hello","b":"","c":"world"}"#.parse().unwrap();
        let refval = r#"["helloworld"]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a+b+c"#.parse().unwrap();
        let doc = r#"{"a":{"x":1},"b":{"x":2},"c":{"y":2}}"#;
        let doc: Json = doc.parse().unwrap();
        let refval = r#"[{"x":2,"y":2}]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_subraction() {
        let mut thunk: Thunk = r#"a-b"#.parse().unwrap();
        let mut c = Context::new();

        let doc: Json = r#"{"a":1,"b":2}"#.parse().unwrap();
        let refval = r#"[-1]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a-b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.3,"b":2.1}"#.parse().unwrap();
        let refval = r#"[-8e-1]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a-b"#.parse().unwrap();
        let doc: Json = r#"{"a":[1,2],"b":[2]}"#.parse().unwrap();
        let refval = r#"[[1]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_multiplication() {
        let mut thunk: Thunk = r#"a*b"#.parse().unwrap();
        let mut c = Context::new();

        let doc: Json = r#"{"a":1,"b":2}"#.parse().unwrap();
        let refval = r#"[2]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a*b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":2}"#.parse().unwrap();
        let refval = r#"[2.4e0]"#;
        let out =thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a*b"#.parse().unwrap();
        let doc: Json = r#"{"a":5,"b":2.5}"#.parse().unwrap();
        let refval = r#"[1.25e1]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a*b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":2.1}"#.parse().unwrap();
        let refval = r#"[2.52e0]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a*b"#.parse().unwrap();
        let doc: Json = r#"{"a":"hello","b":0}"#.parse().unwrap();
        let refval = r#"[null]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a*b"#.parse().unwrap();
        let doc: Json = r#"{"a":"hello","b":2}"#.parse().unwrap();
        let refval = r#"["hellohello"]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a*b"#.parse().unwrap();
        let doc: Json = r#"{"a":{"x":1},"b":{"y":2}}"#.parse().unwrap();
        let refval = r#"[{"x":1,"y":2}]"#;
        let out =thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_division() {
        let mut thunk: Thunk = r#"a/b"#.parse().unwrap();
        let mut c = Context::new();

        let doc: Json = r#"{"a":1,"b":2}"#.parse().unwrap();
        let refval = r#"[5e-1]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a/b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":2}"#.parse().unwrap();
        let refval = r#"[6e-1]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a/b"#.parse().unwrap();
        let doc: Json = r#"{"a":5,"b":2.5}"#.parse().unwrap();
        let refval = r#"[2e0]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a/b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":2.1}"#.parse().unwrap();
        let refval = r#"[5.714285714285714e-1]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a/b"#.parse().unwrap();
        let doc: Json = r#"{"a":1,"b":0}"#.parse().unwrap();
        let refval = r#"[null]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a/b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":0}"#.parse().unwrap();
        let refval = r#"[null]"#;
        let out =  thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a/b"#.parse().unwrap();
        let doc: Json = r#"{"a":"a,b,c,d","b":","}}"#.parse().unwrap();
        let refval = r#"[["a","b","c","d"]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_rem() {
        let mut thunk: Thunk = r#"a%b"#.parse().unwrap();
        let mut c = Context::new();

        let doc: Json = r#"{"a":1,"b":2}"#.parse().unwrap();
        let refval = r#"[1]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a%b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":2}"#.parse().unwrap();
        let refval = r#"[1.2e0]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a%b"#.parse().unwrap();
        let doc: Json = r#"{"a":5,"b":2.5}"#.parse().unwrap();
        let refval = r#"[0e0]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a%b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":2.1}"#.parse().unwrap();
        let refval = r#"[1.2e0]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a%b"#.parse().unwrap();
        let doc: Json = r#"{"a":1,"b":0}"#.parse().unwrap();
        let refval = r#"[null]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"a%b"#.parse().unwrap();
        let doc: Json = r#"{"a":1.2,"b":0}"#.parse().unwrap();
        let refval = r#"[null]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_builtin_len() {
        let mut c = Context::new();
        let mut thunk: Thunk = r#". | length"#.parse().unwrap();
        let doc: Json = r#"null"#.parse().unwrap();
        let refval = r#"[0]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#". | length"#.parse().unwrap();
        let doc: Json = r#"[1,2,3]"#.parse().unwrap();
        let refval = r#"[3]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#". | length"#.parse().unwrap();
        let doc: Json = r#"{"a":1, "b":2}"#.parse().unwrap();
        let refval = r#"[2]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#". | length"#.parse().unwrap();
        let doc: Json = r#""hello world""#.parse().unwrap();
        let refval = r#"[11]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#".[] | length"#.parse().unwrap();
        let doc: Json = r#"[[1,2], "string", {"a":2}, null]"#.parse().unwrap();
        let refval = r#"[2, 6, 1, 0]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"length"#.parse().unwrap();
        let doc: Json = r#""汉语""#.parse().unwrap();
        let refval = r#"[6]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_builtin_chars() {
        let mut c = Context::new();

        let mut thunk: Thunk = r#". | chars | length"#.parse().unwrap();
        let doc: Json = r#""汉语""#.parse().unwrap();
        let refval = r#"[2]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_builtin_keys() {
        let mut c = Context::new();

        let mut thunk: Thunk = r#". | keys"#.parse().unwrap();
        let doc: Json = r#"{"abc": 1, "abcd": 2, "Foo": 3}"#.parse().unwrap();
        let refval = r#"[["Foo","abc","abcd"]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        let mut thunk: Thunk = r#". | keys"#.parse().unwrap();
        let doc: Json = r#"[10,20,30]"#.parse().unwrap();
        let refval = r#"[[0,1,2]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_builtin_has() {
        let mut c = Context::new();

        let mut thunk: Thunk = r#"has("foo")"#.parse().unwrap();
        let doc: Json = r#"{"foo": 1, "abcd": 2, "Foo": 3}"#.parse().unwrap();
        let refval = r#"[true]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"has("foo")"#.parse().unwrap();
        let doc: Json = r#"["foo", 1, "abcd", 2]"#.parse().unwrap();
        let refval = r#"[true]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"has(1)"#.parse().unwrap();
        let doc: Json = r#"[1, 2]"#.parse().unwrap();
        let refval = r#"[true]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_builtin_in() {
        let mut c = Context::new();

        let mut thunk: Thunk = r#".[] | in({"foo": 42})"#.parse().unwrap();
        let doc: Json = r#"["foo", "bar"]"#.parse().unwrap();
        let refval = r#"[true, false]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"[.[] | in([1,0])]"#.parse().unwrap();
        let doc: Json = r#"[2, 0]"#.parse().unwrap();
        let refval = r#"[[false,true]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"map(in([0,1]))"#.parse().unwrap();
        let doc: Json = r#"[2, 0]"#.parse().unwrap();
        let refval = r#"[[false,true]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_builtin_map() {
        let mut c = Context::new();
        let mut thunk: Thunk = r#"map(.)"#.parse().unwrap();

        let doc: Json = r#"["foo", "bar"]"#.parse().unwrap();
        let refval = r#"[["foo","bar"]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"map(.+1)"#.parse().unwrap();
        let doc: Json = r#"[1, 2, 3]"#.parse().unwrap();
        let refval = r#"[[2,3,4]]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"map(.+1)"#.parse().unwrap();
        let doc: Json = r#"{"foo":1, "bar":2}"#.parse().unwrap();
        let refval = r#"[{"bar":3,"foo":2}]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"map(.+1)"#.parse().unwrap();
        let doc: Json = r#"{"a": 1, "b": 2, "c": 3}"#.parse().unwrap();
        let refval = r#"[{"a":2,"b":3,"c":4}]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_builtin_any() {
        let mut c = Context::new();
        let mut thunk: Thunk = r#"any(. == 1)"#.parse().unwrap();

        let doc: Json = r#"[1, 2]"#.parse().unwrap();
        let refval = r#"[true]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"any(. == "a")"#.parse().unwrap();
        let doc: Json = r#"{"x": "a", "y": "b"}"#.parse().unwrap();
        let refval = r#"[true]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"any(. == 1)"#.parse().unwrap();
        let doc: Json = r#"[2,3]"#.parse().unwrap();
        let refval = r#"[false]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"any(. == "a")"#.parse().unwrap();
        let doc: Json = r#"{"x": "c", "y": "b"}"#.parse().unwrap();
        let refval = r#"[false]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }

    #[test]
    fn test_query_builtin_all() {
        let mut c = Context::new();
        let mut thunk: Thunk = r#"all(. == 1)"#.parse().unwrap();

        let doc: Json = r#"[1, 1]"#.parse().unwrap();
        let refval = r#"[true]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"all(. == "a")"#.parse().unwrap();
        let doc: Json = r#"{"x": "a", "y": "a"}"#.parse().unwrap();
        let refval = r#"[true]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"all(. == 1)"#.parse().unwrap();
        let doc: Json = r#"[1,3]"#.parse().unwrap();
        let refval = r#"[false]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));

        thunk = r#"all(. == "a")"#.parse().unwrap();
        let doc: Json = r#"{"x": "a", "y": "b"}"#.parse().unwrap();
        let refval = r#"[false]"#;
        let out = thunk(doc.clone(), &mut c).unwrap();
        assert_eq!(refval, &format!("{:?}", out));
    }
}
