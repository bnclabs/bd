use std::{result, error, cmp};
use std::fmt::{self};

use nom::{self, {types::CompleteStr as S}};

use json::{self, Json, KeyValue};
use jqnom::parse_program_nom;


type Output = Vec<Json>;


pub type Result<T> = result::Result<T,Error>;


#[derive(Debug,Eq,PartialEq)]
pub enum Error {
    Parse(String),
    ParseJson(json::Error),
    Op(Option<json::Error>, String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use jq::Error::*;

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

impl<'a> From<nom::Err<S<'a>>> for Error {
    fn from(err: nom::Err<S<'a>>) -> Error {
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
    let (_, thunk) = parse_program_nom(S(text))?;
    Ok(Box::new(thunk))
}


#[derive(Debug)]
pub enum Thunk {
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
    Compare(Box<Thunk>, Box<Thunk>),
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

impl FnMut<(Json,)> for Thunk {
    extern "rust-call" fn call_mut(&mut self, args: (Json,)) -> Self::Output {
        use jq::Thunk::*;

        let doc = args.0;
        match self {
            Empty => { // vector of single item
                Ok(vec![doc])
            },

            Identity => { // vector of single item
                Ok(vec![doc])
            },

            Recurse => { // vector of one or more items
                let mut list = Vec::new();
                do_recurse(&mut list, doc);
                Ok(list)
            },

            Literal(literal) => { // vecot of single item
                Ok(vec![literal.clone()])
            },

            IndexShortcut(key, off, opt) => { // vector of single item
                let res = do_index_shortcut(key, *off, doc);
                if *opt && res.is_err() { return Ok(vec![]) }
                Ok(vec![res.unwrap()])
            },

            Slice(ref mut thunk, start, end, opt) => { // vector of one or more
                let mut out = Vec::new();
                for item in thunk(doc)?.into_iter() {
                    let mut res = do_slice(*start, *end, item);
                    if *opt && res.is_err() { continue }
                    out.append(&mut res?);
                }
                Ok(out)
            },

            Iterate(ref mut thunk, ref mut thunks, opt) => {
                let mut out = Vec::new();
                for item in thunk(doc)?.into_iter() {
                    let mut res = do_iterate(thunks, *opt, item);
                    if *opt && res.is_err() { continue }
                    out.append(&mut res?);
                }
                Ok(out)
            },

            List(ref mut thunks, opt) => {
                do_list(thunks, *opt, doc)
            },

            Dict(ref mut kv_thunks, opt) => {
                do_dict(kv_thunks, *opt, doc)
            },

            Pipe(ref mut lhs_thunk, ref mut rhs_thunk) => {
                do_pipe(lhs_thunk, rhs_thunk, doc)
            },

            _ => unreachable!(),
        }
    }
}

impl FnOnce<(Json,)> for Thunk {
    type Output=Result<Output>;

    extern "rust-call" fn call_once(mut self, args: (Json,)) -> Self::Output {
        self.call_mut(args)
    }
}

fn do_recurse(list: &mut Vec<Json>, doc: Json) {
    use json::Json::{Null, Bool, Integer, Float, Array, Object, String as S};

    match doc {
        val@Null | val@Bool(_) | val@Integer(_) |
        val@Float(_) | val@S(_) => list.push(val),
        Array(val) => {
            list.push(Array(val.clone()));
            val.into_iter().for_each(|item| do_recurse(list, item))
        },
        Object(val) => {
            list.push(Object(val.clone()));
            val.into_iter().for_each(|item| do_recurse(list, item.1))
        },
    }
}

fn do_index_shortcut(key: &String, off: Option<usize>, doc: Json)
    -> Result<Json>
{
    use json::Json::{Object, Array};

    Ok(match (doc, off) {
        (doc@Object(_), _) => doc.get(key)?,
        (doc@Array(_), Some(off)) => doc.index(off)?,
        (_, _) => {
            return Err(Error::Op(None, "json not an object".to_string()))
        },
    })
}

fn do_slice(start: usize, end: usize, doc: Json) -> Result<Output> {
    use json::Json::{Array};

    match doc {
        Array(arr) => {
            let end = if end == usize::max_value() { arr.len() } else { end };
            let mut res = Vec::new();
            arr[start..end].iter().for_each(|item| res.push(item.clone()));
            Ok(res)
        }
        _ => return Err(Error::Op(None, "json not an array".to_string()))
    }
}

fn do_iterate(thunks: &mut Vec<Thunk>, opt: bool, doc: Json) -> Result<Output> {
    let mut out = Vec::new();
    for thunk in thunks.iter_mut() {
        let mut res = thunk(doc.clone());
        if opt && res.is_err() { continue }
        out.append(&mut res?)
    }
    Ok(out)
}

fn do_list(thunks: &mut Vec<Thunk>, opt: bool, doc: Json) -> Result<Output> {
    let mut out = Vec::new();
    for thunk in thunks.iter_mut() {
        let mut res = thunk(doc.clone());
        if opt && res.is_err() { continue }
        out.append(&mut res?)
    }
    Ok(vec![Json::Array(out)])
}

fn do_dict(kv_thunks: &mut Vec<(Thunk, Thunk)>, opt: bool, doc: Json)
    -> Result<Output>
{
    use json::Json::{String as S};

    let mut dicts: Vec<Json> = vec![Json::Object(vec![])];

    let insert_dicts = |dicts: &mut Vec<Json>, kv: &KeyValue| {
        dicts.iter_mut().for_each(|dict| { dict.upsert_key(kv.clone()); });
    };

    for kv_thunk in kv_thunks.iter_mut() {
        let mut key_res = kv_thunk.0(doc.clone());
        if opt && key_res.is_err() { continue }

        let mut val_res = kv_thunk.1(doc.clone());
        if opt && val_res.is_err() { continue }

        let key_res = key_res?;
        let val_res = val_res?;
        for (i, key) in key_res.into_iter().enumerate() {
            let mut kv = match key {
                S(s) => KeyValue(s, Json::Null),
                _ => continue
            };
            for (j, val) in val_res.clone().into_iter().enumerate() {
                kv.1 = val;
                if i == 0 && j == 0 {
                    insert_dicts(&mut dicts, &kv);
                    continue
                }
                let mut next_dicts = dicts.clone();
                insert_dicts(&mut next_dicts, &kv);
                dicts.append(&mut next_dicts);
            }
        }
    }
    Ok(dicts)
}

fn do_pipe(_lhs_thunk: &mut Thunk, _rhs_thunk: &mut Thunk, _doc: Json)
    -> Result<Output>
{
    unimplemented!()
}

//fn index_array(a: Vec<Json>, key: &str, off: Option<usize>, opt: bool)
//    -> Result<Output>
//{
//    match (off, opt) {
//        (None, true) => {
//            Ok(Output::One(Json::Null))
//        },
//        (None, false) => {
//            Err(Error::Op(format!("not an array index {}", key)))
//        },
//        (Some(off), true) if off >= a.len() => {
//            Ok(Output::One(Json::Null))
//        },
//        (Some(off), false) if off >= a.len() => {
//            Err(Error::Op(format!("offset {} out of bound", off)))
//        },
//        (Some(off), _) => {
//            Ok(Output::One(a.into_iter().nth(off).unwrap()))
//        },
//    }
//}
//
//fn index_object(m: Vec<KeyValue>, key: &str, opt: bool)
//    -> Result<Output>
//{
//    use json::search_by_key;
//
//    match search_by_key(&m, key) {
//        Ok(i) => {
//            Ok(Output::One(m.into_iter().nth(i).unwrap().1))
//        },
//        Err(_) => match opt {
//            true => Ok(Output::One(Json::Null)),
//            false => Err(Error::Op(format!("missing key {}", key))),
//        }
//    }
//}


#[cfg(test)]
mod test {
    use super::*;
    use json::{JsonBuf};
    //use std;

    #[test]
    fn test_jq_empty() {
        match parse("").unwrap() {
            mut thunk@box Thunk::Empty => {
                let mut out = thunk(Json::String("hello".to_string())).unwrap();
                assert_eq!(Output::One(Json::String("hello".to_string())), out);
            },
            _ => {
                panic!("unexpected thunk")
            }
        }
    }

    #[test]
    fn test_jq_empty_ws() {
        match parse("   ").unwrap() {
            mut thunk@box Thunk::Empty => {
                let mut out = thunk(Json::String("hello".to_string())).unwrap();
                assert_eq!(Output::One(Json::String("hello".to_string())), out);
            },
            _ => {
                panic!("unexpected thunk")
            }
        }
    }

    #[test]
    fn test_expr_null() {
        let mut thunk = parse("null").unwrap();
        let doc = JsonBuf::parse_str("[10]").unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!(Output::One(Json::Null), out)
    }

    #[test]
    fn test_expr_true() {
        let mut thunk = parse("true").unwrap();
        let doc = JsonBuf::parse_str("[10]").unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!(Output::One(Json::Bool(true)), out);
    }

    #[test]
    fn test_expr_false() {
        let mut thunk = parse("false").unwrap();
        let doc = JsonBuf::parse_str("[10]").unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!(Output::One(Json::Bool(false)), out);
    }

    #[test]
    fn test_expr_int() {
        let mut thunk = parse("12310231231").unwrap();
        let doc = JsonBuf::parse_str("[10]").unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!(Output::One(Json::Integer(12310231231)), out);
    }

    #[test]
    fn test_expr_float() {
        let doc = JsonBuf::parse_str("[10]").unwrap();
        let testcases = vec![
            ("1.2", 1.2), ("0.2", 0.2), ("1.0", 1.0), ("0.0", 0.0),
            (".2", 0.2), (".0", 0.0),
            ("1.2e2", 120.0), ("0.2e-2", 0.002), ("1.0e+2", 100.0),
            (".2e+0", 0.2), (".0e-0", 0.0),
        ];
        for tc in testcases {
            let mut thunk = parse(tc.0).unwrap();
            let out = thunk(doc.clone()).unwrap();
            assert_eq!(Output::One(Json::Float(tc.1)), out);
        }
    }

    #[test]
    fn test_expr_string() {
        let mut thunk = parse(r#""hello world""#).unwrap();
        let doc = JsonBuf::parse_str("[10]").unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!(
            Output::One(Json::String("hello world".to_string())),
            out
        );
    }

    #[test]
    fn test_expr_array() {
        use self::Json::{Array, Integer};

        let doc = Json::Null;
        let mut thunk = parse(r#"[10]"#).unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!(Output::One(Array(vec![Integer(10)])), out);
    }

    #[test]
    fn test_expr_object() {
        use self::Json::{Object, Integer};

        let doc = Json::Null;
        let mut thunk = parse(r#"{"a": 10}"#).unwrap();
        assert_eq!(
            Output::One(Object(vec![KeyValue("a".to_string(), Integer(10))])),
            thunk(doc.clone()).unwrap()
        );
    }

    #[test]
    fn test_expr_object_index() {
        let doc = JsonBuf::parse_str(r#"{"a":[1,2],"b":[true,1]}"#).unwrap();
        let refout = Output::One(JsonBuf::parse_str("[true,1]").unwrap());

        let mut thunk = parse(r#".b"#).unwrap();
        let res = thunk(doc.clone()).unwrap();
        assert_eq!(refout, res);

        let mut thunk = parse(r#"."b""#).unwrap();
        let res = thunk(doc.clone()).unwrap();
        assert_eq!(refout, res);

        let mut thunk = parse(r#".["b"]"#).unwrap();
        let res = thunk(doc.clone()).unwrap();
        assert_eq!(refout, res);
    }

    #[test]
    fn test_expr_object_index_opt() {
        let doc1 = JsonBuf::parse_str(r#"{"a":[1,2],"b":[true,1]}"#).unwrap();
        let doc2 = JsonBuf::parse_str(r#"null"#).unwrap();

        let mut thunk = parse(r#".not"#).unwrap();
        assert_eq!(
            Error::Op(format!("missing key not")),
            thunk(doc1.clone()).err().unwrap()
        );
        assert_eq!(
            Error::Op(format!("not an object Null")),
            thunk(doc2.clone()).err().unwrap()
        );

        let mut thunk = parse(r#"."not""#).unwrap();
        assert_eq!(
            Error::Op(format!("missing key not")),
            thunk(doc1.clone()).err().unwrap()
        );
        assert_eq!(
            Error::Op(format!("not an object Null")),
            thunk(doc2.clone()).err().unwrap()
        );

        let mut thunk = parse(r#".["not"]"#).unwrap();
        assert_eq!(
            Error::Op(format!("missing key not")),
            thunk(doc1.clone()).err().unwrap()
        );
        assert_eq!(
            Error::Op(format!("not an object Null")),
            thunk(doc2.clone()).err().unwrap()
        );
    }
}
