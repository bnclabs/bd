use std::{result, error, cmp};
use std::fmt::{self};

use json::{self, Json, KeyValue};
use json::{parse_string, parse_array, parse_object};
use lex::Lex;
use nom::{self, {types::CompleteStr as S}};

include!("./jq_nom.rs");
//include!("./jq_context.rs");
include!("./jq_output.rs");

pub type Result<T> = result::Result<T,Error>;


#[derive(Debug,Eq,PartialEq)]
pub enum Error {
    Parse(String),
    ParseJson(json::Error),
    Op(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use jq::Error::*;

        match self {
            Parse(s) => write!(f, "{}", s),
            ParseJson(err) => write!(f, "{}", err),
            Op(s) => write!(f, "{}", s),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&error::Error> { None }
}

impl From<json::Error> for Error {
    fn from(err: json::Error) -> Error {
        Error::ParseJson(err)
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
    let (_, thunk) = nom_program(S(text))?;
    Ok(Box::new(thunk))
}


#[derive(Debug)]
pub enum Thunk {
    // Primary thunks
    Empty,
    Identity,
    Recurse,
    Literal(Json),
    Identifier(String),
    IndexShortcut(String, Option<usize>, bool),
    Slice(Box<Thunk>, usize, usize, bool),
    Iterate(Box<Thunk>, Box<Thunk>, bool),
    List(Box<Thunk>, bool),
    Dict(Box<Thunk>, bool),
    Neg(Box<Thunk>),
    Not(Box<Thunk>),
    // Operations in increasing precedance
    Pipe(Box<Thunk>, Box<Thunk>),
    Or(Box<Thunk>, Box<Thunk>),
    And(Box<Thunk>, Box<Thunk>),
    Compare(Box<Thunk>, Box<Thunk>),
    BitOr(Box<Thunk>, Box<Thunk>),
    BitXor(Box<Thunk>, Box<Thunk>),
    BitAnd(Box<Thunk>, Box<Thunk>),
    Shr(Box<Thunk>, Box<Thunk>),
    Shl(Box<Thunk>, Box<Thunk>),
    Add(Box<Thunk>, Box<Thunk>),
    Sub(Box<Thunk>, Box<Thunk>),
    Mult(Box<Thunk>, Box<Thunk>),
    Div(Box<Thunk>, Box<Thunk>),
    Rem(Box<Thunk>, Box<Thunk>),
    // Builtins
    Builtin(String, Box<Thunk>),
    // comman separated list of expression thunks
    Thunks(Vec<Thunk>),
}

impl Thunk {
    fn do_empty(doc: Json)
        -> Result<Output>
    {
        //println!("Thunk::Empty");
        Ok(Output::One(doc))
    }

    fn do_identity(doc: Json)
        -> Result<Output>
    {
        //println!("Thunk::Identity");
        Ok(Output::One(doc))
    }

    fn do_literal(literal: &Json, _: Json)
        -> Result<Output>
    {
        //println!("Thunk::Literal");
        Ok(Output::One(literal.clone()))
    }

    fn do_index_shortcut(key: &String, off: Option<usize>, opt: bool, doc: Json)
        -> Result<Output>
    {
        use json::Json::{Object, Array};

        //println!("Thunk::IndexKey {:?} {}", doc, key);
        match doc {
            Array(a) => Thunk::index_array(a, &key, off, opt),
            Object(m) => Thunk::index_object(m, &key, opt),
            _ => match opt {
                true => Ok(Output::One(Json::Null)),
                false => Err(Error::Op(format!("not an object {:?}", doc))),
            }
        }
    }

    fn do_slice(
        thunk: &mut Thunk, start: usize, end: usize, opt: bool, doc: Json)
        -> Result<Output>
    {
        match (opt, thunk(doc)?.slice(start, end)) {
            (true, Err(_)) => Ok(Output::One(Json::Null)),
            (_, res) => res,
        }
    }

    fn do_iterate(
        _thunk: &mut Thunk, _thunks: &mut Thunk, _opt: bool, _doc: Json)
        -> Result<Output>
    {
        unimplemented!()
    }

    fn do_list(_thunks: &mut Thunk, _opt: bool, _doc: Json)
        -> Result<Output>
    {
        unimplemented!()
    }

    fn do_pipe(_lhs_thunk: &mut Thunk, _rhs_thunk: &mut Thunk, _doc: Json)
        -> Result<Output>
    {
        unimplemented!()
    }

    fn index_array(a: Vec<Json>, key: &str, off: Option<usize>, opt: bool)
        -> Result<Output>
    {
        match (off, opt) {
            (None, true) => {
                Ok(Output::One(Json::Null))
            },
            (None, false) => {
                Err(Error::Op(format!("not an array index {}", key)))
            },
            (Some(off), true) if off >= a.len() => {
                Ok(Output::One(Json::Null))
            },
            (Some(off), false) if off >= a.len() => {
                Err(Error::Op(format!("offset {} out of bound", off)))
            },
            (Some(off), _) => {
                Ok(Output::One(a.into_iter().nth(off).unwrap()))
            },
        }
    }

    fn index_object(m: Vec<KeyValue>, key: &str, opt: bool)
        -> Result<Output>
    {
        use json::search_by_key;

        match search_by_key(&m, key) {
            Ok(i) => {
                Ok(Output::One(m.into_iter().nth(i).unwrap().1))
            },
            Err(_) => match opt {
                true => Ok(Output::One(Json::Null)),
                false => Err(Error::Op(format!("missing key {}", key))),
            }
        }
    }
}


impl FnMut<(Json,)> for Thunk {
    extern "rust-call" fn call_mut(&mut self, args: (Json,)) -> Self::Output {
        use jq::Thunk::*;

        let doc = args.0;
        match self {
            Empty => {
                Thunk::do_empty(doc)
            },
            Identity => {
                Thunk::do_identity(doc)
            },
            Literal(literal) => {
                Thunk::do_literal(literal, doc)
            },
            IndexShortcut(key, off, opt) => {
                Thunk::do_index_shortcut(key, *off, *opt, doc)
            },
            Slice(ref mut thunk, start, end, opt) => {
                Thunk::do_slice(thunk, *start, *end, *opt, doc)
            },
            Iterate(ref mut thunk, ref mut thunks, opt) => {
                Thunk::do_iterate(thunk, thunks, *opt, doc)
            },
            List(ref mut thunks, opt) => {
                Thunk::do_list(thunks, *opt, doc)
            },
            Pipe(ref mut lhs_thunk, ref mut rhs_thunk) => {
                Thunk::do_pipe(lhs_thunk, rhs_thunk, doc)
            },
            _ => unimplemented!(),
        }
    }
}

impl FnOnce<(Json,)> for Thunk {
    type Output=Result<Output>;

    extern "rust-call" fn call_once(mut self, args: (Json,)) -> Self::Output {
        self.call_mut(args)
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use json::{JsonBuf};
    //use std;

    #[test]
    fn test_jq_empty() {
        match parse("").unwrap() {
            mut thunk @ box Thunk::Empty => {
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
            mut thunk @ box Thunk::Empty => {
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
