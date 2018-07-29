use std::{result, error, cmp};
use std::fmt::{self};

use json::{self, Json, KeyValue};
use json::{parse_string, parse_array, parse_object};
use lex::Lex;
use nom::{self, {types::CompleteStr as S}};

pub type Result<T> = result::Result<T,Error>;


#[derive(Debug,Eq,PartialEq)]
pub enum Error {
    Parse(String),
    ParseJson(json::Error),
    Op(String),
    NomE(u32),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use jq::Error::*;

        match self {
            Parse(s) => write!(f, "{}", s),
            ParseJson(err) => write!(f, "{}", err),
            Op(s) => write!(f, "{}", s),
            NomE(err) => write!(f, "nom err {}", err),
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

impl From<u32> for Error {
    fn from(err: u32) -> Error {
        Error::NomE(err)
    }
}

#[derive(Debug)]
enum Token {
    Empty,
    Null,
    Bool(bool),
    Integer(i128),
    Float(f64),
    String(String),
    Array(Vec<Json>),
    Object(Vec<KeyValue>),
    Dot,
    Key(String, bool),
}


pub fn parse(text: &str) -> Result<Box<Thunk>> {
    let text = S(text);
    let thunk = match parse_token(text)? {
        (text, Token::Empty) if text.len() == 0 => Box::new(Thunk::Empty),
        (_, Token::Empty) => panic!("impossible situation"),
        (text, tok) => token_to_thunk(tok, parse(&text)?),
    };
    Ok(thunk)
}

fn parse_token(text: S) -> Result<(S, Token)> {
    use nom::simple_errors::Context as NomContext;

    //println!("parse_token {}", text);
    match nom_jq_token(text) {
        Ok((rem, token)) => Ok((rem, token)),

        Err(err @ nom::Err::Incomplete(_)) => {
            Err(Error::Parse(format!("{}", err)))
        },
        Err(nom::Err::Error(NomContext::Code(rem, _))) => {
            let rem_till = cmp::min(5, rem.len());
            Err(Error::Parse(format!("error at {:}", &rem[..rem_till])))
        },
        Err(nom::Err::Failure(NomContext::Code(rem, _))) => {
            let rem_till = cmp::min(5, rem.len());
            Err(Error::Parse(format!("failure at {:}", &rem[..rem_till])))
        },
    }
}

fn as_token_literal(text: S) -> Token {
    use jq::Token::{Null, Bool};
    match text.as_ref() {
        "null" => Null,
        "true" => Bool(true),
        "false" => Bool(false),
        _ => panic!("impossible case"),
    }
}

fn as_token_integer(i: i128) -> Token {
    Token::Integer(i)
}

fn as_token_float(f: f64) -> Token {
    Token::Float(f)
}

fn as_token_key1((key, opt): (S, Option<S>)) -> Token {
    //println!("as_token_key1 {:?} {:?}", key, opt);
    Token::Key(key.to_string(), opt.map_or(false, |_| true))
}

fn as_token_key23((key, opt): (Token, Option<S>)) -> Token {
    //println!("as_token_key23 {:?} {:?}", key, opt);
    match key {
        Token::String(key) => Token::Key(key, opt.map_or(false, |_| true)),
        _ => panic!("impossible case")
    }
}

fn token_to_thunk(tok: Token, thunk: Box<Thunk>) -> Box<Thunk> {
    use self::Thunk::{Identity, Literal, IndexKey};

    match tok {
        Token::Null => Box::new(Literal(Json::Null, thunk)),
        Token::Bool(val) => Box::new(Literal(Json::Bool(val), thunk)),
        Token::Integer(val) => Box::new(Literal(Json::Integer(val), thunk)),
        Token::Float(val) => Box::new(Literal(Json::Float(val), thunk)),
        Token::String(val) => Box::new(Literal(Json::String(val), thunk)),
        Token::Array(val) => Box::new(Literal(Json::Array(val), thunk)),
        Token::Object(val) => Box::new(Literal(Json::Object(val), thunk)),
        Token::Dot => Box::new(Identity(thunk)),
        Token::Key(key, opt) => Box::new(IndexKey(key, opt, thunk)),
        Token::Empty => panic!("should have been handled earlier"),
    }
}


named!(nom_jq_null(S) -> S, tag!("null"));
named!(nom_jq_true(S) -> S, tag!("true"));
named!(nom_jq_false(S) -> S, tag!("false"));
named!(nom_jq_int(S) -> i128,
    flat_map!(re_match!(r#"^[+-]?\d+"#), parse_to!(i128))
);
named!(nom_jq_float(S) -> f64,
    flat_map!(
        re_match!(r#"^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?"#),
        parse_to!(f64)
    )
);
fn nom_json_string(text: S) -> nom::IResult<S, Token> {
    //println!("nom_json_string {}", text);
    check_next_byte(text, b'"')?;
    let mut lex = Lex::new(0, 1, 1);
    match parse_string(&text, &mut lex) {
        Ok(Json::String(s)) => {
            Ok((S(&text[lex.off..]), Token::String(s)))
        },

        _ => {
            let kind = nom::ErrorKind::Custom(lex.off as u32);
            return Err(nom::Err::Failure(nom::Context::Code(text, kind)));
        }
    }
}
fn nom_json_array(text: S) -> nom::IResult<S, Token> {
    check_next_byte(text, b'[')?;
    let mut lex = Lex::new(0, 1, 1);
    match parse_array(&text, &mut lex) {
        Ok(Json::Array(arr)) => {
            Ok((S(&text[lex.off..]), Token::Array(arr)))
        },

        _ => {
            let kind = nom::ErrorKind::Custom(lex.off as u32);
            return Err(nom::Err::Failure(nom::Context::Code(text, kind)));
        }
    }
}
fn nom_json_object(text: S) -> nom::IResult<S, Token> {
    check_next_byte(text, b'{')?;
    let mut lex = Lex::new(0, 1, 1);
    match parse_object(&text, &mut lex) {
        Ok(Json::Object(obj)) => {
            Ok((S(&text[lex.off..]), Token::Object(obj)))
        },

        _ => {
            let kind = nom::ErrorKind::Custom(lex.off as u32);
            return Err(nom::Err::Failure(nom::Context::Code(text, kind)));
        }
    }
}
named!(nom_jq_literal<S, Token>,
    ws!(alt!(
        nom_jq_null  => { as_token_literal } |
        nom_jq_true  => { as_token_literal } |
        nom_jq_false => { as_token_literal } |
        nom_jq_int   => { as_token_integer } |
        nom_jq_float => { as_token_float } |
        nom_json_string |
        nom_json_array |
        nom_json_object
    ))
);


named!(nom_jq_key1(S) -> (S, Option<S>), // like .key
    do_parse!(
             nom_jq_dot      >>
        key: re_match!(r#"^[a-zA-Z0-9_]+"#) >>
        opt: opt!(tag!("?")) >>
        (key, opt)
    )
);
named!(nom_jq_key2(S) -> (Token, Option<S>), // like ."key"
    do_parse!(
             nom_jq_dot      >>
        key: nom_json_string >>
        opt: opt!(tag!("?")) >>
        (key, opt)
    )
);
named!(nom_jq_key3(S) -> (Token, Option<S>), // like .["key"]
    do_parse!(
             nom_jq_dot      >>
             tag!("[")       >>
        key: nom_json_string >>
             tag!("]")       >>
        opt: opt!(tag!("?")) >>
        (key, opt)
    )
);
named!(nom_jq_key<S, Token>,
    ws!(alt!(
        nom_jq_key1 => { as_token_key1 } |
        nom_jq_key2 => { as_token_key23 } |
        nom_jq_key3 => { as_token_key23 }
    ))
);


named!(nom_jq_dot(S) -> S, tag!("."));
fn nom_jq_empty(text: S) -> nom::IResult<S, Token> {
    //println!("nom_jq_empty");
    if text.len() == 0 {
        Ok((S(&text[..]), Token::Empty))
    } else {
        panic!("impossible situation")
    }
}
named!(nom_jq_token<S, Token>,
    ws!(alt!(
        nom_jq_literal |
        nom_jq_key |
        // identity
        nom_jq_dot => { |_| Token::Dot } |
        // catch all
        nom_jq_empty // should alway be last.
    ))
);

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



#[derive(Debug)]
pub enum Thunk {
    Empty,
    Identity(Box<Thunk>),
    Literal(Json, Box<Thunk>),
    IndexKey(String, bool, Box<Thunk>),
    //TakeArray(Box<Thunk>, Vec<String>),
    //TakeMap(Box<Thunk>, Vec<String>),
}

impl Thunk {

    fn do_empty(x: Json)
        -> Result<Vec<Json>>
    {
        //println!("Thunk::Empty");
        Ok(vec![x])
    }

    fn do_identity(thunk: &mut Box<Thunk>, doc: Json)
        -> Result<Vec<Json>>
    {
        //println!("Thunk::Identity");
        Ok(thunk(doc)?)
    }

    fn do_literal(thunk: &mut Box<Thunk>, _: Json, val: &Json)
        -> Result<Vec<Json>>
    {
        //println!("Thunk::Literal");
        Ok(thunk(val.clone())?)
    }

    fn do_index_key(thunk: &mut Box<Thunk>, doc: Json, key: &String, opt: bool)
        -> Result<Vec<Json>>
    {
        use json::{search_by_key, Json::{Object}};

        //println!("Thunk::IndexKey {:?} {}", doc, key);
        match doc {
            Object(m) => match search_by_key(&m, key) {
                Ok(i) => {
                    let val = m.into_iter().nth(i).unwrap();
                    Ok(thunk(val.1)?)
                },
                Err(_) => match opt {
                    true => Ok(vec![Json::Null]),
                    false => Err(Error::Op(format!("missing key {}", key))),
                }
            },
            _ => match opt {
                true => Ok(vec![Json::Null]),
                false => Err(Error::Op(format!("not an object {:?}", doc))),
            }
        }
    }
}


impl FnMut<(Json,)> for Thunk {
    extern "rust-call" fn call_mut(&mut self, args: (Json,)) -> Self::Output {
        use jq::Thunk::*;

        match self {
            Empty => {
                Thunk::do_empty(args.0)
            },
            Identity(thunk) => {
                Thunk::do_identity(thunk, args.0)
            }
            Literal(val, thunk) => {
                Thunk::do_literal(thunk, args.0, val)
            }
            IndexKey(key, opt, thunk) => {
                Thunk::do_index_key(thunk, args.0, key, *opt)
            }
        }
    }
}

impl FnOnce<(Json,)> for Thunk {
    type Output=Result<Vec<Json>>;

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
                assert_eq!(1, out.len());
                assert_eq!("hello", out.remove(0).string());
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
                assert_eq!(1, out.len());
                assert_eq!("hello", out.remove(0).string());
            },
            _ => {
                panic!("unexpected thunk")
            }
        }
    }

    #[test]
    fn test_jq_null() {
        let mut thunk = parse("null").unwrap();
        let doc = JsonBuf::parse_str("[10]").unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!(1, out.len());
        assert_eq!(Json::Null, out[0])
    }

    #[test]
    fn test_jq_true() {
        let mut thunk = parse("true").unwrap();
        let doc = JsonBuf::parse_str("[10]").unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!(1, out.len());
        assert_eq!(Json::Bool(true), out[0]);
    }

    #[test]
    fn test_jq_false() {
        let mut thunk = parse("false").unwrap();
        let doc = JsonBuf::parse_str("[10]").unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!(1, out.len());
        assert_eq!(Json::Bool(false), out[0]);
    }

    #[test]
    fn test_jq_int() {
        let mut thunk = parse("12310231231").unwrap();
        let doc = JsonBuf::parse_str("[10]").unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!(1, out.len());
        assert_eq!(Json::Integer(12310231231), out[0]);
    }

    #[test]
    fn test_jq_float() {
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
            assert_eq!(1, out.len());
            assert_eq!(Json::Float(tc.1), out[0]);
        }
    }

    #[test]
    fn test_jq_string() {
        let mut thunk = parse(r#""hello world""#).unwrap();
        let doc = JsonBuf::parse_str("[10]").unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!(1, out.len());
        assert_eq!(Json::String("hello world".to_string()), out[0]);
    }

    #[test]
    fn test_jq_array() {
        use self::Json::{Array, Integer};

        let doc = Json::Null;
        let mut thunk = parse(r#"[10]"#).unwrap();
        let out = thunk(doc.clone()).unwrap();
        assert_eq!(1, out.len());
        assert_eq!(Array(vec![Integer(10)]), out[0]);
    }

    #[test]
    fn test_jq_object() {
        use self::Json::{Object, Integer};

        let doc = Json::Null;
        let mut thunk = parse(r#"{"a": 10}"#).unwrap();
        assert_eq!(
            Object(vec![KeyValue("a".to_string(), Integer(10))]),
            thunk(doc.clone()).unwrap()[0]
        );
    }

    #[test]
    fn test_jq_key() {
        let doc = JsonBuf::parse_str(r#"{"a":[1,2],"b":[true,1]}"#).unwrap();
        let refout = JsonBuf::parse_str("[true,1]").unwrap();

        let mut thunk = parse(r#".b"#).unwrap();
        let res = thunk(doc.clone()).unwrap();
        assert_eq!(res.len(), 1);
        assert_eq!(refout, res[0]);

        let mut thunk = parse(r#"."b""#).unwrap();
        let res = thunk(doc.clone()).unwrap();
        assert_eq!(res.len(), 1);
        assert_eq!(refout, res[0]);

        let mut thunk = parse(r#".["b"]"#).unwrap();
        let res = thunk(doc.clone()).unwrap();
        assert_eq!(res.len(), 1);
        assert_eq!(refout, res[0]);
    }

    #[test]
    fn test_jq_key_opt() {
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
