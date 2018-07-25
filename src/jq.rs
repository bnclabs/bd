use std::{result, error};
use std::fmt::{self};

use json::{self, Json, parse_whitespace, parse_value};
use lex::Lex;


pub type Result<T> = result::Result<T,JqError>;


#[derive(Debug)]
pub enum JqError {
    MissingToken(String),
    InvalidToken(String),
    InvalidJson(json::JsonError),
}

impl fmt::Display for JqError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use jq::JqError::{MissingToken, InvalidJson, InvalidToken};

        match self {
            MissingToken(s) => write!(f, "missing token at {}", s),
            InvalidToken(s) => write!(f, "invalid token at {}", s),
            InvalidJson(s) => write!(f, "invalid json expr at {}", s),
        }
    }
}

impl error::Error for JqError {
    fn cause(&self) -> Option<&error::Error> { None }
}

impl From<json::JsonError> for JqError {
    fn from(err: json::JsonError) -> JqError {
        JqError::InvalidJson(err)
    }
}


pub type Lhs<S>=Option<Box<Thunk<S>>>;

pub enum Thunk<S> where S: Iterator<Item=Json> {
    Empty,
    Source(S),
    Doc(Lhs<S>, Json),
}

impl<S> Thunk<S> where S: Iterator<Item=Json> {
    pub fn new_source(source: S) -> Thunk<S> {
        Thunk::Source(source)
    }
}

impl<S> FnMut<()> for Thunk<S> where S: Iterator<Item=Json> {
    extern "rust-call" fn call_mut(&mut self, _args: ()) -> Self::Output {
        use jq::Thunk::{Empty, Source, Doc};

        match self {
            Empty => {
                //println!("Thunk::Empty");
                None
            },
            Source(ref mut iter) => {
                //println!("Thunk::Source");
                match iter.next() { Some(val) => Some(vec![val]), None => None }
            },
            Doc(None, _) => {
                //println!("Thunk::Doc(None)");
                None
            },
            Doc(Some(lhs_thunk), val) => {
                //println!("Thunk::Doc(Some)");
                match lhs_thunk() { None => None, _ => Some(vec![val.clone()]) }
            },
        }
    }
}

impl<S> FnOnce<()> for Thunk<S> where S: Iterator<Item=Json> {
    type Output=Option<Vec<Json>>;

    extern "rust-call" fn call_once(mut self, _args: ()) -> Self::Output {
        self.call_mut(())
    }
}


pub fn compile<S>(text: &str, mut lhs: Lhs<S>)
    -> Result<Box<Thunk<S>>>
    where S: Iterator<Item=Json>
{
    let mut lex = Lex::new(0, 1, 1);

    parse_whitespace(text, &mut lex);
    if (&text[lex.off..]).len() == 0 { return Ok(Box::new(Thunk::Empty)) }

    while lex.off < text.len() {
        let progbytes = (&text[lex.off..]).as_bytes();
        lhs = match progbytes[0] {
            b'.' => unimplemented!(),
            _ => Some(Box::new(Thunk::Doc(lhs, parse_value(text, &mut lex)?))),
        };
        parse_whitespace(text, &mut lex);
    }

    Ok(lhs.unwrap())
}


#[cfg(test)]
mod test {
    use super::*;
    use json::{JsonBuf, JsonIterate, KeyValue};
    use std;

    #[test]
    fn test_jq_empty() {
        let lhs: Option<Box<Thunk<JsonIterate<std::io::Empty>>>> = None;
        let thunk = compile("  ", lhs).unwrap();
        match thunk {
            box Thunk::Empty => (),
            _ => panic!("unexpected thunk")
        }

        let lhs: Lhs<JsonIterate<std::io::Empty>> = None;
        let mut thunk = compile("null", lhs).unwrap();
        assert_eq!(None, thunk());
    }

    #[test]
    fn test_jq_null() {
        let docs = r#"null 10 10.2 "hello world" true false [1,2] {"a":"b"}"#;
        let docs: &[u8] = docs.as_ref();
        let lhs = Some(Box::new(Thunk::new_source(JsonBuf::iter(docs))));
        let mut thunk = compile("null", lhs).unwrap();

        let refval = Some(vec![Json::Null]);
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(None, thunk());
    }

    #[test]
    fn test_jq_true() {
        let docs = r#"null 10 10.2 "hello world" true false [1,2] {"a":"b"}"#;
        let docs: &[u8] = docs.as_ref();
        let lhs = Some(Box::new(Thunk::new_source(JsonBuf::iter(docs))));
        let mut thunk = compile("true", lhs).unwrap();

        let refval = Some(vec![Json::Bool(true)]);
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(None, thunk());
    }

    #[test]
    fn test_jq_false() {
        let docs = r#"null 10 10.2 "hello world" true false [1,2] {"a":"b"}"#;
        let docs: &[u8] = docs.as_ref();
        let lhs = Some(Box::new(Thunk::new_source(JsonBuf::iter(docs))));
        let mut thunk = compile("false", lhs).unwrap();

        let refval = Some(vec![Json::Bool(false)]);
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(None, thunk());
    }

    #[test]
    fn test_jq_int() {
        let docs = r#"null 10 10.2 "hello world" true false [1,2] {"a":"b"}"#;
        let docs: &[u8] = docs.as_ref();
        let lhs = Some(Box::new(Thunk::new_source(JsonBuf::iter(docs))));
        let mut thunk = compile("10", lhs).unwrap();

        let refval = Some(vec![Json::Integer(10)]);
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(None, thunk());
    }

    #[test]
    fn test_jq_float() {
        let docs = r#"null 10 10.2 "hello world" true false [1,2] {"a":"b"}"#;
        let docs: &[u8] = docs.as_ref();
        let lhs = Some(Box::new(Thunk::new_source(JsonBuf::iter(docs))));
        let mut thunk = compile("10.2", lhs).unwrap();

        let refval = Some(vec![Json::Float(10.2)]);
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(None, thunk());
    }

    #[test]
    fn test_jq_string() {
        let docs = r#"null 10 10.2 "hello world" true false [1,2] {"a":"b"}"#;
        let docs: &[u8] = docs.as_ref();
        let lhs = Some(Box::new(Thunk::new_source(JsonBuf::iter(docs))));
        let mut thunk = compile("\"hello world\"", lhs).unwrap();

        let refval = Some(vec![Json::String("hello world".to_string())]);
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(None, thunk());
    }

    #[test]
    fn test_jq_array() {
        use self::Json::{Array, Integer};

        let docs = r#"null 10 10.2 "hello world" true false [1,2] {"a":"b"}"#;
        let docs: &[u8] = docs.as_ref();
        let lhs = Some(Box::new(Thunk::new_source(JsonBuf::iter(docs))));
        let mut thunk = compile("[1,2]", lhs).unwrap();

        let refval = Some(vec![Array(vec![Integer(1), Integer(2)])]);
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(None, thunk());
    }

    #[test]
    fn test_jq_object() {
        use self::Json::{Object, Integer};

        let docs = r#"null 10 10.2 "hello world" true false [1,2] {"a":"b"}"#;
        let docs: &[u8] = docs.as_ref();
        let lhs = Some(Box::new(Thunk::new_source(JsonBuf::iter(docs))));
        let mut thunk = compile("{\"a\": 10}", lhs).unwrap();

        let refval = Some(
            vec![Object(vec![KeyValue("a".to_string(), Integer(10))])]
        );
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(refval, thunk());
        assert_eq!(None, thunk());
    }
}
