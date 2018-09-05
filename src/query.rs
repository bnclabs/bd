use std::{cmp};
use std::str::FromStr;

use nom::{self, {types::CompleteStr as NS}};

use query_nom::parse_program_nom;
use db::{Document, Input};
use ops;

// TODO: Parametrise Thunk over different types of Document.
// TODO: Better to replace panic with assert!() macro.
// TODO: Don't use vec![] macro, try to use with_capacity.
// TODO: ? operator is not consistent and intuitive, a through review
//       and test cases is required.


pub struct Expr {
    thunk: Thunk,
}

impl Expr {
    pub fn prepare<'a,D>(&'a self, input: Input<'a,D>) -> Input<'a,D>
        where D: 'a + Document
    {
        return self.thunk.prepare(input)
    }
}

impl FromStr for Expr {
    type Err=String;

    fn from_str(text: &str) -> Result<Expr,String> {
        let thunk: Thunk = text.parse()?;
        Ok(Expr{ thunk })
    }
}


#[derive(Debug,Clone)]
pub(crate) enum Thunk where {
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
    Dict(Vec<(Thunk, Thunk)>, bool),
    // Operations in decreasing precedance
    Neg(Box<Thunk>),
    Not(Box<Thunk>),
    Mul(Box<Thunk>, Box<Thunk>),
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

impl Thunk {
    fn prepare<'a, D>(&'a self, input: Input<'a, D>) -> Input<D>
        where
        D: 'a + Document,
    {
        use query::Thunk::*;

        match self {
            Empty | Identity => Box::new(ops::Identity::new(input)),
            Recurse => Box::new(ops::Recurse::new(input)),

            Null(_opt) => Box::new(ops::Null::new(input)),
            Bool(value, _opt) => Box::new(ops::Bool::new(input, *value)),
            Integer(value, _opt) => Box::new(ops::Integer::new(input, *value)),
            Float(value, _opt) => Box::new(ops::Float::new(input, *value)),
            String(value, _opt) => {
                Box::new(ops::StringLit::new(input, value.clone()))
            },

            IndexShortcut(s, n, _opt) => {
                Box::new(ops::Index::new(input, s.clone(), *n))
            },
            Identifier(s, _opt) => {
                Box::new(ops::Identifier::new(input, s.clone()))
            },
            Slice(a, b, _opt) => Box::new(ops::Slice::new(input, *a, *b)),
            IterateValues(_opt) => Box::new(ops::IterValues::new(input)),
            Iterate(thunks, _opt) => {
                let mut inputs = Vec::new();
                for thunk in thunks.iter() {
                    inputs.push(thunk.prepare(input.repeat()));
                }
                Box::new(ops::Iter::new(inputs))
            },
            List(thunks, _opt) => {
                let mut inputs = Vec::new();
                for thunk in thunks.iter() {
                    inputs.push(thunk.prepare(input.repeat()));
                }
                Box::new(ops::List::new(inputs))
            },
            Dict(thunks, _opt) => {
                let mut inputs = Vec::new();
                for (kt, vt) in thunks.iter() {
                    inputs.push(
                        (kt.prepare(input.repeat()), vt.prepare(input.repeat()))
                    );
                }
                Box::new(ops::Dict::new(inputs))
            },

            Neg(thunk) => Box::new(ops::Neg::new(thunk.prepare(input))),
            Not(thunk) => Box::new(ops::Not::new(thunk.prepare(input))),

            Mul(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Mul::new(lhs, rhs))
            },
            Div(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Div::new(lhs, rhs))
            },
            Rem(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Rem::new(lhs, rhs))
            },
            Add(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Add::new(lhs, rhs))
            },
            Sub(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Sub::new(lhs, rhs))
            },
            Shr(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Shr::new(lhs, rhs))
            },
            Shl(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Shl::new(lhs, rhs))
            },
            BitAnd(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Bitand::new(lhs, rhs))
            },
            BitXor(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Bitxor::new(lhs, rhs))
            },
            BitOr(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Bitor::new(lhs, rhs))
            },
            Eq(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Eq::new(lhs, rhs))
            },
            Ne(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Ne::new(lhs, rhs))
            },
            Lt(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Lt::new(lhs, rhs))
            },
            Le(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Le::new(lhs, rhs))
            },
            Gt(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Gt::new(lhs, rhs))
            },
            Ge(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Ge::new(lhs, rhs))
            },
            And(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::And::new(lhs, rhs))
            },
            Or(lthunk, rthunk) => {
                let lhs = lthunk.prepare(input.repeat());
                let rhs = rthunk.prepare(input.repeat());
                Box::new(ops::Or::new(lhs, rhs))
            },
            Pipe(lthunk, rthunk) => rthunk.prepare(lthunk.prepare(input)),

            Builtin(name, thunks) => {
                let mut args = Vec::new();
                for thunk in thunks.iter() {
                    args.push(thunk.prepare(input.repeat()))
                }
                match name.as_str() {
                    "length" => Box::new(ops::BuiltinLength::new(args)),
                    "chars" => Box::new(ops::BuiltinChars::new(args)),
                    "keys" => Box::new(ops::BuiltinKeys::new(args)),
                    //"has" => BuiltinHas::new(args),
                    //"in" => BuiltinIn::new(args),
                    //"map" => BuiltinMap::new(args),
                    //"any" => BuiltinAny::new(args),
                    //"all" => BuiltinAll::new(args),
                    _ => unreachable!(), // TODO: implement BuiltinInvalid
                }
            },
            // _ => unreachable!(),
        }
    }
}

impl FromStr for Thunk {
    type Err=String;

    fn from_str(text: &str) -> Result<Thunk,String> {
        use nom::simple_errors::Context as NomContext;

        let res = parse_program_nom(NS(text));
        match res {
            Ok((_x, thunk)) => Ok(thunk),
            Err(err) => match err {
                nom::Err::Incomplete(_) => {
                    Err(format!("{}", err))
                },
                nom::Err::Error(NomContext::Code(rem, _)) => {
                    let rem_till = cmp::min(5, rem.len());
                    Err(format!("error at {:}", &rem[..rem_till]))
                },
                nom::Err::Failure(NomContext::Code(rem, _)) => {
                    let rem_till = cmp::min(5, rem.len());
                    Err(format!("failure at {}", &rem[..rem_till]))
                },
            }
        }
    }
}

//#[cfg(test)]
//mod test {
//    use super::*;
//    use json::Json;
//    use json::Json::{Integer, String as S};
//
//    #[test]
//    fn test_query_empty() {
//        let mut thunk: Thunk = "".parse().unwrap();
//        let out = thunk(S("hello".to_string()), &mut c).unwrap();
//        assert_eq!(1, out.len());
//        assert_eq!(S("hello".to_string()), out[0]);
//
//        let mut thunk: Thunk = "   \t \n ".parse().unwrap();
//        let out = thunk(Integer(10), &mut c).unwrap();
//        assert_eq!(1, out.len());
//        assert_eq!(Integer(10), out[0]);
//    }
//
//    #[test]
//    fn test_query_literal() {
//        let doc: Json = "[10]".parse().unwrap();
//
//        let mut thunk: Thunk = "null".parse().unwrap();
//        let out = c.eval(&mut thunk, doc.clone()).unwrap();
//        assert_eq!("[null]", &format!("{:?}", out));
//
//        thunk = "true".parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[true]", &format!("{:?}", out));
//
//        thunk = "false".parse().unwrap();
//        let out = c.eval(&mut thunk, doc.clone()).unwrap();
//        assert_eq!("[false]", &format!("{:?}", out));
//
//        thunk = "10".parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[10]", &format!("{:?}", out));
//
//        thunk = "10.2".parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[1.02e1]", &format!("{:?}", out));
//
//        thunk = "\"hello\"".parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[\"hello\"]", &format!("{:?}", out));
//
//        thunk = r#"[10.2, true, null, "hello"]"#.parse().unwrap();
//        //println!("{:?}", thunk);
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(r#"[[1.02e1,true,null,"hello"]]"#, &format!("{:?}", out));
//
//        thunk = r#"{"x":12, "y":[10,20], "z":{"a":true}}"#.parse().unwrap();
//        //println!("{:?}", thunk);
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        let refval = r#"[{"x":12,"y":[10,20],"z":{"a":true}}]"#;
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_identity() {
//        let mut thunk: Thunk = ".".parse().unwrap();
//
//        let doc: Json = "null".parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[null]", &format!("{:?}", out));
//
//        let doc: Json = "true".parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[true]", &format!("{:?}", out));
//
//        let doc: Json = "false".parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[false]", &format!("{:?}", out));
//
//        let doc: Json = "10".parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[10]", &format!("{:?}", out));
//
//        let doc: Json = "10.2".parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[1.02e1]", &format!("{:?}", out));
//
//        let doc: Json = "\"hello\"".parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[\"hello\"]", &format!("{:?}", out));
//
//        let doc: Json = "[true,10]".parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[[true,10]]", &format!("{:?}", out));
//
//        let doc: Json = "{\"a\": 10}".parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[{\"a\":10}]", &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_get() {
//        let mut thunk: Thunk = ".foo".parse().unwrap();
//
//        let doc: Json = r#"{"foo": 10}"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[10]", &format!("{:?}", out));
//
//        let doc: Json = r#"{"notfoo": 10}"#.parse().unwrap();
//        let err = "cannot index foo into Object".to_string();
//        assert_eq!(Err(Error::Op(None, err)), thunk(doc.clone(), &mut c));
//
//        thunk = r#".foo?"#.parse().unwrap();
//        let doc: Json = r#"{"nonfoo": 10}"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[]", &format!("{:?}", out));
//
//        thunk = r#"."foo""#.parse().unwrap();
//        let doc: Json = r#"{"foo": 10}"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[10]", &format!("{:?}", out));
//
//        thunk = r#".["foo"]"#.parse().unwrap();
//        let doc: Json = r#"{"foo": 10}"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[10]", &format!("{:?}", out));
//
//        thunk = r#".["foo"]?"#.parse().unwrap();
//        let doc: Json = r#"{"nonfoo": 10}"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[]", &format!("{:?}", out));
//
//        thunk = r#".[foo]"#.parse().unwrap();
//        let doc: Json = r#"{"foo": 10}"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[10]", &format!("{:?}", out));
//
//        thunk = r#".[foo]?"#.parse().unwrap();
//        let doc: Json = r#"{"nonfoo": 10}"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[]", &format!("{:?}", out));
//
//        thunk = r#"."foo.bar""#.parse().unwrap();
//        let doc: Json = r#"{"foo.bar": 10}"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[10]", &format!("{:?}", out));
//
//        thunk = r#".["foo.bar"]"#.parse().unwrap();
//        let doc: Json = r#"{"foo.bar": 10}"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[10]", &format!("{:?}", out));
//
//        thunk = r#".["foo.bar"?]"#.parse().unwrap();
//        let doc: Json = r#"{"nonfoo.bar": 10}"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[]", &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_index() {
//        let mut thunk: Thunk = ".0".parse().unwrap();
//
//        let doc: Json = r#"[10, true, "hello"]"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[10]", &format!("{:?}", out));
//
//        thunk = ".2".parse().unwrap();
//        let doc: Json = r#"[10, true, "hello"]"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[\"hello\"]", &format!("{:?}", out));
//
//        thunk = ".2".parse().unwrap();
//        let doc: Json = r#"[10, true, "hello"]"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(r#"["hello"]"#, &format!("{:?}", out));
//
//        thunk = ".[0]".parse().unwrap();
//        let doc: Json = r#"[10, true, "hello"]"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[10]", &format!("{:?}", out));
//
//        thunk = ".[2]".parse().unwrap();
//        let doc: Json = r#"[10, true, "hello"]"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[\"hello\"]", &format!("{:?}", out));
//
//        thunk = "-.[2]".parse().unwrap();
//        let doc: Json = r#"[10, true, 20]"#.parse().unwrap();
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!("[-20]", &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_slice_array() {
//        let mut thunk: Thunk = ".[2..4]".parse().unwrap();
//        let doc: Json = r#"["a", "b", "c", "d", "e"]"#.parse().unwrap();
//
//        let refval = r#"[["c","d"]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[2..=3]".parse().unwrap();
//        let refval = r#"[["c","d"]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[..3]".parse().unwrap();
//        let refval = r#"[["a","b","c"]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[..=3]".parse().unwrap();
//        let refval = r#"[["a","b","c","d"]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[2..]".parse().unwrap();
//        let refval = r#"[["c","d","e"]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[..]".parse().unwrap();
//        let refval = r#"[["a","b","c","d","e"]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_slice_string() {
//        let mut thunk: Thunk = ".[2..4]".parse().unwrap();
//        let doc: Json = r#""abcdefghi""#.parse().unwrap();
//
//        let refval = r#"["cd"]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[2..=3]".parse().unwrap();
//        let refval = r#"["cd"]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[..3]".parse().unwrap();
//        let refval = r#"["abc"]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[..=3]".parse().unwrap();
//        let refval = r#"["abcd"]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[2..]".parse().unwrap();
//        let refval = r#"["cdefghi"]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[..]".parse().unwrap();
//        let refval = r#"["abcdefghi"]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_iterator() {
//        let mut thunk: Thunk = ".[]".parse().unwrap();
//
//        let doc: Json = r#"[1,2,3]"#.parse().unwrap();
//        let refval = r#"[1, 2, 3]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[]".parse().unwrap();
//        let doc: Json = r#"{"a": true, "b": 2, "c": null}"#.parse().unwrap();
//        let refval = r#"[true, 2, null]"#;
//        let out =thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[]".parse().unwrap();
//        let doc = r#"[{"na":"JS","go":true},{"name":"XML","good":false}]"#;
//        let doc: Json = doc.parse().unwrap();
//        let refval = r#"[{"go":true,"na":"JS"}, {"good":false,"name":"XML"}]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[]".parse().unwrap();
//        let doc: Json = r#"[]"#.parse().unwrap();
//        let refval = r#"[]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[]".parse().unwrap();
//        let doc: Json = r#"{"a": 1, "b": 1}"#.parse().unwrap();
//        let refval = r#"[1, 1]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[]".parse().unwrap();
//        let doc: Json = r#"10"#.parse().unwrap();
//        assert_eq!(
//            Err(Error::Op(None, "Integer not an iterable".to_string())),
//            thunk(doc.clone(), &mut c)
//        );
//
//        thunk = ".[]?".parse().unwrap();
//        let doc: Json = r#"10"#.parse().unwrap();
//        let refval = r#"[]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[foo, bar]".parse().unwrap();
//        let doc = r#"{"foo": 42, "bar": "something else", "baz": true}"#;
//        let doc: Json = doc.parse().unwrap();
//        let refval = r#"[42, "something else"]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[user, projects.[]]".parse().unwrap();
//        let doc = r#"{"user":"stedolan", "projects": ["jq", "wikiflow"]}"#;
//        let doc: Json = doc.parse().unwrap();
//        let refval = r#"["stedolan", "jq", "wikiflow"]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_pipe() {
//        let mut thunk: Thunk = ".[] | foo".parse().unwrap();
//
//        let doc: Json = r#"[{"foo": 10}, {"foo":20}]"#.parse().unwrap();
//        let refval = r#"[10, 20]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = ".[] | .foo ".parse().unwrap();
//        let doc: Json = r#"[{"foo": 10}, {"foo":20}]"#.parse().unwrap();
//        let refval = r#"[10, 20]"#;
//        let out =thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//
//        thunk = ".a.b.c".parse().unwrap();
//        let doc: Json = r#"{"a": {"b": {"c": 100}}}"#.parse().unwrap();
//        let refval = r#"[100]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#".a | .b | .c"#.parse().unwrap();
//        let doc: Json = r#"{"a": {"b": {"c": 100}}}"#.parse().unwrap();
//        let refval = r#"[100]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#".a | . | .b"#.parse().unwrap();
//        let doc: Json = r#"{"a": {"b": 100}}"#.parse().unwrap();
//        let refval = r#"[100]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_paranthesis() {
//        let mut thunk: Thunk = "2 + . * 15".parse().unwrap();
//
//        let doc: Json = r#"10"#.parse().unwrap();
//        let refval = r#"[152]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"(2 + .) * 15"#.parse().unwrap();
//        let doc: Json = r#"10"#.parse().unwrap();
//        let refval = r#"[180]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_collection_list() {
//        let mut thunk: Thunk = r#"[]"#.parse().unwrap();
//
//        let doc: Json = r#"10"#.parse().unwrap();
//        let refval = r#"[[]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"[1,2,3]"#.parse().unwrap();
//        let doc: Json = r#"10"#.parse().unwrap();
//        let refval = r#"[[1,2,3]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"[foo, .bar, baz]"#.parse().unwrap();
//        let doc: Json = r#"{"foo": 10, "bar":20, "baz":30}"#.parse().unwrap();
//        let refval = r#"[[10,20,30]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"[.0, .4, .2]"#.parse().unwrap();
//        let doc: Json = r#"[10,20,30,40,50]"#.parse().unwrap();
//        let refval = r#"[[10,50,30]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"[.items.[].name]"#.parse().unwrap();
//        let doc = r#"{"items": [{"name": "x"}, {"name":"y"}]}"#;
//        let doc: Json = doc.parse().unwrap();
//        let refval = r#"[["x","y"]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"[.user, .projects.[]]"#.parse().unwrap();
//        let doc = r#"{"user":"stedolan", "projects": ["jq", "wikiflow"]}"#;
//        let doc: Json = doc.parse().unwrap();
//        let refval = r#"[["stedolan","jq","wikiflow"]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_collection_object() {
//        let mut thunk: Thunk = r#"{"a":42,"b":17}"#.parse().unwrap();
//
//        let doc: Json = r#"10"#.parse().unwrap();
//        let refval = r#"[{"a":42,"b":17}]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"{a:42, b:17} "#.parse().unwrap();
//        let doc: Json = r#"10"#.parse().unwrap();
//        let refval = r#"[{"a":42,"b":17}]"#;
//        let out =thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"{(."a"+"-"+."b"):59}"#.parse().unwrap();
//        let doc: Json = r#"{"a":"firstname","b":"lastname"}"#.parse().unwrap();
//        let refval = r#"[{"firstname-lastname":59}]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"{foo:.bar}"#.parse().unwrap();
//        let doc: Json = r#"{"bar":10}"#.parse().unwrap();
//        let refval = r#"[{"foo":10}]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"{user: .user, title: .title}"#.parse().unwrap();
//        let doc = r#"{"user":"prom","title":"testing","age":30,"city":"ax"}"#;
//        let doc: Json = doc.parse().unwrap();
//        let refval = r#"[{"title":"testing","user":"prom"}]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"{user, title}"#.parse().unwrap();
//        let doc = r#"{"user":"prom","title":"testing","age":30,"city":"ax"}"#;
//        let doc: Json = doc.parse().unwrap();
//        let refval = r#"[{"title":"testing","user":"prom"}]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"{(ks.[]), title}"#.parse().unwrap();
//        let doc = r#"{"ks":["age","city"],"age":30,"city":"ax","title":null}"#;
//        let doc: Json = doc.parse().unwrap();
//        let refval = r#"[{"age":30,"city":"ax","title":null}]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"{user, title: .titles.[]}"#.parse().unwrap();
//        let doc = r#"{"user":"sted","titles":["JQ Primer", "More JQ"]}"#;
//        let doc: Json = doc.parse().unwrap();
//        let mut refval = r#"[{"title":"JQ Primer","user":"sted"}, "#.to_string();
//        refval += r#"{"title":"More JQ","user":"sted"}]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(&refval, &format!("{:?}", out));
//
//        thunk = r#"{(.user): .titles}"#.parse().unwrap();
//        let doc = r#"{"user":"stedolan","titles":["JQ Primer", "More JQ"]}"#;
//        let doc: Json = doc.parse().unwrap();
//        let refval = r#"[{"stedolan":["JQ Primer","More JQ"]}]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_recurse() {
//        let mut thunk: Thunk = r#"..|.a?"#.parse().unwrap();
//
//        let doc: Json = r#"[[{"a":1}, {"a":2}],{"a":3}]"#.parse().unwrap();
//        let refval = r#"[1, 2, 3]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_addition() {
//        let mut thunk: Thunk = r#"a+b"#.parse().unwrap();
//
//        let doc: Json = r#"{"a":1,"b":2}"#.parse().unwrap();
//        let refval = r#"[3]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a+b"#.parse().unwrap();
//        let doc: Json = r#"{"a":1.2,"b":2.3}"#.parse().unwrap();
//        let refval = r#"[3.5e0]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a+b+c"#.parse().unwrap();
//        let doc: Json = r#"{"a":[1,2],"b":[],"c":[3,4]}"#.parse().unwrap();
//        let refval = r#"[[1,2,3,4]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a+b+c"#.parse().unwrap();
//        let doc: Json = r#"{"a":"hello","b":"","c":"world"}"#.parse().unwrap();
//        let refval = r#"["helloworld"]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a+b+c"#.parse().unwrap();
//        let doc = r#"{"a":{"x":1},"b":{"x":2},"c":{"y":2}}"#;
//        let doc: Json = doc.parse().unwrap();
//        let refval = r#"[{"x":2,"y":2}]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_subraction() {
//        let mut thunk: Thunk = r#"a-b"#.parse().unwrap();
//
//        let doc: Json = r#"{"a":1,"b":2}"#.parse().unwrap();
//        let refval = r#"[-1]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a-b"#.parse().unwrap();
//        let doc: Json = r#"{"a":1.3,"b":2.1}"#.parse().unwrap();
//        let refval = r#"[-8e-1]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a-b"#.parse().unwrap();
//        let doc: Json = r#"{"a":[1,2],"b":[2]}"#.parse().unwrap();
//        let refval = r#"[[1]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_multiplication() {
//        let mut thunk: Thunk = r#"a*b"#.parse().unwrap();
//
//        let doc: Json = r#"{"a":1,"b":2}"#.parse().unwrap();
//        let refval = r#"[2]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a*b"#.parse().unwrap();
//        let doc: Json = r#"{"a":1.2,"b":2}"#.parse().unwrap();
//        let refval = r#"[2.4e0]"#;
//        let out =thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a*b"#.parse().unwrap();
//        let doc: Json = r#"{"a":5,"b":2.5}"#.parse().unwrap();
//        let refval = r#"[1.25e1]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a*b"#.parse().unwrap();
//        let doc: Json = r#"{"a":1.2,"b":2.1}"#.parse().unwrap();
//        let refval = r#"[2.52e0]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a*b"#.parse().unwrap();
//        let doc: Json = r#"{"a":"hello","b":0}"#.parse().unwrap();
//        let refval = r#"[null]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a*b"#.parse().unwrap();
//        let doc: Json = r#"{"a":"hello","b":2}"#.parse().unwrap();
//        let refval = r#"["hellohello"]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a*b"#.parse().unwrap();
//        let doc: Json = r#"{"a":{"x":1},"b":{"y":2}}"#.parse().unwrap();
//        let refval = r#"[{"x":1,"y":2}]"#;
//        let out =thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_division() {
//        let mut thunk: Thunk = r#"a/b"#.parse().unwrap();
//
//        let doc: Json = r#"{"a":1,"b":2}"#.parse().unwrap();
//        let refval = r#"[5e-1]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a/b"#.parse().unwrap();
//        let doc: Json = r#"{"a":1.2,"b":2}"#.parse().unwrap();
//        let refval = r#"[6e-1]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a/b"#.parse().unwrap();
//        let doc: Json = r#"{"a":5,"b":2.5}"#.parse().unwrap();
//        let refval = r#"[2e0]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a/b"#.parse().unwrap();
//        let doc: Json = r#"{"a":1.2,"b":2.1}"#.parse().unwrap();
//        let refval = r#"[5.714285714285714e-1]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a/b"#.parse().unwrap();
//        let doc: Json = r#"{"a":1,"b":0}"#.parse().unwrap();
//        let refval = r#"[null]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a/b"#.parse().unwrap();
//        let doc: Json = r#"{"a":1.2,"b":0}"#.parse().unwrap();
//        let refval = r#"[null]"#;
//        let out =  thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a/b"#.parse().unwrap();
//        let doc: Json = r#"{"a":"a,b,c,d","b":","}}"#.parse().unwrap();
//        let refval = r#"[["a","b","c","d"]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_rem() {
//        let mut thunk: Thunk = r#"a%b"#.parse().unwrap();
//
//        let doc: Json = r#"{"a":1,"b":2}"#.parse().unwrap();
//        let refval = r#"[1]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a%b"#.parse().unwrap();
//        let doc: Json = r#"{"a":1.2,"b":2}"#.parse().unwrap();
//        let refval = r#"[1.2e0]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a%b"#.parse().unwrap();
//        let doc: Json = r#"{"a":5,"b":2.5}"#.parse().unwrap();
//        let refval = r#"[0e0]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a%b"#.parse().unwrap();
//        let doc: Json = r#"{"a":1.2,"b":2.1}"#.parse().unwrap();
//        let refval = r#"[1.2e0]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a%b"#.parse().unwrap();
//        let doc: Json = r#"{"a":1,"b":0}"#.parse().unwrap();
//        let refval = r#"[null]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"a%b"#.parse().unwrap();
//        let doc: Json = r#"{"a":1.2,"b":0}"#.parse().unwrap();
//        let refval = r#"[null]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_builtin_len() {
//        let mut thunk: Thunk = r#". | length"#.parse().unwrap();
//        let doc: Json = r#"null"#.parse().unwrap();
//        let refval = r#"[0]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#". | length"#.parse().unwrap();
//        let doc: Json = r#"[1,2,3]"#.parse().unwrap();
//        let refval = r#"[3]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#". | length"#.parse().unwrap();
//        let doc: Json = r#"{"a":1, "b":2}"#.parse().unwrap();
//        let refval = r#"[2]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#". | length"#.parse().unwrap();
//        let doc: Json = r#""hello world""#.parse().unwrap();
//        let refval = r#"[11]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#".[] | length"#.parse().unwrap();
//        let doc: Json = r#"[[1,2], "string", {"a":2}, null]"#.parse().unwrap();
//        let refval = r#"[2, 6, 1, 0]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"length"#.parse().unwrap();
//        let doc: Json = r#""汉语""#.parse().unwrap();
//        let refval = r#"[6]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_builtin_chars() {
//        let mut thunk: Thunk = r#". | chars | length"#.parse().unwrap();
//        let doc: Json = r#""汉语""#.parse().unwrap();
//        let refval = r#"[2]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_builtin_keys() {
//        let mut thunk: Thunk = r#". | keys"#.parse().unwrap();
//        let doc: Json = r#"{"abc": 1, "abcd": 2, "Foo": 3}"#.parse().unwrap();
//        let refval = r#"[["Foo","abc","abcd"]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        let mut thunk: Thunk = r#". | keys"#.parse().unwrap();
//        let doc: Json = r#"[10,20,30]"#.parse().unwrap();
//        let refval = r#"[[0,1,2]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_builtin_has() {
//        let mut thunk: Thunk = r#"has("foo")"#.parse().unwrap();
//        let doc: Json = r#"{"foo": 1, "abcd": 2, "Foo": 3}"#.parse().unwrap();
//        let refval = r#"[true]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"has("foo")"#.parse().unwrap();
//        let doc: Json = r#"["foo", 1, "abcd", 2]"#.parse().unwrap();
//        let refval = r#"[true]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"has(1)"#.parse().unwrap();
//        let doc: Json = r#"[1, 2]"#.parse().unwrap();
//        let refval = r#"[true]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_builtin_in() {
//        let mut thunk: Thunk = r#".[] | in({"foo": 42})"#.parse().unwrap();
//        let doc: Json = r#"["foo", "bar"]"#.parse().unwrap();
//        let refval = r#"[true, false]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"[.[] | in([1,0])]"#.parse().unwrap();
//        let doc: Json = r#"[2, 0]"#.parse().unwrap();
//        let refval = r#"[[false,true]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"map(in([0,1]))"#.parse().unwrap();
//        let doc: Json = r#"[2, 0]"#.parse().unwrap();
//        let refval = r#"[[false,true]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_builtin_map() {
//        let mut thunk: Thunk = r#"map(.)"#.parse().unwrap();
//
//        let doc: Json = r#"["foo", "bar"]"#.parse().unwrap();
//        let refval = r#"[["foo","bar"]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"map(.+1)"#.parse().unwrap();
//        let doc: Json = r#"[1, 2, 3]"#.parse().unwrap();
//        let refval = r#"[[2,3,4]]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"map(.+1)"#.parse().unwrap();
//        let doc: Json = r#"{"foo":1, "bar":2}"#.parse().unwrap();
//        let refval = r#"[{"bar":3,"foo":2}]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"map(.+1)"#.parse().unwrap();
//        let doc: Json = r#"{"a": 1, "b": 2, "c": 3}"#.parse().unwrap();
//        let refval = r#"[{"a":2,"b":3,"c":4}]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_builtin_any() {
//        let mut thunk: Thunk = r#"any(. == 1)"#.parse().unwrap();
//
//        let doc: Json = r#"[1, 2]"#.parse().unwrap();
//        let refval = r#"[true]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"any(. == "a")"#.parse().unwrap();
//        let doc: Json = r#"{"x": "a", "y": "b"}"#.parse().unwrap();
//        let refval = r#"[true]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"any(. == 1)"#.parse().unwrap();
//        let doc: Json = r#"[2,3]"#.parse().unwrap();
//        let refval = r#"[false]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"any(. == "a")"#.parse().unwrap();
//        let doc: Json = r#"{"x": "c", "y": "b"}"#.parse().unwrap();
//        let refval = r#"[false]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//
//    #[test]
//    fn test_query_builtin_all() {
//        let mut thunk: Thunk = r#"all(. == 1)"#.parse().unwrap();
//
//        let doc: Json = r#"[1, 1]"#.parse().unwrap();
//        let refval = r#"[true]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"all(. == "a")"#.parse().unwrap();
//        let doc: Json = r#"{"x": "a", "y": "a"}"#.parse().unwrap();
//        let refval = r#"[true]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"all(. == 1)"#.parse().unwrap();
//        let doc: Json = r#"[1,3]"#.parse().unwrap();
//        let refval = r#"[false]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//
//        thunk = r#"all(. == "a")"#.parse().unwrap();
//        let doc: Json = r#"{"x": "a", "y": "b"}"#.parse().unwrap();
//        let refval = r#"[false]"#;
//        let out = thunk(doc.clone(), &mut c).unwrap();
//        assert_eq!(refval, &format!("{:?}", out));
//    }
//}
