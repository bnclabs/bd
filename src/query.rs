use std::{result, error, cmp, iter};
use std::fmt::{self};
use std::str::FromStr;
use std::ops::{IndexMut};
use std::collections::HashMap;

use nom::{self, {types::CompleteStr as NS}};

use json::{self};
use query_nom::parse_program_nom;
use db::{self, Document, Doctype, ItemIterator, Property};

// TODO: Parametrise Thunk over different types of Document.
// TODO: Better to replace panic with assert!() macro.
// TODO: Don't use vec![] macro, try to use with_capacity.
// TODO: ? operator is not consistent and intuitive, a through review
//       and test cases is required.


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

impl Thunk {
    pub fn prepare<D,T>(self, input: Input<D>) -> Output<T>
        where D: Document, T: Document
    {
        use query::Thunk::*;

        let t = T::default();
        match self {
            Empty | Identity => OpIdentity::new(input, t),
            Recurse => OpRecurse::new(input, t),

            Null(_opt) => OpNull::new(input, t),
            Bool(value, _opt) => OpBool::new(input, value, t),
            Integer(value, _opt) => OpInteger::new(input, value, t),
            Float(value, _opt) => OpFloat::new(input, value, t),
            String(value, _opt) => OpString::new(input, value, t),

            IndexShortcut(s, n, _opt) => OpIndex::new(input, s, n, t),
            Identifier(s, _opt) => OpIdentifier::new(input, s, t),
            Slice(a, b, _opt) => OpSlice::new(input, a, b, t),
            IterateValues(_opt) => OpItervalues::new(input, t),
            Iterate(thunks, _opt) => {
                let iters = thunks.map(|t| t.prepare(input.clone())).collect();
                OpIter::new(iters, t);
            },
            List(thunks, _opt) => {
                let iters = thunks.map(|t| t.prepare(input.clone())).collect();
                OpList(iters, t);
            }
            Dict(thunks, _opt) => {
                let iters = thunks.map(|t| t.prepare(input.clone())).collect();
                OpDict(iters, t);
            }

            Neg(thunk) => OpNeg::new(thunk.prepare(input), t),
            Not(thunk) => OpNot::new(thunk.prepare(input), t),

            Mult(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpMult::new(l, rthunk.prepare(input), t)
            },
            Div(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpDiv::new(l, rthunk.prepare(input), t)
            },
            Rem(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpRem::new(l, rthunk.prepare(input), t)
            },
            Add(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpAdd::new(l, rthunk.prepare(input), t)
            },
            Sub(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpSub::new(l, rthunk.prepare(input), t)
            },
            Shr(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpShr::new(l, rthunk.prepare(input), t)
            },
            Shl(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpShl::new(l, rthunk.prepare(input), t)
            },
            BitAnd(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpBitAnd::new(l, rthunk.prepare(input), t)
            },
            BitXor(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpBitXor::new(l, rthunk.prepare(input), t)
            },
            BitOr(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpBitOr::new(l, rthunk.prepare(input), t)
            },
            Eq(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpEq::new(l, rthunk.prepare(input), t)
            },
            Ne(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpNe::new(l, rthunk.prepare(input), t)
            },
            Lt(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpLt::new(l, rthunk.prepare(input), t)
            },
            Le(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpLe::new(l, rthunk.prepare(input), t)
            },
            Gt(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpGt::new(l, rthunk.prepare(input), t)
            },
            Ge(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpGe::new(l, rthunk.prepare(input), t)
            },
            And(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpAnd::new(l, rthunk.prepare(input), t)
            },
            Or(lthunk, rthunk) => {
                let l = lthunk.prepare(input.clone());
                OpOr::new(l, rthunk.prepare(input), t)
            },
            Pipe(lthunk, rthunk) => {
                rthunk.prepare(lthunk.prepare(input))
            },

            Builtin(name, thunks) => {
                let args = thunks.map(|t| t.prepare(input.clone())).collect();
                match name {
                    "length" => BuiltinLength::new(args, t),
                    "chars" => BuiltinChars::new(args, t),
                    "keys" => BuiltinKeys::new(args, t),
                    "has" => BuiltinHas::new(args, t),
                    "in" => BuiltinIn::new(args, t),
                    "map" => BuiltinMap::new(args, t),
                    "any" => BuiltinAny::new(args, t),
                    "all" => BuiltinAll::new(args, t),
                }
            }
        }
    }
}

impl FromStr for Thunk {
    type Err=Error;

    fn from_str(text: &str) -> Result<Thunk> {
        let (_x, thunk) = parse_program_nom(NS(text))?;
        Ok(thunk)
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
