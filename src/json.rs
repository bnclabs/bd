use std::{self, result, char, error, io};
use std::str::{self, FromStr,CharIndices};
use std::fmt::{self, Write};
use std::cmp::Ordering;
use std::ops::{Neg, Not, Mul, Div, Rem, Add, Sub, Shr, Shl, BitAnd, BitXor};
use std::ops::{BitOr, Index, IndexMut};
use std::ops::{Range, RangeFrom, RangeTo, RangeToInclusive, RangeInclusive};
use std::ops::{RangeFull};

use lex::Lex;

include!("./json.rs.lookup");

pub type Result<T> = result::Result<T,Error>;


#[derive(Debug,Eq,PartialEq)]
pub enum Error {
    Parse(String),
    ParseFloat(std::num::ParseFloatError, String),
    ParseInt(std::num::ParseIntError, String),
    NotMyType(String),
    KeyMissing(usize, String),
    IndexUnbound(usize, usize),
}

impl Error {
    fn key_missing_at(&self) -> usize {
        match *self { Error::KeyMissing(at, _) => at, _ => unreachable!() }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use json::Error::*;

        match self {
            Parse(s) => write!(f, "{}", s),
            ParseFloat(err, s) => write!(f, "{}, {}", err, s),
            ParseInt(err, s) => write!(f, "{}, {}", err, s),
            NotMyType(s) => write!(f, "not expected type {}", s),
            KeyMissing(_, key) => write!(f, "missing key {}", key),
            IndexUnbound(s,e) => write!(f, "index out of bound {}..{}", s, e),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&error::Error> {
        match self {
            Error::ParseFloat(err, _) => Some(err),
            Error::ParseInt(err, _) => Some(err),
            _ => None,
        }
    }
}

impl From<std::num::ParseFloatError> for Error {
    fn from(err: std::num::ParseFloatError) -> Error {
        Error::ParseFloat(err, String::new())
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(err: std::num::ParseIntError) -> Error {
        Error::ParseInt(err, String::new())
    }
}



pub struct JsonBuf {
    inner: String,
    lex: Lex,
}

impl JsonBuf {
    fn init(s: Option<String>, l: Option<Lex>) -> JsonBuf {
        let inner = match s { Some(val) => val, None => String::new() };
        let lex = match l { Some(val) => val, None => Lex::new(0, 1, 1) };
        JsonBuf { inner, lex }
    }

    pub fn new() -> JsonBuf {
        JsonBuf::init(None, None)
    }

    pub fn with_capacity(cap: usize) -> JsonBuf {
        JsonBuf::init(Some(String::with_capacity(cap)), None)
    }

    pub fn iter<R>(r: R) -> JsonIterate<R> where R: io::Read {
        JsonIterate::new(r)
    }

    pub fn parse_str(text: &str) -> Result<Json> {
        let mut lex = Lex::new(0, 1, 1);
        parse_value(&text, &mut lex)
    }

    pub fn set<T>(&mut self, text: &T) where T: AsRef<str> + ?Sized {
        self.inner.clear();
        self.lex.set(0, 1, 1);
        self.inner.push_str(text.as_ref());
    }

    pub fn append<T>(&mut self, text: &T) where T: AsRef<str> + ?Sized {
        self.inner.push_str(text.as_ref());
    }

    pub fn parse(&mut self) -> Result<Json> {
        self.lex.set(0, 1, 1);
        let val = parse_value(&self.inner, &mut self.lex)?;
        self.inner = self.inner[self.lex.off..].to_string(); // remaining text.
        Ok(val)
    }
}

impl From<String> for JsonBuf {
    fn from(s: String) -> JsonBuf {
        JsonBuf::init(Some(s), None)
    }
}

impl<'a, T> From<&'a T> for JsonBuf where T: AsRef<str> + ?Sized {
    fn from(s: &'a T) -> JsonBuf {
        JsonBuf::from(s.as_ref().to_string())
    }
}


pub struct JsonIterate<R> where R: io::Read {
    inner: String,
    lex: Lex,
    reader: R,
    buffer: Vec<u8>,
}

impl<R> JsonIterate<R> where R: io::Read {
    const BLOCK_SIZE: usize = 1024;

    fn new(reader: R) -> JsonIterate<R> {
        let inner = String::with_capacity(Self::BLOCK_SIZE);
        let mut buffer = Vec::with_capacity(Self::BLOCK_SIZE);
        let lex = Lex::new(0, 1, 1);
        unsafe{ buffer.set_len(Self::BLOCK_SIZE) };
        JsonIterate{ inner, lex, reader, buffer }
    }
}

impl<R> Iterator for JsonIterate<R> where R: io::Read {
    type Item=Json;

    fn next(&mut self) -> Option<Json> {
        // TODO: automatically adjust the cap/len of self.buffer.
        self.lex.set(0, 1, 1);
        loop {
            if let Ok(val) = parse_value(&self.inner, &mut self.lex) {
                self.inner = self.inner[self.lex.off..].to_string();
                return Some(val)
            }
            let v = match self.reader.read(&mut self.buffer) {
                Ok(0) | Err(_) => return None,
                Ok(n) => unsafe { str::from_utf8_unchecked(&self.buffer[..n]) },
            };
            self.inner.push_str(v);
        }
    }
}


pub fn parse_value(text: &str, lex: &mut Lex) -> Result<Json> {
    parse_whitespace(text, lex);
    check_eof(text, lex)?;

    //println!("text -- {:?}", valtext);
    let v = match (&text[lex.off..]).as_bytes()[0] {
        b'n' => parse_null(text, lex),
        b't' => parse_true(text, lex),
        b'f' => parse_false(text, lex),
        b'0'..=b'9'|b'+'|b'-'|b'.'|b'e'|b'E' => parse_num(text, lex),
        b'"' => parse_string(text, lex),
        b'[' => parse_array(text, lex),
        b'{' => parse_object(text, lex),
        ch => {
            Err(Error::Parse(lex.format(&format!("invalid token {}", ch))))
        }
    };
    //println!("valu -- {:?}", v);

    // gather up lexical position for a subset of error-variants.
    match v {
        Err(Error::ParseFloat(e, _)) => {
            Err(Error::ParseFloat(e, lex.format("invalid float")))
        },

        Err(Error::ParseInt(e, _)) => {
            Err(Error::ParseInt(e, lex.format("invalid integer")))
        }

        rc => rc,
    }
}

fn parse_null(text: &str, lex: &mut Lex) -> Result<Json> {
    let text = &text[lex.off..];
    if text.len() >= 4 && &text[..4] == "null" {
        lex.incr_col(4);
        Ok(Json::Null)
    } else {
        Err(Error::Parse(lex.format("expected null")))
    }
}

fn parse_true(text: &str, lex: &mut Lex) -> Result<Json> {
    let text = &text[lex.off..];
    if text.len() >= 4 && &text[..4] == "true" {
        lex.incr_col(4);
        Ok(Json::Bool(true))
    } else {
        Err(Error::Parse(lex.format("expected true")))
    }
}

fn parse_false(text: &str, lex: &mut Lex) -> Result<Json> {
    let text = &text[lex.off..];
    if text.len() >= 5 && &text[..5] == "false" {
        lex.incr_col(5);
        Ok(Json::Bool(false))
    } else {
        Err(Error::Parse(lex.format("expected false")))
    }
}

fn parse_num(text: &str, lex: &mut Lex) -> Result<Json> {
    let text = &text[lex.off..];
    let mut doparse = |text: &str, i: usize, is_float: bool| -> Result<Json> {
        lex.incr_col(i);
        if is_float {
            let val = text.parse::<f64>()?;
            Ok(Json::Float(val))
        } else {
            let val = text.parse::<i128>()?;
            Ok(Json::Integer(val))
        }
    };

    let mut is_float = false;
    for (i, ch) in text.char_indices() {
        match ch {
            '0'..='9'|'+'|'-' => continue, // valid number
            '.'|'e'|'E' => { is_float = true; continue}, // float number
            _ => (),
        }
        return doparse(&text[..i], i, is_float)
    }
    doparse(text, text.len(), is_float)
}

pub fn parse_string(text: &str, lex: &mut Lex) -> Result<Json> {
    use self::Json::{String as S};

    let mut escape = false;
    let mut res = String::new();
    let mut chars = (&text[lex.off..]).char_indices();

    let (i, ch) = chars.next().unwrap(); // skip the opening quote
    if ch != '"' {
        return Err(Error::Parse(lex.format("not a string")))
    }

    while let Some((i, ch)) = chars.next() {
        if escape == false {
            if ch == '\\' {
                escape = true;
                continue
            }
            match ch {
                '"' => {
                    lex.incr_col(i+1);
                    return Ok(S(res));
                },
                _ => res.push(ch),
            }
            continue
        }

        // previous char was escape
        match ch {
            '"' => res.push('"'),
            '\\' => res.push('\\'),
            '/' => res.push('/'),
            'b' => res.push('\x08'),
            'f' => res.push('\x0c'),
            'n' => res.push('\n'),
            'r' => res.push('\r'),
            't' => res.push('\t'),
            'u' => match decode_json_hex_code(&mut chars, lex)? {
                code1 @ 0xDC00 ... 0xDFFF => {
                    lex.incr_col(i);
                    let err = format!("invalid string codepoint {}", code1);
                    return Err(Error::Parse(lex.format(&err)))
                },
                // Non-BMP characters are encoded as a sequence of
                // two hex escapes, representing UTF-16 surrogates.
                code1 @ 0xD800 ... 0xDBFF => {
                    let code2 = decode_json_hex_code2(&mut chars, lex)?;
                    if code2 < 0xDC00 || code2 > 0xDFFF {
                        lex.incr_col(i);
                        let err = format!("invalid string codepoint {}", code2);
                        return Err(Error::Parse(lex.format(&err)))
                    }
                    let code = (((code1 - 0xD800) as u32) << 10 |
                                 (code2 - 0xDC00) as u32) + 0x1_0000;
                    res.push(char::from_u32(code).unwrap());
                },

                n => match char::from_u32(n as u32) {
                    Some(ch) => res.push(ch),
                    None => {
                        lex.incr_col(i);
                        let err = format!("invalid string escape code {:?}", n);
                        return Err(Error::Parse(lex.format(&err)))
                    },
                },
            },
            _ => {
                lex.incr_col(i);
                let err = "invalid string string escape type";
                return Err(Error::Parse(lex.format(&err)))
            },
        }
        escape = false;
    }
    lex.incr_col(i);
    return Err(Error::Parse(lex.format("incomplete string")))
}

fn decode_json_hex_code(chars: &mut CharIndices, lex: &mut Lex)
    -> Result<u32>
{
    let mut n = 0;
    let mut code = 0_u32;
    while let Some((_, ch)) = chars.next() {
        if (ch as u8) > 128 || HEXNUM[ch as usize] == 20 {
            let err = format!("invalid string escape code {:?}", ch);
            return Err(Error::Parse(lex.format(&err)))
        }
        code = code * 16 + (HEXNUM[ch as usize] as u32);
        n += 1;
        if n == 4 {
            break
        }
    }
    if n != 4 {
        let err = format!("incomplete string escape code {:x}", code);
        return Err(Error::Parse(lex.format(&err)))
    }
    Ok(code)
}

fn decode_json_hex_code2(chars: &mut CharIndices, lex: &mut Lex)
    -> Result<u32>
{
    if let Some((_, ch1)) = chars.next() {
        if let Some((_, ch2)) = chars.next() {
            if ch1 == '\\' && ch2 == 'u' {
                return decode_json_hex_code(chars, lex)
            }
        }
    }
    let err = "invalid string string escape type";
    return Err(Error::Parse(lex.format(err)))
}


pub fn parse_array(text: &str, lex: &mut Lex) -> Result<Json> {
    lex.incr_col(1); // skip '['

    let mut array = Vec::new();
    parse_whitespace(text, lex);
    if (&text[lex.off..]).as_bytes()[0] == b',' {
        return Err(Error::Parse(lex.format("expected ','")))
    }
    loop {
        if (&text[lex.off..]).as_bytes()[0] == b']' { // end of array.
            lex.incr_col(1);
            break Ok(Json::Array(array))
        }

        array.push(parse_value(text, lex)?);

        parse_whitespace(text, lex);
        if (&text[lex.off..]).as_bytes()[0] == b',' { // skip comma
            lex.incr_col(1);
            parse_whitespace(text, lex);
        }
    }
}

pub fn parse_object(text: &str, lex: &mut Lex) -> Result<Json> {
    lex.incr_col(1); // skip '{'

    let mut m = Vec::new();
    parse_whitespace(text, lex);
    if (&text[lex.off..]).as_bytes()[0] == b'}' {
        lex.incr_col(1);
        return Ok(Json::Object(m))
    }
    loop {
        // key
        parse_whitespace(text, lex);
        let key: String = parse_string(text, lex)?.string().unwrap();
        // colon
        parse_whitespace(text, lex);
        check_next_byte(text, lex, b':')?;

        // value
        parse_whitespace(text, lex);
        let value = parse_value(text, lex)?;

        let i = search_by_key(&m, &key).unwrap_or_else(|e| e.key_missing_at());
        //println!("parse {} {} {:?}", key, i, m);
        m.insert(i, KeyValue(key, value));

        // is exit
        parse_whitespace(text, lex);
        if (&text[lex.off..]).len() == 0 {
            break Err(Error::Parse(lex.format("unexpected eof")))
        } else if (&text[lex.off..]).as_bytes()[0] == b'}' { // exit
            lex.incr_col(1);
            break Ok(Json::Object(m))
        } else if (&text[lex.off..]).as_bytes()[0] == b',' { // skip comma
            lex.incr_col(1);
        }
    }
}

pub fn parse_whitespace(text: &str, lex: &mut Lex) {
    for &ch in (&text[lex.off..]).as_bytes() {
        match WS_LOOKUP[ch as usize] {
            0 => break,
            1 => { lex.col += 1 },              // ' ' | '\t' | '\r'
            2 => { lex.row += 1; lex.col = 0 }, // '\n'
            _ => panic!("unreachable code"),
        };
        lex.off += 1;
    }
}

fn check_next_byte(text: &str, lex: &mut Lex, b: u8) -> Result<()> {
    let progbytes = (&text[lex.off..]).as_bytes();

    if progbytes.len() == 0 {
        return Err(Error::Parse(lex.format(&format!("missing token {}", b))));
    }

    if progbytes[0] != b {
        return Err(Error::Parse(lex.format(&format!("invalid token {}", b))));
    }
    lex.incr_col(1);

    Ok(())
}

fn check_eof(text: &str, lex: &mut Lex) -> Result<()> {
    if (&text[lex.off..]).len() == 0 {
        Err(Error::Parse(lex.format("unexpected eof")))

    } else {
        Ok(())
    }
}


#[derive(Debug,Clone)]
pub struct KeyValue(pub String, pub Json);

impl Eq for KeyValue {}

impl PartialEq for KeyValue {
    fn eq(&self, other: &KeyValue) -> bool {
        self.0.eq(&other.0) // compare only the key.
    }
}

impl PartialOrd for KeyValue {
    fn partial_cmp(&self, other: &KeyValue) -> Option<Ordering> {
        Some(self.0.cmp(&other.0)) // compare only the key.
    }
}

impl Ord for KeyValue {
    fn cmp(&self, other: &KeyValue) -> Ordering {
        self.0.cmp(&other.0) // compare only the key.
    }
}

impl From<String> for KeyValue {
    fn from(key: String) -> KeyValue {
        KeyValue(key, Json::Null)
    }
}

#[derive(Clone,Debug,PartialEq,PartialOrd)]
pub enum Json {
    Null,
    Bool(bool),
    Integer(i128),
    Float(f64),
    String(String),
    Array(Vec<Json>),
    Object(Vec<KeyValue>),
}

impl Json {
    pub fn is_array(&self) -> bool {
        match self { Json::Array(_) => true, _ => false }
    }

    pub fn is_bool(&self) -> bool {
        match self { Json::Object(_) => true, _ => false }
    }

    pub fn string(self) -> Result<String> {
        use self::Json::{String as S};
        match self {
            S(s) => Ok(s),
            _ => Err(Error::NotMyType("json not a string".to_string()))
        }
    }

    pub fn array_ref(&self) -> Result<&Vec<Json>> {
        match self {
            Json::Array(a) => Ok(a),
            _ => Err(Error::NotMyType("json is not array".to_string())),
        }
    }

    pub fn array_mut(&mut self) -> Result<&mut Vec<Json>> {
        match self {
            Json::Array(a) => Ok(a),
            _ => Err(Error::NotMyType("json is not array".to_string()))
        }
    }

    pub fn object_ref(&self) -> Result<&Vec<KeyValue>> {
        match self {
            Json::Object(o) => Ok(o),
            _ => Err(Error::NotMyType("json is not object".to_string()))
        }
    }

    pub fn object_mut(&mut self) -> Result<&mut Vec<KeyValue>> {
        match self {
            Json::Object(o) => Ok(o),
            _ => Err(Error::NotMyType("json is not object".to_string()))
        }
    }

    pub fn to_json(&self, text: &mut String) {
        use json::Json::{Null,Bool,Integer,Float,Array,Object, String as S};

        match self {
            Null => text.push_str("null"),
            Bool(true) => text.push_str("true"),
            Bool(false) => text.push_str("false"),
            Integer(val) => write!(text, "{}", val).unwrap(),
            Float(val) => write!(text, "{:e}", val).unwrap(),
            S(val) => Self::encode_string(&val, text),
            Array(val) => {
                if val.len() == 0 {
                    text.push_str("[]");

                } else {
                    text.push('[');
                    val[..val.len()-1]
                        .iter()
                        .for_each(|item| {item.to_json(text); text.push(',')});
                    val[val.len()-1].to_json(text);
                    text.push(']');
                }
            },
            Object(val) => {
                let val_len = val.len();
                if val_len == 0 {
                    text.push_str("{}");

                } else {
                    text.push('{');
                    for (i, kv) in val.iter().enumerate() {
                        Self::encode_string(&kv.0, text);
                        text.push(':');
                        kv.1.to_json(text);
                        if i < (val_len - 1) { text.push(','); }
                    }
                    text.push('}');
                }
            }
        }
    }

    fn encode_string(val: &str, text: &mut String) {
        text.push('"');

        let mut start = 0;
        for (i, byte) in val.bytes().enumerate() {
            let escstr = ESCAPE[byte as usize];
            if escstr.len() == 0 { continue }

            if start < i {
                text.push_str(&val[start..i]);
            }
            text.push_str(escstr);
            start = i + 1;
        }
        if start != val.len() {
            text.push_str(&val[start..]);
        }

        text.push('"');
    }

    pub fn index(self, off: usize) -> Result<Json> {
        match self {
            Json::Array(mut a) => if off < a.len() {
                    Ok(a.remove(off))
                } else {
                    Err(Error::IndexUnbound(off, off))
                },
            _ => Err(Error::NotMyType("json is not array".to_string())),
        }
    }

    pub fn index_ref(&self, i: usize) -> Result<&Json> {
        let a = self.array_ref()?;
        if i < a.len() { Ok(&a[i]) } else { Err(Error::IndexUnbound(i, i)) }
    }

    pub fn index_mut(&mut self, i: usize) -> Result<&mut Json> {
        use json::Error::IndexUnbound;
        let a = self.array_mut()?;
        if i < a.len() { Ok(&mut a[i]) } else { Err(IndexUnbound(i, i)) }
    }

    pub fn get<'a>(self, key: &'a str) -> Result<Json> {
        match self {
            Json::Object(mut obj) => {
                let off = search_by_key(&obj, key)?;
                Ok(obj.remove(off).1)
            },
            _ => Err(Error::NotMyType("json is not object".to_string()))
        }
    }

    pub fn get_ref<'a>(&self, key: &'a str) -> Result<&Json> {
        let obj = self.object_ref()?;
        let off = search_by_key(obj, key)?;
        Ok(&obj[off].1)
    }

    pub fn get_mut<'a>(&mut self, key: &'a str) -> Result<&mut Json> {
        let obj = self.object_mut()?;
        let off = search_by_key(obj, key)?;
        Ok(&mut obj[off].1)
    }

    pub fn upsert_key(&mut self, kv: KeyValue) {
        let o = self.object_mut().unwrap();
        let i = search_by_key(&o, &kv.0).unwrap_or_else(|e| e.key_missing_at());
        o.insert(i, kv);
    }

    pub fn and(&self, other: &Json) -> Json {
        use json::Json::{Null, Bool};

        let lhs = match self { Null | Bool(false) => false, _ => true };
        let rhs = match other { Null | Bool(false) => false, _ => true };
        Bool(lhs & rhs)
    }

    pub fn or(&self, other: &Json) -> Json {
        use json::Json::{Null, Bool};

        let lhs = match self { Null | Bool(false) => false, _ => true };
        let rhs = match other { Null | Bool(false) => false, _ => true };
        Bool(lhs | rhs)
    }
}

impl Index<usize> for Json {
    type Output=Json;

    fn index(&self, off: usize) -> &Json {
        self.index_ref(off).unwrap()
    }
}

impl IndexMut<usize> for Json {
    fn index_mut(&mut self, off: usize) -> &mut Json {
        self.index_mut(off).unwrap()
    }
}

impl<'a> Index<&'a str> for Json {
    type Output=Json;

    fn index(&self, key: &str) -> &Json {
        self.get_ref(key).unwrap()
    }
}

impl<'a> IndexMut<&'a str> for Json {
    fn index_mut(&mut self, key: &str) -> &mut Json {
        self.get_mut(key).unwrap()
    }
}

impl Index<Range<usize>> for Json {
    type Output=[Json];

    fn index(&self, r: Range<usize>) -> &[Json] {
        self.array_ref().unwrap().index(r)
    }
}

impl Index<RangeFrom<usize>> for Json {
    type Output=[Json];

    fn index(&self, r: RangeFrom<usize>) -> &[Json] {
        self.array_ref().unwrap().index(r)
    }
}

impl Index<RangeTo<usize>> for Json {
    type Output=[Json];

    fn index(&self, r: RangeTo<usize>) -> &[Json] {
        self.array_ref().unwrap().index(r)
    }
}

impl Index<RangeToInclusive<usize>> for Json {
    type Output=[Json];

    fn index(&self, r: RangeToInclusive<usize>) -> &[Json] {
        self.array_ref().unwrap().index(r)
    }
}

impl Index<RangeInclusive<usize>> for Json {
    type Output=[Json];

    fn index(&self, r: RangeInclusive<usize>) -> &[Json] {
        self.array_ref().unwrap().index(r)
    }
}

impl Index<RangeFull> for Json {
    type Output=[Json];

    fn index(&self, r: RangeFull) -> &[Json] {
        self.array_ref().unwrap().index(r)
    }
}

impl<'a> Neg for &'a Json {
    type Output=Json;

    fn neg(self) -> Json {
        match self {
            Json::Integer(n) => Json::Integer(-n),
            Json::Float(n) => Json::Float(-n),
            _ => Json::Null,
        }
    }
}

impl<'a> Not for &'a Json {
    type Output=Json;

    fn not(self) -> Json {
        match self {
            Json::Bool(val) => Json::Bool(!val),
            _ => Json::Null,
        }
    }
}

impl<'a> Mul for &'a Json {
    type Output=Json;

    fn mul(self, rhs: &Json) -> Json {
        use json::Json::{Null,Bool,Integer,Float,Array,Object, String as S};

        match (self, rhs) {
            (Integer(l), Integer(r)) => Integer(l*r),
            (Integer(l), Float(r)) => Float((*l as f64)*r),
            (Integer(_), val) => val.mul(self),
            (Float(l), Float(r)) => Float(l*r),
            (Float(l), Integer(r)) => Float(l*(*r as f64)),
            (Float(_), val) => val.mul(self),
            (Null, _) => Null,
            (Bool(false), _) => Null,
            (Bool(true), val) => val.clone(),
            (S(_), Integer(0)) => Null,
            (S(s), Integer(n)) => S(s.repeat(*n as usize)),
            (S(_), _) => Null,
            (Object(o), Bool(true)) => Object(o.clone()),
            (Object(this), Object(other)) => {
                let mut obj = Vec::new();
                obj = mixin_object(obj, this.to_vec());
                obj = mixin_object(obj, other.to_vec());
                Json::Object(obj)
            },
            (Object(_), _) => Null,
            (Array(a), Integer(n)) => {
                let mut v = vec![];
                (0..*n).for_each(|_| v.extend_from_slice(a));
                Array(v)
            },
            (Array(a), Bool(true)) => Array(a.clone()),
            (Array(_), _) => Null,
        }
    }
}

impl<'a> Div for &'a Json {
    type Output=Json;

    fn div(self, rhs: &Json) -> Json {
        use json::Json::{Null,Integer,Float,String as S};

        match (self, rhs) {
            (Integer(_), Integer(0)) => Null,
            (Integer(l), Integer(r)) => Integer(l/r),
            (Integer(_), Float(f)) if *f == 0.0 => Null,
            (Integer(l), Float(r)) => Float((*l as f64)/r),
            (Integer(_), val) => val.div(self),
            (Float(l), Float(r)) => Float(l/r),
            (Float(l), Integer(r)) => Float(l/(*r as f64)),
            (Float(_), val) => val.div(self),
            (S(s), S(patt)) => {
                Json::Array(s.split(patt).map(|s| S(s.to_string())).collect())
            },
            (_, _) => Null,
        }
    }
}

impl<'a> Rem for &'a Json {
    type Output=Json;

    fn rem(self, rhs: &Json) -> Json {
        use json::Json::{Null,Integer,Float};

        match (self, rhs) {
            (Integer(_), Integer(0)) => Null,
            (Integer(l), Integer(r)) => Integer(l%r),
            (Integer(_), Float(f)) if *f == 0.0 => Null,
            (Integer(l), Float(r)) => Float((*l as f64)%r),
            (Integer(_), val) => val.rem(self),
            (Float(l), Float(r)) => Float(l%r),
            (Float(l), Integer(r)) => Float(l%(*r as f64)),
            (Float(_), val) => val.rem(self),
            (_, _) => Null,
        }
    }
}

impl<'a> Add for &'a Json {
    type Output=Json;

    fn add(self, rhs: &Json) -> Json {
        use json::Json::{Null,Integer,Float,Array,Object, String as S};

        match (self, rhs) {
            (Integer(l), Integer(r)) => Integer(l+r),
            (Integer(l), Float(r)) => Float((*l as f64)+r),
            (Integer(_), val) => val.add(self),
            (Float(l), Float(r)) => Float(l+r),
            (Float(l), Integer(r)) => Float(l+(*r as f64)),
            (Float(_), val) => val.add(self),
            (S(l), S(r)) => {
                let mut s = String::new(); s.push_str(l); s.push_str(r);
                S(s)
            }
            (Array(l), Array(r)) => {
                let mut a = vec![];
                a.extend_from_slice(l);
                a.extend_from_slice(r);
                Array(a)
            }
            (Object(l), Object(r)) => {
                let mut obj = Vec::new();
                obj = merge_object(obj, l.to_vec());
                obj = merge_object(obj, r.to_vec());
                Json::Object(obj)
            }
            (_, _) => Null,
        }
    }
}

impl<'a> Sub for &'a Json {
    type Output=Json;

    fn sub(self, rhs: &Json) -> Json {
        use json::Json::{Null,Integer,Float};

        match (self, rhs) {
            (Integer(l), Integer(r)) => Integer(l-r),
            (Integer(l), Float(r)) => Float((*l as f64)-r),
            (Integer(_), val) => val.sub(self),
            (Float(l), Float(r)) => Float(l-r),
            (Float(l), Integer(r)) => Float(l-(*r as f64)),
            (Float(_), val) => val.sub(self),
            (_, _) => Null,
        }
    }
}

impl<'a> Shr for &'a Json {
    type Output=Json;

    fn shr(self, rhs: &Json) -> Json {
        match (self, rhs) {
            (Json::Integer(l), Json::Integer(r)) => Json::Integer(l>>r),
            (_, _) => Json::Null,
        }
    }
}

impl<'a> Shl for &'a Json {
    type Output=Json;

    fn shl(self, rhs: &Json) -> Json {
        match (self, rhs) {
            (Json::Integer(l), Json::Integer(r)) => Json::Integer(l<<r),
            (_, _) => Json::Null,
        }
    }
}

impl<'a> BitAnd for &'a Json {
    type Output=Json;

    fn bitand(self, rhs: &Json) -> Json {
        match (self, rhs) {
            (Json::Integer(l), Json::Integer(r)) => Json::Integer(l&r),
            (_, _) => Json::Null,
        }
    }
}

impl<'a> BitXor for &'a Json {
    type Output=Json;

    fn bitxor(self, rhs: &Json) -> Json {
        match (self, rhs) {
            (Json::Integer(l), Json::Integer(r)) => Json::Integer(l^r),
            (_, _) => Json::Null,
        }
    }
}

impl<'a> BitOr for &'a Json {
    type Output=Json;

    fn bitor(self, rhs: &Json) -> Json {
        match (self, rhs) {
            (Json::Integer(l), Json::Integer(r)) => Json::Integer(l|r),
            (_, _) => Json::Null,
        }
    }
}


impl FromStr for Json {
    type Err=Error;

    fn from_str(s: &str) -> Result<Json> {
        JsonBuf::parse_str(s)
    }
}

impl fmt::Display for Json {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        self.to_json(&mut s);
        write!(f, "{}", s)
    }
}


pub fn search_by_key(obj: &Vec<KeyValue>, key: &str) -> Result<usize> {
    use std::cmp::Ordering::{Greater, Equal, Less};

    let mut size = obj.len();
    if size == 0 {
        return Err(Error::KeyMissing(0, key.to_string()))
    }

    let mut base = 0usize;
    while size > 1 {
        let half = size / 2;
        let mid = base + half;
        // mid is always in [0, size), that means mid is >= 0 and < size.
        // mid >= 0: by definition
        // mid < size: mid = size / 2 + size / 4 + size / 8 ...
        let item: &str = &obj[mid].0;
        base = if item.cmp(key) == Greater { base } else { mid };
        size -= half;
    }
    // base is always in [0, size) because base <= mid.
    let item: &str = &obj[base].0;
    let cmp = item.cmp(key);
    if cmp == Equal {
        Ok(base)
    } else {
        Err(Error::KeyMissing(base + (cmp == Less) as usize, key.to_string()))
    }
}

fn merge_object(mut this: Vec<KeyValue>, other: Vec<KeyValue>)
    -> Vec<KeyValue>
{
    for o in other.into_iter() {
        let i = search_by_key(&this, &o.0)
                .unwrap_or_else(|e| e.key_missing_at());
        this.insert(i, o)
    }
    this
}

fn mixin_object(mut this: Vec<KeyValue>, other: Vec<KeyValue>)
    -> Vec<KeyValue>
{
    use json::Json::{Object};
    use json::Error::{KeyMissing};

    for o in other.into_iter() {
        match search_by_key(&this, &o.0) {
            Ok(i) => match (this[i].clone().1, o) {
                (Object(val), KeyValue(_, Object(val2))) => {
                    this[i].1 = Object(mixin_object(val, val2))
                },
                (_, o) => this.insert(i, o)
            },
            Err(KeyMissing(i, _)) => this.insert(i, o.clone()),
            _ => unreachable!(),
        }
    }
    this
}

pub trait Value {
    fn value(self) -> Json;
    fn value_as_ref(&self) -> &Json;
    fn value_as_mut(&mut self) -> &mut Json;
}

impl Value for Json {
    fn value(self) -> Json {
        self
    }
    fn value_as_ref(&self) -> &Json {
        self
    }
    fn value_as_mut(&mut self) -> &mut Json {
        self
    }
}

impl Value for KeyValue {
    fn value(self) -> Json {
        self.1
    }
    fn value_as_ref(&self) -> &Json {
        &self.1
    }
    fn value_as_mut(&mut self) -> &mut Json {
        &mut self.1
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn test_simple_jsons() {
        use self::Json::{Null, Bool, String, Integer, Float, Array, Object};
        use std::string;

        let jsons = include!("../testdata/test_simple.jsons");
        let mut refs = include!("../testdata/test_simple.jsons.ref");
        let refs_len = refs.len();
        let mut jsonbuf = JsonBuf::new();

        let mut n = 4;
        let obj = Vec::new();
        refs[refs_len - n] = Object(obj);
        n -= 1;

        let mut obj = Vec::new();
        let kv = KeyValue("key1".to_string(), r#""value1""#.parse().unwrap());
        obj.insert(0, kv);
        refs[refs_len - n] = Object(obj);
        n -= 1;

        let mut obj = Vec::new();
        let kv = KeyValue("key1".to_string(), r#""value1""#.parse().unwrap());
        obj.insert(0, kv);
        let kv = KeyValue("key2".to_string(), r#""value2""#.parse().unwrap());
        obj.insert(1, kv);
        refs[refs_len - n] = Object(obj);
        n -= 1;

        let mut obj = Vec::new();
        let kv = KeyValue("a".to_string(), "1".parse().unwrap());
        obj.insert(0, kv);
        let kv = KeyValue("b".to_string(), "1".parse().unwrap());
        obj.insert(1, kv);
        let kv = KeyValue("c".to_string(), "1".parse().unwrap());
        obj.insert(2, kv);
        let kv = KeyValue("d".to_string(), "1".parse().unwrap());
        obj.insert(3, kv);
        let kv = KeyValue("e".to_string(), "1".parse().unwrap());
        obj.insert(4, kv);
        let kv = KeyValue("f".to_string(), "1".parse().unwrap());
        obj.insert(5, kv);
        let kv = KeyValue("x".to_string(), "1".parse().unwrap());
        obj.insert(6, kv);
        let kv = KeyValue("z".to_string(), "1".parse().unwrap());
        obj.insert(7, kv);
        refs[refs_len - n] = Object(obj);

        jsonbuf.set(jsons[51]);
        let value = jsonbuf.parse().unwrap();
        assert_eq!(value, refs[51]);

        let ref_jsons = include!("../testdata/test_simple.jsons.ref.jsons");
        for (i, r) in refs.iter().enumerate() {
            let s = format!("{}", r);
            //println!("{} {}", i, &s);
            assert_eq!(&s, ref_jsons[i], "testcase: {}", i);
        }
    }

    #[test]
    fn test_json_iter() {
        use self::Json::{Integer, Float, Bool, Array, Object, String as S};

        let docs = r#"null 10 10.2 "hello world" true false [1,2] {"a":10}"#;
        let docs: &[u8] = docs.as_ref();
        let mut iter = JsonBuf::iter(docs);
        assert_eq!(Some(Json::Null), iter.next());
        assert_eq!(Some(Integer(10)), iter.next());
        assert_eq!(Some(Float(10.2)), iter.next());
        assert_eq!(Some(S("hello world".to_string())), iter.next());
        assert_eq!(Some(Bool(true)), iter.next());
        assert_eq!(Some(Bool(false)), iter.next());
        assert_eq!(Some(Array(vec![Integer(1), Integer(2)])), iter.next());
        assert_eq!(
            Some(Object(vec![KeyValue("a".to_string(), Integer(10))])),
            iter.next(),
        );
    }

    #[bench]
    fn bench_null(b: &mut Bencher) {
        b.iter(|| {JsonBuf::parse_str("null").unwrap()});
    }

    #[bench]
    fn bench_bool(b: &mut Bencher) {
        b.iter(|| {JsonBuf::parse_str("false").unwrap()});
    }

    #[bench]
    fn bench_num(b: &mut Bencher) {
        b.iter(|| {JsonBuf::parse_str("10.2").unwrap()});
    }

    #[bench]
    fn bench_string(b: &mut Bencher) {
        let s = r#""汉语 / 漢語; Hàn\b \tyǔ ""#;
        b.iter(|| {JsonBuf::parse_str(s).unwrap()});
    }

    #[bench]
    fn bench_array(b: &mut Bencher) {
	    let s = r#" [null,true,false,10,"tru\"e"]"#;
        b.iter(|| {JsonBuf::parse_str(s).unwrap()});
    }

    #[bench]
    fn bench_map(b: &mut Bencher) {
	    let s = r#"{"a": null,"b":true,"c":false,"d\"":-10E-1,"e":"tru\"e"}"#;
        b.iter(|| {JsonBuf::parse_str(s).unwrap()});
    }

    #[bench]
    fn bench_map_nom(b: &mut Bencher) {
        let s = r#"  { "a": 42, "b": ["x","y",12 ] , "c": {"hello":"world"}} "#;
        b.iter(|| {JsonBuf::parse_str(s).unwrap()});
    }

    #[bench]
    fn bench_null_to_json(b: &mut Bencher) {
        let val = JsonBuf::parse_str("null").unwrap();
        let mut outs = String::with_capacity(64);
        b.iter(|| {outs.clear(); val.to_json(&mut outs)});
    }

    #[bench]
    fn bench_bool_to_json(b: &mut Bencher) {
        let val = JsonBuf::parse_str("false").unwrap();
        let mut outs = String::with_capacity(64);
        b.iter(|| {outs.clear(); val.to_json(&mut outs)});
    }

    #[bench]
    fn bench_num_to_json(b: &mut Bencher) {
        let val = JsonBuf::parse_str("10.2").unwrap();
        let mut outs = String::with_capacity(64);
        b.iter(|| {outs.clear(); val.to_json(&mut outs)});
    }

    #[bench]
    fn bench_string_to_json(b: &mut Bencher) {
        let inp = r#""汉语 / 漢語; Hàn\b \tyǔ ""#;
        let val = JsonBuf::parse_str(inp).unwrap();
        let mut outs = String::with_capacity(64);
        b.iter(|| {outs.clear(); val.to_json(&mut outs)});
    }

    #[bench]
    fn bench_array_to_json(b: &mut Bencher) {
	    let inp = r#" [null,true,false,10,"tru\"e"]"#;
        let val = JsonBuf::parse_str(inp).unwrap();
        let mut outs = String::with_capacity(64);
        b.iter(|| {outs.clear(); val.to_json(&mut outs)});
    }

    #[bench]
    fn bench_map_to_json(b: &mut Bencher) {
	    let inp = r#"{"a": null,"b":true,"c":false,"d\"":-10E-1,"e":"tru\"e"}"#;
        let val = JsonBuf::parse_str(inp).unwrap();
        let mut outs = String::with_capacity(64);
        b.iter(|| {outs.clear(); val.to_json(&mut outs)});
    }
}
