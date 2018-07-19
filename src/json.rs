use std::str::{FromStr,CharIndices};
use std::collections::HashMap;
use std::fmt::{self, Write};
use std::convert::{TryFrom,TryInto};
use std::{result, char, error};

include!("./json.rs.lookup");

pub type Result<T> = result::Result<T,JsonError>;

#[derive(Debug)]
pub enum JsonError {
    MissingToken(String),
    InvalidToken(String),
    InvalidNull(String),
    InvalidBoolean(String),
    InvalidNumber(String),
    InvalidString(String),
    InvalidEscape(String),
    InvalidCodepoint(String),
    InvalidArray(String),
    InvalidMap(String),
    DuplicateKey(String),
    InvalidJsonType(String),
    NotBool,
    NotInteger,
    NotFloat,
    NotString,
    NotArray,
    NotObject,
}

impl fmt::Display for JsonError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ::json::JsonError::{MissingToken, InvalidToken, InvalidNull};
        use ::json::JsonError::{InvalidBoolean, InvalidNumber, InvalidString};
        use ::json::JsonError::{InvalidEscape, InvalidCodepoint, InvalidArray};
        use ::json::JsonError::{InvalidMap, DuplicateKey, InvalidJsonType};
        use ::json::JsonError::{NotBool, NotInteger, NotFloat, NotString};
        use ::json::JsonError::{NotArray, NotObject};

        match self {
            MissingToken(s) => write!(f, "missing token at {}", s),
            InvalidToken(s) => write!(f, "invalid token at {}", s),
            InvalidNull(s) => write!(f, "null expected at {}", s),
            InvalidBoolean(s) => write!(f, "boolean expected at {}", s),
            InvalidNumber(s) => write!(f, "invalid number {}", s),
            InvalidString(s) => write!(f, "invalid string at {}", s),
            InvalidEscape(s) => write!(f, "invalid string escape at {}", s),
            InvalidCodepoint(s) => write!(f, "invalid codepoint in string {}", s),
            InvalidArray(s) => write!(f, "invalid array at {}", s),
            InvalidMap(s) => write!(f, "invalid map at {}", s),
            DuplicateKey(s) => write!(f, "duplicate key in map at {}", s),
            InvalidJsonType(s) => write!(f, "invalid json type at {}", s),
            NotBool => write!(f, "Json is not boolean"),
            NotInteger => write!(f, "Json is not integer"),
            NotFloat => write!(f, "Json is not float"),
            NotString => write!(f, "Json is not string"),
            NotArray => write!(f, "Json is not array"),
            NotObject => write!(f, "Json is not map"),
        }
    }
}

impl error::Error for JsonError {
    fn cause(&self) -> Option<&error::Error> { None }
}

struct Lex{ off: usize, row: usize, col: usize }

impl Lex {
    fn new(off: usize, row: usize, col: usize) -> Lex {
        Lex{off, row, col}
    }

    fn incr_col(&mut self, i: usize) {
        self.off += i;
        self.col += i;
    }

    fn format(&self) -> String {
        format!("offset:{} line:{} col:{}", self.off, self.row, self.col)
    }
}

pub struct JsonBuf {
    inner: String,
}

impl JsonBuf {
    pub fn new() -> JsonBuf {
        JsonBuf{ inner: String::new() }
    }

    pub fn with_capacity(cap: usize) -> JsonBuf {
        JsonBuf{ inner: String::with_capacity(cap) }
    }

    pub fn parse_str(text: &str) -> Result<Json> {
        let mut lex = Lex::new(0_usize, 1_usize, 1_usize);
        parse_value(&text[lex.off..], &mut lex)
    }

    pub fn set<T>(&mut self, text: &T) where T: AsRef<str> + ?Sized {
        self.inner.clear();
        self.inner.push_str(text.as_ref());
    }

    pub fn append<T>(&mut self, text: &T) where T: AsRef<str> + ?Sized {
        self.inner.push_str(text.as_ref());
    }

    pub fn parse(&mut self) -> Result<Json> {
        let mut lex = Lex::new(0_usize, 1_usize, 1_usize);
        let val = parse_value(&self.inner[lex.off..], &mut lex)?;
        self.inner = self.inner[lex.off..].to_string(); // remaining text.
        Ok(val)
    }

}

impl From<String> for JsonBuf {
    fn from(s: String) -> JsonBuf {
        JsonBuf { inner: s }
    }
}

impl<'a, T> From<&'a T> for JsonBuf where T: AsRef<str> + ?Sized {
    fn from(s: &'a T) -> JsonBuf {
        JsonBuf::from(s.as_ref().to_string())
    }
}

fn parse_value(text: &str, lex: &mut Lex)
    -> Result<Json>
{
    use ::json::JsonError::{InvalidToken, MissingToken};

    parse_whitespace(text, lex);

    let valtext = &text[lex.off..];
    if valtext.len() == 0 {
        return Err(MissingToken(lex.format()))
    }

    //println!("text -- {:?}", valtext);
    let val = match valtext.as_bytes()[0] {
        b'n' => parse_null(text, lex)?,
        b't' => parse_true(text, lex)?,
        b'f' => parse_false(text, lex)?,
        b'0'..=b'9'|b'+'|b'-'|b'.'|b'e'|b'E' => parse_num(text, lex)?,
        b'"' => parse_string(text, lex)?,
        b'[' => parse_array(text, lex)?,
        b'{' => parse_object(text, lex)?,
        _ => return Err(InvalidToken(lex.format()))
    };
    //println!("valu -- {:?}", val);

    Ok(val)
}

fn parse_null(text: &str, lex: &mut Lex) -> Result<Json> {
    let text = &text[lex.off..];
    if text.len() >= 4 && &text[..4] == "null" {
        lex.incr_col(4);
        Ok(Json::Null)
    } else {
        Err(JsonError::InvalidNull(lex.format()))
    }
}

fn parse_true(text: &str, lex: &mut Lex) -> Result<Json> {
    let text = &text[lex.off..];
    if text.len() >= 4 && &text[..4] == "true" {
        lex.incr_col(4);
        Ok(Json::Bool(true))
    } else {
        Err(JsonError::InvalidBoolean(lex.format()))
    }
}

fn parse_false(text: &str, lex: &mut Lex) -> Result<Json> {
    let text = &text[lex.off..];
    if text.len() >= 5 && &text[..5] == "false" {
        lex.incr_col(5);
        Ok(Json::Bool(false))
    } else {
        Err(JsonError::InvalidBoolean(lex.format()))
    }
}

fn parse_num(text: &str, lex: &mut Lex) -> Result<Json> {
    let text = &text[lex.off..];
    let mut doparse = |text: &str, i: usize, is_float: bool| -> Result<Json> {
        lex.incr_col(i);
        if is_float {
            if let Ok(val) = text.parse::<f64>() {
                return Ok(Json::Float(val))
            }
            Err(JsonError::InvalidNumber(lex.format()))
        } else {
            if let Ok(val) = text.parse::<i128>() {
                return Ok(Json::Integer(val))
            }
            Err(JsonError::InvalidNumber(lex.format()))
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

fn parse_string(text: &str, lex: &mut Lex) -> Result<Json> {
    use ::json::JsonError::{InvalidString, InvalidEscape, InvalidCodepoint};

    let mut escape = false;
    let mut res = String::new();
    let mut chars = (&text[lex.off..]).char_indices();

    let (i, ch) = chars.next().unwrap(); // skip the opening quote
    if ch != '"' {
        return Err(InvalidString(lex.format()))
    }

    while let Some((i, ch)) = chars.next() {
        if escape == false {
            if ch == '\\' {
                escape = true;
                continue
            }
            match ch {
                '"' => { lex.incr_col(i+1); return Ok(Json::String(res)); },
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
                0xDC00 ... 0xDFFF => {
                    lex.incr_col(i);
                    return Err(InvalidString(lex.format()))
                },
                // Non-BMP characters are encoded as a sequence of
                // two hex escapes, representing UTF-16 surrogates.
                code1 @ 0xD800 ... 0xDBFF => {
                    let code2 = decode_json_hex_code2(&mut chars, lex)?;
                    if code2 < 0xDC00 || code2 > 0xDFFF {
                        lex.incr_col(i);
                        return Err(InvalidString(lex.format()))
                    }
                    let code = (((code1 - 0xD800) as u32) << 10 |
                                 (code2 - 0xDC00) as u32) + 0x1_0000;
                    res.push(char::from_u32(code).unwrap());
                },

                n => match char::from_u32(n as u32) {
                    Some(ch) => res.push(ch),
                    None => {
                        lex.incr_col(i);
                        return Err(InvalidCodepoint(lex.format()))
                    },
                },
            },
            _ => {
                lex.incr_col(i);
                return Err(InvalidEscape(lex.format()))
            },
        }
        escape = false;
    }
    lex.incr_col(i);
    return Err(InvalidString(lex.format()))
}

fn decode_json_hex_code(chars: &mut CharIndices, lex: &mut Lex)
    -> Result<u32>
{
    let mut n = 0;
    let mut code = 0_u32;
    while let Some((_, ch)) = chars.next() {
        if (ch as u8) > 128 || HEXNUM[ch as usize] == 20 {
            return Err(JsonError::InvalidString(lex.format()))
        }
        code = code * 16 + (HEXNUM[ch as usize] as u32);
        n += 1;
        if n == 4 { break }
    }
    if n != 4 {
        return Err(JsonError::InvalidString(lex.format()))
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
    return Err(JsonError::InvalidString(lex.format()))
}


fn parse_array(text: &str, lex: &mut Lex)
    -> Result<Json>
{
    use ::json::JsonError::InvalidArray;

    lex.incr_col(1); // skip '['

    let mut array = Vec::new();
    parse_whitespace(text, lex);
    if (&text[lex.off..]).as_bytes()[0] == b',' {
        return Err(InvalidArray(lex.format()))
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

fn parse_object(text: &str, lex: &mut Lex)
    -> Result<Json>
{
    use ::json::JsonError::{InvalidMap, DuplicateKey};

    lex.incr_col(1); // skip '{'

    let mut map = HashMap::new();
    parse_whitespace(text, lex);
    if (&text[lex.off..]).as_bytes()[0] == b'}' {
        lex.incr_col(1);
        return Ok(Json::Object(map))
    }
    loop {
        // key
        parse_whitespace(text, lex);
        let key: String = parse_string(text, lex)?.try_into().unwrap();
        // colon
        parse_whitespace(text, lex);
        if (&text[lex.off..]).len() == 0 {
            break Err(InvalidMap(lex.format()))
        } else if (&text[lex.off..]).as_bytes()[0] != b':' {
            break Err(InvalidMap(lex.format()))
        }
        lex.incr_col(1); // skip ':'
        // value
        parse_whitespace(text, lex);
        let value = parse_value(text, lex)?;

        if let Some(_) = map.insert(key, value) {
            break Err(DuplicateKey(lex.format()))
        }

        // is exit
        parse_whitespace(text, lex);
        if (&text[lex.off..]).len() == 0 {
            break Err(InvalidMap(lex.format()))
        } else if (&text[lex.off..]).as_bytes()[0] == b'}' { // exit
            lex.incr_col(1);
            break Ok(Json::Object(map))
        } else if (&text[lex.off..]).as_bytes()[0] == b',' { // skip comma
            lex.incr_col(1);
        }
    }
}

fn parse_whitespace(text: &str, lex: &mut Lex) {
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

#[derive(Debug,PartialEq)]
pub enum Json {
    Null,
    Bool(bool),
    Integer(i128),
    Float(f64),
    String(String),
    Array(Vec<Json>),
    Object(HashMap<String, Json>),
}

impl Json {
    pub fn to_json(&self, text: &mut String) {
        use self::Json::{Null, Bool, Integer, Float, Array, Object};
        match self {
            Null => text.push_str("null"),
            Bool(true) => text.push_str("true"),
            Bool(false) => text.push_str("false"),
            Integer(val) => write!(text, "{}", val).unwrap(),
            Float(val) => write!(text, "{:e}", val).unwrap(),
            Json::String(val) => encode_string(&val, text),
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
                    for (i, (k, v)) in val.iter().enumerate() {
                        encode_string(&k, text);
                        text.push(':');
                        v.to_json(text);
                        if i < (val_len - 1) { text.push(','); }
                    }
                    text.push('}');
                }
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

impl FromStr for Json {
    type Err=JsonError;

    fn from_str(s: &str) -> Result<Json> {
        JsonBuf::parse_str(s)
    }
}

impl TryFrom<Json> for bool {
    type Error=JsonError;
    fn try_from(val: Json) -> result::Result<bool, JsonError> {
        match val { Json::Bool(s) => Ok(s), _ => Err(JsonError::NotBool) }
    }
}

impl TryFrom<Json> for i128 {
    type Error=JsonError;
    fn try_from(val: Json) -> result::Result<i128, JsonError> {
        match val { Json::Integer(s) => Ok(s), _ => Err(JsonError::NotInteger) }
    }
}

impl TryFrom<Json> for f64 {
    type Error=JsonError;
    fn try_from(val: Json) -> result::Result<f64, JsonError> {
        match val { Json::Float(s) => Ok(s), _ => Err(JsonError::NotFloat) }
    }
}

impl TryFrom<Json> for String {
    type Error=JsonError;
    fn try_from(val: Json) -> result::Result<String, JsonError> {
        match val { Json::String(s) => Ok(s), _ => Err(JsonError::NotString) }
    }
}

impl TryFrom<Json> for Vec<Json> {
    type Error=JsonError;
    fn try_from(val: Json) -> result::Result<Vec<Json>, JsonError> {
        match val { Json::Array(s) => Ok(s), _ => Err(JsonError::NotArray) }
    }
}

impl TryFrom<Json> for HashMap<String,Json> {
    type Error=JsonError;
    fn try_from(val: Json) -> result::Result<HashMap<String,Json>, JsonError> {
        match val { Json::Object(s) => Ok(s), _ => Err(JsonError::NotObject) }
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

        let mut n = 3;
        let obj = HashMap::new();
        refs[refs_len - n] = Object(obj);
        n -= 1;

        let mut obj = HashMap::new();
        obj.insert("key1".to_string(), String("value1".to_string()));
        refs[refs_len - n] = Object(obj);
        n -= 1;

        let mut obj = HashMap::new();
        obj.insert("key1".to_string(), String("value1".to_string()));
        obj.insert("key2".to_string(), String("value2".to_string()));
        refs[refs_len - n] = Object(obj);
        n -= 1;

        for (i, json) in jsons.iter().enumerate() {
            jsonbuf.set(json);
            let value = jsonbuf.parse().unwrap();
            //println!("{} {:?}", i, value);
            assert_eq!(value, refs[i], "testcase: {}", i);
        }

        let ref_jsons = include!("../testdata/test_simple.jsons.ref.jsons");
        let mut s = string::String::new();
        for (i, r) in refs.iter().enumerate() {
            s.clear();
            r.to_json(&mut s);
            //println!("{} {}", i, &s);
            assert_eq!(&s, ref_jsons[i], "testcase: {}", i);
        }
    }

    #[bench]
    fn bench_ws(b: &mut Bencher) {
        b.iter( || {
            let mut lex = Lex::new(0_usize, 1_usize, 1_usize);
            super::parse_null("null", &mut lex).unwrap()
        });
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
	    let s = r#"{"a": null, "b" : true,"c":false, "d\"":-10E-1, "e":"tru\"e" }"#;
        b.iter(|| {JsonBuf::parse_str(s).unwrap()});
    }
}
