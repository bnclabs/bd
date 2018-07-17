use std::collections::HashMap;
use std::result;
use std::str::{FromStr,CharIndices};
use std::fmt::Write;
use std::char;

pub type Result<T> = result::Result<T,Error>;

#[derive(Debug)]
pub enum Error {
    InvalidToken(usize, usize, usize),
    InvalidString(usize, usize, usize),
    InvalidEscape(usize, usize, usize),
    InvalidCodepoint(usize, usize, usize),
    DuplicateKey(usize, usize, usize),
    KeyIsNotString(usize, usize, usize),
    InvalidNumber(usize, usize, usize),
    MissingToken,
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
    use ::json::Error::{InvalidToken, MissingToken};

    parse_whitespace(text, lex);

    let valtext = &text[lex.off..];
    if valtext.len() == 0 {
        return Err(MissingToken)
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
        _ => return Err(InvalidToken(lex.off, lex.row, lex.col)),
    };
    //println!("valu -- {:?}", val);

    Ok(val)
}

fn parse_null(text: &str, lex: &mut Lex) -> Result<Json> {
    use ::json::Error::InvalidToken;

    let text = &text[lex.off..];
    if text.len() >= 4 && &text[..4] == "null" {
        lex.incr_col(4);
        Ok(Json::Null)
    } else {
        Err(InvalidToken(lex.off, lex.row, lex.col))
    }
}

fn parse_true(text: &str, lex: &mut Lex) -> Result<Json> {
    use ::json::Error::InvalidToken;

    let text = &text[lex.off..];
    if text.len() >= 4 && &text[..4] == "true" {
        lex.incr_col(4);
        Ok(Json::Bool(true))
    } else {
        Err(InvalidToken(lex.off, lex.row, lex.col))
    }
}

fn parse_false(text: &str, lex: &mut Lex) -> Result<Json> {
    use ::json::Error::InvalidToken;

    let text = &text[lex.off..];
    if text.len() >= 5 && &text[..5] == "false" {
        lex.incr_col(5);
        Ok(Json::Bool(false))
    } else {
        Err(InvalidToken(lex.off, lex.row, lex.col))
    }
}

fn parse_num(text: &str, lex: &mut Lex) -> Result<Json> {
    use ::json::Error::InvalidNumber;

    let text = &text[lex.off..];
    let mut doparse = |text: &str, i: usize, is_float: bool| -> Result<Json> {
        if is_float {
            if let Ok(num) = text.parse::<f64>() {
                lex.incr_col(i);
                return Ok(Json::Float(num))
            }
            Err(InvalidNumber(lex.off+i, lex.row, lex.col+i))
        } else {
            if let Ok(num) = text.parse::<i128>() {
                lex.incr_col(i);
                return Ok(Json::Integer(num))
            }
            Err(InvalidNumber(lex.off+i, lex.row, lex.col+i))
        }
    };

    let mut is_float = false;
    for (i, ch) in text.char_indices() {
        match ch {
            '0'..='9'|'+'|'-'|'e'|'E' => continue, // valid number
            '.' => { is_float = true; continue}, // float number
            _ => (),
        }
        return doparse(&text[..i], i, is_float)
    }
    doparse(text, text.len(), is_float)
}

static HEXNUM: [u8; 256] = include!("./lookup.hexnum");

fn parse_string(text: &str, lex: &mut Lex) -> Result<Json> {
    use ::json::Error::{InvalidString, InvalidEscape, InvalidCodepoint};

    let mut escape = false;
    let mut res = String::new();
    let mut chars = (&text[lex.off..]).char_indices();

    let (i, _) = chars.next().unwrap(); // skip the opening quote

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
                    return Err(InvalidString(lex.off+i, lex.row, lex.col+i))
                }
                // Non-BMP characters are encoded as a sequence of
                // two hex escapes, representing UTF-16 surrogates.
                code1 @ 0xD800 ... 0xDBFF => {
                    let code2 = decode_json_hex_code2(&mut chars, lex)?;
                    if code2 < 0xDC00 || code2 > 0xDFFF {
                        return Err(InvalidString(lex.off+i, lex.row, lex.col+i))
                    }
                    let code = (((code1 - 0xD800) as u32) << 10 |
                                 (code2 - 0xDC00) as u32) + 0x1_0000;
                    res.push(char::from_u32(code).unwrap());
                }

                n => match char::from_u32(n as u32) {
                    Some(ch) => res.push(ch),
                    None => return Err(
                        InvalidCodepoint(lex.off+i, lex.row, lex.col+i)
                    ),
                },
            },
            _ => return Err(InvalidEscape(lex.off+1, lex.row, lex.col+i)),
        }
        escape = false;
    }
    return Err(InvalidString(lex.off+i, lex.row, lex.col+i))
}

fn decode_json_hex_code(chars: &mut CharIndices, lex: &mut Lex)
    -> Result<u32>
{
    use ::json::Error::InvalidString;

    let mut n = 0;
    let mut code = 0_u32;
    while let Some((_, ch)) = chars.next() {
        if (ch as u8) > 128 || HEXNUM[ch as usize] == 20 {
            return Err(InvalidString(lex.off, lex.row, lex.col))
        }
        code = code * 16 + (HEXNUM[ch as usize] as u32);
        n += 1;
        if n == 4 { break }
    }
    if n != 4 {
        return Err(InvalidString(lex.off, lex.row, lex.col))
    }
    Ok(code)
}

fn decode_json_hex_code2(chars: &mut CharIndices, lex: &mut Lex)
    -> Result<u32>
{
    use ::json::Error::InvalidString;

    if let Some((_, ch1)) = chars.next() {
        if let Some((_, ch2)) = chars.next() {
            if ch1 == '\\' && ch2 == 'u' {
                return decode_json_hex_code(chars, lex)
            }
        }
    }
    return Err(InvalidString(lex.off, lex.row, lex.col))
}


fn parse_array(text: &str, lex: &mut Lex)
    -> Result<Json>
{
    use ::json::Error::InvalidToken;

    lex.incr_col(1); // skip '['

    let mut array = Vec::new();
    parse_whitespace(text, lex);
    if (&text[lex.off..]).as_bytes()[0] == b',' {
        return Err(InvalidToken(lex.off, lex.row, lex.col))
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
    use ::json::Error::{InvalidToken, DuplicateKey, KeyIsNotString};

    lex.incr_col(1); // skip '{'

    let mut map = HashMap::new();
    loop {
        // key
        parse_whitespace(text, lex);
        let key = parse_string(text, lex)?;
        if let Json::String(_) = key {
            break Err(KeyIsNotString(lex.off, lex.row, lex.col))
        }
        let key = key.string();
        // colon
        parse_whitespace(text, lex);
        if (&text[lex.off..]).len() == 0 {
            break Err(InvalidToken(lex.off, lex.row, lex.col))
        } else if (&text[lex.off..]).as_bytes()[0] != b':' {
            break Err(InvalidToken(lex.off, lex.row, lex.col))
        }
        lex.incr_col(1); // skip ':'
        // value
        parse_whitespace(text, lex);
        let value = parse_value(text, lex)?;
        // is exit
        parse_whitespace(text, lex);
        if (&text[lex.off..]).len() == 0 {
            break Err(InvalidToken(lex.off, lex.row, lex.col))
        } else if (&text[lex.off..]).as_bytes()[0] == b'}' { // exit
            lex.incr_col(1);
            break Ok(Json::Object(map))
        }
        if let Some(_) = map.insert(key, value) {
            break Err(DuplicateKey(lex.off, lex.row, lex.col))
        }
    }
}

static WS_LOOKUP: [u8; 256] = include!("./lookup.ws");

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
    pub fn bool(self) -> bool {
        use self::Json::{Bool};
        match self { Bool(val) => val, _ => panic!("Json is not boolean") }
    }

    pub fn integer(self) -> i128 {
        use self::Json::{Integer};
        match self { Integer(val) => val, _ => panic!("Json is not integer") }
    }

    pub fn float(self) -> f64 {
        use self::Json::{Float};
        match self { Float(val) => val, _ => panic!("Json is not float") }
    }

    pub fn string(self) -> String {
        use self::Json::{String};
        match self { String(val) => val, _ => panic!("Json is not string") }
    }
}

fn encode_null(text: &mut String) {
    text.push_str("null");
}

fn encode_bool(val: bool, text: &mut String) {
    if val { text.push_str("true") } else { text.push_str("false") };
}

fn encode_integer(val: i128, text: &mut String) {
    write!(text, "{}", val).unwrap();
}

fn encode_float(val: f64, text: &mut String) {
    write!(text, "{:e}", val).unwrap();
}

static ESCAPE: [&'static str; 256] = include!("./lookup.escape");
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
    type Err=Error;

    fn from_str(s: &str) -> Result<Json> {
        JsonBuf::parse_str(s)
    }
}

#[cfg(test)]
mod tests {
    use super::{JsonBuf, Json, Lex};
    use test::Bencher;

    #[test]
    fn test_null() {
        let mut jsonbuf = JsonBuf::new();
        jsonbuf.set("null");
        assert_eq!(Json::Null, jsonbuf.parse().unwrap());
    }

    #[test]
    fn test_bool() {
        let mut jsonbuf = JsonBuf::new();
        jsonbuf.set("true");
        assert_eq!(Json::Bool(true), jsonbuf.parse().unwrap());
        jsonbuf.set("false");
        assert_eq!(Json::Bool(false), jsonbuf.parse().unwrap());
    }

    #[test]
    fn test_num() {
        let mut jsonbuf = JsonBuf::new();
        jsonbuf.set("102");
        assert_eq!(Json::Integer(102), jsonbuf.parse().unwrap());
        jsonbuf.set("10.2");
        assert_eq!(Json::Float(10.2), jsonbuf.parse().unwrap());
    }

    #[test]
    fn test_string() {
        let mut jsonbuf = JsonBuf::new();
        let inpstr = r#""汉语 / 漢語; Hàn\b \tyǔ ""#;
        let refstr = "汉语 / 漢語; Hàn\u{8} \tyǔ ";
        jsonbuf.set(inpstr);
        assert_eq!(Json::String(refstr.to_string()), jsonbuf.parse().unwrap());
    }

    #[test]
    fn test_array() {
        let mut jsonbuf = JsonBuf::new();
        let mut s = r#"[10.2, null,false,"", true,"#.to_string();
        s += r#"" ", 18014398509481984"#;
        s += r#""hello world", "汉语 / 漢語; Hàn\b \tyǔ "]"#;
        jsonbuf.set(&s);
        let out = jsonbuf.parse().unwrap();
        let refval = Json::Array(vec![
            Json::Float(10.2), Json::Null, Json::Bool(false),
            Json::String("".to_string()), Json::Bool(true),
            Json::String(" ".to_string()), Json::Integer(18014398509481984),
            Json::String("hello world".to_string()),
            Json::String("汉语 / 漢語; Hàn\u{8} \tyǔ ".to_string()),
        ]);
        assert_eq!(refval, out);
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
}
