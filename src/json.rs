use std::collections::HashMap;
use std::result;
use std::str::FromStr;

pub type Result<T> = result::Result<T,Error>;

#[derive(Debug)]
pub enum Error {
    InvalidToken(usize, usize, usize),
    InvalidString(usize, usize, usize),
    DuplicateKey(usize, usize, usize),
    KeyIsNotString(usize, usize, usize),
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

//static PARSER = include!("./parser.lookup");

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
    use ::json::Error::InvalidToken;

    let text = &text[lex.off..];

    let mut doparse = |text: &str, i: usize| -> Result<Json> {
        if let Ok(num) = text.parse::<i128>() {
            lex.incr_col(i);
            return Ok(Json::Integer(num))
        } else if let Ok(num) = text.parse::<f64>() {
            lex.incr_col(i);
            return Ok(Json::Float(num))
        }
        return Err(InvalidToken(lex.off, lex.row, lex.col))
    };

    for (i, ch) in text.char_indices() {
        match ch {
            '0'..='9'|'+'|'-'|'.'|'e'|'E' => continue, // valid number
            _ => (),
        }
        return doparse(&text[..i], i)
    }
    doparse(text, text.len())
}

static ESCAPES: [u8; 256] = include!("./esc.lookup");
static HEXNUM: [u8; 256] = include!("./hexnum.lookup");

fn parse_string(text: &str, lex: &mut Lex)
    -> Result<Json>
{
    use ::json::Error::InvalidString;

    let tlen = (&text[lex.off..]).len();

    let mut s = String::new();
    let mut chars = (&text[lex.off..]).char_indices();
    chars.next(); // skip the opening quote

    while let Some((i, ch)) = chars.next() {
        // normal character  0x20 | 0x21 | 0x23..=0x5B | 0x5D..=0x10FFFF
        if (ch as i32) >= 0x5D || ESCAPES[ch as usize] == 1 {
            s.push(ch);
            continue

        } else if ch == '"' { // '"' exit
            lex.incr_col(i+1);
            return Ok(Json::String(s))

        } else if ch == '\\' { // escape '\\'
            if (tlen-i) < 3 {
                return Err(InvalidString(lex.off+i, lex.row, lex.col))
            }

            let (i, escval) = chars.next().unwrap();

            if escval == 'u' { // unicode escape
                s.push_str("\\u{");
                let mut n = 0;
                while let Some((i, ch)) = chars.next() {
                    if (ch as u8) > 128 || HEXNUM[ch as usize] == 0 {
                        return Err(InvalidString(lex.off+i, lex.row, lex.col))
                    }
                    s.push(ch);
                    n += 1;
                    if n == 4 { break }
                }
                if n != 4 {
                    return Err(InvalidString(lex.off+i, lex.row, lex.col))
                }
                s.push('}');
                continue

            } else if (escval as i32) > 126 { // character as it is
                s.push(escval);
                continue;
            }

            let b = ESCAPES[escval as usize] as u8;
            if b == 0 || b == 1 { // character as it is
                s.push(escval);
                continue

            } else {
                // 'b' (98 as 8)   't' (116 as 9)  'n' (110 as 10)
                // 'f' (102 as 12) 'r' (114 as 13) '"' (34 as 34)
                // '/' (47 as 47)  '\\' (92 as 92)
                s.push(b as char);
                continue
            }
        }
    }
    Err(InvalidString(text.len(), lex.row, lex.col))
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

static WS_LOOKUP: [u8; 256] = include!("./ws.lookup");

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
        let s = r#""汉语 / 漢語; Hàn\b \tyǔ ""#;
        let refstr = "汉语 / 漢語; Hàn\u{8} \tyǔ ";
        jsonbuf.set(s);
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
}
