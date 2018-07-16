use std::collections::HashMap;
use std::result;
use std::str::FromStr;

pub type Result<T> = result::Result<T,Error>;

#[derive(Debug)]
pub enum Error {
    InvalidToken(usize, usize, usize),
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
    codepoint: String,
}

impl JsonBuf {
    pub fn new() -> JsonBuf {
        JsonBuf{ inner: String::new(), codepoint: String::with_capacity(16) }
    }

    pub fn with_capacity(cap: usize) -> JsonBuf {
        JsonBuf{
            inner: String::with_capacity(cap),
            codepoint: String::with_capacity(16)
        }
    }

    pub fn parse_str(text: &str) -> Result<Json> {
        use ::json::Error::{MissingToken};

        let mut lex = Lex::new(0_usize, 1_usize, 1_usize);
        if text.len() == 0 {
            return Err(MissingToken)
        }
        let mut codepoint = String::with_capacity(16);
        parse_value(&text[lex.off..], &mut codepoint, &mut lex)
    }

    pub fn set<T>(&mut self, text: &T) where T: AsRef<str> + ?Sized {
        self.inner.clear();
        self.inner.push_str(text.as_ref());
    }

    pub fn append<T>(&mut self, text: &T) where T: AsRef<str> + ?Sized {
        self.inner.push_str(text.as_ref());
    }

    pub fn parse(&mut self) -> Result<Json> {
        use ::json::Error::{MissingToken};

        let mut lex = Lex::new(0_usize, 1_usize, 1_usize);
        if self.inner.len() == 0 {
            return Err(MissingToken)
        }
        let val = parse_value(
            &self.inner[lex.off..], &mut self.codepoint, &mut lex,
        )?;
        self.inner = self.inner[lex.off..].to_string(); // remaining text.
        Ok(val)
    }

}

impl From<String> for JsonBuf {
    fn from(s: String) -> JsonBuf {
        JsonBuf { inner: s, codepoint: String::with_capacity(16) }
    }
}

impl<'a, T> From<&'a T> for JsonBuf where T: AsRef<str> + ?Sized {
    fn from(s: &'a T) -> JsonBuf {
        JsonBuf::from(s.as_ref().to_string())
    }
}

fn parse_value(text: &str, codepoint: &mut String, lex: &mut Lex)
    -> Result<Json>
{
    use ::json::Error::{InvalidToken};

    parse_whitespace(text, lex);
    //println!("text -- {:?}", &text[lex.off..]);
    let val = match &text[lex.off..].chars().nth(0).unwrap() {
        'n' => parse_null(text, lex)?,
        't' => parse_true(text, lex)?,
        'f' => parse_false(text, lex)?,
        '0'..='9'|'+'|'-'|'.'|'e'|'E' => parse_num(text, lex)?,
        '"' => parse_string(text, codepoint, lex)?,
        '[' => parse_array(text, codepoint, lex)?,
        '{' => parse_object(text, codepoint, lex)?,
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

fn parse_string(text: &str, codepoint: &mut String, lex: &mut Lex)
    -> Result<Json>
{
    use ::json::Error::InvalidToken;

    let text = &text[lex.off..];

    let mut s = String::new();
    let mut esc = false;
    let mut chars = text.char_indices();
    codepoint.clear();

    chars.next(); // skip the opening quote

    for (i, ch) in chars {
        if esc { // escaped
            match ch {
                'b'  => {esc = false; s.push(8 as char)},
                't'  => {esc = false; s.push(9 as char)},
                'n'  => {esc = false; s.push(10 as char)},
                'f'  => {esc = false; s.push(12 as char)},
                'r'  => {esc = false; s.push(13 as char)},
                '"'  => {esc = false; s.push(34 as char)},
                '/'  => {esc = false; s.push(47 as char)},
                '\\' => {esc = false; s.push(92 as char)},
                'u'  => {codepoint.push_str("\\u{")},
                _ => return Err(InvalidToken(lex.off+i, lex.row, lex.col)),
            }
            continue

        } else if codepoint.len() == 7 { // unicode gathered
            codepoint.push('}');
            s.push_str(&codepoint);
            codepoint.clear();
            // fall through

        } else if codepoint.len() > 0 { // unicode escaped
            match ch {
                '0'..='9'|'a'..='f'|'A'..='F' => codepoint.push(ch),
                _ => return Err(InvalidToken(lex.off+i, lex.row, lex.col))
            };
            continue
        }

        match ch as i32 {
            92 => { esc = true; continue; }
            34 => { lex.incr_col(i+1); return Ok(Json::String(s)) } // exit
            0x20 | 0x21 | 0x23..=0x5B | 0x5D..=0x10FFFF => s.push(ch),
            _ => return Err(InvalidToken(lex.off+i, lex.row, lex.col)),
        }
    }
    Err(InvalidToken(text.len(), lex.row, lex.col))
}

fn parse_array(text: &str, codepoint: &mut String, lex: &mut Lex)
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

        array.push(parse_value(text, codepoint, lex)?);

        parse_whitespace(text, lex);
        if (&text[lex.off..]).as_bytes()[0] == b',' { // skip comma
            lex.incr_col(1);
            parse_whitespace(text, lex);
        }
    }
}

fn parse_object(text: &str, codepoint: &mut String, lex: &mut Lex)
    -> Result<Json>
{
    use ::json::Error::{InvalidToken, DuplicateKey, KeyIsNotString};

    lex.incr_col(1); // skip '{'

    let mut map = HashMap::new();
    loop {
        // key
        parse_whitespace(text, lex);
        let key = parse_string(text, codepoint, lex)?;
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
        let value = parse_value(text, codepoint, lex)?;
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

fn parse_whitespace(text: &str, lex: &mut Lex) {
    let text = &text[lex.off..];
    for (_, ch) in text.char_indices() {
        match ch {
            ' ' | '\t' | '\r' => { lex.col += 1 },
            '\n' => { lex.row += 1; lex.col = 0 },
            _ => break,
        }
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
    use ::json::{JsonBuf, Json};
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
    fn bench_null(b: &mut Bencher) {
        let mut jsonbuf = JsonBuf::new();
        b.iter(|| {jsonbuf.set("null"); jsonbuf.parse().unwrap()});
    }

    #[bench]
    fn bench_bool(b: &mut Bencher) {
        let mut jsonbuf = JsonBuf::new();
        b.iter(|| {jsonbuf.set("false"); jsonbuf.parse().unwrap()});
    }

    #[bench]
    fn bench_num(b: &mut Bencher) {
        let mut jsonbuf = JsonBuf::new();
        b.iter(|| {jsonbuf.set("10.2"); jsonbuf.parse().unwrap()});
    }

    #[bench]
    fn bench_string(b: &mut Bencher) {
        let mut jsonbuf = JsonBuf::new();
        let s = r#""汉语 / 漢語; Hàn\b \tyǔ ""#;
        b.iter(|| {jsonbuf.set(s); jsonbuf.parse().unwrap()});
    }
}
