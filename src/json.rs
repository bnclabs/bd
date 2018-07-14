use std::collections::HashMap;
use std::result;

pub type Result<T> = result::Result<T,Error>;

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

    pub fn set<T>(&mut self, text: &T) where T: AsRef<str> + ?Sized {
        self.inner.clear();
        self.inner.push_str(text.as_ref());
    }

    pub fn append<T>(&mut self, text: &T) where T: AsRef<str> + ?Sized {
        self.inner.push_str(text.as_ref());
    }

    pub fn sanitize(&mut self) -> Result<Json> {
        use ::json::Error::{MissingToken};

        let mut lex = Lex::new(0_usize, 1_usize, 1_usize);
        if self.inner.len() == 0 {
            return Err(MissingToken)
        }
        let val = self.sanitize_value(&mut lex)?;
        self.inner = self.inner[lex.off..].to_string(); // remaining text.
        Ok(val)
    }

    pub fn parse(&mut self) -> Json {
        let mut lex = Lex::new(0_usize, 1_usize, 1_usize);

        let val = self.parse_value(&mut lex);
        self.inner = self.inner[lex.off..].to_string(); // remaining text.
        val
    }

    fn sanitize_value(&mut self, lex: &mut Lex) -> Result<Json> {
        use ::json::Error::{InvalidToken};

        self.parse_whitespace(lex);
        let val = match &self.inner[lex.off..].chars().nth(0).unwrap() {
            'n' => self.sanitize_null(lex)?,
            't' => self.sanitize_true(lex)?,
            'f' => self.sanitize_false(lex)?,
            '0'..='9'|'+'|'-'|'.'|'e'|'E' => self.sanitize_num(lex)?,
            '"' => self.sanitize_string(lex)?,
            '[' => self.sanitize_array(lex)?,
            '{' => self.sanitize_object(lex)?,
            _ => return Err(InvalidToken(lex.off, lex.row, lex.col)),
        };

        Ok(val)
    }

    fn parse_value(&mut self, lex: &mut Lex) -> Json {
        self.parse_whitespace(lex);
        let val = match &self.inner[lex.off..].chars().nth(0).unwrap() {
            'n' => self.parse_null(lex),
            't' => self.parse_true(lex),
            'f' => self.parse_false(lex),
            '0'..='9'|'+'|'-'|'.'|'e'|'E' => self.parse_num(lex),
            '"' => self.parse_string(lex),
            '[' => self.parse_array(lex),
            '{' => self.parse_object(lex),
            _ => panic!("invalid token")
        };

        val
    }

    fn sanitize_null(&self, lex: &mut Lex) -> Result<Json> {
        use ::json::Error::InvalidToken;

        let text = &self.inner[lex.off..];
        if text.len() == 4 && &text[0..4] == "null" {
            lex.incr_col(4);
            Ok(Json::Null)
        } else {
            Err(InvalidToken(lex.off, lex.row, lex.col))
        }
    }

    fn parse_null(&self, lex: &mut Lex) -> Json {
        lex.incr_col(4);
        Json::Null
    }

    fn sanitize_true(&self, lex: &mut Lex) -> Result<Json> {
        use ::json::Error::InvalidToken;

        let text = &self.inner[lex.off..];
        if text.len() == 4 && &text[..4] == "true" {
            lex.incr_col(4);
            Ok(Json::Bool(true))
        } else {
            Err(InvalidToken(lex.off, lex.row, lex.col))
        }
    }

    fn parse_true(&self, lex: &mut Lex) -> Json {
        lex.incr_col(4);
        Json::Bool(true)
    }

    fn sanitize_false(&self, lex: &mut Lex) -> Result<Json> {
        use ::json::Error::InvalidToken;

        let text = &self.inner[lex.off..];
        if text.len() == 5 && &text[..5] == "false" {
            lex.incr_col(5);
            Ok(Json::Bool(false))
        } else {
            Err(InvalidToken(lex.off, lex.row, lex.col))
        }
    }

    fn parse_false(&self, lex: &mut Lex) -> Json {
        lex.incr_col(5);
        Json::Bool(false)
    }

    fn sanitize_num(&self, lex: &mut Lex) -> Result<Json> {
        use ::json::Error::InvalidToken;

        let text = &self.inner[lex.off..];
        for (i, ch) in text.chars().enumerate() {
            match ch {
                '0'..='9'|'+'|'-'|'.'|'e'|'E' => continue, // valid number
                _ => (),
            }
            if let Ok(num) = text[..i].parse::<i128>() {
                lex.incr_col(i);
                return Ok(Json::Integer(num))
            } else if let Ok(num) = text[..i].parse::<f64>() {
                lex.incr_col(i);
                return Ok(Json::Float(num))
            }
            return Err(InvalidToken(lex.off, lex.row, lex.col))
        }
        return Err(InvalidToken(lex.off, lex.row, lex.col))
    }

    fn parse_num(&self, lex: &mut Lex) -> Json {
        let text = &self.inner[lex.off..];
        for (i, ch) in text.chars().enumerate() {
            match ch {
                '0'..='9'|'+'|'-'|'.'|'e'|'E' => continue, // valid number
                _ => (),
            }
            if let Ok(num) = text[..i].parse::<i128>() {
                lex.incr_col(i);
                return Json::Integer(num)
            } else if let Ok(num) = text[..i].parse::<f64>() {
                lex.incr_col(i);
                return Json::Float(num)
            }
            break
        }
        panic!("invalid json");
    }

    fn sanitize_string(&mut self, lex: &mut Lex) -> Result<Json> {
        use ::json::Error::InvalidToken;

        let mut s = String::new();
        let mut esc = false;
        let mut chars = self.inner[lex.off..].chars().enumerate();
        self.codepoint.clear();

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
                    'u'  => {self.codepoint.push_str("\\u{")},
                    _ => return Err(InvalidToken(lex.off+i, lex.row, lex.col)),
                }
                continue

            } else if self.codepoint.len() == 7 { // unicode gathered
                self.codepoint.push('}');
                s.push_str(&self.codepoint);
                self.codepoint.clear();
                // fall through

            } else if self.codepoint.len() > 0 { // unicode escaped
                match ch {
                    '0'..='9'|'a'..='f'|'A'..='F' => self.codepoint.push(ch),
                    _ => return Err(InvalidToken(lex.off+i, lex.row, lex.col))
                };
                continue

            }
            if ch == '"' { // exit
                lex.incr_col(i);
                return Ok(Json::String(s))
            }
            match ch as i32 { // normal character
                0x20 | 0x21 | 0x23..=0x5B | 0x5D..=0x10FFFF => s.push(ch),
                _ => return Err(InvalidToken(lex.off+i, lex.row, lex.col)),
            }
        }
        Err(InvalidToken(self.inner.len(), lex.row, lex.col))
    }

    fn parse_string(&mut self, lex: &mut Lex) -> Json {
        if let Ok(val) = self.sanitize_string(lex) {
            return val
        }
        panic!("invalid string")
    }

    fn sanitize_array(&mut self, lex: &mut Lex) -> Result<Json> {
        use ::json::Error::InvalidToken;

        lex.incr_col(1); // skip '['

        let mut array = Vec::new();
        loop {
            self.parse_whitespace(lex);
            if (&self.inner[lex.off..]).len() == 0 {
                break Err(InvalidToken(lex.off, lex.row, lex.col))
            } else if (&self.inner[lex.off..]).as_bytes()[0] == b']' { // exit
                lex.incr_col(1);
                break Ok(Json::Array(array))
            }
            array.push(self.sanitize_value(lex)?);
        }
    }

    fn parse_array(&mut self, lex: &mut Lex) -> Json {
        lex.incr_col(1); // skip '['

        let mut array = Vec::new();
        loop {
            self.parse_whitespace(lex);
            if (&self.inner[lex.off..]).as_bytes()[0] == b']' { // exit
                lex.incr_col(1);
                break Json::Array(array)
            }
            array.push(self.parse_value(lex));
        }
    }

    fn sanitize_object(&mut self, lex: &mut Lex) -> Result<Json> {
        use ::json::Error::{InvalidToken, DuplicateKey, KeyIsNotString};

        lex.incr_col(1); // skip '{'

        let mut map = HashMap::new();
        loop {
            // key
            self.parse_whitespace(lex);
            let key = self.sanitize_string(lex)?;
            if let Json::String(_) = key {
                break Err(KeyIsNotString(lex.off, lex.row, lex.col))
            }
            let key = key.string();
            // colon
            self.parse_whitespace(lex);
            if (&self.inner[lex.off..]).len() == 0 {
                break Err(InvalidToken(lex.off, lex.row, lex.col))
            } else if (&self.inner[lex.off..]).as_bytes()[0] != b':' {
                break Err(InvalidToken(lex.off, lex.row, lex.col))
            }
            lex.incr_col(1); // skip ':'
            // value
            self.parse_whitespace(lex);
            let value = self.sanitize_value(lex)?;
            // is exit
            self.parse_whitespace(lex);
            if (&self.inner[lex.off..]).len() == 0 {
                break Err(InvalidToken(lex.off, lex.row, lex.col))
            } else if (&self.inner[lex.off..]).as_bytes()[0] == b'}' { // exit
                lex.incr_col(1);
                break Ok(Json::Object(map))
            }
            if let Some(_) = map.insert(key, value) {
                break Err(DuplicateKey(lex.off, lex.row, lex.col))
            }
        }
    }

    fn parse_object(&mut self, lex: &mut Lex) -> Json {
        lex.incr_col(1); // skip '{'

        let mut map = HashMap::new();
        loop {
            // key
            self.parse_whitespace(lex);
            let key = self.parse_string(lex).string();
            // colon
            self.parse_whitespace(lex);
            lex.incr_col(1); // skip ':'
            // value
            self.parse_whitespace(lex);
            let value = self.parse_value(lex);
            // is exit
            self.parse_whitespace(lex);
            if (&self.inner[lex.off..]).len() == 0 {
                panic!("invalid token");
            } else if (&self.inner[lex.off..]).as_bytes()[0] == b'}' { // exit
                lex.incr_col(1);
                break Json::Object(map)
            }
            map.insert(key, value);
        }
    }

    fn parse_whitespace(&self, lex: &mut Lex) {
        let text = &self.inner[lex.off..];
        for (i, ch) in text.chars().enumerate() {
            match ch {
                ' ' | '\t' | '\r' => { lex.col += 1; continue },
                '\n' => { lex.row += 1; lex.col = 0 },
                _ => break,
            }
            lex.off += i;
        }
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
