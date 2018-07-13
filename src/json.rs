//use std::collections::HashMap;
use std::result;

pub type Result<T> = result::Result<T,Error>;

pub enum Error {
    InvalidToken(usize, usize, usize),
    MissingToken,
}

struct Lex{ off: usize, row: usize, col: usize }

impl Lex {
    fn new(off: usize, row: usize, col: usize) -> Lex {
        Lex{off, row, col}
    }

    fn update(&mut self, off: usize, row: usize, col: usize) {
        self.off = off;
        self.row = row;
        self.col = col;
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
        self.inner.push_str(text.as_ref())
    }

    pub fn sanitize(&mut self) -> Result<Json> {
        use ::json::Error::{InvalidToken, MissingToken};

        let mut lex = Lex::new(0_usize, 1_usize, 1_usize);

        if self.inner.len() == 0 {
            return Err(MissingToken)
        }

        self.parse_whitespace(&mut lex);
        let val = match &self.inner[lex.off..].chars().nth(0).unwrap() {
            'n' => self.sanitize_null(&mut lex)?,
            't' => self.sanitize_true(&mut lex)?,
            'f' => self.sanitize_false(&mut lex)?,
            '0'..='9'|'+'|'-'|'.'|'e'|'E' => self.sanitize_num(&mut lex)?,
            '"' => self.sanitize_string(&mut lex)?,
            //'[' => validate_array(lex)),
            //'{' => Some(parse_object(lex)),
            _ => return Err(InvalidToken(lex.off, lex.row, lex.col)),
        };

        self.inner = self.inner[lex.off..].to_string(); // remaining text.
        Ok(val)
    }

    fn sanitize_null(&self, lex: &mut Lex) -> Result<Json> {
        use ::json::Error::InvalidToken;

        let text = &self.inner[lex.off..];
        if text.len() == 4 && &text[0..4] == "null" {
            let (off, row, col) = (lex.off+4, lex.row, lex.col+4);
            lex.update(off, row, col);
            Ok(Json::Null)
        } else {
            Err(InvalidToken(lex.off, lex.row, lex.col))
        }
    }

    fn parse_null(&self, lex: &mut Lex) -> Json {
        let (off, row, col) = (lex.off+4, lex.row, lex.col+4);
        lex.update(off, row, col);
        Json::Null
    }

    fn sanitize_true(&self, lex: &mut Lex) -> Result<Json> {
        use ::json::Error::InvalidToken;

        let text = &self.inner[lex.off..];
        if text.len() == 4 && &text[..4] == "true" {
            let (off, row, col) = (lex.off+4, lex.row, lex.col+4);
            lex.update(off, row, col);
            Ok(Json::Bool(true))
        } else {
            Err(InvalidToken(lex.off, lex.row, lex.col))
        }
    }

    fn parse_true(&self, lex: &mut Lex) -> Json {
        let (off, row, col) = (lex.off+4, lex.row, lex.col+4);
        lex.update(off, row, col);
        Json::Bool(true)
    }

    fn sanitize_false(&self, lex: &mut Lex) -> Result<Json> {
        use ::json::Error::InvalidToken;

        let text = &self.inner[lex.off..];
        if text.len() == 5 && &text[..5] == "false" {
            let (off, row, col) = (lex.off+5, lex.row, lex.col+5);
            lex.update(off, row, col);
            Ok(Json::Bool(false))
        } else {
            Err(InvalidToken(lex.off, lex.row, lex.col))
        }
    }

    fn parse_false(&self, lex: &mut Lex) -> Json {
        let (off, row, col) = (lex.off+5, lex.row, lex.col+5);
        lex.update(off, row, col);
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
                let (off, row, col) = (lex.off+i, lex.row, lex.col+i);
                lex.update(off, row, col);
                return Ok(Json::Integer(num))
            } else if let Ok(num) = text[..i].parse::<f64>() {
                let (off, row, col) = (lex.off+i, lex.row, lex.col+i);
                lex.update(off, row, col);
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
                let (off, row, col) = (lex.off+i, lex.row, lex.col+i);
                lex.update(off, row, col);
                return Json::Integer(num)
            } else if let Ok(num) = text[..i].parse::<f64>() {
                let (off, row, col) = (lex.off+i, lex.row, lex.col+i);
                lex.update(off, row, col);
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
                let (off, row, col) = (lex.off+i, lex.row, lex.col+i);
                lex.update(off, row, col);
                return Ok(Json::String(s))
            }
            match ch as i32 { // normal character
                0x20 | 0x21 | 0x23..=0x5B | 0x5D..=0x10FFFF => s.push(ch),
                _ => return Err(InvalidToken(lex.off+i, lex.row, lex.col)),
            }
        }
        Err(InvalidToken(self.inner.len(), lex.row, lex.col))
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
    //Array(Vec<Json>),
    //Object(HashMap<String, Json>),
}

//pub fn parse<'a>(text: &'a str) -> Option<Result<'a>> {
//    let mut lex = Lex{text, off: 0, row: 1, col: 1};
//    parse_value(&mut lex)
//}
//
//fn parse_value<'a>(lex: &'a mut Lex) -> Option<Result<'a>> {
//    use ::json::Error::InvalidToken;
//
//    parse_whitespace(lex);
//
//    if lex.text.len() > 0 {
//        match lex.text.chars().nth(0).unwrap() {
//            'n' => Some(parse_null(lex)),
//            't' => Some(parse_bool(lex)),
//            'f' => Some(parse_bool(lex)),
//            '0'..='9'|'+'|'-'|'.'|'e'|'E' => Some(parse_number(lex)),
//            '"' => Some(parse_string(lex)),
//            '[' => Some(parse_array(lex)),
//            //'{' => Some(parse_object(lex)),
//            _ => Some(Err(InvalidToken(lex.off, lex.row, lex.col))),
//        }
//
//    } else {
//        None
//    }
//}
//
//fn parse_string<'a>(lex: &'a mut Lex) -> Result<'a> {
//    use ::json::Error::InvalidToken;
//
//    let s = String::new();
//    let esc = false;
//    let u = String::new();
//    let chars = lex.text.chars().enumerate();
//
//    chars.next(); // skip the opening quote
//
//    for (i, ch) in lex.text.chars().enumerate() {
//        if esc { // escaped
//            match ch {
//                'b'  => {esc = false; s.push(8 as char)},
//                't'  => {esc = false; s.push(9 as char)},
//                'n'  => {esc = false; s.push(10 as char)},
//                'f'  => {esc = false; s.push(12 as char)},
//                'r'  => {esc = false; s.push(13 as char)},
//                '"'  => {esc = false; s.push(34 as char)},
//                '/'  => {esc = false; s.push(47 as char)},
//                '\\' => {esc = false; s.push(92 as char)},
//                'u'  => {u.push_str("\\u{")},
//                _ => return Err(InvalidToken(lex.off+i, lex.row, lex.col))
//            }
//            continue
//
//        } else if u.len() > 0 { // unicode escaped
//            match ch {
//                '0'..='9' | 'a'..='f' | 'A'..='F' if u.len() < 7 => u.push(ch),
//                _ => return Err(InvalidToken(lex.off+i, lex.row, lex.col))
//            };
//            if u.len() == 7 {
//                u.push('}');
//                s.push_str(&u);
//                u.clear();
//            }
//            continue
//
//        } else if ch == '"' {
//            lex.update(&lex.text[i..], lex.off+i, lex.row, lex.col+i);
//            return Ok(Json::String(s))
//        }
//        match ch as i32 {
//            0x20 | 0x21 | 0x23..=0x5B | 0x5D..=0x10FFFF => s.push(ch as char),
//            _ => return Err(InvalidToken(lex.off+i, lex.row, lex.col)),
//        }
//    }
//    Err(InvalidToken(lex.text.len(), lex.row, lex.col))
//}
//
//fn parse_array<'a>(lex: &'a mut Lex) -> Result<'a> {
//    use ::json::Error::InvalidToken;
//
//    lex.update(&lex.text[1..], lex.off+1, lex.row, lex.col+1);
//
//    let array = Vec::new();
//    loop {
//        parse_whitespace(lex);
//        if lex.text.len() == 0 {
//            break Err(InvalidToken(lex.off, lex.row, lex.col))
//        } else if lex.text.as_bytes()[0] == b']' { // exit array
//            break Ok(Json::Array(array))
//        } else if let Some(value) = parse_value(lex) {
//            array.push(value?);
//            continue
//        } else {
//            break Err(InvalidToken(lex.off, lex.row, lex.col))
//        }
//    }
//}
