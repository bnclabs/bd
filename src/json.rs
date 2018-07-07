use std::collections::HashMap;

pub struct Lex{ offset: i32, row: i32, col: i32 };

pub type Result = Result<(Json, &str), Error>

pub enum Json<'a> {
    // unparsed
    Raw(&'a str),
    // parsed to a valid json type
    Null,
    Bool(bool),
    Integer(i128),
    Float(f64),
    String(&'a str),
    Array(Vec<Json<'a>>),
    Object(HashMap<&'a str, Json<'a>>),
}

impl Json {
    pub fn new(text: &str) -> Json {
        Json::Raw(text)
    }

    pub fn validate() {
    }
}

pub fn parse(text: &str) -> Result {
    parseat(text, Lex{offset: 0, row: 1, col: 1});
}

fn parseat(text: &str, lex: Lex) -> Result {
    match text.trim_left()[0] {
        'n' => parse_null(text, lex),
        't' => parse_bool(text, lex),
        'f' => parse_bool(text, lex),
        '"' => parse_string(text, lex),
        48..57 | '+' | '-' | '.' => parse_number(text, lex),
        '[' => parse_array(text, lex),
        '{' => parse_object(text, lex),
    }
}

fn parse_null(text: &str, lex: Lex) -> Result {
}

fn parse_bool(text: &str, lex: Lex) -> Result {
}

fn parse_string(text: &str, lex: Lex) -> Result {
}

fn parse_string(text: &str, lex: Lex) -> Result {
}

fn parse_array(text: &str, lex: Lex) -> Result {
}

fn parse_object(text: &str) -> Result {
}
