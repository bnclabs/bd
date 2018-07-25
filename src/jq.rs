use json::parse_whitespace;

pub type Result<T> = result::Result<T,JqError>;

pub enum JqError {
    MissingToken(String),
}

impl fmt::Display for JqError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use JqError::{MissingToken};

        match self {
            MissingToken(s) => write!(f, "missing token at {}", s),
        }
    }
}

impl error::Error for JsonError {
    fn cause(&self) -> Option<&error::Error> { None }
}



pub fn parse_program(text: &str, lex: &mut Lex) -> Result<Thunk> {
    use JsonError::MissingToken;

    parse_whitespace(text, lex)

    let valtext = &text[lex.off..];
    if valtext.len() == 0 {
        return Err(MissingToken(lex.format()))
    }
}

enum Thunk<I> where I: Iterator<Item=Json> {
    Source(I),
    Lookup(PathExpr, Thunk),
}

impl Thunk<I> where I: Iterator<Item=Json> {
    fn handle_source(&self, iter: I) -> I {
        return iter
    }

    fn handle_lookup(&self, path_expr: PathExpr, operand: Thunk) -> I {
        let items = Vec::new();
        for item in operand() {
        }
    }
}

impl<I> Fn<()> for Thunk<I> where I: Iterator<Item=Json> {
    extern "rust-call" fn call(&self, args: ()) -> Self::Output {
        match self {
            Source(iter) => self.handle_source(iter),
            Lookup(path_expr, thunk) => self.handle_lookup(path_expr, thunk),
        }
    }
}

impl<I> FnMut<()> for Thunk<I> where I: Iterator<Item=Json> {
    extern "rust-call" fn call_mut(&mut self, args: ()) -> Self::Output {
        self.call()
    }
}

impl<I> FnOnce<()> for Thunk<I> where I: Iterator<Item=Json> {
    type Output=I;

    extern "rust-call" fn call_mut(self, args: ()) -> Self::Output {
        self.call()
    }
}

pub fn compile(program: &str) -> Thunk {
}
