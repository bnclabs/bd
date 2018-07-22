struct PathExpr(String);

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
