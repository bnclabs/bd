pub struct Lex{ pub off: usize, pub row: usize, pub col: usize }

impl Lex {
    pub fn new(off: usize, row: usize, col: usize) -> Lex {
        Lex{off, row, col}
    }

    pub fn incr_col(&mut self, i: usize) {
        self.off += i;
        self.col += i;
    }

    pub fn set(&mut self, off: usize, row: usize, col: usize) {
        self.off = off; self.row = row; self.col = col;
    }

    pub fn format(&self, prefix: &str) -> String {
        format!(
            "{} at offset:{} line:{} col:{}",
            prefix, self.off, self.row, self.col
        )
    }
}
