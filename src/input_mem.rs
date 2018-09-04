use db::{Document, Input, Pipeline, Repeater};
use entry::Entry;

pub struct InputMem<D> where D: Document {
    entries: Vec<Entry<D>>,
}

impl<D> InputMem<D> where D: Document {
    pub fn new(docs: Vec<D>) -> InputMem<D> {
        let entries = docs.into_iter().map(|doc| Entry::new(doc)).collect();
        InputMem{entries}
    }

    pub fn repeat(&self) -> Input<D> {
        Box::new(Iter{entries: &self.entries, offset: 0})
    }
}



struct Iter<'a,D> where D: 'a + Document {
    entries: &'a Vec<Entry<D>>,
    offset: usize,
}

impl<'a,D> Repeater<'a,D> for Iter<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        Box::new(Iter{entries: self.entries, offset: 0})
    }
}

impl<'a, D> Iterator for Iter<'a, D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        if self.offset >= self.entries.len() {
            return None
        }
        let n = self.offset;
        self.offset += 1;
        Some(self.entries[n].clone())
    }
}

impl<'a, D> Pipeline<'a,D> for Iter<'a, D> where D: 'a + Document {
}
