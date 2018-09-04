use db::{Document, Input, Pipeline, Repeater};
use entry::Entry;

struct MemSource<D> where D: Document {
    entries: Vec<Entry<D>>,
    offset: usize,
}

impl<D> MemSource<D> where D: Document {
    pub fn new(docs: Vec<D>) -> MemSource<D> {
        let entries = docs.into_iter().map(|doc| Entry::new(doc));
        MemSource{entries, offset: 0}
    }
}

impl<D> Repeater<D> for MemSource<D> where D: Document {
    fn repeat<'a>(&'a self) -> Input<D> {
        Box::new(Iter{source: self, entries: &self.entries, offset: 0})
    }
}

impl<D> Iterator for MemSource<D> where D: Document {
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



struct Iter<'a,D> where D: 'a + Document {
    source: &'a MemSource<D>,
    entries: &'a Vec<Entry<D>>,
    offset: usize,
}

impl<'a,D> Repeater<D> for Iter<'a,D> where D: Document {
    fn repeat(&self) -> Input<D> {
        self.source.repeat()
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

impl<'a, D> Pipeline<D> for Iter<'a, D> where D: Document {
}
