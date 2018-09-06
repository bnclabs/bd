use db::Document;
use meta::Meta;
use op::Op;

#[derive(Clone)]
pub struct Entry<D> where D: Document {
    pub meta: Meta<D>, // has "domains", "sources", "keys"
    pub op: Op<D>,     // has "errors", "iterpos"
    pub doc: D,
}

impl<D> Entry<D> where D: Document {
    pub fn new(doc: D) -> Entry<D> where D: Document {
        Entry{meta: Meta::none(), op: Op::new(), doc}
    }

    pub fn new_with_meta(meta: Meta<D>, doc: D) -> Entry<D> {
        Entry{meta, op: Op::new(), doc}
    }

    pub fn new_merged(entries: Vec<Entry<D>>, doc: D) -> Entry<D> {
        let mut entry = Entry::new_with_meta(Meta::new(), doc);
        for e in entries.into_iter() {
            entry.meta.merge(e.meta);
            entry.op.merge(e.op);
        }
        entry
    }

    pub fn set_error(&mut self, s: String) {
        self.op.set("errors", <D as From<Vec<D>>>::from(vec![From::from(s)]));
    }

    pub fn has_error(&self) -> bool {
        self.op.get_ref("error").unwrap().array_ref().unwrap().len() > 0
    }

    pub fn iter_position(&self) -> IterPosition {
        let val = self.op.get_ref("iterpos").unwrap().clone().integer().unwrap();
        From::from(val)
    }

    pub fn into<T>(self) -> Entry<T> where T: Document + From<D> {
        Entry{meta:self.meta.into(), op:self.op.into(), doc:From::from(self.doc)}
    }
}



#[derive(Debug,Eq,PartialEq,PartialOrd,Ord)]
pub enum IterPosition {
    Item,
    Next,
    End,
}

impl From<IterPosition> for i128 {
    fn from(val: IterPosition) -> i128 {
        use self::IterPosition::{Item, Next, End};
        match val { Item => 1, Next => 2, End => 3 }
    }
}

impl From<i128> for IterPosition {
    fn from(val: i128) -> IterPosition {
        use self::IterPosition::{Item, Next, End};
        match val { 1 => Item, 2 => Next, 3 => End, _ => unreachable!() }
    }
}

pub fn fixpositions<D>(mut entries: Vec<Entry<D>>)
    -> Vec<Entry<D>> where D: Document
{
    entries.iter_mut().for_each(
        |e| e.op.set(
            "iterpos", <D as From<i128>>::from(From::from(IterPosition::Item))
        )
    );
    let n = entries.len();
    if n > 1 {
        entries[n-1].op.set(
            "iterpos", <D as From<i128>>::from(From::from(IterPosition::End))
        );
    }
    entries
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;
    use json::{Json,Property};

    #[test]
    fn test_iterposition() {
        let val: i128 = From::from(IterPosition::Item);
        let pos: IterPosition = From::from(val);
        assert_eq!(pos, From::from(IterPosition::Item));

        let val: i128 = From::from(IterPosition::Next);
        let pos: IterPosition = From::from(val);
        assert_eq!(pos, From::from(IterPosition::Next));

        let val: i128 = From::from(IterPosition::End);
        let pos: IterPosition = From::from(val);
        assert_eq!(pos, From::from(IterPosition::End));
    }

    #[bench]
    fn bench_entry_new(b: &mut Bencher) {
        let doc: Json = <Json as From<Vec<Property>>>::from(Vec::new());
        b.iter(|| doc.clone());
    }
}
