use db::{Document, DocIterator};

pub struct Entry<D> where D: Document {
    pub meta: Option<D>, // has "domains", "sources", "keys"
    pub op: D,           // has "errors", "iterpos"
    pub doc: D,
}

impl Entry<D> where D: Document {
    fn init(&mut self)  {
        match self.meta {
            Some(meta) => {
                meta.set("domains", From::from(Vec::new::<Vec<D>>()));
                meta.set("sources", From::from(Vec::new::<Vec<D>>()));
                meta.set("keys", From::from(Vec::new::<Vec<D>>()));
            }
            None => (),
        }
        self.op.set("errors", From::from(Vec::new::<Vec<D>>()));
        self.op.set("iterpos", From::from(ITER_POSITION_ITEM));
    }

    pub fn new(doc: D) -> Entry<D> where D: Document {
        let entry = Entry{meta: None, op: Vec::new(), doc};
        entry.init();
        entry
    }

    pub fn new_with_meta(meta: D, doc: D) -> Entry<D> {
        let entry = Entry{meta, op: Vec::new(), doc}
        entry.init();
        entry
    }

    pub fn new_merged(entries: Vec<Entry<D>> doc: D) -> Entry<D> {
        let meta: D = From::from(Vec::new::<Vec<Property<D>>>());
        let mut entry = Entry::new_with_meta(meta, doc);
        for e in entries.into_iter() {
            if e.meta.is_some() { // meta
                let domains = e.meta.unwrap().get("domains").unwrap();
                entry.meta.append("domains", domains);
                let sources = e.meta.unwrap().get("sources").unwrap();
                entry.meta.append("sources", sources);
                let keys = e.meta.unwrap().get("keys").unwrap();
                entry.meta.append("keys", keys);
            }
            let errors = e.op.unwrap().get("errors").unwrap();
            entry.op.append("errors", errors);
        }
    }

    pub fn meta_get(&self, key: &str) -> Option<D> {
        match self.meta {
            Some(meta) => meta.get_ref(key).cloned(),
            None => None
        };
    }

    pub fn op_set(&mut self, key: &str, value: D) {
        self.op.set(key, value)
    }

    pub fn op_append(&mut self, key: &str, value: D) {
        let out: D = match self.op.get_mut(key) {
            Some(v) => v.append(value),
            None => value,
        }
        self.op.op_set(key, out)
    }

    pub fn op_get(&self, key: &str) -> Option<D> {
        self.op.get_ref(key).cloned()
    }

    pub fn set_error(&mut self, s: String) {
        self.op_append("errors", From::from(vec![s]))
    }

    pub fn has_error(&self) -> bool {
        match self.op_get("error") { Some(_) => true, None => false }
    }

    pub fn iter_position(&self) -> i128 {
        self.op_get("iterpos")?.integer()
    }
}

impl<D,T> From<Entry<D>> for Entry<T> where D: Document, T: Document {
    fn from(entry: Entry<D>) -> Entry<T> {
        meta = entry.meta.map(|meta| From::from(meta))
        op = From::from(entry.op)
        doc = From::from(entry.doc)
        Entry{meta, op, doc}
    }
}


const ITER_POSITION_ITEM: i128 = 1;
const ITER_POSITION_NEXT: i128 = 2;
const ITER_POSITION_END: i128 = 3;

fn fixpositions(entries: &mut Vec<Entry<D>>) {
    entries.iter_mut().for_each(|e| e.op_set("iterpos", ITER_POSITION_ITEM));
    if entries.len() > 1 {
        let last = entries[entries.len()-1];
        last.op_set("iterpos", From::from(ITER_POSITION_END));
    }
}
