use db::{Document, DocIterator};

// TODO: is it possible to monomorphise this ?
pub type Input<D> where D: Document = Box<DocIterator<Item=Entry<D>>>


// TODO: is it possible to monomorphise this ?
pub type Output<T> where T: Document = Box<DocIterator<Item=Entry<T>>>


pub struct Entry<D> where D: Document {
    meta: Option<D>,
    op: D,
    doc: D,
}

impl Entry<D> {
    pub fn new(doc: D) -> Entry<D> where D: Document {
        Entry{meta: None, doc}
    }

    pub fn new_with_meta(meta: D, doc: D) -> Entry<D> where D: Document {
        Entry{meta, doc}
    }

    // "domain", "source", "key"
    pub fn meta_get(&self, key: &str) -> Option<D> where D: Document {
        match self.meta {
            Some(meta) => meta.get_ref(key).cloned(),
            None => None
        };
    }

    // "path", "error", "op"
    pub fn op_set(&mut self, key: &str, value: D) {
        self.op.set(key, value)
    }

    pub fn op_append<'a>(&mut self, key: &str, path: String) {
        match meta.op_get(key) {
            Some(mut value) => {
                value.append(path)
                meta.op_set("path", value)
            },
            None => meta.op_set("path", path),
        }
    }

    pub fn op_get(&self, key: &str) -> Option<D> {
        self.op.get_ref(key).cloned()
    }

    pub fn has_error(&self) -> bool {
        match self.op_get("error") { Some(_) => true, None => false }
    }
}

impl Recurse for Entry<Json> {
    fn recurse(self) -> Vec<Entry<Json>> {
        let mut list = Vec::new();
        self.do_recurse_json(self, &mut list);
        list
    }
}

pub fn do_recurse_json(entry: Entry<Json>, list: &mut Vec<Entry<Json>>) {
    use json::Json::{Null, Bool, Integer, Float, String as S, Array, Object};

    match entry.doc {
        doc@Null | doc@Bool(_) | doc@Integer(_) | doc@Float(_) | doc@S(_) => {
            list.push(Entry::new_with_meta(entry.meta, doc))
        },
        Array(vals) => {
            let meta = entry.meta.clone();
            list.push(Entry::new_with_meta(entry.meta, Array(vals.clone())));
            vals.into_iter().enumerate().for_each(|(i, val)| {
                let entry = Entry::new_with_meta(meta.clone(), val);
                entry.op_append("path", format!(".{}", i));
                do_recurse_json(entry, list);
            });
        },
        Object(props) => {
            let meta = entry.meta.clone();
            list.push(Entry::new_with_meta(entry.meta, Object(props.clone())));
            props.iter().for_each(|prop| {
                let key = prop.key_ref().clone();
                let entry = Entry::new_with_meta(meta.clone(), prop.value());
                entry.op_append("path", format!(r#"."{}""#, key));
                do_recurse_json(entry, list)
            })
        },
    }
}


#[derive(Debug,Clone)]
pub struct KeyValue<K,D>(K, D) where K: PartialOrd , D: Document;

impl<K,D> KeyValue<K,D> where K: PartialOrd, D: Document {
    #[inline]
    pub fn new(key: K, value: D) -> KeyValue<K,D> {
        KeyValue(key, value)
    }

    #[inline]
    fn key(self) -> K {
        self.0
    }

    #[inline]
    fn key_ref(&self) -> &K {
        &self.0
    }

    #[inline]
    fn value(self) -> D {
        self.1
    }

    #[inline]
    fn value_ref(&self) -> &D {
        &self.1
    }

    #[inline]
    fn value_mut(&mut self) -> &mut D {
        &mut self.1
    }

    #[inline]
    fn set_value(&mut self, value: D) {
        self.1 = value;
    }
}


pub type ArrayItem<D> = KeyValue<usize,D>;

impl<D> Eq for ArrayItem<D> where D: Document {}

impl<D> PartialEq for ArrayItem<D> where D: Document {
    fn eq(&self, other: &ArrayItem<D>) -> bool {
        self.1 == other.1 // compare the value
    }
}

impl<D> PartialOrd for ArrayItem<D> where D: Document {
    fn partial_cmp(&self, other: &ArrayItem<D>) -> Option<Ordering> {
        self.1.partial_cmp(&other.value_ref()) // compare the value
    }
}


pub type Property<D> = KeyValue<String,D>;

impl<D> Eq for Property<D> where D: Document {}

impl<D> PartialEq for Property<D> where D: Document {
    fn eq(&self, other: &Property<D>) -> bool {
        self.0 == other.0 // compare only the key.
    }
}

impl<D> PartialOrd for Property<D> where D: Document {
    fn partial_cmp(&self, other: &Property<D>) -> Option<Ordering> {
        self.0.partial_cmp(&other.key_ref()) // compare only the key.
    }
}


pub fn search_by_key<D>(obj: &Vec<KeyValue<String,D>>, key: &str)
    -> result::Result<usize,usize> where D: Document
{
    use std::cmp::Ordering::{Greater, Equal, Less};

    let mut size = obj.len();
    if size == 0 { return Err(0) }

    let mut base = 0_usize;
    while size > 1 {
        let half = size / 2;
        let mid = base + half;
        // mid is always in [0, size), that means mid is >= 0 and < size.
        // mid >= 0: by definition
        // mid < size: mid = size / 2 + size / 4 + size / 8 ...
        let item: &str = obj[mid].key_ref();
        let cmp = item.cmp(key);
        base = if cmp == Greater { base } else { mid };
        size -= half;
    }
    // base is always in [0, size) because base <= mid.
    let item: &str = obj[base].key_ref();
    let cmp = item.cmp(key);
    if cmp == Equal { Ok(base) } else { Err(base + (cmp == Less) as usize) }
}

pub fn upsert_object_key<D>(obj: &mut Vec<Property<D>>, kv: Property<D>)
    where D: Document
{
    match search_by_key(obj, kv.key_ref()) {
        Ok(off) => obj[off] = kv,
        Err(off) => obj.insert(off, kv),
    }
}
