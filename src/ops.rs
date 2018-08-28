use std::vec;
use std::marker::PhantomData;

use doc::{Document, DocIterator}


pub struct OpIdentity<D,T> where D: Document, T: Document + From<D> {
    input: Input<D>
    _data: PhantomData<T>
}

impl OpIdentity<D,T> where D: Document, T: Document + From<D> {
    pub fn new(input: Input<D>, _: T) -> OpIdentity<D,T>{
        OpIdentity{input, _data: PhantomData}
    }
}

impl<D,T> Clone for OpIdentity<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpIdentity {
        OpIdentity{input: self.input.clone(), _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpIdentity<D,T>
    where D: Document, T: Document + From<D>
{
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context) -> Option<Entry<T>> {
        let t_entry = From::from(self.input.next(c)?);
        t_entry.op_append("op", "identity ")
        Some(t_entry)
    }
}



pub struct OpRecurse<D,T> where D: Document, T: Document + From<D> {
    input: Input<D>,
    iter: Option<vec::IntoIter<Entry<D>>>,
    _data: PhantomData<T>
}

impl OpRecurse<D,T> {
    pub fn new(input: Input<D>, _: T) -> OpRecurse<D,T> {
        OpRecurse{input, iter: None}
    }
}

impl<D,T> Clone for OpRecurse<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpRecurse {
        if self.iter.is_some() {
            panic!("cannot clone OpRecurse after starting the iteration");
        }
        OpRecurse{input: self.input.clone(), iter: None, _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpRecurse<D,T> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context) -> Option<Entry<T>> {
        if let None = self.iter {
            let d_entry = self.input.next(c)?;
            if d_entry.has_error() { return Some(From::from(d_entry)) }
            self.iter = d_entry.recurse().into_iter();
        }

        let entry = loop {
            match self.iter.unwrap().next() {
                Some(entry) => break entry;
                None = {
                    let d_entry = self.input.next(c)?;
                    if d_entry.has_error() { return Some(From::from(d_entry)) }
                    self.iter = Some(d_entry.recurse().into_iter());
                }
            }
        }
        let t_entry = From::from(entry);
        t_entry.op_append("op", "recurse ")
        return Some(t_entry);
    }
}


pub struct OpNull<D,T> where D: Document, T: Document + From<D> {
    input: Input<D>,
    _data: PhantomData<T>,
}

impl OpNull<D,T> {
    pub fn new(input: Input<D>, _: T) -> OpNull<D,T> {
        OpNull{input, _data: PhantomData}
    }
}

impl<D,T> Clone for OpNull<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpNull {
        OpNull{input: self.input.clone(), _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpNull<D,T> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context) -> Option<Entry<T>> {
        let d_entry = self.input.next(c)?;
        if d_entry.has_error() {
            Some(From::from(d_entry))

        } else {
            let d_entry = D::null();
            let t_entry = From::from(Entry::new_with_meta(entry.meta, d_entry));
            t_entry.op_append("op", "recurse ")
            Some(t_entry)
        }
    }
}


pub struct OpBool<D,T> where D: Document, T: Document + From<D> {
    input: Input<D>,
    value: D,
    _data: PhantomData<T>,
}

impl OpBool<D,T> {
    pub fn new(input: Input<D>, value: bool, _: T) -> OpBool<D,T> {
        let value = From::from(value);
        OpBool{input, value, _data: PhantomData}
    }
}

impl<D,T> Clone for OpNull<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpNull {
        let (input, value) = (self.input.clone(), self.value.clone());
        OpNull{input, value, _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpBool<D,T> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context) -> Option<Entry<T>> {
        let entry = self.input.next(c)?;
        if entry.has_error() { Some(From::from(entry)) }

        Some(From::from(Entry::new_with_meta(entry.meta, self.value.clone())))
    }
}


pub struct OpInteger<D,T> where D: Document, T: Document + From<D> {
    input: Input<D>,
    value: i128,
    _data: PhantomData<T>,
}

impl OpInteger<D,T> {
    pub fn new(input: Input<D>, value: i128, _: T) -> OpInteger<D,T> {
        let value = From::from(value);
        OpInteger{input, value, _data: PhantomData}
    }
}

impl<D,T> Clone for OpInteger<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpInteger {
        let (input, value) = (self.input.clone(), self.value.clone());
        OpInteger{input, value: _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpInteger<D,T> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context) -> Option<Entry<T>> {
        let entry = self.input.next(c)?;
        if entry.has_error() { Some(From::from(entry)) }

        Some(From::from(Entry::new_with_meta(entry.meta, self.value.clone())))
    }
}


pub struct OpFloat<D,T> where D: Document, T: Document + From<D> {
    input: Input<D>,
    value: f64,
    _data: PhantomData<T>,
}

impl OpFloat<D,T> {
    pub fn new(input: Input<D>, value: f64, _: T) -> OpFloat<D,T> {
        let value = From::from(value);
        OpFloat{input, value, _data: PhantomData}
    }
}

impl<D,T> Clone for OpFloat<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpFloat {
        let (input, value) = (self.input.clone(), self.value.clone());
        OpFloat{input, value, _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpFloat<D,T> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context) -> Option<Entry<T>> {
        let entry = self.input.next(c)?;
        if entry.has_error() { Some(From::from(entry)) }

        Some(From::from(Entry::new_with_meta(entry.meta, self.value.clone())))
    }
}


pub struct OpString<D,T> where D: Document, T: Document + From<D> {
    input: Input<D>,
    value: String,
    _data: PhantomData<T>,
}

impl OpString<D,T> {
    pub fn new(input: Input<D>, value: String, _: T) -> OpString<D,T> {
        let value = From::from(value);
        OpString{input, value, _data: PhantomData}
    }
}

impl<D,T> Clone for OpString<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpString {
        let (input, value) = (self.input.clone(), self.value.clone());
        OpString{input, value, _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpString<D,T> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context) -> Option<Entry<T>> {
        let entry = self.input.next(c)?;
        if entry.has_error() { Some(From::from(entry)) }

        Some(From::from(Entry::new_with_meta(entry.meta, self.value.clone())))
    }
}


pub struct OpIndexShortcut<D,T> where D: Document, T: Document + From<D> {
    input: Input<D>,
    key: Option<String>,
    off: Option<isize>,
    _data: PhantomData<T>,
}

impl OpIndexShortcut<D,T> {
    pub fn new(input: Input<D>, key: Option<String>, off: Option<isize>, _: T) ->
        OpIndexShortcut<D,T>
    {
        OpIndexShortcut{input, key, off, _data: PhantomData}
    }

    fn do_get_shortcut<D>(&self, key: &str, doc: D)
        -> Result<D> where D: Document
    {
        if let Some(val) = doc.get(key) {
            Ok(val)
        } else {
            Err(format!("cannot index {} into {:?}", key, doc.doctype()))
        }
    }

    fn do_index_shortcut<D>(&self, off: isize, doc: D)
        -> Result<D> where D: Document
    {
        if let Some(val) = doc.index(off) {
            Ok(val)
        } else {
            Err(format!("cannot index {} into {:?}", key, doc.doctype()))
        }
    }
}

impl<D,T> Clone for OpIndexShortcut<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpIndexShortcut {
        let (input, key) = (self.input.clone(), self.key.clone());
        OpIndexShortcut{input, key, self.off.clone(), _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpIndexShortcut<D,T> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context) -> Option<Entry<T>> {
        let entry = self.input.next(c)?;
        if entry.has_error() { Some(From::from(entry)) }

        let (doc, path) = if let Some(key) = self.key {
            let doc = match do_get_shortcut(key.as_str(), entry.doc) {
                Ok(doc) => doc,
                Err(s) => { meta.set_error(s); D::null() }
            }
            (doc, format!(r#"."{}""#, key)) // TODO: optimize allocation

        } else if let Some(off) = self.off {
            let doc = match do_index_shortcut(key.as_str(), entry.doc) {
                Ok(doc) => doc,
                Err(s) => { c.set_error(s); D::null() }
            }
            (doc,  format!(".{}", off)) // TODO: optimize allocation

        } else {
            unreachable!()
        }
        let entry = Entry::new_with_meta(entry.meta, doc);
        entry.op_append("path", &path)
        Some(From::from(entry))
    }
}
