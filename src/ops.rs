use std::{vec, iter};
use std::marker::PhantomData;

use entry::{self,Entry,IterPosition};
use db::{Document, Doctype, ItemIterator};
use prop::Property;


#[derive(Clone)]
pub enum Op<D,T>
    where
    D: Document + Clone,
    T: Document + From<D> + Clone
{
    Identity{ input: Box<Op> },
    Recurse{ input: Box<Op>, iter: Option<vec::IntoIter<Entry<D>>> },

    Null{ input: Box<Op> },
    Bool{ input: Box<Op>, Value: T },
    Integer{ input: Box<Op>, Value: T },
    Float{ input: Box<Op>, Value: T },
    String{ input: Box<Op>, Value: T },

    Index{ input: Box<Op> key: Option<String>, off: Option<isize> },
    Identifier{ input: Box<Op> symbol: String, index: Option<isize> },
    Slice{ input: Box<Op> start: isize, end: isize },
    IterValues{ input: Box<Op> iter: Option<vec::IntoIter<Entry<D>>> },
    Iter{ inputs: Vec<Op> done: bool, iter: Option<vec::IntoIter<Entry<D>>> },
    List{ inputs: Vec<Op> },
    Dict{ inputs: Vec<(Op, Op)> },

    Neg{ input: Box<Op> },
    Not{ input: Box<Op> },
    Mul{ lhs: Box<Op> rhs: Box<Op> },
    Div{ lhs: Box<Op> rhs: Box<Op> },
    Rem{ lhs: Box<Op> rhs: Box<Op> },
    Add{ lhs: Box<Op> rhs: Box<Op> },
    Sub{ lhs: Box<Op> rhs: Box<Op> },
    Shr{ lhs: Box<Op> rhs: Box<Op> },
    Shl{ lhs: Box<Op> rhs: Box<Op> },
    Bitand{ lhs: Box<Op>, rhs: Box<Op> },
    Bitor{ lhs: Box<Op>, rhs: Box<Op> },
    Bitxor{ lhs: Box<Op>, rhs: Box<Op> },
    Eq{ lhs: Box<Op>, rhs: Box<Op> },
    Ne{ lhs: Box<Op>, rhs: Box<Op> },
    Lt{ lhs: Box<Op>, rhs: Box<Op> },
    Le{ lhs: Box<Op>, rhs: Box<Op> },
    Gt{ lhs: Box<Op>, rhs: Box<Op> },
    Ge{ lhs: Box<Op>, rhs: Box<Op> },

    And{ lhs: Box<Op>, rhs: Box<Op> },
    Or{ lhs: Box<Op>, rhs: Box<Op> },

    BuiltinLength{ arg: Option<Box<Op>> },
    BuiltinChars{ arg: Option<Box<Op>> },
    BuiltinKeys{ arg: Option<Box<Op>> },
}

impl<D,T> for Op<D,T>
    where
    D: Document + Clone,
    T: Document + From<D> + Clone
{
    // constructors
    pub fn new_identity(input: Op) -> Op<D,T> {
        Op::Identity{input: Box::new(input)}
    }

    pub fn new_recurse(input: Op) -> OpRecurse<D,T,I> {
        Op::Recurse{input: Box::new(input), iter: None}
    }

    // privates
    fn recurse_values(input: &mut Op) -> Option<Vec<Entry<D>>> {
        let d_entry = input.next()?;
        if d_entry.has_error() { return Some(vec![d_entry]) }

        let mut entries = Vec::new();
        for value in d_entry.doc.recurse() {
            let mut entry = d_entry.clone();
            entry.doc = value;
            entries.push(entry)
        }
        Some(entry::fixpositions(entries))
    }

    fn recurse_next(iter: &mut vec::IntoIter<Entry<D>>) -> Option<Entry<D> {
        let entry = loop {
            match iter.unwrap().next() {
                Some(entry) => break entry,
                None => { *iter = Some(self.recurse_values()?.into_iter()); },
            }
        };
        Some(entry)
    }
}


impl<D,T> Iterator for Op<D,T> {
    type Item=Entry<T>

    fn next(&mut self) -> Option<Entry<T>> {
        match self {
            Identity(input) => Some(input.next()?.into()),
            Recurse(ref mut input, ref mut iter) => {
                if iter.is_none() {
                    *iter = Some(Op::recurse_values(input)?.into_iter())
                }
                Op::recurse_next(iter).map(|entry| entry.into())
            },
        }
    }
}



impl<D,T,I> OpNull<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(input: I, _: T) -> OpNull<D,T,I> {
        OpNull{input, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpNull<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpNull<D,T,I> {
        OpNull{input: self.input.clone(), _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpNull<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        self.input.next()?; // ignore any meta information, errors from input.
        Some(Entry::new(T::null()))
    }
}


impl<D,T,I> OpBool<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(input: I, value: bool, _: T) -> OpBool<D,T,I> {
        OpBool{input, value: From::from(value), _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpBool<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpBool<D,T,I> {
        let (input, value) = (self.input.clone(), self.value.clone());
        OpBool{input, value, _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpBool<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        self.input.next()?; // ignore any meta information, errors from input.
        Some(Entry::new(self.value.clone()))
    }
}


impl<D,T,I> OpInteger<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(input: I, value: i128, _: T) -> OpInteger<D,T,I> {
        OpInteger{input, value: From::from(value), _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpInteger<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpInteger<D,T,I> {
        let (input, value) = (self.input.clone(), self.value.clone());
        OpInteger{input, value, _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpInteger<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        self.input.next()?; // ignore any meta information, errors from input.
        Some(Entry::new(self.value.clone()))
    }
}


impl<D,T,I> OpFloat<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(input: I, value: f64, _: T) -> OpFloat<D,T,I> {
        OpFloat{input, value: From::from(value), _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpFloat<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpFloat<D,T,I> {
        let (input, value) = (self.input.clone(), self.value.clone());
        OpFloat{input, value, _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpFloat<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        self.input.next()?; // ignore any meta information, errors from input.
        Some(Entry::new(self.value.clone()))
    }
}


impl<D,T,I> OpString<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(input: I, value: String, _: T) -> OpString<D,T,I> {
        OpString{input, value: From::from(value), _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpString<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpString<D,T,I> {
        let (input, value) = (self.input.clone(), self.value.clone());
        OpString{input, value, _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpString<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        self.input.next()?; // ignore any meta information, errors from input.
        Some(Entry::new(self.value.clone()))
    }
}



impl<D,T,I> OpIndex<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(input: I, key: Option<String>, off: Option<isize>, _: T)
        -> OpIndex<D,T,I>
    {
        OpIndex{input, key, off, _data: PhantomData}
    }

    fn do_get_shortcut(&self, doc: D) -> Result<D,String> {
        let key = &self.key.unwrap();
        if let Some(val) = doc.get(key) {
            Ok(val)
        } else {
            Err(format!("cannot index {} into {:?}", key, doc.doctype()))
        }
    }

    fn do_index_shortcut(&self, doc: D) -> Result<D,String> {
        let off = self.off.unwrap();
        if let Some(val) = doc.index(off) {
            Ok(val)
        } else {
            Err(format!("cannot index {} into {:?}", off, doc.doctype()))
        }
    }
}

impl<D,T,I> Clone for OpIndex<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpIndex<D,T,I> {
        let (input, key) = (self.input.clone(), self.key.clone());
        OpIndex{input, key, off: self.off.clone(), _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpIndex<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let mut d_entry = self.input.next()?;
        if d_entry.has_error() { return Some(d_entry.into()) }

        let res = if let Some(key) = self.key {
            self.do_get_shortcut(d_entry.doc)

        } else if let Some(off) = self.off {
            self.do_index_shortcut(d_entry.doc)

        } else {
            unreachable!()
        };

        let d_entry = match res {
            Ok(doc) => {d_entry.doc = doc; d_entry},
            Err(s) => {d_entry.doc = D::null(); d_entry.set_error(s); d_entry},
        };
        Some(d_entry.into())
    }
}


impl<D,T,I> OpIdentifier<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(input: I, symbol: String, _: T) -> OpIdentifier<D,T,I> {
        let index = symbol.parse().ok();
        OpIdentifier{input, symbol, index, _data: PhantomData}
    }

    fn do_lookup(&self, doc: D) -> Result<D,String> {
        if let Some(val) = doc.get(&self.symbol) {
            return Ok(val)
        } else if let Some(off) = self.index {
            if let Some(val) = doc.index(off) { return Ok(val) }
        }
        Err(format!("cannot index {} into {:?}", self.symbol, doc.doctype()))
    }
}

impl<D,T,I> Clone for OpIdentifier<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpIdentifier<D,T,I> {
        let (input, symbol) = (self.input.clone(), self.symbol.clone());
        let index = self.index.clone();
        OpIdentifier{input, symbol, index, _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpIdentifier<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let mut d_entry = self.input.next()?;
        if d_entry.has_error() { return Some(d_entry.into()) }

        match self.do_lookup(d_entry.doc) {
            Ok(doc) => {d_entry.doc = doc},
            Err(s) => {d_entry.doc = D::null(); d_entry.set_error(s)},
        }
        Some(d_entry.into())
    }
}


impl<D,T,I> OpSlice<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(input: I, start: isize, end: isize, _: T) -> OpSlice<D,T,I> {
        OpSlice{input, start, end, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpSlice<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpSlice<D,T,I> {
        let (input, start, end) = (self.input.clone(), self.start, self.end);
        OpSlice{input, start, end, _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpSlice<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let mut d_entry = self.input.next()?;
        if d_entry.has_error() { return Some(d_entry.into()) }

        let dt = d_entry.doc.doctype();
        let d_entry = match d_entry.doc.slice(self.start, self.end) {
            Some(doc) => { d_entry.doc = doc; d_entry },
            None => {
                d_entry.set_error(format!("cannot slice {:?}", dt));
                d_entry
            },
        };
        Some(d_entry.into())
    }
}


impl<D,T,I> OpItervalues<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(input: I, _: T) -> OpItervalues<D,T,I> {
        OpItervalues{input, iter: None, _data: PhantomData}
    }

    pub fn iter_values(&self) -> Option<Vec<Entry<D>>> {
        let mut d_entry = self.input.next()?;
        if d_entry.has_error() { return Some(vec![d_entry]) }

        let dt = d_entry.doc.doctype();
        let mut entries = match docvalues(d_entry.doc.clone()) {
            Some(values) => {
                values.into_iter()
                    .map(|doc| Entry::new_with_meta(d_entry.meta.clone(), doc))
                    .collect()
            },
            None => {
                d_entry.set_error(format!("cannot iterate {:?}", dt));
                vec![d_entry]
            },
        };
        Some(entry::fixpositions(entries))
    }
}

impl<D,T,I> Clone for OpItervalues<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpItervalues<D,T,I> {
        if self.iter.is_some() {
            panic!("cannot clone OpItervalues after starting the iteration");
        }
        OpItervalues{input: self.input.clone(), iter: None, _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpItervalues<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        if self.iter.is_none() {
            let entries = self.iter_values()?;
            self.iter = Some(entries.into_iter());
        }

        let d_entry = loop {
            match self.iter.unwrap().next() {
                Some(entry) => break entry,
                None => {
                    let entries = self.iter_values()?;
                    self.iter = Some(entries.into_iter());
                },
            }
        };
        Some(d_entry.into())
    }
}


impl<D,T,I> OpIter<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(inputs: Vec<I>, _: T) -> OpIter<D,T,I> {
        OpIter{inputs, done: false, iter: None, _data: PhantomData}
    }

    pub fn iter_entries(&mut self) -> Option<Vec<Entry<D>>> {
        let mut entries = Vec::new();
        let mut n = self.inputs.len();
        for input in self.inputs.iter() {
            match input.next() {
                Some(entry) => entries.push(entry),
                None => { n -= 1; entries.push(Entry::new(D::null())) },
            }
        }
        if n > 0 { Some(entry::fixpositions(entries)) } else { None }
    }
}

impl<D,T,I> Clone for OpIter<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpIter<D,T,I> {
        if self.iter.is_some() {
            panic!("cannot clone OpRecurse after starting the iteration");
        }
        let inputs = self.inputs.clone();
        OpIter{inputs, done: self.done, iter: None, _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpIter<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        if self.done { return None }

        if self.iter.is_none() {
            let entries = self.iter_entries()?;
            self.iter = Some(entries.into_iter());
        }

        let d_entry = loop {
            match self.iter.unwrap().next() {
                Some(entry) => break entry,
                None => {
                    let entries = self.iter_entries()?;
                    self.iter = Some(entries.into_iter());
                },
            }
        };
        Some(d_entry.into())
    }
}


impl<D,T,I> OpList<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(inputs: Vec<I>, _: T) -> OpList<D,T,I> {
        OpList{inputs, _data: PhantomData}
    }

    pub fn next_entries(&mut self) -> Option<Vec<Entry<D>>> {
        let mut entries: Vec<Entry<D>> = Vec::new();
        let mut n = self.inputs.len();
        for input in self.inputs.iter_mut() {
            loop {
                match input.next() {
                    Some(entry) => match entry.iter_position() {
                        IterPosition::Next => entries.push(entry),
                        IterPosition::Item | IterPosition::End => {
                            entries.push(entry);
                            break
                        },
                    },
                    None => {
                        n -= 1;
                        entries.push(Entry::new(D::null()));
                        break
                    },
                }
            }
        }
        if n > 0 { Some(entries) } else { None }
    }
}

impl<D,T,I> Clone for OpList<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpList<D,T,I> {
        OpList{inputs: self.inputs.clone(), _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpList<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let entries = self.next_entries()?;
        let values: Vec<D> = entries.iter().map(|e| e.doc.clone()).collect();
        let d_entry = Entry::new_merged(entries, From::from(values));
        Some(d_entry.into())
    }
}


impl<D,T,I> OpDict<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(inputs: Vec<(I,I)>, _: T) -> OpDict<D,T,I> {
        OpDict{inputs, iter: None, _data: PhantomData}
    }

    fn collect_prop(&self, kinput: &I, vinput: &I)
        -> Option<Vec<(Entry<D>, Vec<Entry<D>>)>>
    {
        // gather keys
        let keys = Vec::new();
        loop {
            match kinput.next() {
                Some(entry) => match entry.iter_position() {
                    IterPosition::Next => keys.push(entry),
                    IterPosition::Item | IterPosition::End => {
                        keys.push(entry);
                        break
                    },
                },
                None => break,
            }
        }
        if keys.len() == 0 { return None }

        // gather values
        let values = Vec::new();
        loop {
            match vinput.next() {
                Some(entry) => match entry.iter_position() {
                    IterPosition::Next => values.push(entry),
                    IterPosition::Item | IterPosition::End => {
                        values.push(entry);
                        break
                    },
                },
                None => break,
            }
        }
        if values.len() == 0 { return None }

        // final
        let mut props = Vec::new();
        for key in keys.into_iter() {
            props.push((key, values.clone()))
        }
        Some(props)
    }

    fn collect_props(&self) -> Option<Vec<(Entry<D>, Vec<Entry<D>>)>> {
        let mut props = Vec::new();
        let mut n = self.inputs.len();
        for (kinput, vinput) in self.inputs.iter() {
            match self.collect_prop(kinput, vinput) {
                Some(mut sub_props) => props.append(&mut sub_props),
                None => { n -= 1; },
            }
        }
        if n > 0 { Some(props) } else { None }
    }

    fn collect_dict(bp: Vec<(Entry<D>, Vec<Entry<D>>)>)
        -> Vec<Vec<(Entry<D>, Entry<D>)>>
    {
        let (key, values) = bp.remove(0);
        if bp.len() == 0 {
            return values.into_iter().map(|v| vec![(key.clone(), v)]).collect();
        }
        let tail = Self::collect_dict(bp);
        let mut dict_lists: Vec<Vec<Vec<(Entry<D>, Entry<D>)>>> =
            iter::repeat(tail).take(values.len()).collect();
        for (i, value) in values.into_iter().enumerate() {
            for dict in dict_lists[i].iter_mut() {
                dict.push((key.clone(), value))
            }
        }
        dict_lists.into_iter().flatten().collect()
    }

    fn next_props(&self) -> Option<Vec<Entry<D>>> {
        let bp = self.collect_props()?;
        let full = Vec::new();
        for dict in Self::collect_dict(bp).into_iter() {
            let entries: Vec<Entry<D>> = Vec::new();
            let keys: Vec<String> = Vec::new();
            let values: Vec<D> = Vec::new();
            for (kentry, ventry) in dict {
                entries.push(kentry);
                entries.push(ventry);
                if kentry.has_error() || ventry.has_error() { continue }
                match kentry.doc.string() {
                    Some(s) => { keys.push(s); values.push(ventry.doc) },
                    None => (),
                }
            }
            let props: Vec<Property<D>> = keys.into_iter().zip(values)
                .map(|(k,v)| Property::new(k,v)).collect();
            full.push(Entry::new_merged(entries, From::from(props)));
        }
        Some(full)
    }
}

impl<D,T,I> Clone for OpDict<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpDict<D,T,I> {
        if self.iter.is_some() {
            panic!("cannot clone OpRecurse after starting the iteration");
        }
        OpDict{inputs: self.inputs.clone(), iter: None, _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpDict<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        if let None = self.iter {
            self.iter = Some(self.next_props()?.into_iter());
        }

        let entry = loop {
            match self.iter.unwrap().next() {
                Some(entry) => break entry,
                None => { self.iter = Some(self.next_props()?.into_iter()); },
            }
        };
        Some(entry.into())
    }

}


impl<D,T,I> OpNeg<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(input: I, _: T) -> OpNeg<D,T,I> {
        OpNeg{input, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpNeg<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpNeg<D,T,I> {
        OpNeg{input: self.input.clone(), _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpNeg<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_entry = self.input.next()?;
        d_entry.doc = -d_entry.doc;
        Some(d_entry.into())
    }
}


impl<D,T,I> OpNot<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(input: I, _: T) -> OpNot<D,T,I> {
        OpNot{input, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpNot<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpNot<D,T,I> {
        OpNot{input: self.input.clone(), _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpNot<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_entry = self.input.next()?;
        d_entry.doc = !d_entry.doc;
        Some(d_entry.into())
    }
}


impl<D,T,I> OpMult<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpMult<D,T,I> {
        OpMult{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpMult<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpMult<D,T,I> {
        OpMult{lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpMult<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc * d_rhs.doc;
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpDiv<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpDiv<D,T,I> {
        OpDiv{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpDiv<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpDiv<D,T,I> {
        OpDiv{lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpDiv<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc / d_rhs.doc;
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpRem<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpRem<D,T,I> {
        OpRem{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpRem<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpRem<D,T,I> {
        OpRem{lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpRem<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc % d_rhs.doc;
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpAdd<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpAdd<D,T,I> {
        OpAdd{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpAdd<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpAdd<D,T,I> {
        OpAdd{lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpAdd<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc + d_rhs.doc;
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpSub<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpSub<D,T,I> {
        OpSub{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpSub<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpSub<D,T,I> {
        OpSub{lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpSub<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc - d_rhs.doc;
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpShr<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpShr<D,T,I> {
        OpShr{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpShr<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpShr<D,T,I> {
        OpShr{lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpShr<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc >> d_rhs.doc;
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpShl<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpShl<D,T,I> {
        OpShl{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpShl<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpShl<D,T,I> {
        OpShl{lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData}
    }
}

impl<D,T,I> Iterator for OpShl<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc << d_rhs.doc;
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpBitand<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpBitand<D,T,I> {
        OpBitand{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpBitand<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpBitand<D,T,I> {
        OpBitand{
            lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T,I> Iterator for OpBitand<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc & d_rhs.doc;
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpBitor<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpBitor<D,T,I> {
        OpBitor{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpBitor<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpBitor<D,T,I> {
        OpBitor{
            lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T,I> Iterator for OpBitor<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc | d_rhs.doc;
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpBitxor<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpBitxor<D,T,I> {
        OpBitxor{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpBitxor<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpBitxor<D,T,I> {
        OpBitxor{
            lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T,I> Iterator for OpBitxor<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc ^ d_rhs.doc;
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpEq<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpEq<D,T,I> {
        OpEq{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpEq<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpEq<D,T,I> {
        OpEq{
            lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T,I> Iterator for OpEq<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = From::from(d_lhs.doc == d_rhs.doc);
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpNe<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpNe<D,T,I> {
        OpNe{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpNe<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpNe<D,T,I> {
        OpNe{
            lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T,I> Iterator for OpNe<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = From::from(d_lhs.doc != d_rhs.doc);
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpLt<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpLt<D,T,I> {
        OpLt{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpLt<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpLt<D,T,I> {
        OpLt{
            lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T,I> Iterator for OpLt<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = From::from(d_lhs.doc < d_rhs.doc);
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpLe<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpLe<D,T,I> {
        OpLe{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpLe<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpLe<D,T,I> {
        OpLe{
            lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T,I> Iterator for OpLe<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = From::from(d_lhs.doc >= d_rhs.doc);
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpGt<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpGt<D,T,I> {
        OpGt{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpGt<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpGt<D,T,I> {
        OpGt{
            lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T,I> Iterator for OpGt<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = From::from(d_lhs.doc > d_rhs.doc);
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpGe<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpGe<D,T,I> {
        OpGe{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpGe<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpGe<D,T,I> {
        OpGe{
            lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T,I> Iterator for OpGe<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc = From::from(d_lhs.doc >= d_rhs.doc);
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> Clone for OpAnd<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpAnd<D,T,I> {
        OpAnd{
            lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T,I> Iterator for OpAnd<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc: D = From::from(d_lhs.doc.and(d_rhs.doc));
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> OpOr<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(lhs: I, rhs: I, _: T) -> OpOr<D,T,I> {
        OpOr{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T,I> Clone for OpOr<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> OpOr<D,T,I> {
        OpOr{
            lhs: self.lhs.clone(), rhs: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T,I> Iterator for OpOr<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let entries = vec![d_lhs, d_rhs];
        let doc: D = From::from(d_lhs.doc.or(d_rhs.doc));
        Some(Entry::new_merged(entries, doc).into())
    }
}


impl<D,T,I> BuiltinLength<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(args: Vec<I>, _: T) -> BuiltinLength<D,T,I> {
        if args.len() == 1 {
            BuiltinLength{arg: Some(args[0]), _data: PhantomData}
        } else {
            BuiltinLength{arg: None, _data: PhantomData}
        }
    }
}

impl<D,T,I> Clone for BuiltinLength<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> BuiltinLength<D,T,I> {
        BuiltinLength{ arg: self.arg.clone(), _data: PhantomData }
    }
}

impl<D,T,I> Iterator for BuiltinLength<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        if self.arg.is_none() {
            let entry = Entry::new(T::null());
            entry.set_error(format!("invalid number of args for length"));
            return Some(entry)
        }
        let mut d_entry = self.arg.unwrap().next()?;
        match d_entry.doc.len() {
            Some(n) => {
                d_entry.doc = From::from(n as i128);
            },
            None => {
                let dt = d_entry.doc.doctype();
                d_entry.doc = D::null();
                d_entry.set_error(format!("cannot find length for {:?}", dt));
            },
        };
        Some(d_entry.into())
    }
}


impl<D,T,I> BuiltinChars<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(args: Vec<I>, _: T) -> BuiltinChars<D,T,I> {
        if args.len() == 1 {
            BuiltinChars{arg: Some(args[0]), _data: PhantomData}
        } else {
            BuiltinChars{arg: None, _data: PhantomData}
        }
    }
}

impl<D,T,I> Clone for BuiltinChars<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> BuiltinChars<D,T,I> {
        BuiltinChars{ arg: self.arg.clone(), _data: PhantomData }
    }
}

impl<D,T,I> Iterator for BuiltinChars<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        if self.arg.is_none() {
            let entry = Entry::new(T::null());
            entry.set_error(format!("invalid number of args for chars"));
            return Some(entry)
        }
        let mut d_entry = self.arg.unwrap().next()?;
        match d_entry.doc.into_iter() {
            Some(iter) => {
                d_entry.doc = From::from(iter.collect::<Vec<D>>());
            },
            None => {
                let dt = d_entry.doc.doctype();
                d_entry.doc = D::null();
                d_entry.set_error(format!("cannot find chars for {:?}", dt));
            },
        }
        Some(d_entry.into())
    }
}


impl<D,T,I> BuiltinKeys<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    pub fn new(args: Vec<I>, _: T) -> BuiltinKeys<D,T,I> {
        if args.len() == 1 {
            BuiltinKeys{arg: Some(args[0]), _data: PhantomData}
        } else {
            BuiltinKeys{arg: None, _data: PhantomData}
        }
    }
}

impl<D,T,I> Clone for BuiltinKeys<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    fn clone(&self) -> BuiltinKeys<D,T,I> {
        BuiltinKeys{ arg: self.arg.clone(), _data: PhantomData }
    }
}

impl<D,T,I> Iterator for BuiltinKeys<D,T,I>
    where
    D: Document, T: Document + From<D>,
    I: Iterator<Item=Entry<D>> + Clone
{
    type Item=Entry<T>;

    fn next(&mut self) -> Option<Entry<T>> {
        if self.arg.is_none() {
            let entry = Entry::new(T::null());
            entry.set_error(format!("invalid number of args for keys"));
            return Some(entry)
        }
        let mut d_entry = self.arg.unwrap().next()?;
        match d_entry.doc.doctype() {
            Doctype::Array => {
                let ln = d_entry.doc.len().unwrap();
                let keys = (0..ln).map(|i| From::from(i as i128)).collect();
                d_entry.doc = From::from(keys);
            },
            Doctype::Object => {
                let keys = d_entry.doc.object().unwrap().into_iter()
                    .map(|prop| From::from(prop.key()))
                    .collect::<Vec<D>>();
                d_entry.doc = From::from(keys);
            },
            _ => {
                let dt = d_entry.doc.doctype();
                d_entry.doc = D::null();
                d_entry.set_error(format!("cannot find chars for {:?}", dt));
            },
        }
        Some(d_entry.into())
    }
}


//fn builtin_has<D>(args: &mut Vec<Thunk>, doc: D)
//    -> Result<Output<D>> where D: Document
//{
//    assert_args_len(&args, 1)?;
//    let item = args.index_mut(0)(doc.clone())?.remove(0);
//    let dt = doc.doctype();
//    match dt {
//        Doctype::Array => {
//            let out = <D as ItemIterator<D>>::iter(&doc).unwrap()
//                .any(|value: &D| value == &item);
//            Ok(vec![From::from(out)])
//        },
//        Doctype::Object => {
//            let out = <D as ItemIterator<Property<D>>>::iter(&doc).unwrap()
//                .any(|x| {
//                    let key: D = From::from(x.key_ref().clone());
//                    key == item
//                });
//            Ok(vec![From::from(out)])
//        },
//        _ => Err(Error::Op(None, format!("{:?} not iterable", dt))),
//    }
//}
//
//fn builtin_indoc<D>(args: &mut Vec<Thunk>, item: D)
//    -> Result<Output<D>> where D: Document
//{
//    assert_args_len(&args, 1)?;
//    let doc = args.index_mut(0)(item.clone())?.remove(0);
//    let dt = doc.doctype();
//    match dt {
//        Doctype::Array => {
//            let out = <D as ItemIterator<D>>::iter(&doc).unwrap()
//                .any(|value: &D| value == &item);
//            Ok(vec![From::from(out)])
//        },
//        Doctype::Object => {
//            let out = <D as ItemIterator<Property<D>>>::iter(&doc).unwrap()
//                .any(|x| {
//                    let key: D = From::from(x.key_ref().clone());
//                    key == item
//                });
//            Ok(vec![From::from(out)])
//        },
//        _ => Err(Error::Op(None, format!("{:?} not iterable", dt))),
//    }
//}
//
//fn builtin_map<D>(args: &mut Vec<Thunk>, doc: D)
//    -> Result<Output<D>> where D: Document
//{
//    assert_args_len(&args, 1)?;
//    let thunk = args.index_mut(0);
//    let dt = doc.doctype();
//    match dt {
//        Doctype::String | Doctype::Array => {
//            let mut out = Vec::new();
//            for value in <D as ItemIterator<D>>::into_iter(doc).unwrap() {
//                out.push(thunk(value)?.remove(0));
//            }
//            Ok(vec![From::from(out)])
//        },
//        Doctype::Object => {
//            let mut out = Vec::new();
//            for x in <D as ItemIterator<Property<D>>>::into_iter(doc).unwrap() {
//                let key = x.key_ref().clone();
//                out.push(Property::new(key, thunk(x.value())?.remove(0)));
//            }
//            Ok(vec![From::from(out)])
//        },
//        _ => Err(Error::Op(None, format!("cannot map over {:?}", dt))),
//    }
//}
//
//fn builtin_any<D>(args: &mut Vec<Thunk>, doc: D)
//    -> Result<Output<D>> where D: Document
//{
//    assert_args_len(&args, 1)?;
//    let thunk = args.index_mut(0);
//    let dt = doc.doctype();
//    match dt {
//        Doctype::String | Doctype::Array => {
//            for value in <D as ItemIterator<D>>::into_iter(doc).unwrap() {
//                let out = thunk(value)?.remove(0);
//                if out.boolean().unwrap_or(false) {
//                    return Ok(vec![From::from(true)])
//                }
//            }
//            return Ok(vec![From::from(false)])
//        },
//        Doctype::Object => {
//            for x in <D as ItemIterator<Property<D>>>::into_iter(doc).unwrap() {
//                let out = thunk(x.value())?.remove(0);
//                if out.boolean().unwrap_or(false) {
//                    return Ok(vec![From::from(true)])
//                }
//            }
//            return Ok(vec![From::from(false)])
//        },
//        _ => Err(Error::Op(None, format!("cannot iterate over {:?}", dt))),
//    }
//}
//
//fn builtin_all<D>(args: &mut Vec<Thunk>, doc: D)
//    -> Result<Output<D>> where D: Document
//{
//    assert_args_len(&args, 1)?;
//    let thunk = args.index_mut(0);
//    let dt = doc.doctype();
//    match dt {
//        Doctype::String | Doctype::Array => {
//            for value in <D as ItemIterator<D>>::into_iter(doc).unwrap() {
//                let out = thunk(value)?.remove(0);
//                if out.boolean().unwrap_or(false) == false {
//                    return Ok(vec![From::from(false)])
//                }
//            }
//            return Ok(vec![From::from(true)])
//        },
//        Doctype::Object => {
//            for x in <D as ItemIterator<Property<D>>>::into_iter(doc).unwrap() {
//                let out = thunk(x.value())?.remove(0);
//                if out.boolean().unwrap_or(false) == false {
//                    return Ok(vec![From::from(false)])
//                }
//            }
//            return Ok(vec![From::from(true)])
//        },
//        _ => Err(Error::Op(None, format!("cannot iterate over {:?}", dt))),
//    }
//}
//
//fn assert_args_len(args: &Vec<Thunk>, n: usize) -> Result<()> {
//    if args.len() != n {
//        let err = format!("expected {} args, got {}", n, args.len());
//        return Err(Error::InvalidArg(err))
//    }
//    Ok(())
//}
//
//#[allow(dead_code)] // TODO: Can be removed.
//fn assert_iterator<D>(outs: &Output<D>) -> Result<()> where D: Document {
//    if outs.len() < 2 {
//        let err = format!("output is not an iterator {}", outs.len());
//        return Err(Error::InvalidArg(err))
//    }
//    Ok(())
//}
//
//#[allow(dead_code)]
//fn assert_singular<D>(outs: &Output<D>) -> Result<()> where D: Document {
//    if outs.len() == 1 {
//        let err = format!("output is an iterator {}", outs.len());
//        return Err(Error::InvalidArg(err))
//    }
//    Ok(())
//}

fn docvalues<D>(doc: D) -> Option<Vec<D>> where D: Document {
    let dt = doc.doctype();
    match dt {
        Doctype::Array => {
            let iter = <D as ItemIterator<D>>::into_iter(doc).unwrap();
            Some(iter.collect())
        },
        Doctype::Object => {
            let iter = <D as ItemIterator<Property<D>>>::into_iter(doc).unwrap();
            Some(iter.map(|x| x.value()).collect())
        },
        _ => None,
    }
}
