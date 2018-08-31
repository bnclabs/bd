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

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        Some(From::from(self.input.next(c)?));
    }
}



pub struct OpRecurse<D,T> where D: Document, T: Document + From<D> {
    input: Input<D>,
    iter: Option<vec::IntoIter<Entry<D>>>,
    _data: PhantomData<T>
}

impl OpRecurse<D,T> {
    pub fn new(input: Input<D>, _: T) -> OpRecurse<D,T> {
        OpRecurse{input, iter: None, _data: PhantomData}
    }

    pub fn iter_values(&self) -> Option<Vec<Entry<D>>> {
        let d_entry = self.input.next(c)?;
        if d_entry.has_error() { return Some(vec![From::from(d_entry))] }

        let mut entries = Vec::new();
        for value in d_entry.doc.recurse() {
            let mut entry = d_entry.clone();
            entry.doc = value;
            entries.push(entry)
        }
        fixpositions(&mut entries)
        Some(entries)
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

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        if let None = self.iter {
            self.iter = self.iter_values()?.into_iter();
        }

        let entry = loop {
            match self.iter.unwrap().next() {
                Some(entry) => break entry;
                None = { self.iter = self.iter_values()?.into_iter(); }
            }
        }
        Some(From::from(entry));
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

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        self.input.next(c)?; // ignore any meta information, errors from input.
        Some(Entry::new(T::null()))
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

impl<D,T> Clone for OpBool<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpBool {
        let (input, value) = (self.input.clone(), self.value.clone());
        OpBool{input, value, _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpBool<D,T> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        self.input.next(c)?; // ignore any meta information, errors from input.
        Some(From::from(Entry::new(self.value.clone())))
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

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        self.input.next(c)?; // ignore any meta information, errors from input.
        Some(From::from(Entry::new(self.value.clone())))
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

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        self.input.next(c)?; // ignore any meta information, errors from input.
        Some(From::from(Entry::new(self.value.clone())))
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

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        self.input.next(c)?; // ignore any meta information, errors from input.
        Some(From::from(Entry::new(self.value.clone())))
    }
}



pub struct OpIndex<D,T> where D: Document, T: Document + From<D> {
    input: Input<D>,
    key: Option<String>,
    off: Option<isize>,
    _data: PhantomData<T>,
}

impl OpIndex<D,T> {
    pub fn new(input: Input<D>, key: Option<String>, off: Option<isize>, _: T)
        -> OpIndex<D,T>
    {
        OpIndex{input, key, off, _data: PhantomData}
    }

    fn do_get_shortcut<D>(&self, doc: D) -> Result<D,String> where D: Document {
        let key = &self.key.unwrap();
        if let Some(val) = doc.get(key) {
            Ok(val)
        } else {
            Err(format!("cannot index {} into {:?}", key, doc.doctype()))
        }
    }

    fn do_index_shortcut<D>(&self, doc: D) -> Result<D,String> where D: Document {
        let off = self.off.unwrap();
        if let Some(val) = doc.index(off) {
            Ok(val)
        } else {
            Err(format!("cannot index {} into {:?}", key, doc.doctype()))
        }
    }
}

impl<D,T> Clone for OpIndex<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpIndex {
        let (input, key) = (self.input.clone(), self.key.clone());
        OpIndex{input, key, self.off.clone(), _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpIndex<D,T> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let mut d_entry = self.input.next(c)?;
        if d_entry.has_error() { return Some(From::from(d_entry)) }

        let res = if let Some(key) = self.key {
            self.do_get_shortcut(d_entry.doc)

        } else if let Some(off) = self.off {
            self.do_index_shortcut(d_entry.doc)

        } else {
            unreachable!()
        }

        let d_entry = match res {
            Ok(doc) => { d_entry.doc = doc; d_entry },
            Err(s) => { d_entry.doc = D::null(); d_entry.set_error(s); d_entry }
        }
        Some(From::from(d_entry))
    }
}


pub struct OpIdentifier<D,T> where D: Document, T: Document + From<D> {
    input: Input<D>,
    symbol: String,
    index: Option<usize>,
    _data: PhantomData<T>
}

impl OpIdentifier<D,T> {
    pub fn new(input: Input<D>, symbol: String, _: T) -> OpIdentifier<D,T> {
        let index = symbol.parse().ok();
        OpIdentifier{input, symbol, index, _data: PhantomData}
    }

    fn do_lookup<D>(&self, doc: D) -> Result<D,String> where D: Document {
        if let Some(val) = doc.get(&self.symbol) {
            return Ok(val)
        } else if let Some(off) = self.index {
            if let Some(val) = doc.index(off) { return Ok(val) }
        }
        Err(format!("cannot index {} into {:?}", self.symbol, doc.doctype()))
    }
}

impl<D,T> Clone for OpIdentifier<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpIdentifier {
        let (input, symbol) = (self.input.clone(), self.symbol.clone());
        OpIdentifier{input, symbol, _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpIdentifier<D,T> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let mut d_entry = self.input.next(c)?;
        if d_entry.has_error() { return Some(From::from(d_entry)) }

        if let d_entry = if let Some(iter) = c.get_mut(&self.symbol) {
            iter.next(c)

        } else {
            match self.do_lookup(d_entry.doc) {
                Ok(doc) => {d_entry.doc = doc; d_entry},
                Err(s) => {
                    d_entry.doc = D::null(); d_entry.set_error(s); d_entry
                },
            }
        }
        Some(From::from(d_entry))
    }
}


pub struct OpSlice<D,T> where D: Document, T: Document + From<D> {
    input: Input<D>,
    start: usize,
    end: usize,
    _data: PhantomData<T>
}

impl OpSlice<D,T> {
    pub fn new(input: Input<D>, start: usize, end: usize, _: T) -> OpSlice<D,T> {
        OpSlice{input, start, end, _data: PhantomData}
    }
}

impl<D,T> Clone for OpSlice<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpSlice {
        let (input, start, end) = (self.input.clone(), self.start, self.end);
        OpSlice{input, start, end, _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpSlice<D,T> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let mut d_entry = self.input.next(c)?;
        if d_entry.has_error() { return Some(From::from(d_entry)) }

        let d_entry = match d_entry.doc.clone().slice(self.start, self.end) {
            Some(doc) => { d_entry.doc = doc; d_entry },
            None => {
                d_entry.set_error(format!("cannot slice {:?}", dt));
                d_entry
            }
        }
        Some(From::from(d_entry))
    }
}


pub struct OpItervalues<D,T> where D: Document, T: Document + From<D> {
    input: Input<D>,
    iter: Option<vec::IntoIter<Entry<D>>>,
    _data: PhantomData<T>
}

impl OpItervalues<D,T> {
    pub fn new(input: Input<D>, _: T) -> OpItervalues<D,T> {
        OpItervalues{input, iter: None, _data: PhantomData}
    }

    pub fn iter_values(&self, d_entry: &mut Entry<D>) -> Vec<Entry<D>> {
        if d_entry.has_error() { return None }

        dt = d_entry.doctype();
        let mut entries = match docvalues(d_entry.doc.clone()) {
            Some(values) => {
                values.into_iter()
                    .map(|doc| Entry::new_with_meta(d_entry.meta.clone(), doc)
                    .collect()
            }
            None => {
                d_entry.set_error(format!("cannot iterate {:?}", dt));
                vec![d_entry]
            }
        }
        fixpositions(&mut entries);
        entries
    }
}

impl<D,T> Clone for OpItervalues<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpItervalues {
        if self.iter.is_some() {
            panic!("cannot clone OpItervalues after starting the iteration");
        }
        OpItervalues{input: self.input.clone(), iter: None, _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpItervalues<D,T> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        if self.iter.is_none() {
            let mut d_entry = self.input.next(c)?;
            let entries = self.iter_values(&mut d_entry);
            if d_entry.has_error() { return Some(From::from(d_entry)) }
            self.iter = Some(entries.into_iter());
        }

        let d_entry = loop {
            match self.iter.unwrap().next() {
                Some(entry) => break entry;
                None => {
                    let mut d_entry = self.input.next(c)?;
                    let entries = self.iter_values(&mut d_entry);
                    if d_entry.has_error() { return Some(From::from(d_entry)) }
                    self.iter = Some(entries.into_iter());
                }
            }
        }
        Some(From::from(d_entry));
    }
}


pub struct OpIter<D,T> where D: Document, T: Document + From<D> {
    inputs: Vec<Input<D>>,
    done: bool,
    iter: Option<vec::IntoIter<Entry<D>>>,
    _data: PhantomData<T>
}

impl OpIter<D,T> {
    pub fn new(inputs: Vec<Input<D>>, _: T) -> OpIter<D,T> {
        OpIter{inputs, done: false, iter: None, _data: PhantomData}
    }

    pub fn iter_entries(&mut self, c: &mut Context<D>) -> Option<Vec<Entry<T>>> {
        let mut entries = Vec::new();
        let mut n = self.inputs.len();
        for input in self.inputs.iter() {
            match input.next(c) {
                Some(entry) => entries.push(From::from(entry)),
                None => { n -= 1; entries.push(Entry::new(T::null())) },
            }
        }
        fixpositions(&mut entries)
        if n > 0 { Some(entries) } else { None }
    }
}

impl<D,T> Clone for OpIter<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpIter {
        if self.iter.is_some() {
            panic!("cannot clone OpRecurse after starting the iteration");
        }
        let inputs = self.inputs.clone();
        OpIter{inputs, done: self.done, iter: None, _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpIter<D,T> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        if self.done { return None }

        if self.iter.is_none() {
            let entries = self.iter_entries(c)?
            self.iter = Some(entries.into_iter())
        }

        let t_entry = loop {
            match self.iter.unwrap().next() {
                Some(entry) => break entry;
                None => {
                    let entries = self.iter_entries(c)?
                    self.iter = Some(entries.into_iter());
                }
            }
        }
        Some(t_entry)
    }
}


pub struct OpList<D,T> where D: Document, T: Document + From<D> {
    inputs: Vec<Input<D>>,
    _data: PhantomData<T>
}

impl OpList<D,T> {
    pub fn new(inputs: Vec<Input<D>>, _: T) -> OpList<D,T> {
        OpList{inputs, _data: PhantomData}
    }

    pub fn next_values(&mut self, c: &mut Context<D>) -> Option<Vec<Entry<T>>> {
        let mut entries: Vec<Entry<T>> = Vec::new();
        let mut n = self.inputs.len();
        for input in self.inputs.iter_mut() {
            loop {
                match input.next(c) {
                    Some(entry) => match entry.iter_position() {
                        ITER_POSITION_NEXT => entries.push(From::from(entry)),
                        ITER_POSITION_ITEM | ITER_POSITION_END => {
                            entries.push(From::from(entry));
                            break
                        },
                    }
                    None => {
                        n -= 1;
                        entries.push(Entry::new(T::null()));
                        break
                    }
                }
            }
        }
        if n > 0 { Some(entries) } else { None }
    }
}

impl<D,T> Clone for OpList<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpList {
        OpList{inputs: self.inputs.clone(), _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpList<D,T> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let entries = self.next_values(c)?
        let values = entries.map(|e| e.doc.clone()).collect();
        Some(Entry::new_merged(entries, From::from(values));
    }
}


pub struct OpDict<D,T> where D: Document, T: Document + From<D> {
    inputs: Vec<(Input<D>, Input<D>)>,
    iter: Option<vec::IntoIter<Entry<D>>>,
    _data: PhantomData<T>
}

impl OpDict<D,T> {
    pub fn new(inputs: Vec<Input<D>>, _: T) -> OpDict<D,T> {
        OpDict{inputs, iter: None, _data: PhantomData}
    }

    fn collect_prop(
        &self, kinput: Input<D>, vinput: Intput<D>, c: &mut Context)
        -> Option<Vec<(Entry<D>, Vec<Entry<D>>)>>
    {
        // gather keys
        let keys = Vec::new();
        loop {
            match kinput.next(c) {
                Some(entry) => match entry.iter_position() {
                    ITER_POSITION_NEXT => keys.push(entry),
                    ITER_POSITION_ITEM | ITER_POSITION_END => {
                        keys.push(entry)
                        break
                    },
                },
                None => break;
            }
        }
        if keys.len() == 0 { return None }

        // gather values
        let values = Vec::new();
        loop {
            match vinput.next(c) {
                Some(entry) => match entry.iter_position() {
                    ITER_POSITION_NEXT => values.push(entry),
                    ITER_POSITION_ITEM | ITER_POSITION_END => {
                        values.push(entry)
                        break
                    },
                },
                None => break;
            }
        }
        if values.len() == 0 { None }

        // final
        let mut props = Vec::new();
        for key in keys.into_iter() {
            props.push((key, values.clone()))
        }
        Some(props)
    }

    fn collect_props(&self, c: &mut Context)
        -> Option<Vec<(Entry<D>, Vec<Entry<D>>)>>
    {
        let mut props = Vec::new();
        let mut n = self.inputs.len();
        for (kinput, vinput) in self.input.iter() {
            match self.collect_prop(kinput, vinput, c) {
                Some(sub_props) => props.append(sub_props),
                None => { n -= 1; },
            }
        }
        if n > 0 { Some(props) } else { None }
    }

    fn collect(bp: Vec<(Entry<D>, Vec<Entry<D>>)>
        -> Vec<Vec<(Entry<D>, Entry<D>)>>
    {
        let (key, values) = bp.remove();
        if bp.len() == 0 {
            return values.into_iter().map(|v| vec![(key.clone(), v)]).collect();
        }
        let mut dict_lists = iter::repeat(collect(bp)).take(values.len());
        for (i, value) in values.into_iter().enumerate() {
            for dict in dict_lists[i].iter_mut() {
                dict.insert((key.clone(), value))
            }
        }
        dict_lists.flatten().collect();
    }

    fn next_props(&self, c: &mut Context) -> Option<Vec<Entry<D>>> {
        let bp = self.collect_props(c)?;
        let full = Vec::new();
        for dict in self.collect()..into_iter() {
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
            let props = keys.into_iter().zip(values)
                .map(|(k,v)| Property::new(k,v)).collect();
            full.push(Entry::new_merged(entries, From::from(props)));
        }
        Some(full)
    }
}

impl<D,T> Clone for OpDict<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpDict {
        if self.iter.is_some() {
            panic!("cannot clone OpRecurse after starting the iteration");
        }
        OpDict{inputs: self.inputs.clone(), iter: None, _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpDict<D,T> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        if let None = self.iter {
            self.iter = self.next_props(c)?.into_iter();
        }

        let entry = loop {
            match self.iter().unwrap().next() {
                Some(entry) => break entry;
                None = { self.iter = self.next_props(c)?.into_iter(); }
            }
        }
        Some(From::from(entry))
    }

}


pub struct OpNeg<D,T> where D: Document, T: Document + From<D> {
    input: Input<D>
    _data: PhantomData<T>
}

impl OpNeg<D,T> where D: Document, T: Document + From<D> {
    pub fn new(input: Input<D>, _: T) -> OpNeg<D,T>{
        OpNeg{input, _data: PhantomData}
    }
}

impl<D,T> Clone for OpNeg<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpNeg {
        OpNeg{input: self.input.clone(), _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpNeg<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_entry = self.input.next(c)?;
        d_entry.doc = -d_entry.doc;
        Some(From::from(d_entry))
    }
}


pub struct OpNot<D,T> where D: Document, T: Document + From<D> {
    input: Input<D>
    _data: PhantomData<T>
}

impl OpNot<D,T> where D: Document, T: Document + From<D> {
    pub fn new(input: Input<D>, _: T) -> OpNot<D,T>{
        OpNot{input, _data: PhantomData}
    }
}

impl<D,T> Clone for OpNot<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpNot {
        OpNot{input: self.input.clone(), _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpNot<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_entry = self.input.next(c)?;
        d_entry.doc = -d_entry.doc;
        Some(From::from(d_entry))
    }
}


pub struct OpMult<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpMult<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpMult<D,T>{
        OpMult{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpMult<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpMult {
        OpMult{lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpMult<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc * d_rhs.doc;
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpDiv<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpDiv<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpDiv<D,T>{
        OpDiv{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpDiv<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpDiv {
        OpDiv{lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpDiv<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc / d_rhs.doc;
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpRem<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpRem<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpRem<D,T>{
        OpRem{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpRem<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpRem {
        OpRem{lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpRem<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc % d_rhs.doc;
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpAdd<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpAdd<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpAdd<D,T>{
        OpAdd{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpAdd<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpAdd {
        OpAdd{lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpAdd<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc + d_rhs.doc;
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpSub<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpSub<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpSub<D,T>{
        OpSub{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpSub<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpSub {
        OpSub{lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpSub<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc - d_rhs.doc;
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpShr<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpShr<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpShr<D,T>{
        OpShr{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpShr<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpShr {
        OpShr{lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpShr<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc >> d_rhs.doc;
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpShl<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpShl<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpShl<D,T>{
        OpShl{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpShl<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpShl {
        OpShl{lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData}
    }
}

impl<D,T> DocIterator for OpShl<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc << d_rhs.doc;
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpBitand<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpBitand<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpBitand<D,T>{
        OpBitand{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpBitand<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpBitand {
        OpBitand{
            lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T> DocIterator for OpBitand<D,T>
    where D: Document, T: Document + From<D>
{
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc & d_rhs.doc;
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpBitor<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpBitor<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpBitor<D,T>{
        OpBitor{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpBitor<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpBitor {
        OpBitor{
            lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T> DocIterator for OpBitor<D,T>
    where D: Document, T: Document + From<D>
{
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc | d_rhs.doc;
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpBitxor<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpBitxor<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpBitxor<D,T>{
        OpBitxor{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpBitxor<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpBitxor {
        OpBitxor{
            lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T> DocIterator for OpBitxor<D,T>
    where D: Document, T: Document + From<D>
{
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = d_lhs.doc ^ d_rhs.doc;
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpEq<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpEq<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpEq<D,T>{
        OpEq{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpEq<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpEq {
        OpEq{
            lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T> DocIterator for OpEq<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = From::from(d_lhs.doc == d_rhs.doc);
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpNe<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpNe<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpNe<D,T>{
        OpNe{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpNe<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpNe {
        OpNe{
            lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T> DocIterator for OpNe<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = From::from(d_lhs.doc != d_rhs.doc);
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpLt<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpLt<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpLt<D,T>{
        OpLt{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpLt<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpLt {
        OpLt{
            lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T> DocIterator for OpLt<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = From::from(d_lhs.doc < d_rhs.doc);
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpLe<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpLe<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpLe<D,T>{
        OpLe{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpLe<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpLe {
        OpLe{
            lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T> DocIterator for OpLe<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = From::from(d_lhs.doc >= d_rhs.doc);
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpGt<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpGt<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpGt<D,T>{
        OpGt{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpGt<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpGt {
        OpGt{
            lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T> DocIterator for OpGt<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = From::from(d_lhs.doc > d_rhs.doc);
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpGe<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpGe<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpGe<D,T>{
        OpGe{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpGe<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpGe {
        OpGe{
            lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T> DocIterator for OpGe<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc = From::from(d_lhs.doc >= d_rhs.doc);
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct OpAnd<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpAnd<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpAnd<D,T>{
        OpAnd{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpAnd<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpAnd {
        OpAnd{
            lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T> DocIterator for OpAnd<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc: D = From::from(d_lhs.doc.and(d_rhs.doc));
        Some(From::from(Entry::new_merged(entries, doc)));
    }
}


pub struct OpOr<D,T> where D: Document, T: Document + From<D> {
    lhs: Input<D>
    rhs: Input<D>
    _data: PhantomData<T>
}

impl OpOr<D,T> where D: Document, T: Document + From<D> {
    pub fn new(lhs: Input<D>, rhs: Input<D>, _: T) -> OpOr<D,T>{
        OpOr{lhs, rhs, _data: PhantomData}
    }
}

impl<D,T> Clone for OpOr<D,T> where D: Document, T: Document + From<D> {
    fn clone(&self) -> OpOr {
        OpOr{
            lhs: self.lhs.clone(), rsh: self.rhs.clone(), _data: PhantomData,
        }
    }
}

impl<D,T> DocIterator for OpOr<D,T> where D: Document, T: Document + From<D> {
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        let d_lhs = self.input.next(c)?;
        let d_rhs = self.input.next(c)?;
        let entries = vec![d_lhs, d_rhs];
        let doc: D = From::from(d_lhs.doc.or(d_rhs.doc));
        Some(From::from(Entry::new_merged(entries, doc)))
    }
}


pub struct BuiltinLength<D,T> where D: Document, T: Document + From<D> {
    arg: Option<Input<D>>
    _data: PhantomData<T>
}

impl BuiltinLength<D,T> where D: Document, T: Document + From<D> {
    pub fn new(args: Vec<Input<D>>, _: T) -> BuiltinLength<D,T>{
        if args.len() == 1 {
            BuiltinLength{arg: Option<args[0]>, _data: PhantomData}
        } else {
            BuiltinLength{arg: None, _data: PhantomData}
        }
    }
}

impl<D,T> Clone for BuiltinLength<D,T>
    where D: Document, T: Document + From<D>
{
    fn clone(&self) -> BuiltinLength {
        BuiltinLength{ arg: self.arg.clone(), _data: PhantomData }
    }
}

impl<D,T> DocIterator for BuiltinLength<D,T>
    where D: Document, T: Document + From<D>
{
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        if self.arg.is_none() {
            let entry = Entry::new(T::null());
            entry.set_error(format!("invalid number of args for length"))
            return entry
        }
        let d_entry = self.arg.unwrap().next(c)?;
        match d_entry.doc.len() {
            Some(n) => {
                d_entry.doc = From::from(n as i128);
            },
            None => {
                let dt = d_entry.doc.doctype();
                let d_entry.doc = D::null();
                d_entry.set_error(format!("cannot find length for {:?}", dt));
            }
        }
        Some(From::from(d_entry))
    }
}


pub struct BuiltinChars<D,T> where D: Document, T: Document + From<D> {
    arg: Option<Input<D>>
    _data: PhantomData<T>
}

impl BuiltinChars<D,T> where D: Document, T: Document + From<D> {
    pub fn new(args: Vec<Input<D>>, _: T) -> BuiltinChars<D,T>{
        if args.len() == 1 {
            BuiltinChars{arg: Option<args[0]>, _data: PhantomData}
        } else {
            BuiltinChars{arg: None, _data: PhantomData}
        }
    }
}

impl<D,T> Clone for BuiltinChars<D,T>
    where D: Document, T: Document + From<D>
{
    fn clone(&self) -> BuiltinChars {
        BuiltinChars{ arg: self.arg.clone(), _data: PhantomData }
    }
}

impl<D,T> DocIterator for BuiltinChars<D,T>
    where D: Document, T: Document + From<D>
{
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        if self.arg.is_none() {
            let entry = Entry::new(T::null());
            entry.set_error(format!("invalid number of args for chars"))
            return entry
        }
        let d_entry = self.arg.unwrap().next(c)?;
        match d_entry.doc.into_iter() {
            Some(iter) => {
                d_entry.doc = From::from(iter.collect::<Vec<D>>());
            },
            None => {
                let dt = d_entry.doc.doctype();
                let d_entry.doc = D::null();
                d_entry.set_error(format!("cannot find chars for {:?}", dt));
            }
        }
        Some(From::from(d_entry))
    }
}


pub struct BuiltinKeys<D,T> where D: Document, T: Document + From<D> {
    arg: Option<Input<D>>
    _data: PhantomData<T>
}

impl BuiltinKeys<D,T> where D: Document, T: Document + From<D> {
    pub fn new(args: Vec<Input<D>>, _: T) -> BuiltinKeys<D,T>{
        if args.len() == 1 {
            BuiltinKeys{arg: Option<args[0]>, _data: PhantomData}
        } else {
            BuiltinKeys{arg: None, _data: PhantomData}
        }
    }
}

impl<D,T> Clone for BuiltinKeys<D,T>
    where D: Document, T: Document + From<D>
{
    fn clone(&self) -> BuiltinKeys {
        BuiltinKeys{ arg: self.arg.clone(), _data: PhantomData }
    }
}

impl<D,T> DocIterator for BuiltinKeys<D,T>
    where D: Document, T: Document + From<D>
{
    type Item=Entry<T>;

    fn next(&mut self, c: &mut Context<D>) -> Option<Entry<T>> {
        if self.arg.is_none() {
            let entry = Entry::new(T::null());
            entry.set_error(format!("invalid number of args for keys"))
            return entry
        }
        let d_entry = self.arg.unwrap().next(c)?;
        match d_entry.doc.doctype() {
            Doctype::Array => {
                let keys = (0..d_entry.doc.len().unwrap())
                    .map(|i| From::from(i as i128))
                    .collect()
                d_entry.doc = From::from(keys);
            }
            Doctype::Object => {
                let keys = d_entry.doc.into_iter()
                    .map(|prop| From::from(prop.key()))
                    .collect::<Vec<Property<D>>()
                d_entry.doc = From::from(keys);
            }
            _ => {
                let dt = d_entry.doc.doctype();
                let d_entry.doc = D::null();
                d_entry.set_error(format!("cannot find chars for {:?}", dt));
            }
        }
        Some(From::from(d_entry))
    }
}


//fn builtin_has<D>(args: &mut Vec<Thunk>, doc: D, c: &mut Context<D>)
//    -> Result<Output<D>> where D: Document
//{
//    assert_args_len(&args, 1)?;
//    let item = args.index_mut(0)(doc.clone(), c)?.remove(0);
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
//fn builtin_indoc<D>(args: &mut Vec<Thunk>, item: D, c: &mut Context<D>)
//    -> Result<Output<D>> where D: Document
//{
//    assert_args_len(&args, 1)?;
//    let doc = args.index_mut(0)(item.clone(), c)?.remove(0);
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
//fn builtin_map<D>(args: &mut Vec<Thunk>, doc: D, c: &mut Context<D>)
//    -> Result<Output<D>> where D: Document
//{
//    assert_args_len(&args, 1)?;
//    let thunk = args.index_mut(0);
//    let dt = doc.doctype();
//    match dt {
//        Doctype::String | Doctype::Array => {
//            let mut out = Vec::new();
//            for value in <D as ItemIterator<D>>::into_iter(doc).unwrap() {
//                out.push(thunk(value, c)?.remove(0));
//            }
//            Ok(vec![From::from(out)])
//        },
//        Doctype::Object => {
//            let mut out = Vec::new();
//            for x in <D as ItemIterator<Property<D>>>::into_iter(doc).unwrap() {
//                let key = x.key_ref().clone();
//                out.push(Property::new(key, thunk(x.value(), c)?.remove(0)));
//            }
//            Ok(vec![From::from(out)])
//        },
//        _ => Err(Error::Op(None, format!("cannot map over {:?}", dt))),
//    }
//}
//
//fn builtin_any<D>(args: &mut Vec<Thunk>, doc: D, c: &mut Context<D>)
//    -> Result<Output<D>> where D: Document
//{
//    assert_args_len(&args, 1)?;
//    let thunk = args.index_mut(0);
//    let dt = doc.doctype();
//    match dt {
//        Doctype::String | Doctype::Array => {
//            for value in <D as ItemIterator<D>>::into_iter(doc).unwrap() {
//                let out = thunk(value, c)?.remove(0);
//                if out.boolean().unwrap_or(false) {
//                    return Ok(vec![From::from(true)])
//                }
//            }
//            return Ok(vec![From::from(false)])
//        },
//        Doctype::Object => {
//            for x in <D as ItemIterator<Property<D>>>::into_iter(doc).unwrap() {
//                let out = thunk(x.value(), c)?.remove(0);
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
//fn builtin_all<D>(args: &mut Vec<Thunk>, doc: D, c: &mut Context<D>)
//    -> Result<Output<D>> where D: Document
//{
//    assert_args_len(&args, 1)?;
//    let thunk = args.index_mut(0);
//    let dt = doc.doctype();
//    match dt {
//        Doctype::String | Doctype::Array => {
//            for value in <D as ItemIterator<D>>::into_iter(doc).unwrap() {
//                let out = thunk(value, c)?.remove(0);
//                if out.boolean().unwrap_or(false) == false {
//                    return Ok(vec![From::from(false)])
//                }
//            }
//            return Ok(vec![From::from(true)])
//        },
//        Doctype::Object => {
//            for x in <D as ItemIterator<Property<D>>>::into_iter(doc).unwrap() {
//                let out = thunk(x.value(), c)?.remove(0);
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
        _ => None
    }
}
