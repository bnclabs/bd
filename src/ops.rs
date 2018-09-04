use std::{vec, iter};

use entry::{self,Entry,IterPosition};
use db::{Document, Doctype, ItemIterator, Input, Pipeline, Repeater};
use prop::Property;


pub struct Identity<'a, D> where D: Document {
    input: Input<'a, D>,
}

impl<'a, D> Identity<'a, D> where D: Document {
    pub fn new(input: Input<D>) -> Identity<D> {
        Identity{input}
    }
}

impl<'a, D> Repeater<'a,D> for Identity<'a, D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        Box::new(Identity{input: self.input.repeat()})
    }
}

impl<'a, D> Iterator for Identity<'a, D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        Some(self.input.next()?)
    }
}

impl<'a, D> Pipeline<'a,D> for Identity<'a, D> where D: 'a + Document {
}


pub struct Recurse<'a, D> where D: Document {
    input: Input<'a,D>,
    iter: Option<vec::IntoIter<Entry<D>>>,
}

impl<'a,D> Recurse<'a,D> where D: Document {
    pub fn new(input: Input<D>) -> Recurse<D> {
        Recurse{input, iter: None}
    }

    fn values(&mut self) -> Option<Vec<Entry<D>>> {
        let d_entry = self.input.next()?;
        if d_entry.has_error() { return Some(vec![d_entry]) }

        let mut entries = Vec::new();
        for value in d_entry.doc.clone().recurse() {
            let mut entry = d_entry.clone();
            entry.doc = value;
            entries.push(entry)
        }
        Some(entry::fixpositions(entries))
    }

    fn next_value(&mut self) -> Option<Entry<D>> {
        let entry = loop {
            match self.iter.as_mut().unwrap().next() {
                Some(entry) => break entry,
                None => { self.iter = Some(self.values()?.into_iter()); },
            }
        };
        Some(entry)
    }
}

impl<'a,D> Repeater<'a,D> for Recurse<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        if let Some(_) = self.iter {
            panic!("cannot repeat Recurse after the statement is prepared");
        }
        let x = Recurse{ input: self.input.repeat(), iter: None };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Recurse<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        if self.iter.is_none() {
            self.iter = Some(self.values()?.into_iter())
        }
        self.next_value().map(|entry| entry)
    }
}

impl<'a,D> Pipeline<'a,D> for Recurse<'a,D> where D: 'a + Document {
}


pub struct Null<'a,D> where D: Document {
    input: Input<'a,D>,
}

impl<'a,D> Null<'a,D> where D: Document {
    pub fn new(input: Input<D>) -> Null<D> {
        Null{input}
    }
}

impl<'a,D> Repeater<'a,D> for Null<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Null{input: self.input.repeat()};
        Box::new(x)
    }
}

impl<'a,D> Iterator for Null<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        self.input.next()?; // ignore any meta information, errors from input.
        Some(Entry::new(D::null()))
    }
}

impl<'a,D> Pipeline<'a,D> for Null<'a,D> where D: 'a + Document {
}


pub struct Bool<'a,D> where D: Document {
    input: Input<'a,D>,
    value: D,
}

impl<'a,D> Bool<'a,D> where D: Document {
    pub fn new(input: Input<D>, value: bool) -> Bool<D> {
        Bool{input, value: From::from(value)}
    }
}

impl<'a,D> Repeater<'a,D> for Bool<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Bool{input: self.input.repeat(), value: self.value.clone()};
        Box::new(x)
    }
}

impl<'a,D> Iterator for Bool<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        self.input.next()?; // ignore any meta information, errors from input.
        Some(Entry::new(self.value.clone()))
    }
}

impl<'a,D> Pipeline<'a,D> for Bool<'a,D> where D: 'a + Document {
}


pub struct Integer<'a,D> where D: Document {
    input: Input<'a,D>,
    value: D,
}

impl<'a,D> Integer<'a,D> where D: Document {
    pub fn new(input: Input<D>, value: i128) -> Integer<D> {
        Integer{input, value: From::from(value)}
    }
}

impl<'a,D> Repeater<'a,D> for Integer<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Integer{input: self.input.repeat(), value: self.value.clone()};
        Box::new(x)
    }
}

impl<'a,D> Iterator for Integer<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        self.input.next()?; // ignore any meta information, errors from input.
        Some(Entry::new(self.value.clone()))
    }
}

impl<'a,D> Pipeline<'a,D> for Integer<'a,D> where D: 'a + Document {
}


pub struct Float<'a,D> where D: Document {
    input: Input<'a,D>,
    value: D,
}

impl<'a,D> Float<'a,D> where D: Document {
    pub fn new(input: Input<D>, value: f64) -> Float<D> {
        Float{input, value: From::from(value)}
    }
}

impl<'a,D> Repeater<'a,D> for Float<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Float{input: self.input.repeat(), value: self.value.clone()};
        Box::new(x)
    }
}

impl<'a,D> Iterator for Float<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        self.input.next()?; // ignore any meta information, errors from input.
        Some(Entry::new(self.value.clone()))
    }
}

impl<'a,D> Pipeline<'a,D> for Float<'a,D> where D: 'a + Document {
}


pub struct StringLit<'a,D> where D: Document {
    input: Input<'a,D>,
    value: D,
}

impl<'a,D> StringLit<'a,D> where D: Document {
    pub fn new(input: Input<D>, value: String) -> StringLit<D> {
        StringLit{input, value: From::from(value)}
    }
}

impl<'a,D> Repeater<'a,D> for StringLit<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = StringLit{
            input: self.input.repeat(), value: self.value.clone()
        };
        Box::new(x)
    }
}

impl<'a,D> Iterator for StringLit<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        self.input.next()?; // ignore any meta information, errors from input.
        Some(Entry::new(self.value.clone()))
    }
}

impl<'a,D> Pipeline<'a,D> for StringLit<'a,D> where D: 'a + Document {
}


pub struct Index<'a,D> where D: Document {
    input: Input<'a,D>,
    key: Option<String>,
    off: Option<isize>,
}

impl<'a,D> Index<'a,D> where D: Document {
    pub fn new(input: Input<D>, key: Option<String>, off: Option<isize>)
        -> Index<D>
    {
        Index{input, key, off}
    }

    fn do_get_shortcut(&self, doc: D) -> Result<D,String> {
        let (key, dt) = (self.key.as_ref().unwrap(), doc.doctype());
        if let Some(val) = doc.get(key) {
            Ok(val)
        } else {
            Err(format!("cannot index {} into {:?}", key, dt))
        }
    }

    fn do_index_shortcut(&self, doc: D) -> Result<D,String> {
        let (off, dt) = (self.off.unwrap(), doc.doctype());
        if let Some(val) = doc.index(off) {
            Ok(val)
        } else {
            Err(format!("cannot index {} into {:?}", off, dt))
        }
    }
}

impl<'a,D> Repeater<'a,D> for Index<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let (key, off) = (self.key.clone(), self.off.clone());
        let x = Index{input: self.input.repeat(), key, off};
        Box::new(x)
    }
}

impl<'a,D> Iterator for Index<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let mut d_entry = self.input.next()?;
        if d_entry.has_error() { return Some(d_entry) }

        let res = if let Some(_) = self.key {
            self.do_get_shortcut(d_entry.doc)

        } else if let Some(_) = self.off {
            self.do_index_shortcut(d_entry.doc)

        } else {
            unreachable!()
        };

        let d_entry = match res {
            Ok(doc) => {d_entry.doc = doc; d_entry},
            Err(s) => {d_entry.doc = D::null(); d_entry.set_error(s); d_entry},
        };
        Some(d_entry)
    }
}

impl<'a,D> Pipeline<'a,D> for Index<'a,D> where D: 'a + Document {
}


pub struct Identifier<'a,D> where D: Document {
    input: Input<'a,D>,
    symbol: String,
    index: Option<isize>,
}

impl<'a,D> Identifier<'a,D> where D: Document {
    pub fn new(input: Input<D>, symbol: String) -> Identifier<D> {
        let index = symbol.parse().ok();
        Identifier{input, symbol, index}
    }

    fn do_lookup(&self, doc: D) -> Result<D,String> {
        let dt = doc.doctype();
        if let Some(val) = doc.get_ref(&self.symbol) {
            return Ok(val.clone())
        } else if let Some(off) = self.index {
            if let Some(val) = doc.index_ref(off) { return Ok(val.clone()) }
        }
        Err(format!("cannot index {} into {:?}", self.symbol, dt))
    }
}

impl<'a,D> Repeater<'a,D> for Identifier<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let input = self.input.repeat();
        let (symbol, index) = (self.symbol.clone(), self.index.clone());
        let x = Identifier{input, symbol, index};
        Box::new(x)
    }
}

impl<'a,D> Iterator for Identifier<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let mut d_entry = self.input.next()?;
        if d_entry.has_error() { return Some(d_entry) }

        match self.do_lookup(d_entry.doc) {
            Ok(doc) => {d_entry.doc = doc},
            Err(s) => {d_entry.doc = D::null(); d_entry.set_error(s)},
        }
        Some(d_entry)
    }
}

impl<'a,D> Pipeline<'a,D> for Identifier<'a,D> where D: 'a + Document {
}


pub struct Slice<'a,D> where D: Document {
    input: Input<'a,D>,
    start: isize,
    end: isize,
}

impl<'a,D> Slice<'a,D> where D: Document {
    pub fn new(input: Input<D>, start: isize, end: isize) -> Slice<D> {
        Slice{input, start, end}
    }
}

impl<'a,D> Repeater<'a,D> for Slice<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let (start, end) = (self.start, self.end);
        let x = Slice{ input: self.input.repeat(), start, end };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Slice<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let mut d_entry = self.input.next()?;
        if d_entry.has_error() { return Some(d_entry) }

        let dt = d_entry.doc.doctype();
        let d_entry = match d_entry.doc.clone().slice(self.start, self.end) {
            Some(doc) => { d_entry.doc = doc; d_entry },
            None => {
                d_entry.set_error(format!("cannot slice {:?}", dt));
                d_entry
            },
        };
        Some(d_entry)
    }
}

impl<'a,D> Pipeline<'a,D> for Slice<'a,D> where D: 'a + Document {
}


pub struct IterValues<'a,D> where D: Document {
    input: Input<'a,D>,
    iter: Option<vec::IntoIter<Entry<D>>>,
}

impl<'a,D> IterValues<'a,D> where D: Document {
    pub fn new(input: Input<D>) -> IterValues<D> {
        IterValues{input, iter: None}
    }

    fn iter_values(&mut self) -> Option<Vec<Entry<D>>> {
        let mut d_entry = self.input.next()?;
        if d_entry.has_error() { return Some(vec![d_entry]) }

        let dt = d_entry.doc.doctype();
        let entries = match docvalues(d_entry.doc.clone()) {
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

impl<'a,D> Repeater<'a,D> for IterValues<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        if let Some(_) = self.iter {
            panic!("cannot repeat IterValues after the statement is prepared");
        }
        let x = IterValues{ input: self.input.repeat(), iter: None };
        Box::new(x)
    }
}

impl<'a,D> Iterator for IterValues<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        if self.iter.is_none() {
            let entries = self.iter_values()?;
            self.iter = Some(entries.into_iter());
        }

        let d_entry = loop {
            match self.iter.as_mut().unwrap().next() {
                Some(entry) => break entry,
                None => {
                    let entries = self.iter_values()?;
                    self.iter = Some(entries.into_iter());
                },
            }
        };
        Some(d_entry)
    }
}

impl<'a,D> Pipeline<'a,D> for IterValues<'a,D> where D: 'a + Document {
}


pub struct Iter<'a,D> where D: Document {
    inputs: Vec<Input<'a,D>>,
    done: bool,
    iter: Option<vec::IntoIter<Entry<D>>>,
}

impl<'a,D> Iter<'a,D> where D: Document {
    pub fn new(inputs: Vec<Input<D>>) -> Iter<D> {
        Iter{inputs, done: false, iter: None}
    }

    fn iter_entries(&mut self) -> Option<Vec<Entry<D>>> {
        let mut entries = Vec::new();
        let mut n = self.inputs.len();
        for input in self.inputs.iter_mut() {
            match input.next() {
                Some(entry) => entries.push(entry),
                None => { n -= 1; entries.push(Entry::new(D::null())) },
            }
        }
        if n > 0 { Some(entry::fixpositions(entries)) } else { None }
    }
}

impl<'a,D> Repeater<'a,D> for Iter<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        if let Some(_) = self.iter {
            panic!("cannot repeat Iter after the statement is prepared");
        }
        let (done, iter) = (false, None);
        let inputs = self.inputs.iter().map(|input| input.repeat()).collect();
        Box::new(Iter{ inputs, done, iter })
    }
}

impl<'a,D> Iterator for Iter<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        if self.done { return None }

        if self.iter.is_none() {
            let entries = self.iter_entries()?;
            self.iter = Some(entries.into_iter());
        }

        let d_entry = loop {
            match self.iter.as_mut().unwrap().next() {
                Some(entry) => break entry,
                None => {
                    let entries = self.iter_entries()?;
                    self.iter = Some(entries.into_iter());
                },
            }
        };
        Some(d_entry)
    }
}

impl<'a,D> Pipeline<'a,D> for Iter<'a,D> where D: 'a + Document {
}


pub struct List<'a,D> where D: Document {
    inputs: Vec<Input<'a,D>>,
}

impl<'a,D> List<'a,D> where D: Document {
    pub fn new(inputs: Vec<Input<D>>) -> List<D> {
        List{inputs}
    }

    fn next_entries(&mut self) -> Option<Vec<Entry<D>>> {
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

impl<'a,D> Repeater<'a,D> for List<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let inputs = self.inputs.iter().map(|input| input.repeat()).collect();
        Box::new(List{ inputs })
    }
}


impl<'a,D> Iterator for List<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let entries = self.next_entries()?;
        let values: Vec<D> = entries.iter().map(|e| e.doc.clone()).collect();
        let d_entry = Entry::new_merged(entries, From::from(values));
        Some(d_entry)
    }
}

impl<'a,D> Pipeline<'a,D> for List<'a,D> where D: 'a + Document {
}


pub struct Dict<'a,D> where D: Document {
    inputs: Vec<(Input<'a,D>, Input<'a,D>)>,
    iter: Option<vec::IntoIter<Entry<D>>>,
}

impl<'a,D> Dict<'a,D> where D: Document {
    pub fn new(inputs: Vec<(Input<'a,D>,Input<'a,D>)>) -> Dict<'a,D> {
        Dict{inputs, iter: None}
    }

    fn collect_prop(kinput: &mut Input<D>, vinput: &mut Input<D>)
        -> Option<Vec<(Entry<D>, Vec<Entry<D>>)>>
    {
        // gather keys
        let mut keys = Vec::new();
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
        let mut values = Vec::new();
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

    fn collect_props(&mut self) -> Option<Vec<(Entry<D>, Vec<Entry<D>>)>> {
        let mut props = Vec::new();
        let mut n = self.inputs.len();
        for (kinput, vinput) in self.inputs.iter_mut() {
            match Self::collect_prop(kinput, vinput) {
                Some(mut sub_props) => props.append(&mut sub_props),
                None => { n -= 1; },
            }
        }
        if n > 0 { Some(props) } else { None }
    }

    fn collect_dict(mut bp: Vec<(Entry<D>, Vec<Entry<D>>)>)
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
                dict.push((key.clone(), value.clone()))
            }
        }
        dict_lists.into_iter().flatten().collect()
    }

    fn next_props(&mut self) -> Option<Vec<Entry<D>>> {
        let bp = self.collect_props()?;
        let mut full = Vec::new();
        for dict in Self::collect_dict(bp).into_iter() {
            let mut entries: Vec<Entry<D>> = Vec::new();
            let mut keys: Vec<String> = Vec::new();
            let mut values: Vec<D> = Vec::new();
            for (kentry, ventry) in dict {
                entries.push(kentry.clone());
                entries.push(ventry.clone());
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

impl<'a,D> Repeater<'a,D> for Dict<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        if let Some(_) = self.iter {
            panic!("cannot repeat Dict after the statement is prepared");
        }
        let inputs = self.inputs.iter().map(|(ki,vi)| {
            (ki.repeat(), vi.repeat())
        }).collect();
        Box::new(Dict{ inputs, iter: None })
    }
}

impl<'a,D> Iterator for Dict<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        if let None = self.iter {
            self.iter = Some(self.next_props()?.into_iter());
        }

        let entry = loop {
            match self.iter.as_mut().unwrap().next() {
                Some(entry) => break entry,
                None => { self.iter = Some(self.next_props()?.into_iter()); },
            }
        };
        Some(entry)
    }

}

impl<'a,D> Pipeline<'a,D> for Dict<'a,D> where D: 'a + Document {
}


pub struct Neg<'a,D> where D: Document {
    input: Input<'a,D>,
}

impl<'a,D> Neg<'a,D> where D: Document {
    pub fn new(input: Input<D>) -> Neg<D> {
        Neg{input}
    }
}

impl<'a,D> Repeater<'a,D> for Neg<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Neg{input: self.input.repeat()};
        Box::new(x)
    }
}

impl<'a,D> Iterator for Neg<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let mut d_entry = self.input.next()?;
        let doc = d_entry.doc;
        d_entry.doc = -doc;
        Some(d_entry)
    }
}

impl<'a,D> Pipeline<'a,D> for Neg<'a,D> where D: 'a + Document {
}


pub struct Not<'a,D> where D: Document {
    input: Input<'a,D>,
}

impl<'a,D> Not<'a,D> where D: Document {
    pub fn new(input: Input<D>) -> Not<D> {
        Not{input}
    }
}

impl<'a,D> Repeater<'a,D> for Not<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Not{input: self.input.repeat()};
        Box::new(x)
    }
}

impl<'a,D> Iterator for Not<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let mut d_entry = self.input.next()?;
        let doc = d_entry.doc;
        d_entry.doc = !doc;
        Some(d_entry)
    }
}

impl<'a,D> Pipeline<'a,D> for Not<'a,D> where D: 'a + Document {
}


pub struct Mul<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Mul<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Mul<'a,D> {
        Mul{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Mul<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Mul{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Mul<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = d_lhs.doc.clone() * d_rhs.doc.clone();
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Mul<'a,D> where D: 'a + Document {
}


pub struct Div<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Div<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Div<'a,D> {
        Div{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Div<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Div{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Div<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = d_lhs.doc.clone() / d_rhs.doc.clone();
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Div<'a,D> where D: 'a + Document {
}


pub struct Rem<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Rem<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Rem<'a,D> {
        Rem{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Rem<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Rem{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Rem<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = d_lhs.doc.clone() % d_rhs.doc.clone();
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Rem<'a,D> where D: 'a + Document {
}


pub struct Add<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Add<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Add<'a,D> {
        Add{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Add<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Add{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Add<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = d_lhs.doc.clone() + d_rhs.doc.clone();
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Add<'a,D> where D: 'a + Document {
}


pub struct Sub<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Sub<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Sub<'a,D> {
        Sub{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Sub<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Sub{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Sub<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = d_lhs.doc.clone() - d_rhs.doc.clone();
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Sub<'a,D> where D: 'a + Document {
}


pub struct Shr<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Shr<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Shr<'a,D> {
        Shr{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Shr<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Shr{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Shr<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = d_lhs.doc.clone() >> d_rhs.doc.clone();
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Shr<'a,D> where D: 'a + Document {
}


pub struct Shl<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Shl<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Shl<'a,D> {
        Shl{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Shl<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Shl{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Shl<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = d_lhs.doc.clone() << d_rhs.doc.clone();
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Shl<'a,D> where D: 'a + Document {
}


pub struct Bitand<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Bitand<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Bitand<'a,D> {
        Bitand{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Bitand<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Bitand{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Bitand<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = d_lhs.doc.clone() & d_rhs.doc.clone();
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Bitand<'a,D> where D: 'a + Document {
}


pub struct Bitor<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Bitor<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Bitor<'a,D> {
        Bitor{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Bitor<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Bitor{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Bitor<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = d_lhs.doc.clone() | d_rhs.doc.clone();
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Bitor<'a,D> where D: 'a + Document {
}


pub struct Bitxor<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Bitxor<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Bitxor<'a,D> {
        Bitxor{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Bitxor<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Bitxor{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Bitxor<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = d_lhs.doc.clone() ^ d_rhs.doc.clone();
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Bitxor<'a,D> where D: 'a + 'a + Document {
}


pub struct Eq<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Eq<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Eq<'a,D> {
        Eq{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Eq<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Eq{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Eq<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = From::from(d_lhs.doc.clone() == d_rhs.doc.clone());
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Eq<'a,D> where D: 'a + Document {
}


pub struct Ne<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Ne<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Ne<'a,D> {
        Ne{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Ne<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Ne{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Ne<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = From::from(d_lhs.doc.clone() != d_rhs.doc.clone());
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Ne<'a,D> where D: 'a + Document {
}


pub struct Lt<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Lt<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Lt<'a,D> {
        Lt{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Lt<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Lt{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Lt<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = From::from(d_lhs.doc.clone() < d_rhs.doc.clone());
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Lt<'a,D> where D: 'a + Document {
}


pub struct Le<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Le<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Le<'a,D> {
        Le{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Le<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Le{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Le<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = From::from(d_lhs.doc.clone() >= d_rhs.doc.clone());
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Le<'a,D> where D: 'a + Document {
}


pub struct Gt<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Gt<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Gt<'a,D> {
        Gt{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Gt<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Gt{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Gt<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = From::from(d_lhs.doc.clone() > d_rhs.doc.clone());
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Gt<'a,D> where D: 'a + Document {
}


pub struct Ge<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Ge<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Ge<'a,D> {
        Ge{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Ge<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Ge{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Ge<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc = From::from(d_lhs.doc.clone() >= d_rhs.doc.clone());
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Ge<'a,D> where D: 'a + Document {
}


pub struct And<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> And<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> And<'a,D> {
        And{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for And<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = And{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for And<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc: D = From::from(d_lhs.doc.clone().and(d_rhs.doc.clone()));
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for And<'a,D> where D: 'a + Document {
}


pub struct Or<'a,D> where D: Document {
    lhs: Input<'a,D>,
    rhs: Input<'a,D>,
}

impl<'a,D> Or<'a,D> where D: Document {
    pub fn new(lhs: Input<'a,D>, rhs: Input<'a,D>) -> Or<'a,D> {
        Or{lhs, rhs}
    }
}

impl<'a,D> Repeater<'a,D> for Or<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let x = Or{ lhs: self.lhs.repeat(), rhs: self.rhs.repeat() };
        Box::new(x)
    }
}

impl<'a,D> Iterator for Or<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        let d_lhs = self.lhs.next()?;
        let d_rhs = self.rhs.next()?;
        let doc: D = From::from(d_lhs.doc.clone().or(d_rhs.doc.clone()));
        let entries = vec![d_lhs, d_rhs];
        Some(Entry::new_merged(entries, doc))
    }
}

impl<'a,D> Pipeline<'a,D> for Or<'a,D> where D: 'a + Document {
}


pub struct BuiltinLength<'a,D> where D: Document {
    arg_input: Option<Input<'a,D>>,
}

impl<'a,D> BuiltinLength<'a,D> where D: Document {
    pub fn new(mut args: Vec<Input<D>>) -> BuiltinLength<D> {
        if args.len() == 1 {
            BuiltinLength{arg_input: Some(args.remove(0))}
        } else {
            BuiltinLength{arg_input: None}
        }
    }
}

impl<'a,D> Repeater<'a,D> for BuiltinLength<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let arg_input = match self.arg_input.as_ref() {
            Some(input) => Some(input.repeat()),
            None => None
        };
        Box::new(BuiltinLength{arg_input})
    }
}

impl<'a,D> Iterator for BuiltinLength<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        if self.arg_input.is_none() {
            let mut entry = Entry::new(D::null());
            entry.set_error(format!("invalid number of args for length"));
            return Some(entry)
        }
        let mut d_entry = self.arg_input.as_mut().unwrap().next()?;
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
        Some(d_entry)
    }
}

impl<'a,D> Pipeline<'a,D> for BuiltinLength<'a,D> where D: 'a + Document {
}


pub struct BuiltinChars<'a,D> where D: Document {
    arg_input: Option<Input<'a,D>>,
}

impl<'a,D> BuiltinChars<'a,D> where D: Document {
    pub fn new(mut args: Vec<Input<D>>) -> BuiltinChars<D> {
        if args.len() == 1 {
            BuiltinChars{arg_input: Some(args.remove(0))}
        } else {
            BuiltinChars{arg_input: None}
        }
    }
}

impl<'a,D> Repeater<'a,D> for BuiltinChars<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let arg_input = match self.arg_input.as_ref() {
            Some(input) => Some(input.repeat()),
            None => None
        };
        Box::new(BuiltinChars{arg_input})
    }
}

impl<'a,D> Iterator for BuiltinChars<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        if self.arg_input.is_none() {
            let mut entry = Entry::new(D::null());
            entry.set_error(format!("invalid number of args for chars"));
            return Some(entry)
        }
        let mut d_entry = self.arg_input.as_mut().unwrap().next()?;
        let dt = d_entry.doc.doctype();
        match d_entry.doc.into_iter() {
            Some(iter) => {
                d_entry.doc = From::from(iter.collect::<Vec<D>>());
            },
            None => {
                d_entry.doc = D::null();
                d_entry.set_error(format!("cannot find chars for {:?}", dt));
            },
        }
        Some(d_entry)
    }
}

impl<'a,D> Pipeline<'a,D> for BuiltinChars<'a,D> where D: 'a + Document {
}


pub struct BuiltinKeys<'a,D> where D: Document {
    arg_input: Option<Input<'a,D>>,
}

impl<'a,D> BuiltinKeys<'a,D> where D: Document {
    pub fn new(mut args: Vec<Input<D>>) -> BuiltinKeys<D> {
        if args.len() == 1 {
            BuiltinKeys{arg_input: Some(args.remove(0))}
        } else {
            BuiltinKeys{arg_input: None}
        }
    }
}

impl<'a,D> Repeater<'a,D> for BuiltinKeys<'a,D> where D: 'a + Document {
    fn repeat(&self) -> Input<'a,D> {
        let arg_input = match self.arg_input.as_ref() {
            Some(input) => Some(input.repeat()),
            None => None
        };
        Box::new(BuiltinKeys{arg_input})
    }
}

impl<'a,D> Iterator for BuiltinKeys<'a,D> where D: Document {
    type Item=Entry<D>;

    fn next(&mut self) -> Option<Entry<D>> {
        if self.arg_input.is_none() {
            let mut entry = Entry::new(D::null());
            entry.set_error(format!("invalid number of args for keys"));
            return Some(entry)
        }
        let mut d_entry = self.arg_input.as_mut().unwrap().next()?;
        let dt = d_entry.doc.doctype();
        match dt {
            Doctype::Array => {
                let ln = d_entry.doc.len().unwrap();
                let keys = (0..ln).map(|i| From::from(i as i128));
                d_entry.doc = From::from(keys.collect::<Vec<D>>());
            },
            Doctype::Object => {
                let keys = d_entry.doc.object().unwrap().into_iter()
                    .map(|prop| From::from(prop.key()))
                    .collect::<Vec<D>>();
                d_entry.doc = From::from(keys);
            },
            _ => {
                d_entry.doc = D::null();
                d_entry.set_error(format!("cannot find chars for {:?}", dt));
            },
        }
        Some(d_entry)
    }
}

impl<'a,D> Pipeline<'a,D> for BuiltinKeys<'a,D> where D: 'a + Document {
}



//fn builtin_has<D>(args: &mut Vec<Thunk>, doc: D)
//    -> Result<Input<D>> where D: Document
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
//    -> Result<Input<D>> where D: Document
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
//    -> Result<Input<D>> where D: Document
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
//    -> Result<Input<D>> where D: Document
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
//    -> Result<Input<D>> where D: Document
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
//fn assert_iterator<D>(outs: &Input<D>) -> Result<()> where D: Document {
//    if outs.len() < 2 {
//        let err = format!("output is not an iterator {}", outs.len());
//        return Err(Error::InvalidArg(err))
//    }
//    Ok(())
//}
//
//#[allow(dead_code)]
//fn assert_singular<D>(outs: &Input<D>) -> Result<()> where D: Document {
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
