use std::ops::{Neg, Not, Mul, Div, Rem, Add, Sub};
use std::ops::{Shr, Shl, BitAnd, BitXor, BitOr};
use std::convert::{From};
use std::{result, fmt};
use std::cmp::Ordering;

use query;

pub enum Doctype {
    Null,
    Bool,
    Integer,
    Float,
    String,
    Array,
    Object,
}

// TODO: why should document be marked as Sized ?
pub trait Document :
    From<bool> + From<i128> + From<f64> + From<String> +
    From<Vec<Self>> + From<Vec<ArrayItem<Self>>> + From<Vec<Property<Self>>> +
    fmt::Debug + Default + Clone + Sized +
    Neg<Output=Self> + Not<Output=Self> +
    Mul<Output=Self> + Div<Output=Self> + Rem<Output=Self> +
    Add<Output=Self> + Sub<Output=Self> +
    Shr<Output=Self> + Shl<Output=Self> +
    BitAnd<Output=Self> + BitXor<Output=Self> + BitOr<Output=Self> +
    PartialEq + PartialOrd +
    And<Output=Self> + Or<Output=Self> +
    Recurse<Output=Vec<Self>> + Slice<Output=Option<Self>> {

    type Err: Into<query::Error> + fmt::Debug;

    fn null() -> Self;

    fn doctype(&self) -> Doctype;

    fn string(self) -> Option<String>;

    fn index(self, off: isize) -> result::Result<Self, Self::Err>;

    fn get<'a>(self, key: &'a str) -> result::Result<Self, Self::Err>;

    fn get_ref<'a>(&self, key: &'a str) -> result::Result<&Self, Self::Err>;

    fn values<'a>(self) -> Option<Box<Iterator<Item=Self>>>;

    fn len(self) -> Result<Self, Self::Err>;

    fn chars(self) -> Result<Self, Self::Err>;

    fn keys(self) -> Result<Self, Self::Err>;

    fn has(self, &Self) -> Result<Self, Self::Err>;
}

pub trait And<Rhs=Self> {
    type Output=Self;

    fn and(self, other: Rhs) -> Self::Output;
}

pub trait Or<Rhs=Self> {
    type Output=Self;

    fn or(self, other: Rhs) -> Self::Output;
}

pub trait Recurse : Sized {
    type Output=Vec<Self>;

    fn recurse(&self) -> Self::Output;
}

pub trait Slice : Sized {
    type Output=Option<Self>;

    fn slice(self, start: isize, end: isize) -> Self::Output;
}


#[derive(Debug,Clone)]
pub struct KeyValue<K,D>(K, D) where K: PartialEq + PartialOrd + Ord , D: Document;

pub type Property<D> = KeyValue<String,D>;

pub type ArrayItem<D> = KeyValue<i32,D>;

impl<K,D> Eq for KeyValue<K,D> where K: PartialEq + PartialOrd + Ord, D: Document {}

impl<K,D> PartialEq for KeyValue<K,D>
    where K: PartialEq + PartialOrd + Ord, D: Document
{
    fn eq(&self, other: &KeyValue<K,D>) -> bool {
        self.0 == other.0 // compare only the key.
    }
}

impl<K,D> PartialEq<str> for KeyValue<K,D>
    where K: PartialEq<str> + PartialOrd<str> + Ord, D: Document
{
    fn eq(&self, other: &str) -> bool {
        self.0.eq(other) // compare only the key.
    }
}

impl<K,D> PartialOrd for KeyValue<K,D>
    where K: PartialEq + PartialOrd + Ord, D: Document
{
    fn partial_cmp(&self, other: &KeyValue<K,D>) -> Option<Ordering> {
        self.0.partial_cmp(&other.key_ref()) // compare only the key.
    }
}

impl<K,D> PartialOrd<str> for KeyValue<K,D>
    where K: PartialEq<str> + PartialOrd<str> + Ord, D: Document
{
    fn partial_cmp(&self, other: &str) -> Option<Ordering> {
        self.0.partial_cmp(other) // compare only the key.
    }
}

impl<K,D> Ord for KeyValue<K,D>
    where K: PartialEq + PartialOrd + Ord, D: Document
{
    fn cmp(&self, other: &KeyValue<K,D>) -> Ordering {
        self.0.cmp(&other.0) // compare only the key.
    }
}

impl<K,D> KeyValue<K,D>
    where K: PartialEq + PartialOrd + Ord, D: Document
{
    #[inline]
    pub fn new(key: K, value: D) -> KeyValue<K,D> {
        KeyValue(key, value)
    }

    #[inline]
    pub fn key(self) -> K {
        self.0
    }

    #[inline]
    pub fn key_ref(&self) -> &K {
        &self.0
    }

    #[inline]
    pub fn value(self) -> D {
        self.1
    }

    #[inline]
    pub fn value_ref(&self) -> &D {
        &self.1
    }

    #[inline]
    pub fn value_mut(&mut self) -> &mut D {
        &mut self.1
    }

    #[inline]
    pub fn set_value(&mut self, value: D) {
        self.1 = value;
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
        let cmp = item.partial_cmp(key).unwrap();
        base = if cmp == Greater { base } else { mid };
        size -= half;
    }
    // base is always in [0, size) because base <= mid.
    let item: &str = obj[base].key_ref();
    let cmp = item.partial_cmp(key).unwrap();
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
