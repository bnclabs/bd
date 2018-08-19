use std::ops::{Neg, Not, Mul, Div, Rem, Add, Sub};
use std::ops::{Shr, Shl, BitAnd, BitXor, BitOr};
use std::convert::{From};
use std::{result, fmt};
use std::cmp::Ordering;

use json::Json;
use query;

// TODO: why should document be marked as Sized ?
pub trait Document :
    From<bool> + From<i128> + From<f64> + From<String> +
    From<Vec<Self>> + From<Vec<KeyValue<Self>>> +
    // TODO: should we remove From<Json>
    fmt::Debug + Default + Clone + Sized + From<Json> +
    Neg<Output=Self> + Not<Output=Self> +
    Mul<Output=Self> + Div<Output=Self> + Rem<Output=Self> +
    Add<Output=Self> + Sub<Output=Self> +
    Shr<Output=Self> + Shl<Output=Self> +
    BitAnd<Output=Self> + BitXor<Output=Self> + BitOr<Output=Self> +
    PartialEq + PartialOrd +
    And<Output=Self> + Or<Output=Self> +
    Recurse<Output=Vec<Self>> + Slice<Output=Option<Self>> {

    type Err: Into<query::Error> + fmt::Debug;

    fn string(self) -> result::Result<String, Self::Err>;

    fn index(self, off: isize) -> result::Result<Self, Self::Err>;

    fn get<'a>(self, key: &'a str) -> result::Result<Self, Self::Err>;

    fn get_ref<'a>(&self, key: &'a str) -> result::Result<&Self, Self::Err>;

    fn values<'a>(self) -> Option<Box<Iterator<Item=Self>>>;

    fn len(self) -> Result<Self, Self::Err>;

    fn chars(self) -> Result<Self, Self::Err>;
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
pub struct KeyValue<D>(String, D) where D: Document;

impl<D> Eq for KeyValue<D> where D: Document {}

impl<D> PartialEq for KeyValue<D> where D: Document {
    fn eq(&self, other: &KeyValue<D>) -> bool {
        self.0.eq(&other.0) // compare only the key.
    }
}

impl<D> PartialOrd for KeyValue<D> where D: Document {
    fn partial_cmp(&self, other: &KeyValue<D>) -> Option<Ordering> {
        Some(self.0.cmp(&other.0)) // compare only the key.
    }
}

impl<D> Ord for KeyValue<D> where D: Document {
    fn cmp(&self, other: &KeyValue<D>) -> Ordering {
        self.0.cmp(&other.0) // compare only the key.
    }
}

impl<D> From<String> for KeyValue<D> where D: Document {
    fn from(key: String) -> KeyValue<D> {
        KeyValue::new(key, D::default())
    }
}

impl<D> KeyValue<D> where D: Document {

    #[inline]
    pub fn new(key: String, value: D) -> KeyValue<D> {
        KeyValue(key, value)
    }

    #[inline]
    pub fn key(self) -> String {
        self.0
    }

    #[inline]
    pub fn key_ref(&self) -> &str {
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
