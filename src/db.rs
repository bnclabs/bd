use std::{result, fmt, slice, vec};
use std::ops::{Neg, Not, Mul, Div, Rem, Add, Sub};
use std::ops::{Shr, Shl, BitAnd, BitXor, BitOr};
use std::convert::{From};
use std::cmp::Ordering;

use query;
use json::Json;


pub trait DocIterator : Clone {
    type Item;

    fn next(&mut self, c: &mut Context) -> Option<Self::Item>;
}


pub trait Recurse : Sized {
    fn recurse(self) -> Vec<Self>;
}


#[derive(Debug,Clone,Copy)]
pub enum Doctype {
    Null,
    Bool,
    Integer,
    Float,
    String,
    Array,
    Object,
}


pub trait Document :
    From<bool> + From<i128> + From<f64> + From<String> +
    From<Vec<Json>> + From<Vec<Property<Json>>> +
    fmt::Debug + Default + Clone +
    PartialEq + PartialOrd +

    Neg<Output=Self> + Not<Output=Self> +
    Mul<Rhs=Json,Output=Self,Rhs=Json> + Div<Rhs=Json,Output=Self> +
    Rem<Rhs=Json,Output=Self> +
    Add<Rhs=Json,Output=Self> + Sub<Rhs=Json,Output=Self> +
    Shr<Rhs=Json,Output=Self> + Shl<Rhs=Json,Output=Self> +
    BitAnd<Rhs=Json,Output=Self> + BitXor<Rhs=Json,Output=Self> +
    BitOr<Rhs=Json,Output=Self> +

    Value + Slice +
    And<Rhs=Json,Output=Self> + Or<Rhs=Json,Output=Self> +
    Docindex<isize> +
    ItemIterator<Self> + ItemIterator<Property<Self>> +
    Append<String> + Append<Vec<Self>> + Append<Vec<Property<Self>>> +
{

    type Err: Into<query::Error> + fmt::Debug;

    fn doctype(&self) -> Doctype;

    fn len(self) -> Option<usize>;

    fn set(&mut self, value: D);
}

pub trait And<Rhs=Self> {
    type Output=Self;

    fn and(self, other: Rhs) -> Self::Output;
}

pub trait Or<Rhs=Self> {
    type Output=Self;

    fn or(self, other: Rhs) -> Self::Output;
}

pub trait Value {
    fn null() -> Self;

    fn boolean(self) -> Option<bool>;

    fn string(self) -> Option<String>;

    fn integer(self) -> Option<i128>;

    fn float(self) -> Option<f64>;

    fn array(self) -> Option<Vec<Self>>;

    fn object(self) -> Option<Vec<Property<Self>>>;
}

pub trait Slice : Sized {
    fn slice(self, start: isize, end: isize) -> Option<Self>;
}

pub trait Docindex<Idx> : Sized {
    fn index(self, i: Idx) -> Option<Self>;

    fn index_ref(&self, i: Idx) -> Option<&Self>;

    fn index_mut(&mut self, i: Idx) -> Option<&mut Self>;

    fn get<'a>(self, key: &'a str) -> Option<Self>;

    fn get_ref<'a>(&self, key: &'a str) -> Option<&Self>;
}

pub trait ItemIterator<T> {
    fn iter(&self) -> Option<slice::Iter<T>>;

    fn into_iter(self) -> Option<vec::IntoIter<T>>;
}

pub trait Append<T> { // T can be string, Vec<Document>, Vec<KeyValue>
    fn append(&mut self, value: T);
}
