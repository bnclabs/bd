use std::{fmt, slice, vec};
use std::ops::{Neg, Not, Mul, Div, Rem, Add, Sub};
use std::ops::{Shr, Shl, BitAnd, BitXor, BitOr};
use std::convert::{From};

use prop::Property;
use entry::Entry;


pub type Input<'a, D> = Box<dyn Pipeline<'a,D,Item=Entry<D>> + 'a>;

pub trait Pipeline<'a,D,Item=Entry<D>>: Iterator<Item=Entry<D>> + Repeater<'a,D>
    where D: 'a + Document
{
}

pub trait Repeater<'a, D> where D: 'a + Document, Self: 'a {
    fn repeat(&self) -> Input<'a,D>;
}

#[derive(Debug,Clone,Copy,PartialEq,PartialOrd)]
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
    From<Self> + From<Vec<Self>> + From<Vec<Property<Self>>> +
    fmt::Debug + Default + Clone +
    PartialEq + PartialOrd +

    Neg<Output=Self> + Not<Output=Self> +
    Mul<Self,Output=Self> + Div<Self,Output=Self> +
    Rem<Self,Output=Self> +
    Add<Self,Output=Self> + Sub<Self,Output=Self> +
    Shr<Self,Output=Self> + Shl<Self,Output=Self> +
    BitAnd<Self,Output=Self> + BitXor<Self,Output=Self> +
    BitOr<Self,Output=Self> +

    Recurse<item=Self> + Value<item=Self> + Slice<item=Self> +
    And<Self,Output=Self> + Or<Self,Output=Self> +
    Docindex<isize,item=Self> +
    ItemIterator<Self> + ItemIterator<Property<Self>> +
    Append<String> + Append<Vec<Self>> + Append<Vec<Property<Self>>> +
{
    fn doctype(&self) -> Doctype;

    fn len(&self) -> Option<usize>;

    fn set(&mut self, key: &str, value: &Self);
}

pub trait And<Rhs=Self> {
    type Output;

    fn and(self, other: Rhs) -> Self::Output;
}

pub trait Or<Rhs=Self> {
    type Output;

    fn or(self, other: Rhs) -> Self::Output;
}

pub trait Recurse {
    type item: Document;

    fn recurse(self) -> Vec<Self::item>;
}

pub trait Value {
    type item: Document;

    fn null() -> Self::item;

    fn boolean(self) -> Option<bool>;

    fn string(self) -> Option<String>;

    fn integer(self) -> Option<i128>;

    fn float(self) -> Option<f64>;

    fn array_ref(&self) -> Option<&Vec<Self::item>>;

    fn array(self) -> Option<Vec<Self::item>>;

    fn object_ref(&self) -> Option<&Vec<Property<Self::item>>>;

    fn object(self) -> Option<Vec<Property<Self::item>>>;
}

pub trait Slice {
    type item: Document;

    fn slice(self, start: isize, end: isize) -> Option<Self::item>;
}

pub trait Docindex<Idx> {
    type item: Document;

    fn index(self, i: Idx) -> Option<Self::item>;

    fn index_ref(&self, i: Idx) -> Option<&Self::item>;

    fn index_mut(&mut self, i: Idx) -> Option<&mut Self::item>;

    fn get<'a>(self, key: &'a str) -> Option<Self::item>;

    fn get_ref<'a>(&self, key: &'a str) -> Option<&Self::item>;

    fn get_mut<'a>(&mut self, key: &'a str) -> Option<&mut Self::item>;
}

pub trait ItemIterator<T> {
    fn iter(&self) -> Option<slice::Iter<T>>;

    fn into_iter(self) -> Option<vec::IntoIter<T>>;
}

pub trait Append<T> { // T can be string, Vec<Document>, Vec<KeyValue>
    fn append(&mut self, value: T);
}
