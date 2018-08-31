use std::{result, fmt, slice, vec};
use std::ops::{Neg, Not, Mul, Div, Rem, Add, Sub};
use std::ops::{Shr, Shl, BitAnd, BitXor, BitOr};
use std::convert::{From};
use std::cmp::Ordering;


// TODO: is it possible to monomorphise this ?
pub type Input<D> where D: Document = Box<DocIterator<Item=Entry<D>>>


// TODO: is it possible to monomorphise this ?
pub type Output<T> where T: Document = Box<DocIterator<Item=Entry<T>>>


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


pub trait DocIterator : Clone {
    type Item;

    fn next(&mut self, c: &mut Context) -> Option<&mut Self::Item>;
}


pub trait Document :
    From<bool> + From<i128> + From<f64> + From<String> + From<&str> +
    From<Self> + From<Vec<Self>> + From<Vec<Property<Self>>> +
    fmt::Debug + Default + Clone +
    PartialEq + PartialOrd +

    Neg<Output=Self> + Not<Output=Self> +
    Mul<Rhs=Self,Output=Self,Rhs=Self> + Div<Rhs=Self,Output=Self> +
    Rem<Rhs=Self,Output=Self> +
    Add<Rhs=Self,Output=Self> + Sub<Rhs=Self,Output=Self> +
    Shr<Rhs=Self,Output=Self> + Shl<Rhs=Self,Output=Self> +
    BitAnd<Rhs=Self,Output=Self> + BitXor<Rhs=Self,Output=Self> +
    BitOr<Rhs=Self,Output=Self> +

    Recurse + Value + Slice +
    And<Rhs=Self,Output=Self> + Or<Rhs=Self,Output=Self> +
    Docindex<isize> +
    ItemIterator<Self> + ItemIterator<Property<Self>> +
    Append<&str> + Append<Vec<Self>> + Append<Vec<Property<Self>>> +
{
    fn doctype(&self) -> Doctype;

    fn len(self) -> Option<usize>;

    fn set(&mut self, value: D);
}

pub trait And<Rhs=Self> {
    fn and(self, other: Rhs) -> bool;
}

pub trait Or<Rhs=Self> {
    fn or(self, other: Rhs) -> bool;
}

pub trait Recurse : Sized {
    fn recurse(self) -> Vec<Self>;
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

    fn get_mut<'a>(&mut self, key: &'a str) -> Option<&mut Self>;
}

pub trait ItemIterator<T> {
    fn iter(&self) -> Option<slice::Iter<T>>;

    fn into_iter(self) -> Option<vec::IntoIter<T>>;
}

pub trait Append<T> { // T can be string, Vec<Document>, Vec<KeyValue>
    fn append(&mut self, value: T);
}
