use std::ops::{Neg, Not, Mul, Div, Rem, Add, Sub};
use std::ops::{Shr, Shl, BitAnd, BitXor, BitOr};
use std::iter::{Iterator};
use std::convert::{From};
use std::result;

use json::Json;
use jq;

// TODO: why should document be marked as Sized ?
pub trait Document :
    Clone + Sized + From<bool> + From<Json> +
    Neg<Output=Self> + Not<Output=Self> +
    Mul<Output=Self> + Div<Output=Self> + Rem<Output=Self> +
    Add<Output=Self> + Sub<Output=Self> +
    Shr<Output=Self> + Shl<Output=Self> +
    BitAnd<Output=Self> + BitXor<Output=Self> + BitOr<Output=Self> +
    PartialEq + PartialOrd +
    And<Output=Self> + Or<Output=Self> +
    Recurse + Slice + Comprehension {

    type Err;

    fn string(self) -> jq::Result<String>;

    fn index(self, off: usize) -> result::Result<Self, Self::Err>;

    fn get<'a>(self, key: &'a str) -> jq::Result<Self>;
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
    fn recurse(&self) -> Vec<Self>;
}

pub trait Slice : Sized {
    fn slice(self, start: usize, end: usize) -> Option<Vec<Self>>;
}

pub trait Comprehension: Sized {
    type Output;

    fn map_comprehend(iter: impl Iterator<Item=(Vec<String>, Vec<Self>)>) -> Vec<Self>;
    fn list_comprehend(iter: impl Iterator<Item=Vec<Self>>) -> Vec<Self>;
}
