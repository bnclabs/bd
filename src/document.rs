use std::ops::{Neg, Not, Mul, Div, Rem, Add, Sub};
use std::ops::{Shr, Shl, BitAnd, BitXor, BitOr};
use std::convert::{From};
use std::{result};

use json::Json;
use query;

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

    type Err: Into<query::Error>;

    fn string(self) -> result::Result<String, Self::Err>;

    fn index(self, off: isize) -> result::Result<Self, Self::Err>;

    fn get<'a>(self, key: &'a str) -> result::Result<Self, Self::Err>;

    //fn iter_mut(&mut self) -> I: Iterator<Item=&mut Self>;
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
    fn slice(self, start: isize, end: isize) -> Option<Self>;
}

pub trait Comprehension: Sized {
    type Output=Self;

    fn map_comprehend(iter: impl Iterator<Item=(Vec<String>, Vec<Self>)>) -> Vec<Self>;
    fn list_comprehend(iter: impl Iterator<Item=Vec<Self>>) -> Vec<Self>;
}

pub(super) fn slice_range_check(start: isize, end: isize, len: isize)
    -> Option<(usize, usize)>
{
    let start = if start < 0 { start + len } else { start };
    if start < 0 || start >= len { return None }

    let end = if end < 0 {
        end + len
    } else if end == isize::max_value() {
        len
    } else {
        end
    };
    if end < 0 || end > len { return None }

    if start > end { return None }

    Some((start as usize, end as usize))
}
