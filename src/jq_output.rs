#[derive(Debug,Clone,PartialEq)]
pub enum Output {
    One(Json),
    Many(Vec<Json>),
}

impl Output {
    pub fn is_one(&self) -> bool {
        if let Output::One(_) = self { true } else {false }
    }

    pub fn is_many(&self) -> bool {
        if let Output::Many(_) = self { true } else {false }
    }

    fn slice(self, start: usize, end: usize) -> Result<Output> {
        use jq::Output::{One, Many};
        match self {
            One(scalar) => Output::slice_one(scalar, start, end),
            Many(vector) => Output::slice_many(vector, start, end),
        }
    }

    fn slice_one(doc: Json, start: usize, end: usize) -> Result<Output> {
        match doc.slice(start, end) {
            Some(arr) => Ok(Output::One(arr)),
            None => Err(Error::Op(format!("cannot slice")))
        }
    }

    fn slice_many(docs: Vec<Json>, start: usize, end: usize)
        -> Result<Output>
    {
        let mut outs = Vec::new();

        for doc in docs {
            match doc.slice(start, end) {
                Some(arr) => outs.push(arr),
                None => return Err(Error::Op(format!("can't slice")))
            }
        }
        Ok(Output::Many(outs))
    }
}
