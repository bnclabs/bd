use db::Document;
use prop::Property;
use entry::IterPosition;
use util;


#[derive(Debug,Clone,PartialEq,PartialOrd)] // TODO: revisit Debug
pub struct Op<D>(D) where D: Document;

pub const KEYS: [&'static str; 2] = [ "errors", "iterpos" ];

impl<D> Op<D> where D: Document {

    fn init(doc: &mut D) {
        doc.set("errors", <D as From<Vec<D>>>::from(Vec::new()));
        doc.set(
            "iterpos", <D as From<i128>>::from(From::from(IterPosition::Item))
        );
    }

    pub fn new() -> Op<D> {
        let mut doc = <D as From<Vec<Property<D>>>>::from(Vec::new());
        Op::init(&mut doc);
        Op(doc)
    }

    pub fn with(op: D) -> Op<D> {
        for key in KEYS.iter().filter(|key| !util::has_key(&op, key)) {
            panic!("{} key expected in op", key)
        }
        Op(op)
    }

    pub fn set(&mut self, key: &str, value: D) {
        self.0.set(key, value)
    }

    pub fn append(&mut self, key: &str, value: D) {
        use db::Doctype::{Array, Object, String as S};

        let d = self.0.get_mut(key).unwrap();
        let (x, y) = (value.doctype(), d.doctype());
        match (x, y) {
            (S, S) => d.append(value.string().unwrap()),
            (Array, Array) => d.append(value.array().unwrap()),
            (Object, Object) => d.append(value.object().unwrap()),
            _ => panic!("cannot append {:?} with {:?}", y, x),
        }
    }

    pub fn merge(&mut self, op: Op<D>) {
        self.append("errors", op.0.get("errors").unwrap());
    }

    pub fn get_ref(&self, key: &str) -> Option<&D> {
        self.0.get_ref(key)
    }

    pub fn into<T>(self) -> Op<T> where T: Document + From<D> {
        Op(From::from(self.0))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_op() {
        use json::{Json};
        use db::Value;

        let mut op: Op<Json> = Op::new();
        assert_eq!(op.get_ref("errors").unwrap().array_ref().unwrap().len(), 0);
        assert_eq!(op.get_ref("iterpos").unwrap().clone().integer().unwrap(), 1);
        let err: Vec<Json> = vec![From::from("my error".to_string())];
        op.set("errors", From::from(err));

        let mut o = Op::with(op.0.clone());
        assert_eq!(op, o);

        o.append("errors", Json::Array(vec![Json::null()]));
        op.merge(o);
        let mut r = r#"Op({"errors":["my error","my error",null],"#.to_string();
        r += r#""iterpos":1})"#;
        assert_eq!(r, format!("{:?}", op));
    }
}
