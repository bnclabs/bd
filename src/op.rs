use db::Document;
use prop::Property;
use entry::IterPosition;
use util;


#[derive(Clone)]
pub struct Op<D>(D) where D: Document;


impl<D> Op<D> where D: Document {
    const KEYS: [&'static str; 2] = [ "errors", "iterpos" ];

    pub fn new() -> Op<D> {
        let mut op = <D as From<Vec<Property<D>>>>::from(Vec::new());
        let value = <D as From<Vec<D>>>::from(Vec::new());
        op.set("errors", &value);
        let value = <D as From<i128>>::from(From::from(IterPosition::Item));
        op.set("iterpos", &value);
        Op(op)
    }

    pub fn with(op: D) -> Op<D> {
        for key in Self::KEYS.iter().filter(|key| util::has_key(&op, key)) {
            panic!("{} key expected in op", key)
        }
        Op(op)
    }

    pub fn set(&mut self, key: &str, value: D) {
        self.0.set(key, &value)
    }

    pub fn append(&mut self, key: &str, value: &D) {
        use db::Doctype::{Array, Object, String as S};

        let d = self.0.get_mut(key).unwrap();
        let (x, y) = (value.doctype(), d.doctype());
        match (x, y) {
            (S, S) => d.append(value.string_ref().unwrap()),
            (Array, Array) => d.append(value.array_ref().unwrap()),
            (Object, Object) => d.append(value.object_ref().unwrap()),
            _ => panic!("cannot append {:?} with {:?}", y, x),
        }
    }

    pub fn merge(&mut self, op: &Op<D>) {
        self.append("errors", op.0.get_ref("errors").unwrap());
    }

    pub fn get_ref(&self, key: &str) -> Option<&D> {
        self.0.get_ref(key)
    }

    pub fn into<T>(self) -> Op<T> where T: Document + From<D> {
        Op(From::from(self.0))
    }
}
