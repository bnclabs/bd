use db::{Document};
use prop::Property;
use util;


#[derive(Clone)]
pub struct Meta<D>(Option<D>) where D: Document;


impl<D> Meta<D> where D: Document {
    const KEYS: [&'static str; 3] = [ "domains", "sources", "keys" ];

    pub fn new() -> Meta<D> {
        let value = <D as From<Vec<D>>>::from(Vec::new());
        let mut meta = <D as From<Vec<Property<D>>>>::from(Vec::new());
        meta.set("domains", &value);
        meta.set("sources", &value);
        meta.set("keys", &value);
        Meta(Some(meta))
    }

    pub fn some(m: D) -> Meta<D> {
        for key in Self::KEYS.iter().filter(|key| util::has_key(&m, key)) {
            panic!("{} key expected in meta", key)
        }
        Meta(Some(m))
    }

    pub fn none() -> Meta<D> {
        Meta(None)
    }

    pub fn set(&mut self, key: &str, value: D) {
        if self.0.is_none() { return }

        self.0.as_mut().unwrap().set(key, &value)
    }

    pub fn append(&mut self, key: &str, value: D) {
        use db::Doctype::{Array, Object, String as S};

        if self.0.is_none() { return }

        let d = self.0.as_mut().unwrap().get_mut(key).unwrap();
        let (dt1, dt2) = (value.doctype(), d.doctype());
        match (dt1, dt2) {
            (S, S) => d.append(value.string().unwrap()),
            (Array, Array) => d.append(value.array().unwrap()),
            (Object, Object) => d.append(value.object().unwrap()),
            _ => panic!("cannot append {:?} with {:?}", dt2, dt1),
        }
    }

    pub fn merge(&mut self, meta: &Meta<D>) {
        if self.0.is_none() { return }
        if meta.0.is_none() { return }

        let d = meta.0.as_ref().unwrap();
        self.append("domains", d.get_ref("domains").unwrap().clone());
        self.append("sources", d.get_ref("sources").unwrap().clone());
        self.append("keys", d.get_ref("keys").unwrap().clone());
    }

    pub fn get_ref(&self, key: &str) -> Option<&D> {
        if self.0.is_none() { return None }

        self.0.as_ref().unwrap().get_ref(key)
    }

    pub fn into<T>(self) -> Meta<T> where T: Document + From<D> {
        Meta(self.0.map(|doc| From::from(doc)))
    }
}

