use db::{Document};
use prop::Property;
use util;


#[derive(Debug,Clone,PartialEq,PartialOrd)] // TODO: revisit Debug
pub struct Meta<D>(Option<D>) where D: Document;

pub const KEYS: [&'static str; 3] = [ "domains", "sources", "keys" ];

impl<D> Meta<D> where D: Document {

    fn init(doc: &mut D) {
        let value = <D as From<Vec<D>>>::from(Vec::new());
        doc.set("domains", &value);
        doc.set("sources", &value);
        doc.set("keys", &value);
    }

    pub fn new() -> Meta<D> {
        let mut doc = <D as From<Vec<Property<D>>>>::from(Vec::new());
        Meta::init(&mut doc);
        Meta(Some(doc))
    }

    pub fn some(m: D) -> Meta<D> {
        for key in KEYS.iter().filter(|key| !util::has_key(&m, key)) {
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

    pub fn append(&mut self, key: &str, value: &D) {
        use db::Doctype::{Array, Object, String as S};

        if self.0.is_none() { return }

        if self.0.as_ref().unwrap().get_ref(key).is_none() {
            self.0.as_mut().unwrap().set(key, value);
            return
        }

        let d = self.0.as_mut().unwrap().get_mut(key).unwrap();
        let (dt1, dt2) = (value.doctype(), d.doctype());
        match (dt1, dt2) {
            (S, S) => d.append(value.string_ref().unwrap()),
            (Array, Array) => d.append(value.array_ref().unwrap()),
            (Object, Object) => d.append(value.object_ref().unwrap()),
            _ => panic!("cannot append {:?} with {:?}", dt2, dt1),
        }
    }

    pub fn merge(&mut self, meta: &Meta<D>) {
        if meta.0.is_none() { return }

        if self.0.is_none() {
            self.0 = Some(<D as From<Vec<Property<D>>>>::from(Vec::new()));
        }

        let d = meta.0.as_ref().unwrap().object_ref().unwrap();
        for prop in d.iter() {
            self.append(prop.key_ref(), prop.value_ref());
        }
    }

    pub fn get_ref(&self, key: &str) -> Option<&D> {
        if self.0.is_none() { return None }

        self.0.as_ref().unwrap().get_ref(key)
    }

    pub fn into<T>(self) -> Meta<T> where T: Document + From<D> {
        Meta(self.0.map(|doc| From::from(doc)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_meta() {
        use json::{Json, Property};
        use db::Value;

        let mut meta: Meta<Json> = Meta::new();
        for key in KEYS.iter() {
            assert_eq!(meta.get_ref(key).unwrap().array_ref().unwrap().len(), 0);
        }

        assert_eq!(Meta::some(meta.0.as_ref().unwrap().clone()), meta);

        meta.0.take();
        assert_eq!(Meta::none(), meta);

        let mut m = Meta::new();
        m.set("key_string", Json::String("hello".to_string()));
        m.append("key_string", &Json::String(" world".to_string()));
        let value = m.get_ref("key_string").unwrap().clone();
        assert_eq!("hello world", value.string_ref().unwrap());

        m.set("key_array", Json::Array(vec![]));
        m.append("key_array", &Json::Array(vec![Json::null()]));
        let value = m.get_ref("key_array").unwrap().clone();
        assert_eq!(vec![Json::null()], value.array_ref().unwrap().clone());

        let prop = Property::new("key".to_string(), Json::null());
        m.set("key_object", Json::Object(Vec::new()));
        m.append("key_object", &Json::Object(vec![prop.clone()]));
        let value = m.get_ref("key_object").unwrap().clone();
        let refval: Json = From::from(vec![
            Property::new("key".to_string(), Json::null()),
        ]);
        assert_eq!(refval, value.clone());

        let mut meta: Meta<Json> = Meta::new();
        meta.merge(&m);
        let mut refval = r#"Meta(Some({"domains":[],"#.to_string();
        refval += r#""key_array":[null],"#;
        refval += r#""key_object":{"key":null},"key_string":"hello world","#;
        refval += r#""keys":[],"sources":[]}))"#;
        assert_eq!(refval, format!("{:?}", meta));
    }
}
