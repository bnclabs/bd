use std::cmp::Ordering;

use db::Document;


#[derive(Debug,Clone)]
pub struct KeyValue<K,D>(K, D) where K: Ord, D: Document;

impl<K,D> KeyValue<K,D> where K: Ord, D: Document {
    #[inline]
    pub fn new(key: K, value: D) -> KeyValue<K,D> {
        KeyValue(key, value)
    }

    #[inline]
    pub fn key(self) -> K {
        self.0
    }

    #[inline]
    pub fn key_ref(&self) -> &K {
        &self.0
    }

    #[inline]
    pub fn value(self) -> D {
        self.1
    }

    #[inline]
    pub fn value_ref(&self) -> &D {
        &self.1
    }

    #[inline]
    pub fn key_value(self) -> (K, D) {
        (self.0, self.1)
    }

    #[inline]
    pub fn value_mut(&mut self) -> &mut D {
        &mut self.1
    }

    #[inline]
    pub fn set_value(&mut self, value: D) {
        self.1 = value;
    }
}


pub type ArrayItem<D> = KeyValue<usize,D>;

impl<D> Eq for ArrayItem<D> where D: Document {}

impl<D> PartialEq for ArrayItem<D> where D: Document {
    fn eq(&self, other: &ArrayItem<D>) -> bool {
        self.1 == other.1 // compare the value
    }
}

impl<D> PartialOrd for ArrayItem<D> where D: Document {
    fn partial_cmp(&self, other: &ArrayItem<D>) -> Option<Ordering> {
        self.1.partial_cmp(&other.value_ref()) // compare the value
    }
}


pub type Property<D> = KeyValue<String,D>;

impl<D> Eq for Property<D> where D: Document {}

impl<D> PartialEq for Property<D> where D: Document {
    fn eq(&self, other: &Property<D>) -> bool {
        self.0 == other.0 // compare only the key.
    }
}

impl<D> PartialOrd for Property<D> where D: Document {
    fn partial_cmp(&self, other: &Property<D>) -> Option<Ordering> {
        self.0.partial_cmp(&other.key_ref()) // compare only the key.
    }
}


pub fn search_by_key<D>(obj: &Vec<KeyValue<String,D>>, key: &str)
    -> Result<usize,usize> where D: Document
{
    use std::cmp::Ordering::{Greater, Equal, Less};

    let mut size = obj.len();
    if size == 0 { return Err(0) }

    let mut base = 0_usize;
    while size > 1 {
        let half = size / 2;
        let mid = base + half;
        // mid is always in [0, size), that means mid is >= 0 and < size.
        // mid >= 0: by definition
        // mid < size: mid = size / 2 + size / 4 + size / 8 ...
        let item: &str = obj[mid].key_ref();
        let cmp = item.cmp(key);
        base = if cmp == Greater { base } else { mid };
        size -= half;
    }
    // base is always in [0, size) because base <= mid.
    let item: &str = obj[base].key_ref();
    let cmp = item.cmp(key);
    if cmp == Equal { Ok(base) } else { Err(base + (cmp == Less) as usize) }
}

#[allow(dead_code)]
pub fn upsert_object_key<D>(obj: &mut Vec<Property<D>>, kv: Property<D>)
    where D: Document
{
    match search_by_key(obj, kv.key_ref()) {
        Ok(off) => obj[off] = kv,
        Err(off) => obj.insert(off, kv),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyvalue() {
        use json::Json;

        let value: Json = From::from("value".to_string());
        let prop = KeyValue::new("key".to_string(), value.clone());
        assert_eq!(prop.key_ref(), "key");
        assert_eq!(prop.value_ref(), &value);

        let value: Json = From::from("value".to_string());
        let prop = KeyValue::new("key".to_string(), value);
        assert_eq!(prop.key(), "key".to_string());

        let value: Json = From::from("value".to_string());
        let prop = KeyValue::new("key".to_string(), value.clone());
        assert_eq!(prop.value(), value);

        let value: Json = From::from("value".to_string());
        let prop = KeyValue::new("key".to_string(), value.clone());
        assert_eq!(prop.key_value(), ("key".to_string(), value));

        let value: Json = From::from("value".to_string());
        let mut prop = KeyValue::new("key".to_string(), value);
        let value: Json = From::from("another_value".to_string());
        *prop.value_mut() = value.clone();
        assert_eq!(prop.value(), value);

        let value: Json = From::from("value".to_string());
        let mut prop = KeyValue::new("key".to_string(), value);
        let value: Json = From::from("yet_another_value".to_string());
        prop.set_value(value.clone());
        assert_eq!(prop.value(), value);
    }

    #[test]
    fn test_sort_kv_array() {
        use json::Json;

        let mut props = vec![
            ArrayItem::new(2, <Json as From<i128>>::from(3)),
            ArrayItem::new(1, <Json as From<i128>>::from(3)),
            ArrayItem::new(3, <Json as From<i128>>::from(3)),
            ArrayItem::new(0, <Json as From<i128>>::from(3)),
        ];
        {
            let x = props.as_mut_slice();
            x.sort_by_key(|item| item.key_ref().clone());
        }

        let mut refstr = "[KeyValue(0, 3), KeyValue(1, 3), ".to_string();
        refstr += "KeyValue(2, 3), KeyValue(3, 3)]";
        assert_eq!(refstr, format!("{:?}", props));
    }

    #[test]
    fn test_sort_kv_property() {
        use json::Json;

        let mut props = vec![
            Property::new("b".to_string(), <Json as From<i128>>::from(3)),
            Property::new("c".to_string(), <Json as From<i128>>::from(3)),
            Property::new("z".to_string(), <Json as From<i128>>::from(3)),
            Property::new("a".to_string(), <Json as From<i128>>::from(3)),
        ];
        {
            let x = props.as_mut_slice();
            x.sort_by_key(|item| item.key_ref().clone());
        }

        let mut refstr = r#"[KeyValue("a", 3), KeyValue("b", 3), "#.to_string();
        refstr += r#"KeyValue("c", 3), KeyValue("z", 3)]"#;
        assert_eq!(refstr, format!("{:?}", props));
    }
}
