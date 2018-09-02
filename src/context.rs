use std::collections::HashMap;

use db::Document;

#[derive(Clone)]
pub struct Context {
    namespace: HashMap<String, Box<Document>>,
}

impl Context {
    pub fn new() -> Context {
        let namespace = HashMap::with_capacity(64);
        Context{namespace}
    }

    pub fn get(&self, name: &str) -> Option<Box<Document>> {
        self.namespace.get(name).cloned()
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut D> {
        self.namespace.get_mut(name)
    }

    pub fn set(&mut self, name: &str, value: D) {
        self.namespace.insert(name.to_string(), value);
    }
}
