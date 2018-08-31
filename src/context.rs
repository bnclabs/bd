#[derive(Clone)]
pub struct Context<D> where D: Document {
    namespace: HashMap<String, Input<D>>
}

impl<D> Context<D> where D: Document {
    pub fn new() -> Context<D> {
        let namespace = HashMap::with_capacity(64);
        let mut c = Context{namespace};
        c
    }

    pub fn get(&self, name: &str) -> Option<Input<D>> {
        self.namespace.get(name).cloned()
    }

    pub fn get_mut(&self, name: &str) -> Option<&Input<D>> {
        self.namespace.get_mut(name)
    }

    pub fn set(&mut self, name: &str, value: Input<D>) {
        self.namespace.insert(name.to_string(), value);
    }
}
