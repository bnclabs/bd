#[derive(Clone)]
pub struct Context<D> where D: Document {
    namespace: HashMap<String, Callable<D>>,
}

impl<D> Context<D> where D: Document {
    pub fn new() -> Context<D> {
        let namespace = HashMap::with_capacity(64);
        let mut c = Context{namespace};
        c.set_name("length", builtin_length);
        c.set_name("chars", builtin_chars);
        c.set_name("keys", builtin_keys);
        c.set_name("has", builtin_has);
        c.set_name("in", builtin_indoc);
        c.set_name("map", builtin_map);
        c.set_name("any", builtin_any);
        c.set_name("all", builtin_all);
        c
    }

    fn get_name(&self, name: &str) -> Option<Callable<D>> {
        self.namespace.get(name).cloned()
    }

    pub fn set_name(&mut self, name: &str, value: Callable<D>) {
        self.namespace.insert(name.to_string(), value);
    }

    pub fn eval(&mut self, thunk: &mut Thunk, doc: D)
        -> Result<Output<D>> where D: Document
    {
        thunk(doc, self)
    }
}
