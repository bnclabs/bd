enum Scope {
    Global,
    Doc,
    ListIterate,
    DictIterate,
    ListCollection,
    DictCollection,
}

struct Context {
    scope: Scope,
    opt: bool,
}
