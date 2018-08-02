Bigdata with Rust
-----------------

`db-rs` is a big-data package with following primary scope.

* [x] Interpret data in JSON format.
* [ ] Expression language to query JSON documents, similar to [jq][jq].
* [ ] Data collation algorithm for JSON.
* [ ] Patch algorithm based on [RFC6902][jsonpatch].
* [ ] LLRB memory optimized, self balancing, type parametrized, binary tree.
* [ ] MVCC variant for LLRB.
* [ ] Database optimized in-memory storage.
* [ ] Bottoms up b-tree for disk snapshots.
* [ ] Organize JSON documents into buckets of data.
* [ ] Index them for big-data operations.

**Secondary scope**

* Interpret and index data in other formats like CBOR, MsgPack etc.
* Explore query language that is agnostic to JSON.
* Data science algorithms on big-data.

[jq]: https://stedolan.github.io/jq/manual
[jsonpatch]: https://tools.ietf.org/html/rfc6902
