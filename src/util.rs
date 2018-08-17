use std::result;

use document::{Document,KeyValue};

pub fn slice_range_check(start: isize, end: isize, len: isize)
    -> Option<(usize, usize)>
{
    let start = if start < 0 { start + len } else { start };
    if start < 0 || start >= len { return None }

    let end = if end < 0 {
        end + len
    } else if end == isize::max_value() {
        len
    } else {
        end
    };
    if end < 0 || end > len { return None }

    if start > end { return None }

    Some((start as usize, end as usize))
}

pub fn normalized_offset(off: isize, len: isize) -> Option<isize> {
    let off = if off < 0 { off + len } else { off };
    if off >= 0 && off < len { Some(off) } else { None }
}

pub fn search_by_key<D>(obj: &Vec<KeyValue<D>>, key: &str)
    -> result::Result<usize,usize> where D: Document
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
        base = if item.cmp(key) == Greater { base } else { mid };
        size -= half;
    }
    // base is always in [0, size) because base <= mid.
    let item: &str = obj[base].key_ref();
    let cmp = item.cmp(key);
    if cmp == Equal { Ok(base) } else { Err(base + (cmp == Less) as usize) }
}

pub fn upsert_object_key<D>(obj: &mut Vec<KeyValue<D>>, kv: KeyValue<D>)
    where D: Document
{
    match search_by_key(obj, kv.key_ref()) {
        Ok(off) => obj[off] = kv,
        Err(off) => obj.insert(off, kv),
    }
}
