use db::{Document, Doctype};


pub fn slice_range_check(start: isize, end: isize, len: usize)
    -> Option<(usize, usize)>
{
    let len = len as isize;
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

pub fn normalized_offset(off: isize, len: usize) -> Option<usize> {
    let len = len as isize;
    let off = if off < 0 { off + len } else { off };
    if off >= 0 && off < len { Some(off as usize) } else { None }
}

pub fn has_key<D>(obj: &D, key: &str) -> bool where D: Document {
    if obj.doctype() != Doctype::Object { return false }

    for prop in obj.object_ref().unwrap().iter() {
        if prop.key_ref() == key { return true }
    }
    false
}
