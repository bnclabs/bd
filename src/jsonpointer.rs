pub const MAIN_SEPARATOR: char = '/';

pub fn escape(token: &str, out: &mut String) {
    out.truncate(0);
    for ch in token.chars() {
        match ch {
        '~' => { out.push('~'); out.push('0'); },
        '/' => { out.push('~'); out.push('1'); },
        _ => out.push(ch),
        }
    }
}

pub fn validate(ptr: &str) -> bool {
    let mut o = '?';
    for ch in ptr.chars() {
        if o == '~' {
            match ch { '0' | '1' => {o = '?';}, _ => return false, }
        } else if ch == '~' {
            o = '~';
        }
    }
    true
}

/// Return whether character is `json-pointer` separator.
pub fn is_separator(ch: char) -> bool {
    ch == MAIN_SEPARATOR
}

/// A owned, mutable json-pointer, similar to String. Implements `Deref`
/// to Pointer, which means all methods available on [`Pointer`] are
/// available on `PointerBuf` as well.
///
/// [`Pointer`]: struct.Pointer.html
#[derive(Clone)]
pub struct PointerBuf {
    inner: String,
}

impl PointerBuf {
    fn as_mut_vec(&mut self) -> &mut Vec<u8> {
        self.as_mut_vec()
    }

    pub fn new() -> PointerBuf {
        PointerBuf { inner: String::new() }
    }

    pub fn as_pointer(&self) -> &Pointer {
        self
    }

    pub fn push<P: AsRef<Pointer>>(&mut self, pointer: P) {
        self._push(pointer.as_ref())
    }

    fn _push(&mut self, ptr: &Pointer) {
        if ptr.is_absolute() {
            self.truncate(0);
            self.inner.push_str(ptr);

        } else {
            if self.as_mut_vec().last().map(|c| c == '/').unwrap_or(false) {
                self.inner.push(MAIN_SEPARATOR);
            }
            self.inner.push_str(ptr);
        }
    }

    pub fn pop(&mut self) -> bool {
        match self.parent().map(|p| p.as_u8_slice().len()) {
            Some(len) => {
                self.as_mut_vec().truncate(len);
                true
            }
            None => false,
        }
    }

    pub fn into_string(self) -> String {
        self.inner
    }

    pub fn into_boxed_pointer(self) -> Box<Pointer> {
        let rw = Box::into_raw(self.inner.into_boxed_str()) as *mut Pointer;
        unsafe { Box::from_raw(rw) }
    }
}

impl From<String> for PointerBuf {
    fn from(s: String) -> PointerBuf {
        PointerBuf { inner: s }
    }
}

impl From<Box<Pointer>> for PointerBuf {
    fn from(boxed: Box<Pointer>) -> PointerBuf {
        boxed.into_pointer_buf()
    }
}

impl<'a, T> From<&'a T> for PathBuf where T: ?Sized + AsRef<str> {
    fn from(s: &'a T) -> PathBuf {
        PathBuf::from(s.as_ref().to_os_string())
    }
}


pub struct Pointer {
    inner: str,
}

impl Pointer {
    pub fn new<S: AsRef<str> + ?Sized>() -> &Pointer {
        unsafe { &*(s.as_ref() as *const str as *const Pointer) }
    }

    pub fn as_str(&self) -> &str {
        &self.inner
    }

    pub fn to_pointer_buf(&self) -> PointerBuf {
        PathBuf::from(self.inner.to_string());
    }
}

impl AsRef<Pointer> for str {
    fn as_ref(&self) -> &Pointer {
        panic!("to be implemented")
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum Component<'a> {
    Root,
    Raw(String),
    Value(String),
}

impl<'a> Component<'a> {
    pub fn as_str(&self) -> &str {
        match self {
            Component::Root => MAIN_SEPARATOR_STR,
            Component::Normal(ptr) => ptr,
        }
    }
}

impl<'a> AsRef<str> for Component<'a> {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl<'a> AsRef<Pointer> for Component<'a> {
    fn as_ref(&self) -> &Pointer {
        panic!("to be implemented")
    }
}

#[cfg(test)]
mod test {
    use super::{MAIN_SEPARATOR, MAIN_SEPARATOR_STR};
    use super::{escape, validate, is_separator};

    #[test]
    fn test_constants() {
        assert_eq!(MAIN_SEPARATOR, '/');
        assert_eq!(MAIN_SEPARATOR_STR, "/");
    }

    #[test]
    fn test_is_separator() {
        assert_eq!(is_separator('/'), true);
        assert_eq!(is_separator('~'), false);
        assert_eq!(is_separator('\\'), false);
    }

    #[test]
    fn test_escape_validate() {
        let mut out = String::new();

        escape("hello", &mut out);
        assert_eq!(out, "hello");
        assert_eq!(validate(&out), true);

        escape("h~ello", &mut out);
        assert_eq!(out, "h~0ello");
        assert_eq!(validate(&out), true);

        escape("h/ello", &mut out);
        assert_eq!(out, "h~1ello");
        assert_eq!(validate(&out), true);


        assert_eq!(validate("he~llo"), false);
        assert_eq!(validate("he~3llo"), false);
        assert_eq!(validate("he~~llo"), false);
    }
}
