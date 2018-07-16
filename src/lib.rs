#![feature(test)]

extern crate test;

pub mod jsonpointer;
pub mod json;

#[cfg(test)]
mod tests {
    use test::Bencher;

    #[test]
    fn test_codepoint() {
        let s = "å".to_string();
        let mut chars = s.chars();
        assert_eq!('a', chars.next().unwrap());
        assert_eq!('\u{30a}', chars.next().unwrap());

        let mut s = String::new();
        s.push_str("\u{0061}");
        s.push_str("\u{030a}");
        assert_eq!("å", s);
    }

    #[bench]
    fn bench_string_copy_1_k(b: &mut Bencher) {
        let mut s = String::with_capacity(1000);
        let mut d = String::with_capacity(1000);
        let r = "helloworld";
        (1..100).for_each(|_| s.push_str(r));

        b.iter(|| {d.clone_from(&s); d.truncate(0)} );
    }

    #[bench]
    fn bench_string_copy_4_k(b: &mut Bencher) {
        let mut s = String::with_capacity(4000);
        let mut d = String::with_capacity(4000);
        let r = "helloworld";
        (1..400).for_each(|_| s.push_str(r));

        b.iter(|| {d.clone_from(&s); d.truncate(0)} );
    }

    #[bench]
    fn bench_string_copy_10_k(b: &mut Bencher) {
        let mut s = String::with_capacity(10000);
        let mut d = String::with_capacity(10000);
        let r = "helloworld";
        (1..1000).for_each(|_| s.push_str(r));

        b.iter(|| {d.clone_from(&s); d.truncate(0)} );
    }

    #[bench]
    fn bench_string_copy_1_m(b: &mut Bencher) {
        let mut s = String::with_capacity(1000000);
        let mut d = String::with_capacity(1000000);
        let r = "helloworld";
        (1..100000).for_each(|_| s.push_str(r));

        b.iter(|| {d.clone_from(&s); d.truncate(0)} );
    }
}
