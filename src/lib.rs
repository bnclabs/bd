#![feature(test)]

extern crate test;

pub mod jsonpointer;
pub mod json;

#[cfg(test)]
mod tests {
    use test::Bencher;

    #[bench]
    fn bench_string_copy_1K(b: &mut Bencher) {
        let mut s = String::with_capacity(1000);
        let mut d = String::with_capacity(1000);
        let r = "helloworld";
        (1..100).for_each(|_| s.push_str(r));

        b.iter(|| {d.clone_from(&s); d.truncate(0)} );
    }

    #[bench]
    fn bench_string_copy_4K(b: &mut Bencher) {
        let mut s = String::with_capacity(4000);
        let mut d = String::with_capacity(4000);
        let r = "helloworld";
        (1..400).for_each(|_| s.push_str(r));

        b.iter(|| {d.clone_from(&s); d.truncate(0)} );
    }

    #[bench]
    fn bench_string_copy_10K(b: &mut Bencher) {
        let mut s = String::with_capacity(10000);
        let mut d = String::with_capacity(10000);
        let r = "helloworld";
        (1..1000).for_each(|_| s.push_str(r));

        b.iter(|| {d.clone_from(&s); d.truncate(0)} );
    }

    #[bench]
    fn bench_string_copy_1M(b: &mut Bencher) {
        let mut s = String::with_capacity(1000000);
        let mut d = String::with_capacity(1000000);
        let r = "helloworld";
        (1..100000).for_each(|_| s.push_str(r));

        b.iter(|| {d.clone_from(&s); d.truncate(0)} );
    }
}
