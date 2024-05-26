use criterion::{criterion_group, criterion_main, Criterion};
use rand::Rng;

const ASCII_PRINTABLE: &[u8] =
    b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ \t `~!@#$%^&*()_+-=[]{}\\|;:'\",<.>/?";

pub fn criterion_benchmark(c: &mut Criterion) {
    let rng = rand::thread_rng();
    let dist = rand::distributions::Slice::new(ASCII_PRINTABLE).unwrap();

    let input: String = rng
        .sample_iter(&dist)
        .take(2048)
        .copied()
        .map(char::from)
        .collect();

    let mut grid = rterm::grid::Grid::new(20, 80);
    c.bench_function("write_text_at_cursor", |b| {
        b.iter(|| {
            grid.write_text_at_cursor(&input);
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
