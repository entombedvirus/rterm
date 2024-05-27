use criterion::{black_box, criterion_group, criterion_main, Criterion};
use pprof::criterion::{Output, PProfProfiler};
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

    c.bench_function("write_text_at_cursor", |b| {
        b.iter(|| {
            let mut grid = rterm::grid::Grid::new(20, 80);
            grid.write_text_at_cursor(black_box(&input));
        })
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    targets = criterion_benchmark
}
criterion_main!(benches);
