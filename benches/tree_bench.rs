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

    c.bench_function("write_text_at_cursor_20x80", |b| {
        b.iter(|| {
            let mut grid = rterm::grid::Grid::new(20, 80);
            grid.max_scrollback_lines(5000);
            grid.write_text_at_cursor(black_box(&input));
        })
    });
    c.bench_function("write_text_at_cursor_200x80", |b| {
        b.iter(|| {
            let mut grid = rterm::grid::Grid::new(200, 80);
            grid.max_scrollback_lines(5000);
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
