use std::str::FromStr;

use aoc::day18::Area;

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

const EXAMPLE1: &str = r#"
    #########
    #b.A.@.a#
    #########
"#;

const EXAMPLE2: &str = r#"
    ########################
    #f.D.E.e.C.b.A.@.a.B.c.#
    ######################.#
    #d.....................#
    ########################
"#;

const EXAMPLE3: &str = r#"
    ########################
    #...............b.C.D.f#
    #.######################
    #.....@.a.B.c.d.A.e.F.g#
    ########################
"#;

const EXAMPLE4: &str = r#"
    #################
    #i.G..c...e..H.p#
    ########.########
    #j.A..b...f..D.o#
    ########@########
    #k.E..a...g..B.n#
    ########.########
    #l.F..d...h..C.m#
    #################
"#;

pub fn criterion_benchmark(c: &mut Criterion) {
    let examples = [EXAMPLE1, EXAMPLE2, EXAMPLE3, EXAMPLE4];

    for (i, &example) in examples.iter().enumerate() {
        let area = Area::from_str(example).unwrap();
        let distances = area.distances();
        // let name = format!("example-{}", i + 1);

        c.bench_with_input(BenchmarkId::from_parameter(i + 1), &distances, |b, d| {
            let mut d = d.clone();
            b.iter(|| d.shortest().unwrap())
        });
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
