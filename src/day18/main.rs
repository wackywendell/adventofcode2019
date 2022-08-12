use clap::{App, Arg};
use log::debug;

use aoc::day18::Area;

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 18")
        .arg(
            Arg::with_name("input")
                .short('i')
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day18.txt");

    debug!("Using input {}", input_path);
    // let file = File::open(input_path)?;
    // let buf_reader = BufReader::new(file);

    // let line = buf_reader
    //     .lines()
    //     .next()
    //     .ok_or_else(|| anyhow::format_err!("Expected a line in file"))??;

    let area: Area = std::fs::read_to_string(input_path)?.parse()?;

    // println!("Found Area: {}", area);

    let shortest = area.distances().shortest().unwrap();

    println!("-- Part One --");
    println!("Took path {}", shortest);

    println!("-- Part Two --");
    let mut split_area = area;
    split_area.split_entrance();
    println!("Area:\n{}", split_area.map);
    let mut distances = split_area.distances();
    let pairs: usize = distances.distances.values().map(|m| m.len()).sum();
    println!(
        "Got {} distance pairs for {} points",
        pairs,
        distances.distances.len()
    );
    let shortest = distances.shortest_subs().unwrap();
    println!("Took path {}", shortest);

    Ok(())
}
