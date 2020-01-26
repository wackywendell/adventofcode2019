use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use log::{debug, info};

use aoc::parse::parse_err_iter;

pub fn cost(mass: i64) -> i64 {
    (mass / 3) - 2
}

pub fn iterative_cost(mass: i64) -> i64 {
    debug!("Starting iterative cost calculation: {}", mass);
    let mut current = mass;
    let mut sum = 0;
    loop {
        let c = cost(current);
        debug!("  Current: {}, Cost: {}", current, c);
        if c <= 0 {
            break;
        }
        sum += c;
        debug!("    sum: {}", sum);
        current = c;
    }
    sum
}

fn main() -> Result<(), failure::Error> {
    env_logger::init();

    let matches = App::new("Day 1")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day1.txt");

    info!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let parsed: Vec<i64> = parse_err_iter(buf_reader.lines())?;
    let sum: i64 = parsed.iter().map(|&n| cost(n)).sum();

    println!("Total cost: {}", sum);

    let sum: i64 = parsed.iter().map(|&n| iterative_cost(n)).sum();
    println!("Total iterative cost: {}", sum);

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    use super::*;

    #[test]
    fn test_cost() {
        assert_eq!(cost(12), 2);
        assert_eq!(cost(14), 2);
        assert_eq!(cost(1969), 654);
        assert_eq!(cost(100756), 33583);
    }

    #[test]
    fn test_iterative_cost() {
        assert_eq!(iterative_cost(14), 2);
        assert_eq!(iterative_cost(1969), 966);
        assert_eq!(iterative_cost(100756), 50346);
    }
}
