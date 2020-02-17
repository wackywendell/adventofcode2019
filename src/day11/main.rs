use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use log::debug;

pub enum Direction {
    North,
    East,
    South,
    West,
}

pub struct Robot {
    position: (i64, i64),
    direction: Direction,
}

pub enum Color {
    White,
    Black,
}

pub struct Panels {
    paint: HashMap<(i64, i64), Color>,
}

fn main() -> Result<(), failure::Error> {
    env_logger::init();

    let matches = App::new("Day 11")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day11.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    for line in buf_reader.lines() {
        println!("{}", line?)
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    // use super::*;

    #[test]
    fn test_thing() -> Result<(), failure::Error> {
        Ok(())
    }
}
