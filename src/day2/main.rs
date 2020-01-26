use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use log::debug;

fn main() -> Result<(), failure::Error> {
    env_logger::init();

    let matches = App::new("Day 2")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day2.txt");

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
    // use test_env_log::test;

    // use super::*;
}
