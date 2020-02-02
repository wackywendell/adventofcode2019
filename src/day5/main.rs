use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use log::debug;

use aoc::intcomp::IntComp;

fn main() -> Result<(), failure::Error> {
    env_logger::init();

    let matches = App::new("Day 5")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day5.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let line = buf_reader.lines().next().unwrap()?;
    let mut cp: IntComp = str::parse(&line)?;
    cp.inputs.push_back(1);

    while cp.step()? {
        while let Some(s) = cp.outputs.pop_front() {
            let pntr = cp
                .position
                .map(|n| n.to_string())
                .unwrap_or_else(|| "?".to_owned());

            if s == 0 {
                log::info!("{}: Passed", pntr);
            } else {
                println!("{}: Got diagnostic code {}", pntr, s);
            }
        }
    }

    println!("Halted. Outputs:");
    while let Some(s) = cp.outputs.pop_front() {
        println!("  {}", s);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    // use test_env_log::test;

    // use super::*;
}
