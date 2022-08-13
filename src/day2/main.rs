use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use log::debug;

use aoc::intcomp::{IntComp, Stopped, Value};

fn main() -> anyhow::Result<()> {
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

    let line = buf_reader.lines().next().unwrap()?;
    let orig_cp: IntComp = str::parse(&line)?;
    let mut cp = orig_cp.clone();
    cp.values[1] = 12;
    cp.values[2] = 2;

    cp.run_to_io()?.expect(Stopped::Halted)?;

    println!("At position 0: {}", cp.values[0]);
    let mut found = None;
    for ix1 in 0..cp.values.len() {
        for ix2 in 0..cp.values.len() {
            let mut cp = orig_cp.clone();
            cp.values[1] = ix1 as Value;
            cp.values[2] = ix2 as Value;
            cp.run_to_io()?.expect(Stopped::Halted)?;

            if cp.values[0] == 19_690_720 {
                found = Some((ix1, ix2));
                break;
            }
        }
    }

    match found {
        Some((ix1, ix2)) => println!("Found values {}, {} -> {}", ix1, ix2, ix1 * 100 + ix2),
        None => println!("No luck."),
    }

    Ok(())
}
