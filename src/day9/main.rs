use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use log::debug;

use aoc::intcomp::IntComp;

fn main() -> Result<(), failure::Error> {
    env_logger::init();

    let matches = App::new("Day 9")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day9.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;

    let buf_reader = BufReader::new(file);
    let line = buf_reader.lines().next().unwrap()?;
    let orig_cp: IntComp = str::parse(&line)?;
    let mut cp = orig_cp.clone();
    cp.inputs.push_back(1);

    cp.run()?;

    println!(
        "Diagnostic Output ({} steps, {} size):",
        cp.stepped,
        cp.values.len()
    );
    for v in &cp.outputs {
        println!("  {}", v);
    }

    let mut cp = orig_cp;
    cp.inputs.push_back(2);

    cp.run()?;

    println!(
        "Run Output ({} steps, {} size):",
        cp.stepped,
        cp.values.len()
    );
    for v in &cp.outputs {
        println!("  {}", v);
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
