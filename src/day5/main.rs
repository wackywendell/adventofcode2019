use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use log::debug;

use aoc::intcomp::{IntComp, Stopped, UnexpectedState};

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
    let orig_cp: IntComp = str::parse(&line)?;

    println!("Diagnostic test: 1");
    let mut cp = orig_cp.clone();
    cp.run_to_io()?.expect(Stopped::Input)?;
    cp.process_input(1)?;

    loop {
        match cp.run_to_io()? {
            Stopped::Input => {
                return Err(UnexpectedState::new(Stopped::Output, Stopped::Input).into())
            }
            Stopped::Halted => break,
            Stopped::Output => {
                let s = cp.consume_output().unwrap();
                let pntr = cp.position;
                if s == 0 {
                    log::info!("{}: Passed", pntr);
                } else {
                    println!("{}: Got diagnostic code {}", pntr, s);
                }
            }
        }
    }

    println!("Diagnostic test: 5");
    let mut cp = orig_cp;
    cp.run_to_io()?.expect(Stopped::Input)?;
    cp.process_input(5)?;
    loop {
        match cp.run_to_io()? {
            Stopped::Input => {
                return Err(UnexpectedState::new(Stopped::Output, Stopped::Input).into())
            }
            Stopped::Halted => break,
            Stopped::Output => {
                let s = cp.consume_output().unwrap();
                println!("  Output: {}", s);
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    // use test_env_log::test;

    // use super::*;
}
