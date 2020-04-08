use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use log::debug;

use aoc::intcomp::{IntComp, OutputVec, Stopped};

fn main() -> anyhow::Result<()> {
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
    let mut out_vec = OutputVec::default();
    cp.process(vec![1], &mut out_vec)?.expect(Stopped::Halted)?;
    let OutputVec(outputs) = out_vec;

    println!(
        "Diagnostic Output ({} steps, {} size):",
        cp.stepped,
        cp.values.len()
    );
    for v in &outputs {
        println!("  {}", v);
    }

    let mut cp = orig_cp;
    let mut out_vec = OutputVec::default();
    cp.process(vec![2], &mut out_vec)?.expect(Stopped::Halted)?;
    let OutputVec(outputs) = out_vec;

    println!(
        "Run Output ({} steps, {} size):",
        cp.stepped,
        cp.values.len()
    );
    for v in &outputs {
        println!("  {}", v);
    }

    Ok(())
}
