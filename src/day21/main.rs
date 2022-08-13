use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use log::debug;

use aoc::intcomp::{IntComp, OutputVec, Stopped};

/*
const INTO_HOLE_PROGRAM: &str = "\
NOT D J
WALK
";
*/

/*
((!A || !B || !C) && D)
*/

/*
const JUMP_PROGRAM1: &str = "\
NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
WALK
";
*/

/*
((!A || (!B && ) || !C) && D)
*/
const JUMP_PROGRAM2: &str = "\
NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
NOT E T
NOT T T
OR H T
AND T J
RUN
";

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 21")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day21.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let line: String = buf_reader
        .lines()
        .next()
        .ok_or_else(|| anyhow::format_err!("No line found"))??;

    let mut cp: IntComp = str::parse(&line)?;
    let input: Vec<i64> = JUMP_PROGRAM2.as_bytes().iter().map(|&n| n as i64).collect();

    let mut outputs = OutputVec::new();
    cp.run_to_input(&mut outputs)?.expect(Stopped::Input)?;
    let output_bytes: Vec<u8> = outputs.0.iter().map(|&n| n as u8).collect();
    let output_str = std::str::from_utf8(&output_bytes)?;
    println!("-> {}", output_str);
    outputs.0.clear();
    cp.process(input, &mut outputs)?.expect(Stopped::Halted)?;

    println!("Output length: {}", outputs.0.len());
    let output_vec: Vec<i64> = outputs.0.iter().copied().collect();
    println!("Output: {:?}", output_vec);

    let output_bytes: Vec<u8> = outputs
        .0
        .iter()
        .map(|&n| if n <= 255 { n as u8 } else { 255 })
        .collect();

    match std::str::from_utf8(&output_bytes) {
        Ok(output_str) => {
            println!("-> ...\n{}", output_str);
            return Ok(());
        }
        Err(e) => {
            let n = e.valid_up_to();
            let output_str = std::str::from_utf8(&output_bytes[..n])?;
            println!("-> {}", output_str);
            let back: i64 = *(outputs
                .0
                .back()
                .ok_or_else(|| anyhow::anyhow!("Expected non-empty output"))?);
            println!("-> {}", back);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_log::test;

    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_thing() -> anyhow::Result<()> {
        Ok(())
    }
}
