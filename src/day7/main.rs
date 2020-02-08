use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::iter::FromIterator;

use clap::{App, Arg};
use itertools::Itertools;
use log::debug;

use aoc::intcomp::{IntComp, Value};

pub struct Amplifiers {
    _inputs: Vec<Value>,
    comps: Vec<IntComp>,
}

impl Amplifiers {
    pub fn new(inputs: Vec<Value>, instructions: Vec<Value>) -> Amplifiers {
        let mut comps = Vec::with_capacity(inputs.len());
        for &i in &inputs {
            let mut c = IntComp::new(instructions.clone());
            c.inputs.push_back(i);
            comps.push(c);
        }

        Amplifiers {
            _inputs: inputs,
            comps,
        }
    }

    pub fn run(&mut self) -> Result<Vec<Value>, failure::Error> {
        let mut signal = 0;
        let mut outputs = Vec::with_capacity(self.comps.len());
        for c in self.comps.iter_mut() {
            c.inputs.push_back(signal);
            c.run()?;
            if c.outputs.len() != 1 {
                return Err(failure::err_msg(format!(
                    "Expected 1 output, got {}",
                    c.outputs.len()
                )));
            }
            signal = c.outputs.pop_front().unwrap();
            outputs.push(signal);
        }

        Ok(outputs)
    }
}

// Returns (sequence, outputs)
fn max_output(
    inputs: usize,
    instructions: Vec<Value>,
) -> Result<(Vec<Value>, Vec<Value>), failure::Error> {
    let mut max_found = None;

    let seq0 = Vec::from_iter(0..(inputs as Value));

    let perms = seq0.iter().copied().permutations(seq0.len());

    for p in perms {
        let mut amp = Amplifiers::new(p.clone(), instructions.clone());
        let outputs = amp.run()?;
        let output = *outputs.last().unwrap();

        let last = match max_found {
            None => {
                max_found = Some((p, outputs));
                continue;
            }
            Some((_, ref l)) => *l.last().unwrap(),
        };

        if last < output {
            max_found = Some((p, outputs));
        }
    }

    Ok(max_found.unwrap())
}

fn main() -> Result<(), failure::Error> {
    env_logger::init();

    let matches = App::new("Day 7")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day7.txt");

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

    use super::*;

    #[test]
    fn test_amplifiers_run() -> Result<(), failure::Error> {
        let instructions = vec![
            3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0,
        ];
        let sequence = vec![4, 3, 2, 1, 0];

        let mut amp = Amplifiers::new(sequence, instructions);
        let output = amp.run()?;
        assert_eq!(output.last(), Some(&43210));

        let instructions = vec![
            3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23,
            99, 0, 0,
        ];
        let sequence = vec![0, 1, 2, 3, 4];

        let mut amp = Amplifiers::new(sequence, instructions);
        let output = amp.run()?;
        assert_eq!(output.last(), Some(&54321));

        let instructions = vec![
            3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1,
            33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0,
        ];
        let sequence = vec![1, 0, 4, 3, 2];

        let mut amp = Amplifiers::new(sequence, instructions);
        let output = amp.run()?;
        assert_eq!(output.last(), Some(&65210));

        Ok(())
    }

    #[test]
    fn test_amplifiers_max() -> Result<(), failure::Error> {
        let instructions = vec![
            3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0,
        ];
        let sequence = vec![4, 3, 2, 1, 0];

        let (seq, output) = max_output(5, instructions)?;
        assert_eq!(output.last(), Some(&43210));
        assert_eq!(seq, sequence);

        let instructions = vec![
            3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23,
            99, 0, 0,
        ];
        let sequence = vec![0, 1, 2, 3, 4];

        let (seq, output) = max_output(5, instructions)?;
        assert_eq!(output.last(), Some(&54321));
        assert_eq!(seq, sequence);

        let instructions = vec![
            3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1,
            33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0,
        ];
        let sequence = vec![1, 0, 4, 3, 2];

        let (seq, output) = max_output(5, instructions)?;
        assert_eq!(output.last(), Some(&65210));
        assert_eq!(seq, sequence);

        Ok(())
    }
}
