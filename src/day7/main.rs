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

    pub fn feedback(&mut self) -> Result<Vec<Value>, failure::Error> {
        let mut signal = Some(0);
        let mut outputs = Vec::with_capacity(self.comps.len());
        loop {
            let mut ran: Vec<bool> = self.comps.iter().map(|_| false).collect();
            for (i, c) in self.comps.iter_mut().enumerate() {
                if let Some(s) = signal {
                    c.inputs.push_back(s);
                }
                signal = c.run_to_output()?;
                if let Some(v) = signal {
                    ran[i] = true;
                    outputs.push(v);
                }
            }

            let halted: usize = ran.iter().map(|&r| if r { 0 } else { 1 }).sum();
            if halted == ran.len() {
                break;
            } else if halted == 0 {
                continue;
            }
            return Err(failure::err_msg(format!(
                "Expected all machines to halt at the same time. Saw {:?}",
                ran
            )));
        }

        Ok(outputs)
    }
}

// Returns (sequence, outputs)
pub fn max_output(
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

// Returns (sequence, outputs)
pub fn max_feedback(instructions: Vec<Value>) -> Result<(Vec<Value>, Vec<Value>), failure::Error> {
    let mut max_found = None;

    let seq0 = Vec::from_iter(5..=9);

    let perms = seq0.iter().copied().permutations(seq0.len());

    for p in perms {
        let mut amp = Amplifiers::new(p.clone(), instructions.clone());
        let outputs = amp.feedback()?;
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

    let line: String = buf_reader
        .lines()
        .next()
        .ok_or_else(|| failure::err_msg("No line found"))??;
    let ints: Vec<i64> = line
        .trim()
        .split(',')
        .map(str::parse::<i64>)
        .collect::<Result<Vec<i64>, _>>()?;

    let (seq, outputs) = max_output(5, ints.clone())?;

    println!("Used sequence {:?} with outputs {:?}", seq, outputs);

    let (seq, outputs) = max_feedback(ints)?;

    println!(
        "Feedback used sequence {:?} with outputs {:?}",
        seq, outputs
    );

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

    #[test]
    fn test_feedback() -> Result<(), failure::Error> {
        let instructions = vec![
            3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1,
            28, 1005, 28, 6, 99, 0, 0, 5,
        ];
        let sequence = vec![9, 8, 7, 6, 5];

        let mut amp = Amplifiers::new(sequence, instructions);
        let output = amp.feedback()?;
        assert_eq!(output.last(), Some(&139_629_729));

        let instructions = vec![
            3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54,
            -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4,
            53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10,
        ];
        let sequence = vec![9, 7, 8, 5, 6];

        let mut amp = Amplifiers::new(sequence, instructions);
        let output = amp.feedback()?;
        assert_eq!(output.last(), Some(&18216));

        Ok(())
    }

    #[test]
    fn test_max_feedback() -> Result<(), failure::Error> {
        let instructions = vec![
            3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1,
            28, 1005, 28, 6, 99, 0, 0, 5,
        ];
        let expected_sequence = vec![9, 8, 7, 6, 5];

        let (seq, output) = max_feedback(instructions)?;
        assert_eq!(seq, expected_sequence);
        assert_eq!(output.last(), Some(&139_629_729));

        let instructions = vec![
            3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54,
            -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4,
            53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10,
        ];
        let expected_sequence = vec![9, 7, 8, 5, 6];

        let (seq, output) = max_feedback(instructions)?;
        assert_eq!(seq, expected_sequence);
        assert_eq!(output.last(), Some(&18216));

        Ok(())
    }
}
