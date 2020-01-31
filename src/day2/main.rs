use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::num::ParseIntError;
use std::str::FromStr;

use clap::{App, Arg};
use log::{debug, info};

type Value = i64;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum Code {
    Add,
    Multiply,
    Halt,
}

impl Code {
    fn registers_needed(self) -> usize {
        match self {
            Code::Add => 3,
            Code::Multiply => 3,
            Code::Halt => 0,
        }
    }
}

impl fmt::Display for Code {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Code::Add => "Add",
            Code::Multiply => "Mul",
            Code::Halt => "Halt",
        };

        f.write_str(s)
    }
}

struct CodeFindError {
    value: Value,
}

impl fmt::Display for CodeFindError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error converting {} to value", self.value)
    }
}

impl fmt::Debug for CodeFindError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error converting {} to value", self.value)
    }
}

impl TryFrom<Value> for Code {
    type Error = CodeFindError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Code::Add),
            2 => Ok(Code::Multiply),
            99 => Ok(Code::Halt),
            v => Err(CodeFindError { value: v }),
        }
    }
}

impl Error for CodeFindError {}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Intcodes {
    position: Option<usize>,
    values: Vec<Value>,
}

impl Intcodes {
    pub fn new(values: Vec<Value>) -> Intcodes {
        Intcodes {
            position: Some(0),
            values,
        }
    }

    fn get(&self, value: Value) -> Result<Value, failure::Error> {
        if self.values.len() <= value as usize {
            return Err(failure::err_msg(format!(
                "Position {} >= length {}",
                value,
                self.values.len()
            )));
        }

        Ok(self.values[value as usize])
    }

    fn get_mut(&mut self, value: Value) -> Result<&mut Value, failure::Error> {
        if self.values.len() <= value as usize {
            return Err(failure::err_msg(format!(
                "Position {} >= length {}",
                value,
                self.values.len()
            )));
        }

        Ok(&mut self.values[value as usize])
    }

    // Return value is 'keeps going'
    pub fn step(&mut self) -> Result<bool, failure::Error> {
        let pos = match self.position {
            None => return Ok(false),
            Some(p) => p,
        };

        let &val = match self.values.get(pos as usize) {
            None => {
                return Err(failure::err_msg(format!(
                    "No value. Position {} >= length {}",
                    pos,
                    self.values.len()
                )))
            }
            Some(v) => v,
        };

        let code = Code::try_from(val)?;
        let needed = code.registers_needed();

        if pos + needed >= self.values.len() {
            return Err(failure::err_msg(format!(
                "Found Code {} at Position {}, needed {} more, but length {} too short",
                code,
                pos,
                needed,
                self.values.len()
            )));
        }

        let registers = Vec::from(&(self.values[pos + 1..=pos + needed]));

        match code {
            Code::Halt => {
                self.position = None;
                return Ok(false);
            }
            Code::Add => {
                info!(
                    "{:>3}: {:>4} {:>3}   {:>3} -> {:3}",
                    pos, code, registers[0], registers[1], registers[2]
                );
                let r1 = self.get(registers[0])?;
                let r2 = self.get(registers[1])?;
                let out = self.get_mut(registers[2])?;
                let prev = *out;
                *out = r1 + r2;
                info!(
                    "         {:>3} + {:>3} -> {:>3} (prev: {})",
                    r1, r2, *out, prev
                );
            }
            Code::Multiply => {
                info!(
                    "{:>3}: {:>4} {:>3}   {:>3} -> {:>3}",
                    pos, code, registers[0], registers[1], registers[2]
                );
                let r1 = self.get(registers[0])?;
                let r2 = self.get(registers[1])?;
                let out = self.get_mut(registers[2])?;
                let prev = *out;
                *out = r1 * r2;
                info!(
                    "         {:>3} ✖️ {:>3} -> {:>3} (prev: {})",
                    r1, r2, *out, prev
                );
            }
        }

        self.position = Some(pos + 4);
        Ok(true)
    }

    pub fn run(&mut self) -> Result<(), failure::Error> {
        while self.step()? {}

        Ok(())
    }
}

impl FromStr for Intcodes {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pieces = s.trim().split(',');

        let v: Result<Vec<Value>, ParseIntError> = pieces.map(str::parse).collect();

        Ok(Intcodes::new(v?))
    }
}

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

    let line = buf_reader.lines().next().unwrap()?;
    let orig_cp: Intcodes = str::parse(&line)?;
    let mut cp = orig_cp.clone();
    cp.values[1] = 12;
    cp.values[2] = 2;

    cp.run()?;

    println!("At position 0: {}", cp.values[0]);
    let mut found = None;
    for ix1 in 0..cp.values.len() {
        for ix2 in 0..cp.values.len() {
            let mut cp = orig_cp.clone();
            cp.values[1] = ix1 as Value;
            cp.values[2] = ix2 as Value;
            cp.run()?;

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

#[cfg(test)]
mod tests {
    use test_env_log::test;

    use super::*;

    #[test]
    fn test_intcodes() -> Result<(), failure::Error> {
        let start = vec![1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50];
        let mut cp = Intcodes::new(start.clone());

        assert_eq!(cp.step()?, true);
        assert_eq!(cp.values[0..4], [1, 9, 10, 70]);
        assert_eq!(cp.position, Some(4));

        assert_eq!(cp.step()?, true);
        assert_eq!(cp.values[0], 3500);

        assert_eq!(cp.step()?, false);

        assert_eq!(
            cp.values,
            vec![3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
        );

        cp = Intcodes::new(start.clone());
        cp.run()?;
        assert_eq!(
            cp.values,
            vec![3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
        );

        Ok(())
    }

    #[test]
    fn test_more_intcodes() -> Result<(), failure::Error> {
        let mut cp = Intcodes::new(vec![1, 0, 0, 0, 99]);
        cp.run()?;
        assert_eq!(cp.values, vec![2, 0, 0, 0, 99]);

        cp = Intcodes::new(vec![2, 3, 0, 3, 99]);
        cp.run()?;
        assert_eq!(cp.values, vec![2, 3, 0, 6, 99]);

        cp = Intcodes::new(vec![2, 4, 4, 5, 99, 0]);
        cp.run()?;
        assert_eq!(cp.values, vec![2, 4, 4, 5, 99, 9801]);

        cp = Intcodes::new(vec![1, 1, 1, 4, 99, 5, 6, 0, 99]);
        cp.run()?;
        assert_eq!(cp.values, vec![30, 1, 1, 4, 2, 5, 6, 0, 99]);

        Ok(())
    }
}
