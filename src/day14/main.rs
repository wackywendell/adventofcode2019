use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::iter::FromIterator;
use std::str::FromStr;

use clap::{App, Arg};
use log::debug;

#[derive(Default, Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Operand {
    quantity: i64,
    chemical: String,
}

impl FromStr for Operand {
    type Err = failure::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let splits: Vec<_> = s.split(' ').collect();
        match splits.as_slice() {
            &[ns, cs] => {
                let n: i64 = ns.parse()?;
                return Ok(Operand {
                    quantity: n,
                    chemical: cs.to_string(),
                });
            }
            _ => return Err(failure::format_err!("Cannot convert '{}' to Operand", s)),
        };
    }
}

struct Reaction {
    input: Vec<Operand>,
    output: Operand,
}

impl FromStr for Reaction {
    type Err = failure::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let splits: Vec<_> = s.split(" => ").collect();
        let (inputs, output) = match splits.as_slice() {
            &[inp, outp] => (inp, outp),
            _ => return Err(failure::format_err!("Cannot convert '{}' to Reactions", s)),
        };

        let output: Operand = str::parse(output)?;
        let inputs: Result<Vec<Operand>, failure::Error> =
            inputs.split(", ").map(|rs| str::parse(rs)).collect();

        Ok(Reaction {
            input: inputs?,
            output: output,
        })
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Reactions {
    // output -> inputs
    values: HashMap<Operand, Vec<Operand>>,
}

impl FromIterator<Reaction> for Reactions {
    fn from_iter<I: IntoIterator<Item = Reaction>>(iter: I) -> Self {
        let values: HashMap<Operand, Vec<Operand>> =
            iter.into_iter().map(|r| (r.output, r.input)).collect();
        Reactions { values }
    }
}

fn main() -> Result<(), failure::Error> {
    env_logger::init();

    let matches = App::new("Day 14")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day14.txt");

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

    // use super::*;

    #[test]
    fn test_thing() -> Result<(), failure::Error> {
        Ok(())
    }
}
