use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use log::debug;

use aoc::grid::{Map, Position};
use aoc::intcomp::{IntComp, OutputVec, Stopped, Value};

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum TractorBeam {
    Pulled,
    Outside,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TractorReader {
    computer: IntComp,
    map: Map<TractorBeam>,
}

impl TractorReader {
    pub fn from_computer(computer: IntComp, size: (Value, Value)) -> anyhow::Result<Self> {
        let mut map: Map<TractorBeam> = Default::default();

        let (mx, my) = size;
        for x in 0..mx {
            for y in 0..my {
                let mut cp = computer.clone();
                let mut outputs = OutputVec::new();
                cp.process(vec![x, y], &mut outputs)?
                    .expect(Stopped::Halted)?;
                let output_vec: Vec<Value> = outputs.0.into();
                let state = match output_vec.as_slice() {
                    [0] => TractorBeam::Outside,
                    [1] => TractorBeam::Pulled,
                    _ => return Err(anyhow::format_err!("Unexpected output: {:?}", &output_vec)),
                };

                map.insert(Position(x, y), state);
            }
        }

        Ok(TractorReader { computer, map })
    }

    pub fn affected_size(&self) -> usize {
        self.map
            .grid
            .values()
            .copied()
            .filter(|&v| v == TractorBeam::Pulled)
            .count()
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 19")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day19.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let line: String = buf_reader
        .lines()
        .next()
        .ok_or_else(|| anyhow::format_err!("No line found"))??;

    let mut cp: IntComp = str::parse(&line)?;
    let tr = TractorReader::from_computer(cp.clone(), (50, 50))?;
    println!("Affected: {}", tr.affected_size());

    let mut outputs = OutputVec::new();
    cp.process(vec![0, 0], &mut outputs)?
        .expect(Stopped::Halted)?;

    print!("Outputs:");
    for o in &outputs.0 {
        println!(" {}", o);
    }

    println!();

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    // use super::*;

    #[test]
    fn test_thing() -> anyhow::Result<()> {
        Ok(())
    }
}
