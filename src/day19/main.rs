use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::time::Instant;

use clap::{App, Arg};
use log::debug;

use aoc::grid::{Map, Position};
use aoc::intcomp::{IntComp, OutputVec, Stopped, Value};

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum TractorBeam {
    Pulled,
    Outside,
}

impl From<TractorBeam> for char {
    fn from(sq: TractorBeam) -> char {
        match sq {
            TractorBeam::Pulled => '#',
            TractorBeam::Outside => '.',
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum TractorBeamSquared {
    Pulled,
    Outside,
    Square,
}

impl From<TractorBeam> for TractorBeamSquared {
    fn from(sq: TractorBeam) -> TractorBeamSquared {
        match sq {
            TractorBeam::Pulled => TractorBeamSquared::Pulled,
            TractorBeam::Outside => TractorBeamSquared::Outside,
        }
    }
}

impl From<TractorBeamSquared> for char {
    fn from(sq: TractorBeamSquared) -> char {
        match sq {
            TractorBeamSquared::Pulled => '#',
            TractorBeamSquared::Outside => '.',
            TractorBeamSquared::Square => 'O',
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TractorReader {
    pub computer: IntComp,
    pub map: Map<TractorBeam>,
    outputs: OutputVec,
}

impl TractorReader {
    fn calculate(&mut self, pos: Position) -> TractorBeam {
        let Position(x, y) = pos;
        let mut cp = self.computer.clone();
        cp.process(vec![x, y], &mut self.outputs)
            .expect("Processing should work")
            .expect(Stopped::Halted)
            .expect("Expected code to halt");
        let state = match self.outputs.0.as_slices() {
            ([], [0]) => TractorBeam::Outside,
            ([0], []) => TractorBeam::Outside,
            ([], [1]) => TractorBeam::Pulled,
            ([1], []) => TractorBeam::Pulled,
            _ => panic!("Unexpected output: {:?}", &self.outputs.0),
        };

        self.map.insert(Position(x, y), state);
        self.outputs.0.clear();
        state
    }

    pub fn determine(&mut self, pos: Position) -> TractorBeam {
        if let Some(&v) = self.map.get(pos) {
            return v;
        }

        self.calculate(pos)
    }

    pub fn from_computer(computer: IntComp, size: (Value, Value)) -> Self {
        let mut tr = TractorReader {
            computer,
            map: Default::default(),
            outputs: OutputVec::new(),
        };

        let (mx, my) = size;
        for x in 0..mx {
            for y in 0..my {
                tr.calculate(Position(x, y));
            }
        }

        tr
    }

    pub fn affected_size(&self) -> usize {
        self.map
            .grid
            .values()
            .copied()
            .filter(|&v| v == TractorBeam::Pulled)
            .count()
    }

    fn rect_size(&mut self, position: Position) -> (Value, Value) {
        // TODO this assumes that if the upper-left, lower-left, and upper-right
        // corners exist, then the square is complete
        let mut is_pulled = |x, y| self.determine(Position(x, y)) == TractorBeam::Pulled;
        let Position(x, y) = position;

        if !is_pulled(x, y) {
            return (0, 0);
        }

        let Position(mut x_low, mut y_low) = position;

        let mut x_high = x_low + 1;
        loop {
            if !is_pulled(x_high, y) {
                break;
            }
            x_high = x_high * 2 - x_low;
        }
        // log::info!("Starting with x [{}, {})", x_low, x_high);

        while x_high > x_low + 1 {
            let mid = (x_high + x_low) / 2;
            if is_pulled(mid, y) {
                x_low = mid;
            // log::info!("  Upping to x [{}, {})", x_low, x_high);
            } else {
                x_high = mid;
                // log::info!("  Dropping to x [{}, {})", x_low, x_high);
            }
        }

        let mut y_high = y_low + 1;
        loop {
            if !is_pulled(x, y_high) {
                break;
            }
            y_high = y_high * 2 - x_low;
        }
        // log::info!("Starting with y [{}, {})", y_low, y_high);

        while y_high > y_low + 1 {
            let mid = (y_high + y_low) / 2;
            if is_pulled(x, mid) {
                y_low = mid;
            // log::info!("  Upping to y [{}, {})", y_low, y_high);
            } else {
                y_high = mid;
                // log::info!("  Dropping to y [{}, {})", y_low, y_high);
            }
        }

        (x_high - x, y_high - y)
    }

    pub fn find_square(&mut self, shape: Value) -> Position {
        // let found = HashMap::new();
        let start = Position(20, 20);
        let Position(mut x, mut y) = start;

        while self.rect_size(Position(x, y)) == (0, 0) {
            if x > y * 3 + 10 {
                y += 1;
                x = 0;
            } else {
                x += 1;
            }
        }
        log::info!(
            "Found initial square at ({}, {}): {:?}",
            x,
            y,
            self.rect_size(Position(x, y))
        );

        loop {
            let (x_width, y_height) = self.rect_size(Position(x, y));
            log::info!("Found: ({}, {}) -> ({}, {})", x, y, x_width, y_height);
            if x_width >= shape && y_height >= shape {
                return Position(x, y);
            }
            if x_width > y_height {
                x += 1;
                continue;
            }
            y += 1;
        }
    }
}

fn with_square(
    tr: &mut TractorReader,
    shape: Value,
    position: Position,
) -> Map<TractorBeamSquared> {
    let Position(x, y) = position;
    let mut sq_map: Map<TractorBeamSquared> = tr.map.convert();

    for nx in x..=x + shape {
        for ny in y..=y + shape {
            let p = Position(nx, ny);
            let tb = tr.determine(p);
            let tbs = if nx < x + shape && ny < y + shape {
                assert_eq!(tb, TractorBeam::Pulled);
                TractorBeamSquared::Square
            } else {
                tb.into()
            };
            sq_map.insert(p, tbs);
        }
    }

    sq_map
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

    let orig_cp: IntComp = str::parse(&line)?;
    let tr = TractorReader::from_computer(orig_cp.clone(), (50, 50));
    println!("Affected: {}", tr.affected_size());

    let mut cp = orig_cp.clone();
    let mut outputs = OutputVec::new();
    cp.process(vec![0, 0], &mut outputs)?
        .expect(Stopped::Halted)?;

    print!("Outputs:");
    for o in &outputs.0 {
        println!(" {}", o);
    }

    let mut tr2 = TractorReader::from_computer(orig_cp.clone(), (3, 3));
    let shape = 8;
    let square_pos = tr2.find_square(shape);

    println!("Found {} at {}", shape, square_pos);

    println!("Grid:\n{}", with_square(&mut tr2, shape, square_pos));

    println!("--- Part Two ---");

    let mut tr3 = TractorReader::from_computer(orig_cp, (3, 3));
    let sz = 100;

    let start = Instant::now();
    let pos = tr3.find_square(sz);
    let elapsed = Instant::now().duration_since(start);
    println!("Size {} at {}, duration {:?}", sz, pos, elapsed);
    let Position(x, y) = pos;
    println!("  Answer: {}", x * 10000 + y);

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
