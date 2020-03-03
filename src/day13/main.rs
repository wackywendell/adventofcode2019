use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
// use std::str::FromStr;

use clap::{App, Arg};
use failure::Fail;
use log::debug;

use aoc::intcomp::{IntComp, OutputVec, Stopped};

pub type Value = i64;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Tile {
    Empty,
    Wall,
    Block,
    Paddle,
    Ball,
}

impl fmt::Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Tile::Empty => ".",
            Tile::Wall => "|",
            Tile::Block => "X",
            Tile::Paddle => "P",
            Tile::Ball => "O",
        };

        f.write_str(s)
    }
}

#[derive(Fail, Debug)]
#[fail(display = "No known tile of value {}", value)]
pub struct TileError {
    value: Value,
}

impl TryFrom<Value> for Tile {
    type Error = TileError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Tile::Empty),
            1 => Ok(Tile::Wall),
            2 => Ok(Tile::Block),
            3 => Ok(Tile::Paddle),
            4 => Ok(Tile::Ball),
            v => Err(TileError { value: v }),
        }
    }
}

#[derive(Debug, Default)]
pub struct Game {
    tiles: HashMap<(Value, Value), Tile>,
    score: Value,
}

impl Game {
    pub fn add(&mut self, xy: (Value, Value), tile: Tile) -> Option<Tile> {
        self.tiles.insert(xy, tile)
    }

    pub fn from_values<I>(iter: I) -> Result<Self, failure::Error>
    where
        I: IntoIterator<Item = Value>,
    {
        let (mut x, mut y) = (None, None);
        let mut tiles = HashMap::new();
        let mut score = 0;

        for val in iter {
            let xv = match x {
                None => {
                    x = Some(val);
                    continue;
                }
                Some(v) => v,
            };
            let yv = match y {
                None => {
                    y = Some(val);
                    continue;
                }
                Some(v) => v,
            };

            if (xv, yv) == (-1, 0) {
                score = val;
            } else {
                let t = Tile::try_from(val)?;
                tiles.insert((xv, yv), t);
            }
            x = None;
            y = None;
        }

        if x.is_some() || y.is_some() {
            panic!("Expected groups of three, ended with {:?}, {:?}", x, y);
        }

        Ok(Game { tiles, score })
    }

    pub fn counts(&self) -> HashMap<Tile, usize> {
        let mut counts = HashMap::new();
        for &v in self.tiles.values() {
            let t = counts.entry(v).or_default();
            *t += 1;
        }

        counts
    }
}

// impl FromStr for Game {
//     type Err = failure::Error;

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         let pieces = s.trim().split(',');
//         let (mut x, mut y) = (None, None);
//         let mut tiles = HashMap::new();

//         for val in pieces {
//             let val: Value = str::parse(val)?;
//             let x = match x {
//                 None => {
//                     x = Some(val);
//                     continue;
//                 }
//                 Some(v) => v,
//             };
//             let y = match y {
//                 None => {
//                     y = Some(val);
//                     continue;
//                 }
//                 Some(v) => v,
//             };

//             let t = Tile::try_from(val)?;
//             tiles.insert((x, y), t);
//         }

//         Ok(Game { tiles })
//     }
// }

pub struct Arcade {
    game: Game,
    software: IntComp,
}

impl Arcade {
    pub fn new(game: Game, software: IntComp) -> Self {
        Arcade { game, software }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Direction {
    Left,
    Center,
    Right,
}

fn main() -> Result<(), failure::Error> {
    env_logger::init();

    let matches = App::new("Day 13")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day13.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let line: String = buf_reader
        .lines()
        .next()
        .ok_or_else(|| failure::err_msg("No line found"))??;

    let mut cp: IntComp = str::parse(&line)?;
    let mut outputs = OutputVec::new();
    cp.process(Vec::new(), &mut outputs)?
        .expect(Stopped::Halted)?;

    let vs = outputs.0;
    println!("Found {} Outputs", vs.len());

    let tiles = Game::from_values(vs)?;

    for (k, v) in tiles.counts() {
        println!("{}: {}", k, v);
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
