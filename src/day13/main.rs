use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::fmt::Write;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::iter::FromIterator;
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

impl From<Tile> for char {
    fn from(tile: Tile) -> char {
        match tile {
            Tile::Empty => '.',
            Tile::Wall => '|',
            Tile::Block => 'X',
            Tile::Paddle => 'P',
            Tile::Ball => 'O',
        }
    }
}

impl fmt::Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char(char::from(*self))
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
    grid: Vec<Vec<Tile>>,
    score: Value,
}

impl Game {
    pub fn add(&mut self, xy: (Value, Value), tile: Tile) -> Tile {
        let (x, y) = xy;
        // log::debug!(
        //     "lens: ({}, {}); insert: ({}, {})",
        //     self.grid.,
        //     self.ylen,
        //     x,
        //     y,
        // );
        if !self.grid.is_empty() && y as usize >= self.grid[0].len() {
            for v in self.grid.iter_mut() {
                v.extend((v.len()..=y as usize).map(|_| Tile::Empty));
            }
        }
        if x as usize >= self.grid.len() {
            let xln = self.grid.len();
            let yln = if xln > 0 {
                self.grid[0].len()
            } else {
                y as usize + 1
            };
            let length = self.grid.len();
            self.grid
                .extend((length..=xln).map(|_| vec![Tile::Empty; yln]))
        }

        // log::debug!(
        //     "  maxs now: ({}, {}); insert: ({}, {})",
        //     self.maxx,
        //     self.maxy,
        //     x,
        //     y
        // );

        let prev = self.grid[x as usize][y as usize];
        self.grid[x as usize][y as usize] = tile;
        prev
    }

    pub fn from_values<I>(iter: I) -> Result<Self, failure::Error>
    where
        I: IntoIterator<Item = Value>,
    {
        let mut game: Game = Default::default();
        game.update(iter)?;
        Ok(game)
    }

    pub fn update<I>(&mut self, iter: I) -> Result<(), failure::Error>
    where
        I: IntoIterator<Item = Value>,
    {
        let (mut x, mut y) = (None, None);

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
                self.score = val;
            } else {
                let t = Tile::try_from(val)?;
                self.add((xv, yv), t);
            }
            x = None;
            y = None;
        }

        if x.is_some() || y.is_some() {
            panic!("Expected groups of three, ended with {:?}, {:?}", x, y);
        }

        Ok(())
    }

    pub fn counts(&self) -> HashMap<Tile, usize> {
        let mut counts = HashMap::new();
        for row in &self.grid {
            for &v in row {
                let t = counts.entry(v).or_default();
                *t += 1;
            }
        }

        counts
    }

    pub fn to_strings(&self) -> Vec<String> {
        self.grid
            .iter()
            .map(|row| String::from_iter(row.iter().map(|&t| char::from(t))))
            .collect()
    }
}

impl fmt::Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in &self.grid {
            for &t in row {
                f.write_char(t.into())?;
            }
            f.write_char('\n')?;
        }

        Ok(())
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

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Direction {
    Left,
    Center,
    Right,
}

impl From<Direction> for Value {
    fn from(dir: Direction) -> Value {
        match dir {
            Direction::Left => -1,
            Direction::Center => 0,
            Direction::Right => 1,
        }
    }
}

pub struct Arcade {
    game: Game,
    software: IntComp,
    outputs: OutputVec,
}

impl Arcade {
    pub fn new(game: Game, software: IntComp) -> Self {
        Arcade {
            game,
            software,
            outputs: Default::default(),
        }
    }

    fn update(&mut self) -> Result<Stopped, failure::Error> {
        self.outputs.0.clear();
        let state = self.software.process(Vec::new(), &mut self.outputs)?;
        self.game.update(self.outputs.0.iter().copied())?;
        Ok(state)
    }

    pub fn step(&mut self, direction: Direction) -> Result<bool, failure::Error> {
        self.outputs.0.clear();
        match self.update()? {
            Stopped::Halted => Ok(false),
            Stopped::Output => unreachable!("Should have absorbed output"),
            Stopped::Input => {
                if self.software.process_input(direction.into())? {
                    Ok(true)
                } else {
                    unreachable!("Stopped::Input should accept input")
                }
            }
        }
    }
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

    let orig_cp: IntComp = str::parse(&line)?;
    let mut cp = orig_cp.clone();
    let mut outputs = OutputVec::new();
    cp.process(Vec::new(), &mut outputs)?
        .expect(Stopped::Halted)?;

    let vs = outputs.0;
    println!("Found {} Outputs", vs.len());

    let tiles = Game::from_values(vs)?;

    for (k, v) in tiles.counts() {
        println!("{}: {}", k, v);
    }

    let mut cp = orig_cp;
    cp.values[0] = 2;

    let mut arcade = Arcade::new(Game::default(), cp);

    while arcade.step(Direction::Center)? {
        println!("----------------------------------------");
        println!("{}", arcade.game);
        println!("-------------------- Score: {}", arcade.game.score);
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
