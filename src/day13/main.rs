use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::fmt::Write;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::iter::FromIterator;
use std::str::FromStr;

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
    ball: (Value, Value),
    paddle: (Value, Value),
    score: Value,
}

impl Game {
    /// (y, x)
    pub fn shape(&self) -> (usize, usize) {
        let yln = self.grid.len();
        let xln = if yln > 0 { self.grid[0].len() } else { 0 };

        (yln, xln)
    }

    pub fn add(&mut self, xy: (Value, Value), tile: Tile) -> Tile {
        let (x, y) = xy;

        if !self.grid.is_empty() && x as usize >= self.grid[0].len() {
            for v in self.grid.iter_mut() {
                v.extend((v.len()..=x as usize).map(|_| Tile::Empty));
            }
        }
        if y as usize >= self.grid.len() {
            let yln = self.grid.len();
            let xln = if yln > 0 {
                self.grid[0].len()
            } else {
                x as usize + 1
            };
            self.grid
                .extend((yln..=y as usize).map(|_| vec![Tile::Empty; xln]))
        }

        // log::debug!(
        //     "  maxs now: ({}, {}); insert: ({}, {})",
        //     self.maxx,
        //     self.maxy,
        //     x,
        //     y
        // );

        match tile {
            Tile::Ball => self.ball = (y, x),
            Tile::Paddle => self.paddle = (y, x),
            _ => {}
        }

        let prev = self.grid[y as usize][x as usize];
        self.grid[y as usize][x as usize] = tile;
        prev
    }

    pub fn from_values<I: IntoIterator<Item = Value>>(iter: I) -> Result<Self, failure::Error> {
        let mut game: Game = Default::default();
        game.update(iter)?;
        Ok(game)
    }

    pub fn get(&self, index: (Value, Value)) -> Option<Tile> {
        let (iy, ix) = index;
        if iy < 0 || ix < 0 {
            return None;
        }
        let row = self.grid.get(iy as usize)?;

        row.get(ix as usize).copied()
    }

    pub fn update<I: IntoIterator<Item = Value>>(&mut self, iter: I) -> Result<(), failure::Error> {
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

// Mostly unused
impl FromStr for Game {
    type Err = failure::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pieces = s.trim().split(',');
        let values: Vec<Value> = pieces
            .map(str::parse)
            .collect::<Result<Vec<Value>, std::num::ParseIntError>>()?;

        Game::from_values(values)
    }
}

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
    velocity: (Value, Value),
    outputs: OutputVec,
}

impl Arcade {
    pub fn new(game: Game, software: IntComp) -> Self {
        Arcade {
            game,
            software,
            velocity: Default::default(),
            outputs: Default::default(),
        }
    }

    fn update(&mut self) -> Result<Stopped, failure::Error> {
        self.outputs.0.clear();
        let state = self.software.process(Vec::new(), &mut self.outputs)?;
        let (by, bx) = self.game.ball;
        self.game.update(self.outputs.0.iter().copied())?;
        let (by2, bx2) = self.game.ball;
        self.velocity = (by2 - by, bx2 - bx);
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

    pub fn auto(&mut self) -> Result<bool, failure::Error> {
        let (_, prediction) = self.predict();
        let (_, paddle) = self.game.paddle;
        let dir = match prediction.cmp(&paddle) {
            std::cmp::Ordering::Greater => Direction::Right,
            std::cmp::Ordering::Equal => Direction::Center,
            std::cmp::Ordering::Less => Direction::Left,
        };

        self.step(dir)
    }

    // Location of the ball as (y, x)
    pub fn ball(&self) -> (Value, Value) {
        return self.game.ball;
    }

    // Predicted next location of the ball as (y, x). Prediction ignores blocks and paddle.
    pub fn predict(&self) -> (Value, Value) {
        let (by, bx) = self.game.ball;
        let (vy, vx) = self.velocity;
        if vy > 1 || vy < -1 || vx > 1 || vx < -1 {
            panic!("Unexpected velocity: ({}, {})", vy, vx);
        }

        let (mut nexty, mut nextx) = (by + vy, bx + vx);

        let (szy, szx) = self.game.shape();
        let (szy, szx) = (szy as Value, szx as Value);

        // Calculate bounces
        if nexty <= 0 {
            nexty = 1 - vy;
        } else if nexty >= szy {
            nexty = 2 * szy - vy - 1;
        }
        if nextx <= 0 {
            nextx = 1 - vx;
        } else if nextx >= szx {
            nextx = 2 * szx - vx - 1;
        }

        (nexty, nextx)
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
    let mut prediction = arcade.predict();

    while arcade.auto()? {
        println!("----------------------------------------");
        println!("{}", arcade.game);
        for i in 0..=arcade.game.shape().1 {
            print!("{}", i % 10);
        }
        println!("");
        println!("-------------------- Score: {}", arcade.game.score);
        let (by, bx) = arcade.ball();
        let (py, px) = prediction;
        let s = if (by, bx) == (py, px) { "" } else { "!!" };
        println!(
            "-------------------- Ball: ({}, {}) -> ({}, {}) {}",
            py, px, by, bx, s,
        );

        prediction = arcade.predict();
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
