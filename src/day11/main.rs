use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::iter::FromIterator;



use clap::{App, Arg};
use log::debug;

use aoc::intcomp::{IntComp, Stopped};

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Robot {
    position: (i64, i64),
    direction: Direction,
}

impl Robot {
    pub fn forward(&mut self) {
        let (ref mut x, ref mut y) = self.position;
        match self.direction {
            Direction::North => *y += 1,
            Direction::East => *x += 1,
            Direction::South => *y -= 1,
            Direction::West => *x -= 1,
        }
    }

    pub fn left(&mut self) {
        self.direction = match self.direction {
            Direction::North => Direction::West,
            Direction::East => Direction::North,
            Direction::South => Direction::East,
            Direction::West => Direction::South,
        }
    }

    pub fn right(&mut self) {
        self.direction = match self.direction {
            Direction::North => Direction::East,
            Direction::East => Direction::South,
            Direction::South => Direction::West,
            Direction::West => Direction::North,
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Color {
    White,
    Black,
}

impl From<Color> for i64 {
    fn from(c: Color) -> Self {
        match c {
            Color::Black => 0,
            Color::White => 1,
        }
    }
}

impl TryFrom<i64> for Color {
    type Error = anyhow::Error;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Color::Black),
            1 => Ok(Color::White),
            _ => Err(anyhow::format_err!("Unexpected value for color: {}", value)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spaceship {
    paint: HashMap<(i64, i64), Color>,
    robot: Robot,
    program: IntComp,
}

impl Spaceship {
    pub fn new(program: IntComp) -> Self {
        Spaceship {
            paint: HashMap::default(),
            robot: Robot {
                position: (0, 0),
                direction: Direction::North,
            },
            program,
        }
    }

    pub fn camera(&self) -> Color {
        match self.paint.get(&self.robot.position) {
            Some(&c) => c,
            None => Color::Black,
        }
    }

    pub fn step(&mut self) -> anyhow::Result<bool> {
        match self.program.run_to_io()? {
            Stopped::Halted => return Ok(false),
            Stopped::Input => {
                let color: i64 = self.camera().into();
                self.program.process_input(color)?;
                log::info!(
                    "At position {:?}, found color {:?}",
                    self.robot.position,
                    color
                );
            }
            Stopped::Output => {
                let color_val = self.program.consume_output().unwrap();
                let color = Color::try_from(color_val)?;
                self.paint.insert(self.robot.position, color);
                self.program.run_to_io()?.expect(Stopped::Output)?;
                let instruction = self.program.consume_output().unwrap();
                match instruction {
                    0 => self.robot.left(),
                    1 => self.robot.right(),
                    n => panic!("Expected 0 or 1 as instruction, found {}", n),
                };
                log::info!(
                    "At position {:?}, painting color {:?}, turning {}",
                    self.robot.position,
                    color,
                    instruction,
                );
                self.robot.forward();
            }
        }

        Ok(true)
    }

    pub fn run(&mut self) -> anyhow::Result<()> {
        while self.step()? {}

        Ok(())
    }

    pub fn show<'a>(&'a self) -> impl Iterator<Item = String> + 'a {
        let (mut minx, mut maxx) = (0, 0);
        let (mut miny, mut maxy) = (0, 0);
        for &(x, y) in self.paint.keys() {
            minx = if x < minx { x } else { minx };
            maxx = if x > maxx { x } else { maxx };
            miny = if y < miny { y } else { miny };
            maxy = if y > maxy { y } else { maxy };
        }

        (miny..=maxy).rev().map(move |y| {
            String::from_iter((minx..=maxx).map(|x| match self.paint.get(&(x, y)) {
                Some(Color::Black) => ' ',
                Some(Color::White) => '█',
                None => ' ',
            }))
        })
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 11")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day11.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let line: String = buf_reader
        .lines()
        .next()
        .ok_or_else(|| anyhow::format_err!("No line found"))??;
    let ints: Vec<i64> = line
        .trim()
        .split(',')
        .map(str::parse::<i64>)
        .collect::<Result<Vec<i64>, _>>()?;

    let cp = IntComp::new(ints);
    let mut ss = Spaceship::new(cp.clone());

    ss.run()?;
    println!("Ran, painted {} panels", ss.paint.len());

    let mut ss = Spaceship::new(cp);
    ss.paint.insert((0, 0), Color::White);

    ss.run()?;
    println!("Ran, painted {} panels", ss.paint.len());

    for s in ss.show() {
        println!("{}", s);
    }

    Ok(())
}
