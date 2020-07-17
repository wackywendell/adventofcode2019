use std::collections::HashSet;
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::str::FromStr;

use clap::{App, Arg};
use log::debug;

use aoc::intcomp::{IntComp, OutputVec, Stopped};

type Value = i64;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Loc(Value, Value);

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.0, self.1)
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Compass {
    North,
    South,
    East,
    West,
}

impl fmt::Display for Compass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let c = match self {
            Compass::North => "N",
            Compass::South => "S",
            Compass::East => "E",
            Compass::West => "W",
        };
        f.write_str(c)
    }
}

impl std::ops::Add<Compass> for Loc {
    type Output = Self;

    fn add(self: Self, rhs: Compass) -> Self {
        let Loc(x, y) = self;
        match rhs {
            Compass::North => Loc(x, y - 1),
            Compass::South => Loc(x, y + 1),
            Compass::East => Loc(x + 1, y),
            Compass::West => Loc(x - 1, y),
        }
    }
}
impl std::ops::Add<Turn> for Compass {
    type Output = Self;

    fn add(self: Self, rhs: Turn) -> Self {
        match (self, rhs) {
            (Compass::North, Turn::Left) => Compass::West,
            (Compass::North, Turn::Right) => Compass::East,
            (Compass::South, Turn::Left) => Compass::East,
            (Compass::South, Turn::Right) => Compass::West,
            (Compass::East, Turn::Left) => Compass::North,
            (Compass::East, Turn::Right) => Compass::South,
            (Compass::West, Turn::Left) => Compass::South,
            (Compass::West, Turn::Right) => Compass::North,
            (Compass::North, Turn::Straight) => Compass::North,
            (Compass::South, Turn::Straight) => Compass::South,
            (Compass::East, Turn::Straight) => Compass::East,
            (Compass::West, Turn::Straight) => Compass::West,
        }
    }
}

pub struct Grid {
    shape: (Value, Value),
    bot: Loc,
    grid: HashSet<Loc>,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Turn {
    Right,
    Straight,
    Left,
}

impl fmt::Display for Turn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let c = match self {
            Turn::Right => "R",
            Turn::Straight => ">",
            Turn::Left => "L",
        };
        f.write_str(c)
    }
}

pub struct Instruction {
    pub turn: Turn,
    pub steps: Value,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.turn, self.steps)
    }
}

impl Grid {
    pub fn intersections(&self) -> HashSet<Loc> {
        let mut ret = HashSet::new();
        for &loc in &self.grid {
            let others = [Compass::North, Compass::South, Compass::East, Compass::West];
            let all = others.iter().all(|&dir| self.grid.contains(&(loc + dir)));
            if all {
                ret.insert(loc);
            }
        }

        ret
    }

    pub fn instructions(&self) -> anyhow::Result<Vec<Instruction>> {
        let mut unvisited = self.grid.clone();
        let mut bot = self.bot;
        unvisited.remove(&bot);
        let mut facing = Compass::North;

        let mut turn = if self.grid.contains(&(bot + (facing + Turn::Left))) {
            Turn::Left
        } else if self.grid.contains(&(bot + (facing + Turn::Right))) {
            Turn::Right
        } else {
            let new_grid = Grid {
                shape: self.shape,
                grid: unvisited,
                bot,
            };
            return Err(anyhow::format_err!(
                "Can't go anywhere from {}\n{}",
                bot,
                new_grid
            ));
        };

        facing = facing + turn;

        let mut instrs = Vec::new();
        while !unvisited.is_empty() {
            let mut steps: Value = 0;
            let mut next = bot + facing;
            while self.grid.contains(&next) {
                bot = next;
                steps += 1;
                unvisited.remove(&bot);
                next = bot + facing;
            }

            instrs.push(Instruction { turn, steps });

            turn = if self.grid.contains(&(bot + (facing + Turn::Left))) {
                Turn::Left
            } else if self.grid.contains(&(bot + (facing + Turn::Right))) {
                Turn::Right
            } else if unvisited.is_empty() {
                break;
            } else {
                let new_grid = Grid {
                    shape: self.shape,
                    grid: unvisited,
                    bot,
                };
                return Err(anyhow::format_err!(
                    "Can't go anywhere from {}\n{}",
                    bot,
                    new_grid
                ));
            };
            facing = facing + turn;
        }

        Ok(instrs)
    }
}

impl FromStr for Grid {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> anyhow::Result<Grid> {
        let mut maxx = 0;
        let mut maxy = 0;
        let (mut x, mut y) = (0i64, 0i64);
        let mut bot: Option<Loc> = None;
        let mut grid = HashSet::new();

        for c in s.chars() {
            match c {
                ' ' => continue,
                '\n' => {
                    if x == 0 {
                        continue;
                    }
                    maxy = y;
                    y += 1;
                    x = 0;
                    continue;
                }
                '#' => {
                    grid.insert(Loc(x, y));
                }
                '^' => {
                    grid.insert(Loc(x, y));
                    if let Some(Loc(bx, by)) = bot {
                        return Err(anyhow::format_err!(
                            "Found bot at ({}, {}) and  ({}, {})",
                            bx,
                            by,
                            x,
                            y,
                        ));
                    }
                    bot = Some(Loc(x, y));
                }
                '.' => {}
                c => {
                    return Err(anyhow::format_err!("Unexpected character {:?}", c));
                }
            }

            if x > maxx {
                maxx = x;
            }

            x += 1;
        }

        let bot = bot.ok_or_else(|| anyhow::format_err!("No bot found"))?;

        Ok(Grid {
            bot,
            shape: (maxx, maxy),
            grid,
        })
    }
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (maxx, maxy) = self.shape;
        for y in 0..=maxy {
            for x in 0..=maxx {
                let loc = Loc(x, y);
                let c = if self.grid.contains(&loc) {
                    if loc == self.bot {
                        '^'
                    } else {
                        '#'
                    }
                } else {
                    '.'
                };
                write!(f, "{}", c)?;
            }

            writeln!(f, "")?;
        }

        Ok(())
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 17")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day17.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let line: String = buf_reader
        .lines()
        .next()
        .ok_or_else(|| anyhow::format_err!("No line found"))??;

    let mut cp: IntComp = str::parse(&line)?;

    let mut outputs = OutputVec::new();
    cp.process(Vec::new(), &mut outputs)?
        .expect(Stopped::Halted)?;

    let vs = outputs.0;
    println!("Found {} Outputs", vs.len());
    let ascii: Vec<u8> = vs.iter().map(|&n| n as u8).collect();
    let stringed = String::from_utf8(ascii)?;
    println!("{}", stringed);

    let grid = Grid::from_str(&stringed)?;
    println!("Grid:\n{}", grid);

    let intersections = grid.intersections();
    let points: i64 = intersections.iter().map(|&Loc(x, y)| x * y).sum();

    println!("\nIntersections: {} => {}", intersections.len(), points);
    for &Loc(x, y) in &intersections {
        println!("  ({}, {})", x, y);
    }

    println!("----------------------------------------");
    let instrs = grid.instructions()?;
    println!("Instructions:");
    for i in instrs {
        println!("  {}", i);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::fmt::Write;

    use test_env_log::test;

    const EXAMPLE1: &str = r#"
        ..#..........
        ..#..........
        #######...###
        #.#...#...#.#
        #############
        ..#...#...#..
        ..#####...^.."#;

    const EXAMPLE2: &str = r#"
        #######...#####
        #.....#...#...#
        #.....#...#...#
        ......#...#...#
        ......#...###.#
        ......#.....#.#
        ^########...#.#
        ......#.#...#.#
        ......#########
        ........#...#..
        ....#########..
        ....#...#......
        ....#...#......
        ....#...#......
        ....#####......"#;

    use super::*;

    #[test]
    fn test_parse() -> anyhow::Result<()> {
        let grid = Grid::from_str(EXAMPLE1)?;
        let intersections = grid.intersections();
        let mut iv: Vec<_> = intersections.iter().copied().collect();
        iv.sort();
        let expected: Vec<Loc> = vec![Loc(2, 2), Loc(2, 4), Loc(6, 4), Loc(10, 4)];
        assert_eq!(iv, expected);

        Ok(())
    }

    #[test]
    fn test_instrs() -> anyhow::Result<()> {
        let grid = Grid::from_str(EXAMPLE2)?;
        let instrs = grid.instructions()?;

        let mut s = String::new();
        for i in instrs {
            write!(s, "{},{},", i.turn, i.steps)?;
        }

        assert_eq!(
            s,
            "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2,"
        );

        Ok(())
    }
}
