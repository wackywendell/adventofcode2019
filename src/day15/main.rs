use std::collections::{HashMap, VecDeque};
use std::convert::TryFrom;
use std::fmt;
use std::fmt::Write;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use log::debug;

use aoc::grid::{Compass, Map, Position, Turn};
use aoc::intcomp::IntComp;

type Value = i64;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Square {
    Wall,
    Empty,
    OxygenSystem,
}

impl TryFrom<Value> for Square {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Square::Wall),
            1 => Ok(Square::Empty),
            2 => Ok(Square::OxygenSystem),
            _ => Err(anyhow::format_err!(
                "Can't convert value {} to Location",
                value
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Area {
    map: Map<Square>,
}

impl Area {
    fn with_bot<'a, 'b>(&'a self, bot: &'b Bot) -> AreaWithBot<'a> {
        AreaWithBot {
            map: &self.map,
            bot: bot.position,
        }
    }

    pub fn oxygen(&self) -> Option<Position> {
        for (&p, &l) in &self.map.grid {
            if l == Square::OxygenSystem {
                return Some(p);
            }
        }
        None
    }

    pub fn distances(&self, start: Position) -> HashMap<Position, Value> {
        let mut queue = VecDeque::new();
        queue.push_back((0, start));
        let mut seen = HashMap::new();

        let directions = [Compass::North, Compass::East, Compass::South, Compass::West];

        while let Some((dist, pos)) = queue.pop_front() {
            log::info!("Trying {}: {:?}", dist, pos);
            if let Some(Square::Wall) = self.map.grid.get(&pos) {
                log::info!("  Hit a Wall");
                continue;
            }

            if let Some(&d) = seen.get(&pos) {
                if d <= dist {
                    // Been here before, and it was shorter than
                    continue;
                }
            }

            seen.insert(pos, dist);

            for &d in &directions {
                let next = pos + d;
                queue.push_back((dist + 1, next));
            }
            log::info!(
                "Finished with {:?}: {}, queue size: {}",
                pos,
                dist,
                queue.len()
            );
        }

        seen
    }

    fn shortest_route(&self, start: Position) -> Value {
        let oxy = self
            .oxygen()
            .expect("Can't get shortest route if there is no OxygenSystem");
        let distances = self.distances(start);
        *distances.get(&oxy).unwrap()
    }
}

struct AreaWithBot<'a> {
    map: &'a Map<Square>,
    bot: Position,
}

impl<'a> fmt::Display for AreaWithBot<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (mut minx, mut maxx) = (0i64, 0i64);
        let (mut miny, mut maxy) = (0i64, 0i64);

        for &Position(x, y) in self.map.grid.keys() {
            if x < minx {
                minx = x;
            }
            if x > maxx {
                maxx = x;
            }
            if y < miny {
                miny = y;
            }
            if y > maxy {
                maxy = y;
            }
        }

        for x in minx - 1..=maxx + 1 {
            for y in miny - 1..=maxy + 1 {
                if Position(x, y) == self.bot {
                    f.write_char('X')?;
                    continue;
                }

                let c = match self.map.grid.get(&Position(x, y)) {
                    None => '.',
                    Some(Square::Empty) => ' ',
                    Some(Square::Wall) => '#',
                    Some(Square::OxygenSystem) => 'O',
                };
                f.write_char(c)?;
            }
            f.write_char('\n')?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bot {
    program: IntComp,
    position: Position,
}

impl From<IntComp> for Bot {
    fn from(program: IntComp) -> Self {
        Bot {
            program,
            position: Position(0, 0),
        }
    }
}

impl Bot {
    fn compass_value(dir: Compass) -> Value {
        match dir {
            Compass::North => 1,
            Compass::South => 2,
            Compass::West => 3,
            Compass::East => 4,
        }
    }

    pub fn step(&mut self, direction: Compass) -> anyhow::Result<(Square, Position)> {
        let input: Value = Self::compass_value(direction);
        log::info!(
            "step {:?}: {}; {:?} -> {:?}",
            direction,
            input,
            self.position,
            self.position + direction
        );
        let state = self.program.run_to_io()?;
        log::info!("  {}", state);
        let processed = self.program.process_input(input)?;
        log::info!("  {}", processed);
        if !processed {
            return Err(anyhow::format_err!("Error: Could not process input"));
        }

        let state = self.program.run_to_io()?;
        log::info!("  {}", state);
        let out = match self.program.consume_output() {
            None => return Err(anyhow::format_err!("Error: Could not consume output")),
            Some(v) => v,
        };

        let loc = Square::try_from(out)?;

        self.position = match loc {
            Square::Wall => self.position,
            Square::Empty => self.position + direction,
            Square::OxygenSystem => self.position + direction,
        };

        Ok((loc, self.position))
    }

    pub fn explore(&mut self) -> anyhow::Result<Area> {
        // TODO: doesn't explore inner areas

        let mut known = Area::default();
        known.map.insert(self.position, Square::Empty);

        // Start by finding a wall
        loop {
            let (loc, _) = self.step(Compass::North)?;
            if loc == Square::Wall {
                known.map.insert(self.position + Compass::North, loc);
                break;
            }

            known.map.insert(self.position, loc);
        }
        log::info!("Hit wall at {:?}", self.position);

        let start = self.position;

        // Turn East, and follow the wall
        let mut dir = Compass::East;
        loop {
            let (loc, pos) = self.step(dir)?;
            log::info!("Stepped: {:?}, {:?}", loc, pos);

            if loc == Square::Wall {
                known.map.insert(self.position + dir, loc);
                // Turn right when you hit a wall, left when you don't
                dir = dir + Turn::Right;
                if self.position == start && dir == Compass::East {
                    break;
                }
                continue;
            }

            known.map.insert(self.position, loc);
            dir = dir + Turn::Left;

            log::info!("\n{}", known.with_bot(self));

            if self.position == start && dir == Compass::East {
                break;
            }
        }

        Ok(known)
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 15")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day15.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let line: String = buf_reader
        .lines()
        .next()
        .ok_or_else(|| anyhow::format_err!("No line found"))??;

    let cp: IntComp = str::parse(&line)?;

    let mut bot: Bot = cp.into();
    let area = bot.explore()?;

    println!("{}", area.with_bot(&bot));

    println!(
        "Shortest route to Oxygen: {}",
        area.shortest_route(Position(0, 0))
    );

    let distances = area.distances(area.oxygen().unwrap());
    let &max = distances.values().max().unwrap();

    println!("Furthest point from Oxygen: {}", max);

    Ok(())
}
