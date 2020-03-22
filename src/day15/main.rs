use std::collections::{HashMap, VecDeque};
use std::convert::TryFrom;
use std::fmt;
use std::fmt::Write;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::ops::Add;

use clap::{App, Arg};
use log::debug;

use aoc::intcomp::IntComp;

type Value = i64;

type Position = (Value, Value);

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Direction {
    North,
    South,
    West,
    East,
}

impl From<Direction> for Value {
    fn from(dir: Direction) -> Value {
        match dir {
            Direction::North => 1,
            Direction::South => 2,
            Direction::West => 3,
            Direction::East => 4,
        }
    }
}

impl Direction {
    fn left(self) -> Self {
        match self {
            Direction::North => Direction::West,
            Direction::South => Direction::East,
            Direction::West => Direction::South,
            Direction::East => Direction::North,
        }
    }
    fn right(self) -> Self {
        match self {
            Direction::North => Direction::East,
            Direction::South => Direction::West,
            Direction::West => Direction::North,
            Direction::East => Direction::South,
        }
    }
}

impl Add<Direction> for Position {
    type Output = Position;

    fn add(self, dir: Direction) -> Self {
        let (y, x) = self;
        match dir {
            Direction::North => (y - 1, x),
            Direction::South => (y + 1, x),
            Direction::West => (y, x - 1),
            Direction::East => (y, x + 1),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Location {
    Wall,
    Empty,
    OxygenSystem,
}

impl TryFrom<Value> for Location {
    type Error = failure::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Location::Wall),
            1 => Ok(Location::Empty),
            2 => Ok(Location::OxygenSystem),
            _ => Err(failure::format_err!(
                "Can't convert value {} to Location",
                value
            )),
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Area {
    locations: HashMap<Position, Location>,
}

impl Area {
    fn with_bot<'a, 'b>(&'a self, bot: &'b Bot) -> AreaWithBot<'a> {
        AreaWithBot {
            locations: &self.locations,
            bot: bot.position,
        }
    }

    pub fn shortest_route(&self, start: Position) -> i64 {
        let mut queue = VecDeque::new();
        queue.push_back((0, start));
        let mut seen = HashMap::new();

        let directions = [
            Direction::North,
            Direction::East,
            Direction::South,
            Direction::West,
        ];

        let mut oxy: Option<Position> = None;

        while let Some((dist, pos)) = queue.pop_front() {
            log::info!("Trying {}: {:?}", dist, pos);
            match self.locations.get(&pos) {
                Some(Location::Wall) => {
                    log::info!("  Hit a Wall");
                    continue;
                }
                Some(Location::OxygenSystem) => {
                    oxy = Some(pos);
                }

                _ => {}
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
            println!(
                "Finished with {:?}: {}, queue size: {}",
                pos,
                dist,
                queue.len()
            );
        }

        println!("oxy: {:?}", oxy);

        return *seen.get(&oxy.unwrap()).unwrap();
    }
}

struct AreaWithBot<'a> {
    locations: &'a HashMap<Position, Location>,
    bot: Position,
}

impl<'a> fmt::Display for AreaWithBot<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (mut minx, mut maxx) = (0, 0);
        let (mut miny, mut maxy) = (0, 0);

        for (&(x, y), _) in self.locations {
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
                if (x, y) == self.bot {
                    f.write_char('X')?;
                    continue;
                }

                let c = match self.locations.get(&(x, y)) {
                    None => '.',
                    Some(Location::Empty) => ' ',
                    Some(Location::Wall) => '#',
                    Some(Location::OxygenSystem) => 'O',
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
            position: (0, 0),
        }
    }
}

impl Bot {
    pub fn step(&mut self, direction: Direction) -> Result<(Location, Position), failure::Error> {
        let input: Value = direction.into();
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
            return Err(failure::format_err!("Error: Could not process input"));
        }

        let state = self.program.run_to_io()?;
        log::info!("  {}", state);
        let out = match self.program.consume_output() {
            None => return Err(failure::format_err!("Error: Could not consume output")),
            Some(v) => v,
        };

        let loc = Location::try_from(out)?;

        self.position = match loc {
            Location::Wall => self.position,
            Location::Empty => self.position + direction,
            Location::OxygenSystem => self.position + direction,
        };

        Ok((loc, self.position))
    }

    pub fn explore(&mut self) -> Result<Area, failure::Error> {
        // TODO: doesn't explore inner areas

        let mut known = Area::default();
        known.locations.insert(self.position, Location::Empty);

        // Start by finding a wall
        loop {
            let (loc, _) = self.step(Direction::North)?;
            if loc == Location::Wall {
                known
                    .locations
                    .insert(self.position + Direction::North, loc);
                break;
            }

            known.locations.insert(self.position, loc);
        }
        log::info!("Hit wall at {:?}", self.position);

        let start = self.position;

        // Turn East, and follow the wall
        let mut dir = Direction::East;
        loop {
            let (loc, pos) = self.step(dir)?;
            log::info!("Stepped: {:?}, {:?}", loc, pos);

            if loc == Location::Wall {
                known.locations.insert(self.position + dir, loc);
                // Turn right when you hit a wall, left when you don't
                dir = dir.right();
                if self.position == start && dir == Direction::East {
                    break;
                }
                continue;
            }

            known.locations.insert(self.position, loc);
            dir = dir.left();

            log::info!("\n{}", known.with_bot(self));

            if self.position == start && dir == Direction::East {
                break;
            }
        }

        Ok(known)
    }
}

fn main() -> Result<(), failure::Error> {
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
        .ok_or_else(|| failure::err_msg("No line found"))??;

    let cp: IntComp = str::parse(&line)?;

    let mut bot: Bot = cp.into();
    let area = bot.explore()?;

    println!("{}", area.with_bot(&bot));

    println!("Shortest: {}", area.shortest_route((0, 0)));

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
