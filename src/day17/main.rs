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

#[allow(clippy::suspicious_arithmetic_impl)]
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

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
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

            writeln!(f)?;
        }

        Ok(())
    }
}

struct InstructionSet(Vec<Instruction>);

impl fmt::Display for InstructionSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for instr in &self.0 {
            if !first {
                f.write_str(",")?;
            } else {
                first = false;
            }
            write!(f, "{},{}", instr.turn, instr.steps)?;
        }

        Ok(())
    }
}

fn repeats<T: Eq>(sub: &[T], larger: &[T]) -> Vec<usize> {
    let sz = sub.len();
    let mut ixs = vec![];
    let mut ix = 0usize;
    loop {
        if ix + sz > larger.len() {
            break;
        }
        let matches = larger[ix..ix + sz].iter().zip(sub).all(|(a, b)| a == b);
        if matches {
            ixs.push(ix);
            ix += sz;
            continue;
        }
        ix += 1;
    }

    ixs
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Routine {
    // start is inclusive
    start: usize,
    // end is non-inclusive
    end: usize,

    // id is last so derived sort is based on (start, end)
    id: usize,
}

pub fn routines(instrs: &[Instruction]) -> Vec<Routine> {
    let mut routines: Vec<Routine> = Vec::new();
    let mut start_ix = 0usize;
    let mut next_id = 0usize;

    loop {
        let mut end_ix = instrs.len();
        for r in &routines {
            match (start_ix.cmp(&r.start), start_ix.cmp(&r.end)) {
                (std::cmp::Ordering::Less, _) => {
                    // Our routine start is before this section, all is well.
                    // Set end_ix to the start of this routine, as the new
                    // sub-routine can't be longer than that.
                    end_ix = r.start;
                    break;
                }
                (_, std::cmp::Ordering::Less) => {
                    // Our routine starts within this, bump it
                    start_ix = r.end;
                    continue;
                }
                _ => {
                    // This routine ends before the start_ix, so see the next
                    continue;
                }
            }
        }

        if start_ix >= instrs.len() {
            // Routines cover the whole instruction set, so we're done.
            break;
        }

        // Search for the subroutine starting at start_ix that saves the most
        let mut best: (Vec<Routine>, usize) = (Vec::new(), 0);
        // A subroutine must be at least 2 long
        let min_length = 2;
        for end in start_ix + min_length..=end_ix {
            let routine = Routine {
                start: start_ix,
                end,
                id: next_id,
            };
            let sub_instrs = InstructionSet(instrs[routine.start..routine.end].to_owned());
            let sub_len = routine.end - routine.start;
            let mut cur_repeats = vec![routine];

            let mut last_end = routine.end;
            for r in &routines {
                if r.start <= last_end {
                    // No space between routines here
                    continue;
                }
                let open_space = last_end..r.start;

                last_end = r.end;
                let repeated = repeats(&sub_instrs.0, &instrs[open_space.clone()]);
                for sub_ix in repeated {
                    cur_repeats.push(Routine {
                        start: open_space.start + sub_ix,
                        end: open_space.start + sub_ix + sub_len,
                        id: routine.id,
                    });
                }
            }

            if last_end < instrs.len() {
                let open_space = last_end..instrs.len();
                let repeated = repeats(&sub_instrs.0, &instrs[open_space.clone()]);
                for sub_ix in repeated {
                    cur_repeats.push(Routine {
                        start: open_space.start + sub_ix,
                        end: open_space.start + sub_ix + sub_len,
                        id: routine.id,
                    });
                }
            }

            // We've found all our repeats; let's see if they are "the best"
            // "the best" is defined as # of instructions that will be "saved"
            let score = sub_len * (cur_repeats.len() - 1);
            if score > best.1 {
                best = (cur_repeats, score);
            }
        }

        // We have our "best" set of subroutines. Let's add them to the pool and
        // carry on
        routines.append(&mut best.0);
        routines.sort();
        next_id += 1;
    }

    routines
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
    for i in &instrs {
        println!("  {}", i);
    }

    let reps = routines(&instrs);
    println!("Routines: {:?}", reps);

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

    #[test]
    fn test_repeats() -> anyhow::Result<()> {
        let grid = Grid::from_str(EXAMPLE2)?;
        let instrs = grid.instructions()?;

        let reps = routines(&instrs);
        println!("Routines: {:?}", reps);
        let mut last_end = 0usize;
        for r in &reps {
            assert_eq!(r.start, last_end);
            last_end = r.end;
        }
        assert_eq!(last_end, instrs.len());
        let ids: Vec<usize> = reps.iter().map(|r| r.id).collect();
        // A,B,C,B,A,C
        let expected_ids: Vec<usize> = vec![0, 1, 2, 1, 0, 2];
        assert_eq!(ids, expected_ids);

        Ok(())
    }
}
