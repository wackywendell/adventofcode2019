use std::collections::HashSet;
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::str::FromStr;

use clap::{App, Arg};
use log::debug;

use aoc::intcomp::{IntComp, OutputVec, Stopped};

struct Grid {
    shape: (i64, i64),
    bot: (i64, i64),
    grid: HashSet<(i64, i64)>,
}

impl Grid {
    pub fn intersections(&self) -> HashSet<(i64, i64)> {
        let mut ret = HashSet::new();
        for &(x, y) in &self.grid {
            let others = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)];
            let all = others.iter().all(|pos| self.grid.contains(pos));
            if all {
                ret.insert((x, y));
            }
        }

        ret
    }
}

impl FromStr for Grid {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> anyhow::Result<Grid> {
        let mut maxx = 0;
        let mut maxy = 0;
        let (mut x, mut y) = (0i64, 0i64);
        let mut bot: Option<(i64, i64)> = None;
        let mut grid = HashSet::new();

        for c in s.chars() {
            match c {
                ' ' => continue,
                '\n' => {
                    if x > 0 {
                        maxy = y;
                    }
                    y += 1;
                    x = 0;
                    continue;
                }
                '#' => {
                    grid.insert((x, y));
                }
                '^' => {
                    grid.insert((x, y));
                    if let Some((bx, by)) = bot {
                        return Err(anyhow::format_err!(
                            "Found bot at ({}, {}) and  ({}, {})",
                            bx,
                            by,
                            x,
                            y,
                        ));
                    }
                    bot = Some((x, y));
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
                let c = if self.grid.contains(&(x, y)) {
                    if (x, y) == self.bot {
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
    let points: i64 = intersections.iter().map(|&(x, y)| x * y).sum();

    println!("\nIntersections: {} => {}", intersections.len(), points);
    for &(x, y) in &intersections {
        println!("  ({}, {})", x, y);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    const EXAMPLE1: &str = r#"
    ..#..........
    ..#..........
    #######...###
    #.#...#...#.#
    #############
    ..#...#...#..
    ..#####...^.."#;

    use super::*;

    #[test]
    fn test_parse() -> anyhow::Result<()> {
        let grid = Grid::from_str(EXAMPLE1)?;
        let intersections = grid.intersections();
        let mut iv: Vec<_> = intersections.iter().copied().collect();
        iv.sort();
        let expected: Vec<(i64, i64)> = vec![(2, 2), (4, 2), (4, 6), (4, 10)];
        assert_eq!(iv, expected);

        Ok(())
    }
}
