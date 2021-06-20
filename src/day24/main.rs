use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::str::FromStr;

use clap::{App, Arg};
use log::debug;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Space {
    Bug,
    Empty,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Area {
    width: usize,
    grid: Vec<Space>,
}

impl Area {
    fn count_neighbors(&self, n: usize) -> usize {
        let n = n as isize;
        let w = self.width as isize;
        let dns = [-w, -1, w, 1];
        dns.iter()
            .copied()
            .map(|dn| {
                let newn = n as isize + dn;
                #[allow(clippy::if_same_then_else)]
                if newn < 0 || newn >= self.grid.len() as isize {
                    // println!("Neighbor {}:{}: outside", n, newn);
                    0
                } else if dn.abs() == 1 && newn / w != n / w {
                    // println!("Neighbor {}:{}: end of row", n, newn);
                    0
                } else if self.grid.get(newn as usize).copied() == Some(Space::Bug) {
                    // println!("Neighbor {}:{}: bug", n, newn);
                    1
                } else {
                    // println!("Neighbor {}:{}: other", n, newn);
                    0
                }
            })
            .sum()
    }

    pub fn next_minute(&mut self) {
        let mut next = self.grid.clone();
        for (ix, sp) in next.iter_mut().enumerate() {
            let neighbors = self.count_neighbors(ix);
            if neighbors == 1 || (*sp == Space::Empty && neighbors == 2) {
                // println!(
                //     "  Setting {}:{:?} with {} neighbors to Bug",
                //     ix, *sp, neighbors
                // );
                *sp = Space::Bug;
            } else {
                // println!(
                //     "  Setting {}:{:?} with {} neighbors to Empty",
                //     ix, *sp, neighbors
                // );
                *sp = Space::Empty;
            }
        }

        self.grid = next;
    }

    pub fn biodiversity(&self) -> i64 {
        let mut bd: i64 = 0;
        for (ix, sp) in self.grid.iter().copied().enumerate() {
            if sp == Space::Empty {
                continue;
            }
            bd += 1 << (ix as usize);
        }

        bd
    }

    pub fn process_until_repeat(&mut self) {
        let mut seen: HashSet<i64> = HashSet::new();
        seen.insert(self.biodiversity());

        loop {
            self.next_minute();
            let bd = self.biodiversity();
            if seen.contains(&bd) {
                return;
            }
            seen.insert(bd);
        }
    }
}

impl FromStr for Area {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut width = 0;
        let mut spaces = Vec::new();

        for line in s.lines() {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }

            let row = parse_row(trimmed)?;
            if width == 0 {
                width = row.len();
            } else if width != row.len() {
                return Err(anyhow::anyhow!(
                    "Non-matching widths: {} != {}",
                    width,
                    row.len()
                ));
            }

            spaces.extend_from_slice(&row);
        }

        Ok(Area {
            width,
            grid: spaces,
        })
    }
}

fn parse_row(s: &str) -> anyhow::Result<Vec<Space>> {
    s.chars()
        .map(|c| match c {
            '.' => Ok(Space::Empty),
            '#' => Ok(Space::Bug),
            c => Err(anyhow::anyhow!("Could not parse character {}", c)),
        })
        .collect()
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 24")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day24.txt");

    debug!("Using input {}", input_path);
    let mut file = File::open(input_path)?;
    let mut s: String = String::new();
    let read = file.read_to_string(&mut s)?;
    log::info!("Read {} bytes", read);

    let mut area = Area::from_str(&s)?;

    println!("Got Area. Biodiversity: {}", area.biodiversity());

    area.process_until_repeat();

    println!("After repeat, Biodiversity: {}", area.biodiversity());

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    #[allow(unused_imports)]
    use super::*;

    const STATE1: &str = r#"
        ....#
        #..#.
        #..##
        ..#..
        #....
    "#;

    const STATE2: &str = r#"
        #..#.
        ####.
        ###.#
        ##.##
        .##..
    "#;

    const STATE3: &str = r#"
        #####
        ....#
        ....#
        ...#.
        #.###
    "#;

    const STATE4: &str = r#"
        #....
        ####.
        ...##
        #.##.
        .##.#
    "#;

    const STATE5: &str = r#"
        ####.
        ....#
        ##..#
        .....
        ##...
    "#;

    #[test]
    fn test_evolution() -> anyhow::Result<()> {
        let mut initial = Area::from_str(STATE1)?;

        initial.next_minute();
        assert_eq!(initial, Area::from_str(STATE2)?);

        initial.next_minute();
        assert_eq!(initial, Area::from_str(STATE3)?);

        initial.next_minute();
        assert_eq!(initial, Area::from_str(STATE4)?);

        initial.next_minute();
        assert_eq!(initial, Area::from_str(STATE5)?);

        Ok(())
    }

    #[test]
    fn test_repeat() -> anyhow::Result<()> {
        let mut area = Area::from_str(STATE1)?;

        area.process_until_repeat();

        assert_eq!(area.biodiversity(), 2129920);

        Ok(())
    }
}
