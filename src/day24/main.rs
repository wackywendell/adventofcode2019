use std::collections::{BTreeMap, HashSet};
use std::fmt::{self, Write};
use std::fs::File;
use std::io::prelude::*;
use std::iter::FromIterator;
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
    fn empty(width: usize) -> Self {
        let grid = (0..width * width).map(|_| Space::Empty).collect();
        Area { width, grid }
    }

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

    fn count_local_neighbors(&self, n: usize) -> usize {
        let n = n as isize;
        let w = self.width as isize;
        let middle = w * w / 2;
        let dns = [-w, -1, w, 1];
        dns.iter()
            .copied()
            .map(|dn| {
                let newn = n as isize + dn;
                #[allow(clippy::if_same_then_else)]
                if newn < 0 || newn >= self.grid.len() as isize {
                    // println!("Neighbor {}:{}: outside", n, newn);
                    0
                } else if newn == middle {
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

    fn bug_to_num(s: Space) -> usize {
        if s == Space::Bug {
            1usize
        } else {
            0
        }
    }

    // Number of "Bugs" on the edges of the inner box
    // (top, bottom, left, right)
    fn middle_counts(&self) -> (usize, usize, usize, usize) {
        let w = self.width;
        let middle = w * w / 2;
        (
            Area::bug_to_num(self.grid[middle - w]),
            Area::bug_to_num(self.grid[middle + w]),
            Area::bug_to_num(self.grid[middle - 1]),
            Area::bug_to_num(self.grid[middle + 1]),
        )
    }

    // Number of "Bugs" on the outer edges
    // (top, bottom, left, right)
    fn edge_counts(&self) -> (usize, usize, usize, usize) {
        fn bug_counter<I: Iterator<Item = usize>>(area: &Area, rng: I) -> usize {
            rng.map(|ix| Area::bug_to_num(area.grid[ix])).sum::<usize>()
        }

        let w = self.width;
        // let top = self.grid[0..w].iter().copied().map(bug_count).sum();
        let top = bug_counter(self, 0..w);
        let bottom = bug_counter(self, (w * (w - 1))..w * w);
        let left = bug_counter(self, (0..w).map(|n| n * w));
        let right = bug_counter(self, (0..w).map(|n| (n + 1) * w - 1));

        (top, bottom, left, right)
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

    pub fn next_minute_recursive(&self, inner: Option<&Area>, outer: Option<&Area>) -> Area {
        let mut next = self.grid.clone();
        let w = self.width;
        let middle = w * w / 2;

        let (it, ib, il, ir) = inner.map_or((0, 0, 0, 0), Area::edge_counts);
        let (ot, ob, ol, or) = outer.map_or((0, 0, 0, 0), Area::middle_counts);

        for (ix, sp) in next.iter_mut().enumerate() {
            if ix == middle {
                *sp = Space::Empty;
                continue;
            }

            let mut neighbors = self.count_local_neighbors(ix);
            if ix < w {
                // outer top
                neighbors += ot;
            }
            if ix >= w * w - w {
                neighbors += ob;
            }
            if ix % w == 0 {
                neighbors += ol;
            }
            if ix % w == w - 1 {
                neighbors += or;
            }
            if ix == middle - w {
                neighbors += it;
            }
            if ix == middle + w {
                neighbors += ib;
            }
            if ix == middle - 1 {
                neighbors += il;
            }
            if ix == middle + 1 {
                neighbors += ir;
            }

            // let x = ix % w;
            // let y = ix / w;
            // println!("  {} ({}, {}): {} neighbors", ix, x, y, neighbors);

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

        Area {
            grid: next,
            width: self.width,
        }
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

    pub fn bug_count(&self) -> i64 {
        self.grid
            .iter()
            .copied()
            .map(|n| Area::bug_to_num(n) as i64)
            .sum()
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

impl fmt::Display for Area {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in 0..self.width {
            for col in 0..self.width {
                let c = match self.grid[row * self.width + col] {
                    Space::Bug => '#',
                    Space::Empty => '.',
                };

                f.write_char(c)?;
            }

            f.write_char('\n')?;
        }

        Ok(())
    }
}

fn parse_row(s: &str) -> anyhow::Result<Vec<Space>> {
    s.chars()
        .map(|c| match c {
            '.' => Ok(Space::Empty),
            '?' => Ok(Space::Empty),
            '#' => Ok(Space::Bug),
            c => Err(anyhow::anyhow!("Could not parse character {}", c)),
        })
        .collect()
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct RecursiveAreas {
    areas: BTreeMap<isize, Area>,
}

impl RecursiveAreas {
    pub fn new(area: Area) -> Self {
        RecursiveAreas {
            areas: BTreeMap::from_iter(vec![(0, area)]),
        }
    }

    pub fn next_minute(&mut self) {
        let lowest_level = self.areas.keys().next().copied().unwrap();
        let highest_level = self.areas.keys().next_back().copied().unwrap();
        let w = self.areas[&lowest_level].width;

        let mut previous = Area::empty(w);
        let new_lowest = previous.next_minute_recursive(Some(&self.areas[&lowest_level]), None);
        if new_lowest.grid.iter().any(|&s| s == Space::Bug) {
            self.areas.insert(lowest_level - 1, new_lowest);
        }

        for depth in lowest_level..=highest_level {
            let next = self.areas[&depth]
                .next_minute_recursive(self.areas.get(&(depth + 1)), Some(&previous));
            previous = self.areas.insert(depth, next).unwrap();
        }

        let new_highest = Area::empty(w).next_minute_recursive(None, Some(&previous));
        if new_highest.grid.iter().any(|&s| s == Space::Bug) {
            self.areas.insert(highest_level + 1, new_highest);
        }
    }

    pub fn bug_count(&self) -> i64 {
        self.areas.values().map(Area::bug_count).sum()
    }
}

impl fmt::Display for RecursiveAreas {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (&d, a) in &self.areas {
            writeln!(f, "Depth: {}", d)?;
            writeln!(f, "{}", a)?;
        }

        Ok(())
    }
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

    let initial = Area::from_str(&s)?;

    let mut area = initial.clone();

    println!("Got Area. Biodiversity: {}", area.biodiversity());

    area.process_until_repeat();

    println!("After repeat, Biodiversity: {}", area.biodiversity());

    let mut rec = RecursiveAreas::new(initial);

    for _ in 0..200 {
        rec.next_minute();
    }

    println!(
        "After 200 minutes: {} levels, {} bugs",
        rec.areas.len(),
        rec.bug_count()
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    fn from_strs(strs: &[(isize, &str)]) -> anyhow::Result<RecursiveAreas> {
        let areas = strs
            .iter()
            .map(|&(d, s)| (Area::from_str(s).map(|a| (d, a))))
            .collect::<Result<BTreeMap<isize, Area>, anyhow::Error>>()?;

        Ok(RecursiveAreas { areas })
    }

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

    #[test]
    fn test_recursive_steps() -> anyhow::Result<()> {
        const START_STATES: [(isize, &str); 2] = [
            (
                1,
                r#"
                #....
                .....
                ..?..
                .....
                ....#"#,
            ),
            (
                0,
                r#"
                .....
                ..#..
                .#?#.
                ..#..
                ....."#,
            ),
        ];

        const END_STATES: [(isize, &str); 2] = [
            (
                1,
                r#"
                .####
                #...#
                #.?.#
                #...#
                ####."#,
            ),
            (
                0,
                r#"
                ..#..
                .###.
                ##?##
                .###.
                ..#.."#,
            ),
        ];

        let mut rec = from_strs(&START_STATES)?;

        rec.next_minute();

        println!("{}", rec);

        let exp = from_strs(&END_STATES)?;
        assert_eq!(rec, exp);

        Ok(())
    }

    #[test]
    fn test_recursive_steps2() -> anyhow::Result<()> {
        const START_STATES: [(isize, &str); 2] = [
            (
                1,
                r#"
                .#.#.
                .....
                #.?.#
                .....
                .#.#."#,
            ),
            (
                0,
                r#"
                .....
                .....
                ..?..
                .....
                ....."#,
            ),
        ];

        const END_STATES: [(isize, &str); 2] = [
            (
                1,
                r#"
                #.#.#
                ##.##
                .#?#.
                ##.##
                #.#.#"#,
            ),
            (
                0,
                r#"
                .....
                ..#..
                .#?#.
                ..#..
                ....."#,
            ),
        ];

        let mut rec = from_strs(&START_STATES)?;

        rec.next_minute();

        println!("{}", rec);

        let exp = from_strs(&END_STATES)?;
        assert_eq!(rec, exp);

        Ok(())
    }

    const REC_STATES: [(isize, &str); 11] = [
        (
            -5,
            r#"
            ..#..
            .#.#.
            ..?.#
            .#.#.
            ..#.."#,
        ),
        (
            -4,
            r#"
            ...#.
            ...##
            ..?..
            ...##
            ...#."#,
        ),
        (
            -3,
            r#"
            #.#..
            .#...
            ..?..
            .#...
            #.#.."#,
        ),
        (
            -2,
            r#"
            .#.##
            ....#
            ..?.#
            ...##
            .###."#,
        ),
        (
            -1,
            r#"
            #..##
            ...##
            ..?..
            ...#.
            .####"#,
        ),
        (
            0,
            r#"
            .#...
            .#.##
            .#?..
            .....
            ....."#,
        ),
        (
            1,
            r#"
            .##..
            #..##
            ..?.#
            ##.##
            #####"#,
        ),
        (
            2,
            r#"
            ###..
            ##.#.
            #.?..
            .#.##
            #.#.."#,
        ),
        (
            3,
            r#"
            ..###
            .....
            #.?..
            #....
            #...#"#,
        ),
        (
            4,
            r#"
            .###.
            #..#.
            #.?..
            ##.#.
            ....."#,
        ),
        (
            5,
            r#"
            ####.
            #..#.
            #.?#.
            ####.
            ....."#,
        ),
    ];

    #[test]
    fn test_recursive_example() -> anyhow::Result<()> {
        let area = Area::from_str(STATE1)?;
        assert_eq!(area.middle_counts(), (0, 1, 0, 1));
        assert_eq!(area.edge_counts(), (1, 1, 3, 2));

        let mut rec = RecursiveAreas::new(area);

        for _ in 0..10 {
            rec.next_minute();
        }

        assert_eq!(rec.areas.len(), 11);

        let expected_areas = from_strs(&REC_STATES)?;

        println!("=== Expected ===\n{}", expected_areas);
        println!("=== Found ===\n{}", rec);

        assert_eq!(rec, expected_areas);
        assert_eq!(rec.bug_count(), 99);

        Ok(())
    }
}
