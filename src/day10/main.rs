use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::str::FromStr;

use clap::{App, Arg};
use log::debug;
use num::integer::gcd;

pub type Value = i64;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Point(Value, Value);

impl std::ops::Sub for Point {
    type Output = Self;

    fn sub(self: Self, rhs: Self) -> Self {
        Point(self.0 - rhs.0, self.1 - rhs.1)
    }
}

pub struct Asteroids {
    locations: HashSet<Point>,
    // max: Point,
}

impl Asteroids {
    pub fn new<I: IntoIterator<Item = Point>>(iter: I) -> Self {
        let it = iter.into_iter();
        let mut locations = match it.size_hint() {
            (0, None) => HashSet::new(),
            (ln, None) => HashSet::with_capacity(ln),
            (_, Some(ln)) => HashSet::with_capacity(ln),
        };

        // let (mut mx, mut my) = (0, 0);
        for Point(x, y) in it {
            // if x > mx {
            //     mx = x;
            // }
            // if y > my {
            //     my = y;
            // }
            locations.insert(Point(x, y));
        }

        Asteroids {
            locations,
            // max: Point(mx, my),
        }
    }

    pub fn visible(&self, pt: Point) -> isize {
        let mut directions = HashSet::new();
        let mut visible = 0;

        for &cp in &self.locations {
            if cp == pt {
                continue;
            }
            let Point(dx, dy) = cp - pt;
            let g = gcd(dx, dy);

            let dir = (dx / g, dy / g);
            if directions.contains(&dir) {
                // Either this asteroid isn't visible,
                // or one we already counted as visible isn't
                continue;
            }
            visible += 1;
            directions.insert(dir);
        }

        visible
    }

    pub fn max_visible(&self) -> (Point, isize) {
        let mut mx = None;

        for &pt in &self.locations {
            let vis = self.visible(pt);
            mx = Some(match mx {
                None => (pt, vis),
                Some((_, mv)) if vis > mv => (pt, vis),
                Some((mp, mv)) => (mp, mv),
            });
        }

        mx.unwrap()
    }
}

impl FromStr for Asteroids {
    type Err = failure::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut y = 0;
        let mut points = Vec::new();

        for line in s.lines() {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            for x in parse_row(trimmed) {
                points.push(Point(x, y));
            }

            y += 1;
        }

        Ok(Asteroids::new(points))
    }
}

pub fn parse_row(s: &str) -> Vec<Value> {
    let trimmed = s.trim();
    let mut values = Vec::with_capacity(trimmed.len());

    for (n, c) in s.chars().enumerate() {
        match c {
            '.' => continue,
            '#' => values.push(n as Value),
            _ => panic!("Unexpected character: {}", c),
        }
    }

    values
}

fn main() -> Result<(), failure::Error> {
    env_logger::init();

    let matches = App::new("Day 10")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day10.txt");

    debug!("Using input {}", input_path);
    let mut file = File::open(input_path)?;
    let mut s: String = String::new();
    let read = file.read_to_string(&mut s)?;
    log::info!("Read {} bytes", read);

    let asteroids = Asteroids::from_str(&s)?;
    let (Point(x, y), mx) = asteroids.max_visible();

    println!("Found maximum at ({}, {}): {}", x, y, mx);

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    use super::*;

    #[test]
    fn test_num() -> Result<(), failure::Error> {
        assert_eq!(gcd(30, 21), 3);
        assert_eq!(gcd(30, -21), 3);
        assert_eq!(gcd(-30, -21), 3);

        Ok(())
    }

    const EXAMPLE1: &str = r#"
        .#..#
        .....
        #####
        ....#
        ...##
    "#;

    const EXAMPLE2: &str = r#"
        ......#.#.
        #..#.#....
        ..#######.
        .#.#.###..
        .#..#.....
        ..#....#.#
        #..#....#.
        .##.#..###
        ##...#..#.
        .#....####
    "#;

    const EXAMPLE3: &str = r#"
        #.#...#.#.
        .###....#.
        .#....#...
        ##.#.#.#.#
        ....#.#.#.
        .##..###.#
        ..#...##..
        ..##....##
        ......#...
        .####.###.
    "#;

    const EXAMPLE4: &str = r#"
        .#..#..###
        ####.###.#
        ....###.#.
        ..###.##.#
        ##.##.#.#.
        ....###..#
        ..#.#..#.#
        #..#.#.###
        .##...##.#
        .....#.#..
    "#;

    const EXAMPLE5: &str = r#"
        .#..##.###...#######
        ##.############..##.
        .#.######.########.#
        .###.#######.####.#.
        #####.##.#.##.###.##
        ..#####..#.#########
        ####################
        #.####....###.#.#.##
        ##.#################
        #####.##.###..####..
        ..######..##.#######
        ####.##.####...##..#
        .#####..#.######.###
        ##...#.##########...
        #.##########.#######
        .####.#.###.###.#.##
        ....##.##.###..#####
        .#.#.###########.###
        #.#.#.#####.####.###
        ###.##.####.##.#..##
    "#;

    #[test]
    fn test_parse() -> Result<(), failure::Error> {
        let locations = vec![
            Point(1, 0),
            Point(4, 0),
            Point(0, 2),
            Point(1, 2),
            Point(2, 2),
            Point(3, 2),
            Point(4, 2),
            Point(4, 3),
            Point(3, 4),
            Point(4, 4),
        ];

        let asteroids1 = Asteroids::from_str(EXAMPLE1)?;
        let asteroids2 = Asteroids::new(locations);

        assert_eq!(asteroids1.locations, asteroids2.locations);

        Ok(())
    }

    #[test]
    fn test_visible() -> Result<(), failure::Error> {
        let locations = vec![
            Point(1, 0),
            Point(4, 0),
            Point(0, 2),
            Point(1, 2),
            Point(2, 2),
            Point(3, 2),
            Point(4, 2),
            Point(4, 3),
            Point(3, 4),
            Point(4, 4),
        ];
        let asteroids = Asteroids::new(locations);

        let visibilities = vec![
            (Point(1, 0), 7),
            (Point(4, 0), 7),
            (Point(0, 2), 6),
            (Point(1, 2), 7),
            (Point(2, 2), 7),
            (Point(3, 2), 7),
            (Point(4, 2), 5),
            (Point(4, 3), 7),
            (Point(3, 4), 8),
            (Point(4, 4), 7),
        ];

        for &(pt, count) in &visibilities {
            assert_eq!(asteroids.visible(pt), count);
        }

        Ok(())
    }

    #[test]
    fn test_more_visible() -> Result<(), failure::Error> {
        let asteroids2 = Asteroids::from_str(EXAMPLE2)?;
        let (pt, mx) = asteroids2.max_visible();
        assert_eq!((pt, mx), (Point(5, 8), 33));

        let asteroids3 = Asteroids::from_str(EXAMPLE3)?;
        let (pt, mx) = asteroids3.max_visible();
        assert_eq!((pt, mx), (Point(1, 2), 35));

        let asteroids4 = Asteroids::from_str(EXAMPLE4)?;
        let (pt, mx) = asteroids4.max_visible();
        assert_eq!((pt, mx), (Point(6, 3), 41));

        let asteroids5 = Asteroids::from_str(EXAMPLE5)?;
        let (pt, mx) = asteroids5.max_visible();
        assert_eq!((pt, mx), (Point(11, 13), 210));

        Ok(())
    }
}
