use std::collections::{HashSet, VecDeque};
use std::f64::consts::PI;
use std::fs::File;
use std::io::prelude::*;
use std::str::FromStr;

use clap::{App, Arg};
use log::debug;
use num::integer::gcd;

pub type Value = i64;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Point(Value, Value);

impl Point {
    pub fn degrees(self) -> f64 {
        let Point(x, y) = self;

        let ang0 = num::Float::atan2(-y as f64, x as f64) * 180. / PI;
        let mut ang = -ang0 + 90.;
        if ang < 0.0 {
            ang += 360.
        }
        log::info!("For ({}, {}): {} -> {}", x, y, ang0, ang);
        ang
        // let neg = match (x, y) {
        //     (0, y) => y > 0,
        //     (x, _) => x < 0,
        // };
        // let tan = (y as f64) / (x as f64);

        // (neg, tan)
    }

    pub fn direction(self, other: Self) -> (Self, Value) {
        let Point(dx, dy) = other - self;
        let g = gcd(dx, dy);

        log::info!(
            "Direction {:?} -> {:?}: ({},{}) -> {}",
            self,
            other,
            dx,
            dy,
            g,
        );

        (Point(dx / g, dy / g), g)
    }
}

impl std::ops::Sub for Point {
    type Output = Self;

    fn sub(self: Self, rhs: Self) -> Self {
        Point(self.0 - rhs.0, self.1 - rhs.1)
    }
}

pub struct Asteroids {
    locations: HashSet<Point>,
    laser: Point,
}

impl Asteroids {
    pub fn new<I: IntoIterator<Item = Point>>(iter: I, laser: Point) -> Self {
        let it = iter.into_iter();
        let mut locations = match it.size_hint() {
            (0, None) => HashSet::new(),
            (ln, None) => HashSet::with_capacity(ln),
            (_, Some(ln)) => HashSet::with_capacity(ln),
        };

        for Point(x, y) in it {
            locations.insert(Point(x, y));
        }

        Asteroids { locations, laser }
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

    pub fn place_laser(&mut self, pt: Point) {
        self.laser = pt;
        self.locations.remove(&pt);
    }

    pub fn laser_angle(&self, pt: Point) -> f64 {
        let dir = pt - self.laser;
        dir.degrees()
    }

    pub fn laser_order(&self) -> Vec<Point> {
        let mut in_order = Vec::with_capacity(self.locations.len());
        for &pt in &self.locations {
            let (min, cnt) = self.laser.direction(pt);
            log::info!(
                "Location ({}, {}) -> ({}, {}): Direction ({}, {}) ✖️ {}",
                self.laser.0,
                self.laser.1,
                pt.0,
                pt.1,
                min.0,
                min.1,
                cnt,
            );
            let ang = min.degrees();
            in_order.push((ang, cnt, min, pt));
        }

        in_order.sort_by(|a, b| a.partial_cmp(b).unwrap());
        let mut queue = VecDeque::from(in_order);
        let mut finished = Vec::with_capacity(self.locations.len());

        while !queue.is_empty() {
            let mut last = Point(0, 0);
            let mut next_queue = VecDeque::new();
            for (ang, cnt, min, pt) in queue {
                log::info!(
                    "{} Popped point ({}, {}), with angle {:.0} direction ({}, {}) ✖️ {}",
                    finished.len(),
                    pt.0,
                    pt.1,
                    ang,
                    min.0,
                    min.1,
                    cnt
                );

                if min == last {
                    next_queue.push_back((ang, cnt, min, pt));
                    continue;
                }

                last = min;
                finished.push(pt);
            }
            queue = next_queue;
        }

        finished
    }
}

impl FromStr for Asteroids {
    type Err = failure::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut y = 0;
        let mut points = Vec::new();
        let mut laser = None;

        for line in s.lines() {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            let (row_laser, pts) = parse_row(trimmed);
            if let Some(x) = row_laser {
                laser = Some(Point(x, y));
            }
            for x in pts {
                points.push(Point(x, y));
            }

            y += 1;
        }

        Ok(Asteroids::new(points, laser.unwrap_or_default()))
    }
}

pub fn parse_row(s: &str) -> (Option<Value>, Vec<Value>) {
    let trimmed = s.trim();
    let mut values = Vec::with_capacity(trimmed.len());
    let mut laser = None;

    for (n, c) in s.chars().enumerate() {
        match c {
            '.' => continue,
            '#' => values.push(n as Value),
            'X' => laser = Some(n as Value),
            _ => panic!("Unexpected character: {}", c),
        }
    }

    (laser, values)
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

    let mut asteroids = Asteroids::from_str(&s)?;
    let (Point(x, y), mx) = asteroids.max_visible();

    println!("Found maximum at ({}, {}): {}", x, y, mx);

    asteroids.place_laser(Point(x, y));
    let lased = asteroids.laser_order();
    let Point(x, y) = lased[199];

    // 247 is too low
    println!("200th Lased: ({}, {}) -> {}", x, y, x * 100 + y);

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    use super::*;

    #[test]
    fn test_tan() -> Result<(), failure::Error> {
        let ang = Point(0, -3).degrees();
        assert!((ang - 0.0) < 1e-8);

        let ang = Point(2, -2).degrees();
        assert!((ang - 45.) < 1e-8);

        let ang = Point(4, 0).degrees();
        assert!((ang - 90.) < 1e-8);

        let ang = Point(4, 4).degrees();
        assert!((ang - 135.) < 1e-8);

        let ang = Point(0, 4).degrees();
        assert!((ang - 180.) < 1e-8);

        let points_in_reverse = vec![
            Point(-1, -5),
            Point(-3, -1),
            Point(-4, 1),
            Point(-4, 11),
            Point(3, 8),
            Point(3, 1),
            Point(3, -1),
            Point(1, -6),
        ];

        for p in &points_in_reverse {
            log::info!("{:?}: {:.1}", p, p.degrees());
        }

        let mut points_in_order = points_in_reverse.clone();
        points_in_order.reverse();

        let mut sorted = points_in_reverse.clone();
        sorted.sort_by(|a, b| a.degrees().partial_cmp(&b.degrees()).unwrap());

        assert_eq!(sorted, points_in_order);
        assert_ne!(sorted, points_in_reverse);

        Ok(())
    }

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
        let asteroids2 = Asteroids::new(locations, Point(0, 0));

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
        let asteroids = Asteroids::new(locations, Point(0, 0));

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

    const EXAMPLE6: &str = r#"
        .#....#####...#..
        ##...##.#####..##
        ##...#...#.#####.
        ..#.....X...###..
        ..#.#.....#....##
    "#;

    #[test]
    fn test_laser() -> Result<(), failure::Error> {
        let asteroids = Asteroids::from_str(EXAMPLE6)?;

        let lased = asteroids.laser_order();

        assert_eq!(asteroids.laser, Point(8, 3));

        assert_eq!(lased.len(), asteroids.locations.len());

        assert_eq!(lased[0], Point(8, 1));
        assert_eq!(lased[1], Point(9, 0));
        assert_eq!(lased[2], Point(9, 1));
        assert_eq!(lased[3], Point(10, 0));
        assert_eq!(lased[4], Point(9, 2));

        Ok(())
    }

    #[test]
    fn test_laser_big() -> Result<(), failure::Error> {
        let mut asteroids = Asteroids::from_str(EXAMPLE5)?;
        asteroids.place_laser(Point(11, 13));

        let lased = asteroids.laser_order();

        assert_eq!(asteroids.laser, Point(11, 13));

        assert_eq!(lased.len(), asteroids.locations.len());

        assert_eq!(lased[0], Point(11, 12));
        assert_eq!(lased[2 - 1], Point(12, 1));
        assert_eq!(lased[3 - 1], Point(12, 2));
        assert_eq!(lased[10 - 1], Point(12, 8));
        assert_eq!(lased[20 - 1], Point(16, 0));
        assert_eq!(lased[50 - 1], Point(16, 9));
        assert_eq!(lased[100 - 1], Point(10, 16));
        assert_eq!(lased[199 - 1], Point(9, 6));
        assert_eq!(lased[200 - 1], Point(8, 2));
        assert_eq!(lased[201 - 1], Point(10, 9));
        assert_eq!(lased[299 - 1], Point(11, 1));

        Ok(())
    }
}
