use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::num::ParseIntError;
use std::str::FromStr;

use clap::{App, Arg};
use log::debug;

use aoc::parse::parse_err_iter;

pub type Val = i64;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Point(Val, Val);

impl Point {
    pub fn manhattan(self) -> Val {
        let Point(x, y) = self;
        x.abs() + y.abs()
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Direction {
    Vertical,
    Horizontal,
}

impl Direction {
    fn other(self) -> Self {
        match self {
            Direction::Vertical => Direction::Horizontal,
            Direction::Horizontal => Direction::Vertical,
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Section(Direction, Val);

impl std::ops::Add<Section> for Point {
    type Output = Self;

    fn add(self, sec: Section) -> Self {
        let Point(x, y) = self;
        match sec {
            Section(Direction::Vertical, v) => Point(x, y + v),
            Section(Direction::Horizontal, v) => Point(x + v, y),
        }
    }
}

pub enum SectionError {
    Direction(char),
    Int(ParseIntError),
}

impl fmt::Display for SectionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SectionError::Direction(c) => write!(f, "Error seeing direction {}", c),
            SectionError::Int(e) => write!(f, "ParseIntError: {}", e),
        }
    }
}

impl fmt::Debug for SectionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SectionError::Direction(c) => write!(f, "Error seeing direction {}", c),
            SectionError::Int(e) => write!(f, "ParseIntError: {}", e),
        }
    }
}

impl std::error::Error for SectionError {}

impl FromStr for Section {
    type Err = SectionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut chars = s.chars();

        let (direction, mul) = match chars.next() {
            Some('U') => (Direction::Vertical, 1),
            Some('D') => (Direction::Vertical, -1),
            Some('R') => (Direction::Horizontal, 1),
            Some('L') => (Direction::Horizontal, -1),
            None => return Err(SectionError::Direction('␀')),
            Some(c) => return Err(SectionError::Direction(c)),
        };

        match str::parse::<Val>(chars.as_str()) {
            Ok(n) => Ok(Section(direction, n * mul)),
            Err(e) => Err(SectionError::Int(e)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Sections(Vec<Section>);

impl FromStr for Sections {
    type Err = SectionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pieces = s.trim().split(',');

        let v: Result<Vec<Section>, SectionError> = pieces.map(str::parse).collect();

        Ok(Sections(v?))
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Piece {
    direction: Direction,
    line: Val,
    start: Val,
    end: Val,
    wire: usize,
    // Length so far
    length: Val,
}

impl Piece {
    fn new(start: Point, section: Section, wire: usize, length: Val) -> Self {
        let Point(x, y) = start;
        let Section(direction, dist) = section;

        let (line, n0) = if direction == Direction::Vertical {
            (x, y)
        } else {
            (y, x)
        };

        let (start, end) = (n0, n0 + dist);

        Piece {
            direction,
            line,
            start,
            end,
            wire,
            length,
        }
    }

    fn lowest(self) -> Val {
        if self.start < self.end {
            self.start
        } else {
            self.end
        }
    }

    fn highest(self) -> Val {
        if self.start < self.end {
            self.end
        } else {
            self.start
        }
    }

    fn intersection(self, other: Self) -> Option<Point> {
        if self.line < other.lowest() || self.line > other.highest() {
            return None;
        }
        if other.line < self.lowest() || other.line > self.highest() {
            return None;
        }

        Some(if self.direction == Direction::Vertical {
            Point(self.line, other.line)
        } else {
            Point(other.line, self.line)
        })
    }

    fn points(self) -> (Point, Point) {
        match self.direction {
            Direction::Horizontal => (Point(self.start, self.line), Point(self.end, self.line)),
            Direction::Vertical => (Point(self.line, self.start), Point(self.line, self.end)),
        }
    }
}

pub struct Wires {
    pieces: Vec<Piece>,
    _wires: Vec<Sections>,
}

impl Wires {
    pub fn new(wires: Vec<Sections>) -> Wires {
        let mut pieces = Vec::with_capacity(wires.len());

        for (wix, Sections(v)) in wires.iter().enumerate() {
            let mut loc = Point(0, 0);
            let mut dist = 0;
            for &sec in v {
                let piece = Piece::new(loc, sec, wix, dist);
                pieces.push(piece);
                log::debug!(
                    "Pushing piece {:?} -> {:?}, {}: {:?}",
                    piece.points().0,
                    piece.points().1,
                    dist,
                    piece,
                );
                loc = loc + sec;
                let Section(_, sd) = sec;
                dist += sd.abs();
            }
        }

        pieces.sort();

        Wires {
            _wires: wires,
            pieces,
        }
    }

    pub fn intersections(&self) -> Vec<(Piece, Piece)> {
        let mut pairs = Vec::new();

        if self.pieces.is_empty() {
            return vec![];
        }

        let d = self.pieces[0].direction;
        let split_res = self
            .pieces
            .binary_search_by_key(&(d.other(), 0), |p| (p.direction, 1));
        let split = match split_res {
            Ok(ix) => ix,
            Err(ix) => ix,
        };

        if split >= self.pieces.len() {
            return vec![];
        }

        let (sec1, sec2) = self.pieces.split_at(split);

        for &p in sec1 {
            let start_res =
                sec2.binary_search_by_key(&(p.lowest(), p.line), |p2| (p2.line, p2.lowest()));
            let start_ix = match start_res {
                Ok(ix) => ix,
                Err(ix) => ix,
            };
            if start_ix >= sec2.len() {
                continue;
            }

            for &p2 in &sec2[start_ix..] {
                if p2.lowest() > p.line || p2.highest() < p.line {
                    continue;
                }
                if p2.line > p.highest() {
                    break;
                }

                if p.wire == p2.wire {
                    continue;
                }

                let intersec = if p.direction == Direction::Vertical {
                    Point(p.line, p2.line)
                } else {
                    Point(p2.line, p.line)
                };

                if intersec != Point(0, 0) {
                    log::debug!("Found intersection: {:?} - {:?} -> {:?}", p, p2, intersec);
                    log::debug!("    {:?} - {:?}", p.points(), p2.points());
                } else {
                    log::debug!(
                        "Found zero intersection: {:?} - {:?} -> {:?}",
                        p,
                        p2,
                        intersec
                    );
                    log::debug!("    {:?} - {:?}", p.points(), p2.points());
                    continue;
                }

                pairs.push((p, p2));
            }
        }

        pairs
    }

    pub fn shortest_intersection(&self) -> Option<(Val, Point)> {
        let inters = self.intersections();
        let points = inters.iter().filter_map(|op| (op.0.intersection(op.1)));
        points.map(|p| (p.manhattan(), p)).min()
    }

    pub fn fastest_intersection(&self) -> Option<(Val, Point)> {
        let mut inters = self.intersections();

        let f = |p0: Piece, p1: Piece| {
            p0.length + p1.length + (p1.line - p0.start).abs() + (p0.line - p1.start).abs()
        };

        inters.sort_by_key(|&(p0, p1)| f(p0, p1));
        for (n, &(p0, p1)) in inters.iter().enumerate().take(5) {
            log::debug!(
                "{} ({}): {:?}, {:?}; lengths sum {}, dists {}, {}",
                n,
                f(p0, p1),
                p0,
                p1,
                p0.length + p1.length,
                p0.line - p1.start,
                p1.line - p0.start
            );
        }

        let points = inters.iter().filter_map(|op| {
            (op.0.intersection(op.1).map(|p| {
                (
                    op.0.length
                        + op.1.length
                        + (op.1.line - op.0.start).abs()
                        + (op.0.line - op.1.start).abs(),
                    p,
                )
            }))
        });
        points.min()
    }
}

fn main() -> Result<(), failure::Error> {
    env_logger::init();

    let matches = App::new("Day 3")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day3.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let parsed: Vec<Sections> = parse_err_iter(buf_reader.lines())?;
    let wires = Wires::new(parsed);

    let (dist, Point(x, y)) = wires.shortest_intersection().unwrap();
    println!("Shortest: ({}, {}) -> {}", x, y, dist);

    let (dist, Point(x, y)) = wires.fastest_intersection().unwrap();
    println!("Fastest: ({}, {}) -> {}", x, y, dist);

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    use super::*;

    #[test]
    fn test_intersections() -> Result<(), failure::Error> {
        let w0 = Sections::from_str("R8,U5,L5,D3")?;
        let exp = Sections(vec![
            Section(Direction::Horizontal, 8),
            Section(Direction::Vertical, 5),
            Section(Direction::Horizontal, -5),
            Section(Direction::Vertical, -3),
        ]);
        assert_eq!(w0, exp);

        let w1 = Sections::from_str("U7,R6,D4,L4")?;
        let exp = Sections(vec![
            Section(Direction::Vertical, 7),
            Section(Direction::Horizontal, 6),
            Section(Direction::Vertical, -4),
            Section(Direction::Horizontal, -4),
        ]);
        assert_eq!(w1, exp);
        let wires = Wires::new(vec![w0, w1]);
        // R8,U5,L5,D3
        let mut pieces = vec![
            Piece {
                direction: Direction::Horizontal,
                line: 0,
                start: 0,
                end: 8,
                wire: 0,
                length: 0,
            },
            Piece {
                direction: Direction::Vertical,
                line: 8,
                start: 0,
                end: 5,
                wire: 0,
                length: 8,
            },
            Piece {
                direction: Direction::Horizontal,
                line: 5,
                start: 8,
                end: 3,
                wire: 0,
                length: 13,
            },
            Piece {
                direction: Direction::Vertical,
                line: 3,
                start: 5,
                end: 2,
                wire: 0,
                length: 18,
            },
            // U7,R6,D4,L4
            Piece {
                direction: Direction::Vertical,
                line: 0,
                start: 0,
                end: 7,
                wire: 1,
                length: 0,
            },
            Piece {
                direction: Direction::Horizontal,
                line: 7,
                start: 0,
                end: 6,
                wire: 1,
                length: 7,
            },
            Piece {
                direction: Direction::Vertical,
                line: 6,
                start: 7,
                end: 3,
                wire: 1,
                length: 13,
            },
            Piece {
                direction: Direction::Horizontal,
                line: 3,
                start: 6,
                end: 2,
                wire: 1,
                length: 17,
            },
        ];

        assert_eq!(wires.pieces.len(), pieces.len());

        pieces.sort();

        for (i, (&p0, &p1)) in pieces.iter().zip(&wires.pieces).enumerate() {
            assert_eq!((i, p0), (i, p1));
        }

        assert_eq!(wires.pieces, pieces);

        let i = wires.shortest_intersection();
        assert_eq!(i, Some((6, Point(3, 3))));

        Ok(())
    }

    #[test]
    fn test_more_wires() -> Result<(), failure::Error> {
        let wires = Wires::new(vec![
            Sections::from_str("R75,D30,R83,U83,L12,D49,R71,U7,L72")?,
            Sections::from_str("U62,R66,U55,R34,D71,R55,D58,R83")?,
        ]);
        let s = wires.shortest_intersection().map(|p| p.0);
        assert_eq!(s, Some(159));

        let wires = Wires::new(vec![
            Sections::from_str("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")?,
            Sections::from_str("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")?,
        ]);
        let s = wires.shortest_intersection().map(|p| p.0);
        assert_eq!(s, Some(135));

        Ok(())
    }

    #[test]
    fn test_fastest() -> Result<(), failure::Error> {
        let wires = Wires::new(vec![
            Sections::from_str("R8,U5,L5,D3")?,
            Sections::from_str("U7,R6,D4,L4")?,
        ]);
        let s = wires.fastest_intersection().map(|p| p.0);
        assert_eq!(s, Some(30));

        let wires = Wires::new(vec![
            Sections::from_str("R75,D30,R83,U83,L12,D49,R71,U7,L72")?,
            Sections::from_str("U62,R66,U55,R34,D71,R55,D58,R83")?,
        ]);
        let s = wires.fastest_intersection().map(|p| p.0);
        assert_eq!(s, Some(610));

        let wires = Wires::new(vec![
            Sections::from_str("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")?,
            Sections::from_str("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")?,
        ]);
        let s = wires.fastest_intersection().map(|p| p.0);
        assert_eq!(s, Some(410));

        Ok(())
    }
}
