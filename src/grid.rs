use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fmt;
use std::hash::Hash;
use std::ops;

type Value = i64;

// Note to self: Days 3, 10, 13, 15, and 18 are on grids.
// 15 and 18 are maps.
// 12 is 3D, not really a grid.

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Position(pub Value, pub Value);

impl fmt::Display for Position {
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

impl Compass {
    pub const fn all() -> [Compass; 4] {
        [Compass::North, Compass::East, Compass::South, Compass::West]
    }
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
impl ops::Add<Compass> for Position {
    type Output = Self;

    fn add(self: Self, rhs: Compass) -> Self {
        let Position(x, y) = self;
        match rhs {
            Compass::North => Position(x, y - 1),
            Compass::South => Position(x, y + 1),
            Compass::East => Position(x + 1, y),
            Compass::West => Position(x - 1, y),
        }
    }
}
impl ops::Add<Turn> for Compass {
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

pub enum Token<T> {
    Ignore,
    Empty,
    NewRow,
    Item(T),
}

pub trait FromSequence<C>: Sized {
    type Error;

    fn to_token(c: C) -> Result<Token<Self>, Self::Error>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Map<T> {
    pub shape: (Value, Value),
    pub grid: HashMap<Position, T>,
}

impl<T> Default for Map<T> {
    // Derived default requires T: Default; this manual impl does not
    fn default() -> Self {
        Map {
            shape: Default::default(),
            grid: Default::default(),
        }
    }
}

impl<T> Map<T> {
    pub fn get(&self, pos: Position) -> Option<&T> {
        self.grid.get(&pos)
    }
    pub fn insert(&mut self, pos: Position, item: T) -> Option<T> {
        let Position(x, y) = pos;
        if x > self.shape.0 {
            self.shape = (x, self.shape.1);
        }
        if y > self.shape.1 {
            self.shape = (self.shape.0, y);
        }
        self.grid.insert(pos, item)
    }
}

impl<T> std::iter::FromIterator<Token<T>> for Map<T> {
    fn from_iter<I: IntoIterator<Item = Token<T>>>(iter: I) -> Self {
        let mut maxx = 0;
        let mut maxy = 0;
        let (mut x, mut y) = (0i64, 0i64);
        let mut grid = HashMap::new();

        for token in iter {
            let pos = Position(x, y);
            match token {
                Token::Ignore => continue,
                Token::Empty => {
                    x += 1;
                }
                Token::NewRow => {
                    if x == 0 {
                        continue;
                    }
                    maxy = y;
                    y += 1;
                    x = 0;
                    continue;
                }
                Token::Item(item) => {
                    grid.insert(pos, item);
                    if x > maxx {
                        maxx = x;
                    }

                    x += 1;
                }
            }
        }

        Map {
            shape: (maxx, maxy),
            grid,
        }
    }
}

pub struct Distances<'a, T> {
    map: &'a Map<T>,
    // Queue of (distance, place) of places not yet visited or gone beyond
    queue: BinaryHeap<(Reverse<Value>, Position)>,
    seen: HashSet<Position>,
}

pub enum Found<'a, T> {
    Item(Value, Position, &'a T),
    Unknown(Value, Position),
    None,
}

impl<'a, T> Distances<'a, T> {
    pub fn new(start: Position, map: &'a Map<T>) -> Self {
        let queue = BinaryHeap::from(vec![(Reverse(0i64), start)]);
        Distances {
            map,
            queue,
            seen: Default::default(),
        }
    }

    pub fn next<F>(&mut self, passable: F) -> Found<'a, T>
    where
        F: Fn(&'a T) -> bool,
    {
        let (Reverse(dist), pos) = match self.queue.pop() {
            None => return Found::None,
            Some(sq) => sq,
        };
        let item = match self.map.get(pos) {
            None => return Found::Unknown(dist, pos),
            Some(it) => it,
        };

        if passable(item) {
            for &dir in &Compass::all() {
                let new_pos = pos + dir;
                if self.seen.contains(&new_pos) {
                    continue;
                }
                self.queue.push((Reverse(dist + 1), new_pos));
            }
        }

        self.seen.insert(pos);
        Found::Item(dist, pos, item)
    }
}

pub enum Structure {
    Passable,
    Blocked,
}

pub struct Grid {
    pub shape: (Value, Value),
    pub grid: HashSet<Position>,
}

pub trait SquareAttributes {
    fn is_passable(&self) -> bool;
    fn is_item(&self) -> bool;
}

impl<T: Eq + Hash + SquareAttributes> From<Map<T>> for (Grid, Vec<(T, Position)>) {
    fn from(map: Map<T>) -> Self {
        let mut grid = Grid {
            shape: map.shape,
            grid: HashSet::new(),
        };
        let mut items = Vec::new();

        for (pos, square) in map.grid {
            if square.is_passable() {
                grid.grid.insert(pos);
            }
            if square.is_item() {
                items.push((square, pos));
            }
        }

        (grid, items)
    }
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    use super::*;

    // Adapted from day 15
    const EXAMPLE1: &'static str = r#"
    -##---
    #..##-
    #.#..#
    #.O.#-
    -###--
    "#;

    enum ShipSquare {
        Empty,
        Wall,
        OxygenSystem,
    }

    #[test]
    fn test_map() -> anyhow::Result<()> {
        let token_stream = EXAMPLE1.chars().map(|c| {
            Ok(match c {
                ' ' => Token::Ignore,
                '-' => Token::Empty,
                '\n' => Token::NewRow,
                '#' => Token::Item(ShipSquare::Wall),
                '.' => Token::Item(ShipSquare::Empty),
                'O' => Token::Item(ShipSquare::OxygenSystem),
                _ => return Err(anyhow::format_err!("Unexpected char {:?}", c)),
            })
        });

        let maybe_map: Result<Map<ShipSquare>, anyhow::Error> = token_stream.collect();
        let map = maybe_map?;

        assert_eq!(map.shape, (5, 4));

        Ok(())
    }
}
