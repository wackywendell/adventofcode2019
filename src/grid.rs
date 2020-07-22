use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;
use std::ops;

type Value = i64;

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
    Skip,
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

impl<T: Eq> Map<T> {
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

impl<T> std::iter::FromIterator<Token<T>> for Map<T>
where
    T: Hash + Eq + fmt::Debug,
    // <T as FromSequence<C>>::Error: std::error::Error + Sync + Send + 'static,
{
    // type Error = anyhow::Error;
    fn from_iter<I: IntoIterator<Item = Token<T>>>(iter: I) -> Self {
        let mut maxx = 0;
        let mut maxy = 0;
        let (mut x, mut y) = (0i64, 0i64);
        let mut grid = HashMap::new();

        for token in iter {
            let pos = Position(x, y);
            match token {
                Token::Skip => continue,
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
                }
            }

            if x > maxx {
                maxx = x;
            }

            x += 1;
        }

        Map {
            shape: (maxx, maxy),
            grid,
        }
    }
}

// impl<T> FromStr for Map<T>
// where
//     T: Hash + Eq + fmt::Debug,
//     T: TryFrom<(char, Position)>,
//     <T as TryFrom<(char, Position)>>::Error: std::error::Error + Sync + Send + 'static,
// {
//     type Err = anyhow::Error;

//     fn from_str(s: &str) -> anyhow::Result<Map<T>> {
//         let mut maxx = 0;
//         let mut maxy = 0;
//         let (mut x, mut y) = (0i64, 0i64);
//         let mut grid = HashMap::new();

//         for c in s.chars() {
//             let pos = Position(x, y);
//             match c {
//                 ' ' => continue,
//                 '\n' => {
//                     if x == 0 {
//                         continue;
//                     }
//                     maxy = y;
//                     y += 1;
//                     x = 0;
//                     continue;
//                 }
//                 c => {
//                     let square: T = TryFrom::try_from((c, pos))?;
//                     grid.insert(pos, square);
//                 }
//             }

//             if x > maxx {
//                 maxx = x;
//             }

//             x += 1;
//         }

//         Ok(Map {
//             shape: (maxx, maxy),
//             grid,
//         })
//     }
// }

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
