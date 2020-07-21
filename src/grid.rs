use std::collections::{hash_map::Entry, HashMap, HashSet};
use std::convert::TryFrom;
use std::fmt;
use std::hash::Hash;
use std::ops;
use std::str::FromStr;

type Value = i64;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Position(Value, Value);

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

pub struct Grid<T> {
    pub shape: (Value, Value),
    pub grid: HashMap<Position, T>,
}

impl<T: TryFrom<char>> FromStr for Grid<T>
where
    <T as TryFrom<char>>::Error: std::error::Error + Sync + Send + 'static,
{
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Grid<T>> {
        let mut maxx = 0;
        let mut maxy = 0;
        let (mut x, mut y) = (0i64, 0i64);

        let mut grid = HashMap::new();

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
                c => {
                    let place = T::try_from(c)?;
                    grid.insert(Position(x, y), place);
                }
            }

            if x > maxx {
                maxx = x;
            }

            x += 1;
        }

        Ok(Grid {
            shape: (maxx, maxy),
            grid,
        })
    }
}

pub enum Structure {
    Passable,
    Blocked,
}

pub enum Square<Special> {
    Passable,
    Blocked,
    PassableSpecial(Special),
    BlockedSpecial(Special),
}

pub struct Map<T: Hash + Eq> {
    pub shape: (Value, Value),
    pub grid: HashSet<Position>,
    pub specials: HashMap<T, Position>,
}

impl<T: Hash + Eq + fmt::Debug> Map<T> {
    fn insert_special(&mut self, s: T, pos: Position) -> anyhow::Result<()> {
        match self.specials.entry(s) {
            Entry::Occupied(e) => Err(anyhow::format_err!(
                "Found {:?} at {} and {}",
                e.key(),
                e.get(),
                pos
            )),
            Entry::Vacant(e) => {
                e.insert(pos);
                Ok(())
            }
        }
    }
}

impl<T> FromStr for Map<T>
where
    T: Hash + Eq + fmt::Debug,
    Square<T>: TryFrom<(char, Position)>,
    <Square<T> as TryFrom<(char, Position)>>::Error: std::error::Error + Sync + Send + 'static,
{
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Map<T>> {
        let mut maxx = 0;
        let mut maxy = 0;
        let (mut x, mut y) = (0i64, 0i64);

        let mut map = Map {
            shape: (0, 0),
            grid: HashSet::new(),
            specials: HashMap::new(),
        };

        for c in s.chars() {
            let pos = Position(x, y);
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
                c => {
                    let place: Square<T> = TryFrom::try_from((c, pos))?;
                    match place {
                        Square::Blocked => continue,
                        Square::Passable => {
                            map.grid.insert(pos);
                        }
                        Square::BlockedSpecial(s) => {
                            map.insert_special(s, pos)?;
                        }
                        Square::PassableSpecial(s) => {
                            map.grid.insert(pos);
                            map.insert_special(s, pos)?;
                        }
                    }
                }
            }

            if x > maxx {
                maxx = x;
            }

            x += 1;
        }

        map.shape = (maxx, maxy);

        Ok(map)
    }
}
