use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs::File;
use std::io::prelude::*;
use std::str::FromStr;

use clap::{App, Arg};
use log::debug;
use thiserror::Error;

use aoc::grid::{Compass, Map, Position, Token};

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Label(char, char);

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Square {
    Empty,
    Wall,
    PortalLabel(char),
    Portal(Label),
}

impl std::fmt::Display for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = match *self {
            Square::Empty => '.',
            Square::Wall => '#',
            Square::PortalLabel(c) => c,
            Square::Portal(_) => '-',
        };

        write!(f, "{}", c)
    }
}

pub struct Maze {
    pub map: Map<Square>,
    pub portals: HashMap<Label, (Position, Position)>,
    pub start: Position,
    pub end: Position,
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Unknown square type {0}")]
    BadCharacter(char),

    #[error("Only one portal found for {0}")]
    MissingExit(Label),

    #[error("Portal '{0}' found at 3 positions: {1}, {2}, and {3}")]
    ExtraExit(Label, Position, Position, Position),

    #[error("No start found")]
    MissingStart,

    #[error("Multiple starts found at {0} and {1}")]
    ExtraStart(Position, Position),

    #[error("No end found")]
    MissingEnd,

    #[error("Multiple ends found at {0} and {1}")]
    ExtraEnd(Position, Position),
}

impl TryFrom<Map<Square>> for Maze {
    type Error = ParseError;

    fn try_from(map: Map<Square>) -> Result<Self, ParseError> {
        let mut map = map;
        let mut pairs: HashMap<Label, (Position, Option<Position>)> = HashMap::new();
        let mut start = None;
        let mut end = None;

        for (&pos, &sq) in &map.grid {
            let c = if let Square::PortalLabel(c) = sq {
                c
            } else {
                continue;
            };

            let north = map.grid.get(&(pos + Compass::North)).copied();
            let south = map.grid.get(&(pos + Compass::South)).copied();
            let east = map.grid.get(&(pos + Compass::East)).copied();
            let west = map.grid.get(&(pos + Compass::West)).copied();

            let (dir, c_first, c_mid) = match (north, south, east, west) {
                (Some(Square::Empty), Some(Square::PortalLabel(c_south)), _, _) => {
                    (Compass::North, c, c_south)
                }
                (Some(Square::PortalLabel(c_north)), Some(Square::Empty), _, _) => {
                    (Compass::South, c_north, c)
                }
                (_, _, Some(Square::Empty), Some(Square::PortalLabel(c_west))) => {
                    (Compass::East, c_west, c)
                }
                (_, _, Some(Square::PortalLabel(c_east)), Some(Square::Empty)) => {
                    (Compass::West, c, c_east)
                }
                _ => {
                    debug!(
                        "Skipping {} at {}. N: {:?}, S: {:?}, E: {:?}, W: {:?}",
                        c, pos, north, south, east, west
                    );
                    continue;
                }
            };

            let portal_pos = pos + dir;
            let label = Label(c_first, c_mid);
            debug!("Found label {} at {}", label, portal_pos);

            if label == Label('A', 'A') {
                // This is the entrance to the maze
                start = match start {
                    None => Some(portal_pos),
                    Some(p) => return Err(ParseError::ExtraStart(p, portal_pos)),
                };
                continue;
            }

            if label == Label('Z', 'Z') {
                // This is the entrance to the maze
                end = match end {
                    None => Some(portal_pos),
                    Some(p) => return Err(ParseError::ExtraEnd(p, portal_pos)),
                };
                continue;
            }

            match pairs.entry(label) {
                Entry::Occupied(mut o) => {
                    let &(first, second) = o.get();
                    if let Some(second) = second {
                        return Err(ParseError::ExtraExit(label, first, second, portal_pos));
                    }
                    o.insert((first, Some(portal_pos)));
                }
                Entry::Vacant(v) => {
                    v.insert((portal_pos, None));
                }
            }
        }

        let start = start.ok_or(ParseError::MissingStart)?;
        let end = end.ok_or(ParseError::MissingEnd)?;

        let mut portals = HashMap::new();
        for (label, (first, maybe_second)) in pairs {
            let second = maybe_second.ok_or_else(|| ParseError::MissingExit(label))?;
            // Sort them for consistency
            let (first, second) = if first < second {
                (first, second)
            } else {
                (second, first)
            };
            portals.insert(label, (first, second));
            map.insert(first, Square::Portal(label));
            map.insert(second, Square::Portal(label));
        }

        Ok(Maze {
            map,
            portals,
            start,
            end,
        })
    }
}
impl FromStr for Maze {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let map = Map::from_str(s, |c| match c {
            ' ' => Ok(Token::Empty),
            '\n' => Ok(Token::NewRow),
            '#' => Ok(Token::Item(Square::Wall)),
            '.' => Ok(Token::Item(Square::Empty)),
            c @ 'A'..='Z' => Ok(Token::Item(Square::PortalLabel(c))),
            _ => Err(ParseError::BadCharacter(c)),
        })?;

        Maze::try_from(map)
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 20")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day20.txt");

    debug!("Using input {}", input_path);
    let mut file = File::open(input_path)?;
    let mut data = String::new();
    file.read_to_string(&mut data)?;

    let maze: Maze = str::parse(&data)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    use super::*;

    const EXAMPLE1: &str = r#"
         A
         A
  #######.#########
  #######.........#
  #######.#######.#
  #######.#######.#
  #######.#######.#
  #####  B    ###.#
BC...##  C    ###.#
  ##.##       ###.#
  ##...DE  F  ###.#
  #####    G  ###.#
  #########.#####.#
DE..#######...###.#
  #.#########.###.#
FG..#########.....#
  ###########.#####
             Z
             Z
    "#;

    const EXAMPLE2: &str = r#"
                   A
                   A
  #################.#############
  #.#...#...................#.#.#
  #.#.#.###.###.###.#########.#.#
  #.#.#.......#...#.....#.#.#...#
  #.#########.###.#####.#.#.###.#
  #.............#.#.....#.......#
  ###.###########.###.#####.#.#.#
  #.....#        A   C    #.#.#.#
  #######        S   P    #####.#
  #.#...#                 #......VT
  #.#.#.#                 #.#####
  #...#.#               YN....#.#
  #.###.#                 #####.#
DI....#.#                 #.....#
  #####.#                 #.###.#
ZZ......#               QG....#..AS
  ###.###                 #######
JO..#.#.#                 #.....#
  #.#.#.#                 ###.#.#
  #...#..DI             BU....#..LF
  #####.#                 #.#####
YN......#               VT..#....QG
  #.###.#                 #.###.#
  #.#...#                 #.....#
  ###.###    J L     J    #.#.###
  #.....#    O F     P    #.#...#
  #.###.#####.#.#####.#####.###.#
  #...#.#.#...#.....#.....#.#...#
  #.#####.###.###.#.#.#########.#
  #...#.#.....#...#.#.#.#.....#.#
  #.###.#####.###.###.#.#.#######
  #.#.........#...#.............#
  #########.###.###.#############
           B   J   C
           U   P   P
    "#;

    #[test]
    fn test_parse() -> anyhow::Result<()> {
        let maze: Maze = str::parse(EXAMPLE1)?;
        let expected_portals = vec![
            (Label('B', 'C'), (Position(2, 8), Position(9, 6))),
            (Label('D', 'E'), (Position(2, 13), Position(6, 10))),
            (Label('F', 'G'), (Position(2, 15), Position(11, 12))),
        ]
        .iter()
        .copied()
        .collect();
        assert_eq!(maze.portals, expected_portals);
        assert_eq!(maze.start, Position(9, 2));
        assert_eq!(maze.end, Position(13, 16));

        Ok(())
    }

    #[test]
    fn test_parse2() -> anyhow::Result<()> {
        let maze: Maze = str::parse(EXAMPLE2)?;
        assert_eq!(maze.portals.len(), 10);

        Ok(())
    }
}
