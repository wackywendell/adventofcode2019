use std::cmp::Reverse;
use std::collections::hash_map::Entry;
use std::collections::{BinaryHeap, HashMap};
use std::convert::TryFrom;
use std::fs::File;
use std::io::prelude::*;
use std::str::FromStr;

use clap::{App, Arg};
use log::debug;
use thiserror::Error;

use aoc::grid::{Compass, Map, Position, Token};

type Value = i64;

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
    Entrance,
    Exit,
}

impl std::fmt::Display for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = match *self {
            Square::Empty => '.',
            Square::Wall => '#',
            Square::PortalLabel(c) => c,
            Square::Portal(_) => '-',
            Square::Entrance => '+',
            Square::Exit => '+',
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Route {
    total_distance: Value,

    pub start: Position,
    // Distance walked to a portal; portal gone through; is_outer
    pub legs: Vec<(Value, Label, bool)>,
    // Distance to the end after last portal; Level; position at end
    pub end: (Value, Value, Position),
}

impl std::fmt::Display for Route {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Route starting at {}:\n", self.start)?;

        let mut level = 0;
        for &(d, label, is_outer) in &self.legs {
            let portal_str = if is_outer {
                level -= 1;
                "outer"
            } else {
                level += 1;
                "inner"
            };
            write!(
                f,
                "  Stepped {} to {} {} ({})\n",
                d, portal_str, label, level
            )?;
        }

        let (d, elevel, exit) = self.end;
        write!(
            f,
            "  Stepped {} to Exit at ({}=={}) {}\n",
            d, level, elevel, exit
        )?;
        write!(f, "Total Distance: {}", self.total_distance)
    }
}

impl Route {
    fn start(position: Position) -> Self {
        Route {
            total_distance: 0,
            start: position,
            legs: Vec::new(),
            end: (0, 0, position),
        }
    }

    fn step(&mut self, direction: Compass) {
        let (d, lvl, pos) = self.end;
        self.total_distance += 1;
        self.end = (d + 1, lvl, pos + direction);
    }

    fn jump(&mut self, position: Position, label: Label, level_change: Value) {
        let (d, lvl, _) = self.end;
        self.total_distance += 1;
        self.legs.push((d, label, level_change < 0));
        self.end = (0, lvl + level_change, position);
    }
}

impl Maze {
    pub fn shortest_route(&self, recursive: bool) -> anyhow::Result<Route> {
        let start = Route::start(self.start);

        // Maps (Position, Level) -> Dist
        let mut seen: HashMap<(Position, Value), Value> = HashMap::new();
        let mut queue: BinaryHeap<Reverse<Route>> = BinaryHeap::new();
        queue.push(Reverse(start));

        while let Some(Reverse(next)) = queue.pop() {
            let (_, level, pos) = next.end;
            if pos == self.end && (!recursive || level == 0) {
                // We've made it to the end!
                return Ok(next);
            }

            let seen_key = if recursive { (pos, level) } else { (pos, 0) };
            match seen.entry(seen_key) {
                Entry::Vacant(v) => {
                    v.insert(next.total_distance);
                }
                Entry::Occupied(mut o) => {
                    let previous_distance = *o.get();
                    if previous_distance <= next.total_distance {
                        // We've been here before, and with a shorter route
                        log::info!(
                            "Skipping {}:{}, been here before ({})",
                            pos,
                            next.total_distance,
                            previous_distance
                        );
                        continue;
                    }
                    o.insert(next.total_distance);
                }
            }

            for &d in &Compass::all() {
                let new_pos = pos + d;
                let new_route = match self.map.get(new_pos).copied() {
                    None => return Err(anyhow::format_err!("Somehow we got to the edge {}", pos)),
                    Some(Square::Wall) => continue,
                    Some(Square::PortalLabel(_)) => {
                        // If we just popped out of a portal, then there should
                        // be an label next to us
                        continue;
                    }
                    Some(Square::Empty) => {
                        let mut new = next.clone();
                        new.step(d);
                        new
                    }
                    Some(Square::Entrance) => continue,
                    Some(Square::Exit) => {
                        if recursive && level != 0 {
                            // If we are in a recursive maze, and the level is
                            // not 0, we can't use the exit.
                            continue;
                        }
                        let mut new = next.clone();
                        new.step(d);
                        return Ok(new);
                    }
                    Some(Square::Portal(label)) => {
                        let &(outer, inner) = self
                            .portals
                            .get(&label)
                            .ok_or_else(|| anyhow::format_err!("Can't find portal {}", label))?;
                        let (jumped_pos, level_change) = match new_pos {
                            p if outer == p => (inner, -1),
                            p if inner == p => (outer, 1),
                            _ => {
                                return Err(anyhow::format_err!(
                                    "Stepped to {} at portal {}, but it has edges {}-{}",
                                    new_pos,
                                    label,
                                    outer,
                                    inner
                                ));
                            }
                        };

                        if recursive && level + level_change < 0 {
                            // Can't use an outer portal at level 0
                            continue;
                        } else if recursive && level + level_change > 50 {
                            continue;
                        }

                        let mut new = next.clone();
                        new.step(d);
                        new.jump(jumped_pos, label, level_change);
                        new
                    }
                };

                log::info!(
                    "Stepping {} -> {} ({}): {:?}",
                    pos,
                    new_route.end.1,
                    new_route.total_distance,
                    new_route,
                );
                queue.push(Reverse(new_route));
            }
        }

        Err(anyhow::format_err!("No end to be found!"))
    }
}

// struct MazeRoute<'a, 'b> {
//     maze: &'a Maze,
//     route: &'b Route,
// }

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

    #[error("Portal {0} does not map outer <-> inner: found at {1} and {2}")]
    BadPair(Label, Position, Position),
}

impl TryFrom<Map<Square>> for Maze {
    type Error = ParseError;

    fn try_from(map: Map<Square>) -> Result<Self, ParseError> {
        let mut map = map;
        let mut pairs: HashMap<Label, (Position, Option<Position>)> = HashMap::new();
        let mut start = None;
        let mut end = None;

        let mut wall_max = (0, 0);

        for (&pos, &sq) in &map.grid {
            if sq == Square::Wall {
                let (mx, my) = wall_max;
                let Position(x, y) = pos;
                wall_max = (x.max(mx), y.max(my));
                continue;
            }
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
        map.insert(start, Square::Entrance);
        let end = end.ok_or(ParseError::MissingEnd)?;
        map.insert(end, Square::Exit);

        let is_outer = |p| {
            let Position(x, y) = p;
            let (mx, my) = wall_max;

            x == 2 || y == 2 || x == mx || y == my
        };

        let mut portals = HashMap::new();
        for (label, (first, maybe_second)) in pairs {
            let second = maybe_second.ok_or_else(|| ParseError::MissingExit(label))?;

            let (outer, inner) = if is_outer(first) && !is_outer(second) {
                (first, second)
            } else if !is_outer(first) && is_outer(second) {
                (second, first)
            } else {
                log::warn!(
                    "Bad Pair {}: {}:{} {}:{}, walls:({}, {})",
                    label,
                    first,
                    is_outer(first),
                    second,
                    is_outer(second),
                    wall_max.0,
                    wall_max.1,
                );
                return Err(ParseError::BadPair(label, first, second));
            };
            portals.insert(label, (outer, inner));
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

    println!("--- Part One ---");
    let route = maze.shortest_route(false)?;
    println!("Started at {}", route.start);
    for &(d, label, is_outer) in &route.legs {
        println!(
            "  Stepped {} to {} {}",
            d,
            if is_outer { "outer" } else { "inner" },
            label
        );
    }

    let (d, _, exit) = route.end;
    println!("  Stepped {} to Exit at {}", d, exit);
    println!("Total Distance: {}", route.total_distance);

    println!("--- Part Two ---");
    let route = maze.shortest_route(true)?;
    println!("{}", route);

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::iter::FromIterator;

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
    fn test_parse1() -> anyhow::Result<()> {
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

    #[test]
    fn test_shortest1() -> anyhow::Result<()> {
        let maze: Maze = str::parse(EXAMPLE1)?;
        let shortest = maze.shortest_route(false)?;

        let expected = (
            vec![
                (4, Label('B', 'C'), false),
                (6, Label('D', 'E'), false),
                (4, Label('F', 'G'), true),
            ],
            (6, 1, maze.end),
        );
        assert_eq!((shortest.legs, shortest.end), expected);

        assert_eq!(shortest.total_distance, 23);

        // Part Two
        let shortest = maze.shortest_route(true)?;
        assert_eq!(shortest.total_distance, 26);

        Ok(())
    }

    #[test]
    fn test_shortest2() -> anyhow::Result<()> {
        let maze: Maze = str::parse(EXAMPLE2)?;
        let shortest = maze.shortest_route(false)?;

        let expected = (
            vec![
                Label('A', 'S'),
                Label('Q', 'G'),
                Label('B', 'U'),
                Label('J', 'O'),
            ],
            (maze.end),
        );

        let found = Vec::from_iter(shortest.legs.iter().map(|&(_, l, _)| l));
        assert_eq!((found, shortest.end.2), expected);

        assert_eq!(shortest.total_distance, 58);

        // Part Two
        let shortest = maze.shortest_route(true);
        if let Ok(ref route) = shortest {
            log::warn!("shortest route:\n{}", route);
        }
        assert!(shortest.is_err());

        Ok(())
    }
}
