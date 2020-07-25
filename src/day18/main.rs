use std::cmp::{Ord, Ordering, PartialOrd, Reverse};
use std::collections::{hash_map::Entry, BTreeSet, BinaryHeap, HashMap, HashSet};
use std::convert::TryFrom;
use std::fmt;
use std::str::FromStr;
use thiserror::Error;

use clap::{App, Arg};
use log::debug;

use aoc::grid::{Map, Token};

type Value = i64;

#[derive(Error, Debug)]
#[error("Unexpected character {}", character)]
pub struct InvalidChar {
    character: char,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Square {
    Wall,
    Empty,
    Entrance,
    Key(char),
    Door(char),
}

impl TryFrom<char> for Square {
    type Error = InvalidChar;

    fn try_from(character: char) -> Result<Square, InvalidChar> {
        Ok(match character {
            '#' => Square::Wall,
            '.' => Square::Empty,
            '@' => Square::Entrance,
            c if ('a'..='z').contains(&c) => Square::Key(c),
            c if ('A'..='Z').contains(&c) => Square::Door(c),
            _ => return Err(InvalidChar { character }),
        })
    }
}

impl Square {
    pub fn tokenize(character: char) -> Result<Token<Square>, InvalidChar> {
        Ok(match character {
            ' ' => Token::Ignore,
            '\n' => Token::NewRow,
            c => return Square::try_from(c).map(Token::Item),
        })
    }
}

impl From<Square> for char {
    fn from(sq: Square) -> char {
        match sq {
            Square::Wall => '#',
            Square::Empty => '.',
            Square::Entrance => '@',
            Square::Key(c) => c,
            Square::Door(c) => c,
        }
    }
}

pub struct Distances<'a> {
    area: &'a Area,
    pub distances: HashMap<Square, HashMap<Square, Value>>,
    pub shortests: HashMap<(Square, BTreeSet<char>), ShortestPath>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct Progress {
    distance: Value,
    square: Square,
    collected: BTreeSet<char>,
    path: Vec<Square>,
}

impl Progress {
    // fn keys_iter<'a>(&'a self) -> impl Iterator<Item = char> + 'a {
    //     self.path.iter().filter_map(|&sq| {
    //         if let Square::Key(c) = sq {
    //             Some(c)
    //         } else {
    //             None
    //         }
    //     })
    // }

    fn start<S: Into<Square>>(location: S) -> Self {
        let sq = location.into();
        let mut start = Progress {
            distance: 0,
            square: sq,
            collected: Default::default(),
            path: vec![sq],
        };
        if let Square::Key(c) = sq {
            start.collected.insert(c);
        };
        start
    }

    fn step<S: Into<Square>>(&self, location: S, dist: Value) -> Self {
        let sq = location.into();
        let mut new = Progress {
            distance: self.distance + dist,
            square: sq,
            collected: self.collected.clone(),
            path: self.path.clone(),
        };

        new.path.push(sq);
        if let Square::Key(c) = sq {
            new.collected.insert(c);
        }

        new
    }

    // fn keys(&self) -> Vec<char> {
    //     let mut collected: Vec<char> = self
    //         .path
    //         .iter()
    //         .filter_map(|&sq| {
    //             if let Square::Key(c) = sq {
    //                 Some(c)
    //             } else {
    //                 None
    //             }
    //         })
    //         .collect();
    //     collected.sort_unstable();
    //     collected.dedup();
    //     collected
    // }

    fn ordering_key<'a>(&'a self) -> impl Ord + 'a {
        // The less distance gone, and the more collected, the better
        (
            Reverse(self.distance),
            self.collected.len(),
            self.square,
            &self.path,
        )
    }
}

impl Ord for Progress {
    fn cmp(&self, other: &Self) -> Ordering {
        self.ordering_key().cmp(&other.ordering_key())
    }
}

impl PartialOrd for Progress {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub struct ShortestPath {
    pub path: Vec<Square>,
    pub dist: Value,
}

impl fmt::Display for ShortestPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ShortestPath[{}: ", self.dist)?;
        for &sq in &self.path {
            write!(f, "{}", char::from(sq))?;
        }
        write!(f, "]")
    }
}

impl From<Progress> for ShortestPath {
    fn from(p: Progress) -> Self {
        ShortestPath {
            path: p.path,
            dist: p.distance,
        }
    }
}

impl<'a> Distances<'a> {
    pub fn shortest(&mut self) -> Option<ShortestPath> {
        let initial = Progress::start(Square::Entrance);

        let mut queue = BinaryHeap::from(vec![initial]);

        log::info!("--- Starting shortest ---");

        let mut i = 0;

        while let Some(p) = queue.pop() {
            let graph_key = (p.square, p.collected.clone());
            match self.shortests.entry(graph_key) {
                Entry::Occupied(o) if o.get().dist <= p.distance => {
                    log::info!(
                        "Skipping {:?}, distance {}, collected {:?}",
                        p.square,
                        p.distance,
                        p.collected,
                    );
                    continue;
                }
                Entry::Occupied(mut o) => {
                    o.insert(p.clone().into());
                }
                Entry::Vacant(e) => {
                    e.insert(p.clone().into());
                }
            }

            i += 1;

            if i >= 400000 {
                log::warn!("Stopping after {} steps; queue length {}", i, queue.len());
                for queued in queue.iter().take(20) {
                    let collected: String = queued.collected.iter().copied().collect();
                    let path: String = queued.path.iter().copied().map(char::from).collect();
                    log::warn!(
                        "    {} {}, collected {}, path {}",
                        char::from(queued.square),
                        queued.distance,
                        collected,
                        path
                    )
                }
                break;
            }

            if p.collected.len() == self.area.keys.len() {
                return Some(p.into());
            }

            for (&next, &dist) in self.distances.get(&p.square).unwrap() {
                if let Square::Door(d) = next {
                    if !p.collected.contains(&d.to_ascii_lowercase()) {
                        // We've made it to a door, but have no keys. That is not progress.
                        continue;
                    }
                };

                queue.push(p.step(next, dist));

                // if i % 1 == 0 {
                //     log::debug!("  Queue: {:?}", queue);
                // }
            }
        }

        None
    }
}

pub struct Area {
    map: Map<Square>,
    keys: HashSet<char>,
}

impl FromStr for Area {
    type Err = InvalidChar;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut keys = HashSet::new();

        let tokenize = |c: char| {
            let token = Square::tokenize(c);
            if let Ok(Token::Item(Square::Key(c))) = token {
                keys.insert(c);
            }
            token
        };

        let map = Map::from_str(s, tokenize)?;

        Ok(Area { map, keys })
    }
}

impl Area {
    pub fn distances(&self) -> Distances {
        let mut starts = Vec::with_capacity(self.keys.len() * 2 + 1);

        for (&pos, &item) in &self.map.grid {
            match item {
                Square::Entrance => {}
                Square::Key(_) => {}
                Square::Door(_) => {}
                _ => continue,
            }

            starts.push((pos, item));
        }

        let mut distances = HashMap::new();
        for &(start_position, start_item) in &starts {
            let passable = |sq| match sq {
                s if s == start_item => true,
                Square::Entrance => false,
                Square::Key(_) => false,
                Square::Door(_) => false,
                Square::Empty => true,
                Square::Wall => false,
            };

            let mut this_map = HashMap::new();

            for loc in self.map.distances(start_position, |&sq| passable(sq)) {
                let &item = loc.item.expect("Walked off the edge of the map!");
                match item {
                    Square::Empty => continue,
                    Square::Wall => continue,
                    _ if item == start_item => continue,
                    _ => {}
                }

                this_map.insert(item, loc.distance);
            }

            distances.insert(start_item, this_map);
        }

        Distances {
            distances,
            area: &self,
            shortests: HashMap::new(),
        }
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 18")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day18.txt");

    debug!("Using input {}", input_path);
    // let file = File::open(input_path)?;
    // let buf_reader = BufReader::new(file);

    // let line = buf_reader
    //     .lines()
    //     .next()
    //     .ok_or_else(|| anyhow::format_err!("Expected a line in file"))??;

    let area: Area = std::fs::read_to_string(input_path)?.parse()?;

    // println!("Found Area: {}", area);

    let shortest = area.distances().shortest().unwrap();

    println!("-- Part One --");
    println!("Took path {}", shortest);

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    use super::*;

    const EXAMPLE1: &str = r#"
        #########
        #b.A.@.a#
        #########
    "#;

    const EXAMPLE2: &str = r#"
        ########################
        #f.D.E.e.C.b.A.@.a.B.c.#
        ######################.#
        #d.....................#
        ########################
    "#;

    #[test]
    fn test_parse() -> anyhow::Result<()> {
        let map = Map::from_str(EXAMPLE1, Square::tokenize)?;

        assert_eq!(map.shape, (9, 3));

        Ok(())
    }

    #[test]
    fn test_distances() -> anyhow::Result<()> {
        let area = Area::from_str(EXAMPLE1)?;
        let distances = area.distances();

        let all_dists: HashSet<(char, char, Value)> = distances
            .distances
            .iter()
            .flat_map(|(&sq1, m)| {
                let c1: char = sq1.into();

                m.iter().map(move |(&sq2, &d)| (c1, sq2.into(), d))
            })
            .collect();

        let expected_dists: HashSet<(char, char, Value)> = vec![
            ('b', 'A', 2),
            ('A', 'b', 2),
            ('A', '@', 2),
            ('@', 'A', 2),
            ('@', 'a', 2),
            ('a', '@', 2),
        ]
        .iter()
        .copied()
        .collect();

        assert_eq!(all_dists, expected_dists);

        Ok(())
    }

    #[test]
    fn test_progress_order() -> anyhow::Result<()> {
        let create = |start: char, steps: Vec<(Value, char)>| {
            let start = Square::try_from(start).unwrap();
            let mut p = Progress::start(start);
            for (d, c) in steps {
                let sq = Square::try_from(c).unwrap();
                p = p.step(sq, d);
            }
            p
        };

        // Some progress
        // a -2> b -3> B
        let p1 = create('a', vec![(2, 'b'), (2, 'B')]);

        // More progress
        // a -2> b -3> B -5> c
        let p2 = create('a', vec![(2, 'b'), (2, 'B'), (5, 'c')]);

        // Less progress
        // a -2> b -3> A -2> B
        let p3 = create('a', vec![(2, 'b'), (3, 'A'), (2, 'B')]);

        // Less distance covered needs to be expanded first
        assert!(p1 > p2);
        assert!(p1 > p3);
        assert!(p3 > p2);

        Ok(())
    }

    #[test]
    fn test_shortest_easy() -> anyhow::Result<()> {
        let area = Area::from_str(EXAMPLE1)?;
        let shortest = area.distances().shortest().unwrap();
        assert_eq!(shortest.dist, 8);

        let area = Area::from_str(EXAMPLE2)?;
        let shortest = area.distances().shortest().unwrap();

        assert_eq!(shortest.dist, 86);

        Ok(())
    }

    const EXAMPLE3: &str = r#"
        ########################
        #...............b.C.D.f#
        #.######################
        #.....@.a.B.c.d.A.e.F.g#
        ########################
    "#;

    const EXAMPLE4: &str = r#"
        #################
        #i.G..c...e..H.p#
        ########.########
        #j.A..b...f..D.o#
        ########@########
        #k.E..a...g..B.n#
        ########.########
        #l.F..d...h..C.m#
        #################
    "#;

    const EXAMPLE5: &str = r#"
        ########################
        #@..............ac.GI.b#
        ###d#e#f################
        ###A#B#C################
        ###g#h#i################
        ########################
    "#;

    #[test]
    fn test_shortest_more() -> anyhow::Result<()> {
        let area = Area::from_str(EXAMPLE3)?;
        let shortest = area.distances().shortest().unwrap();
        // let expected_path = vec!['b', 'a', 'c', 'd', 'f', 'e', 'g']
        //     .iter()
        //     .map(|&c| Square::try_from(c).unwrap())
        //     .collect();
        println!("{}", shortest);
        assert_eq!(shortest.dist, 132);

        let area = Area::from_str(EXAMPLE4)?;
        let shortest = area.distances().shortest().unwrap();
        assert_eq!(shortest.dist, 136);

        let area = Area::from_str(EXAMPLE5)?;
        let shortest = area.distances().shortest().unwrap();
        assert_eq!(shortest.dist, 81);

        Ok(())
    }
}
