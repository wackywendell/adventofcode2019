use std::cmp::{Ord, Ordering, PartialOrd, Reverse};
use std::collections::{hash_map::Entry, BTreeSet, BinaryHeap, HashMap, HashSet};
use std::convert::TryFrom;
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
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
    since_collected: HashSet<Square>,
    seen: Vec<Square>,
}

impl Progress {
    fn key<'a>(&'a self) -> impl Ord + 'a {
        // The less distance gone, and the more collected, the better
        (
            Reverse(self.distance),
            self.collected.len(),
            self.square,
            Reverse(self.since_collected.len()),
            &self.seen,
        )
    }
}

impl Ord for Progress {
    fn cmp(&self, other: &Self) -> Ordering {
        self.key().cmp(&other.key())
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
            path: p.seen,
            dist: p.distance,
        }
    }
}

impl<'a> Distances<'a> {
    pub fn shortest(&mut self) -> Option<ShortestPath> {
        let initial = Progress {
            distance: 0,
            square: Square::Entrance,
            collected: BTreeSet::new(),
            since_collected: HashSet::new(),
            seen: Vec::new(),
        };

        let mut queue = BinaryHeap::from(vec![initial]);

        log::info!("--- Starting shortest ---");

        let mut i = 0;

        while let Some(p) = queue.pop() {
            let graph_key = (p.square, p.collected.clone());
            match self.shortests.entry(graph_key) {
                Entry::Occupied(o) if o.get().dist <= p.distance => {
                    // log::info!(
                    //     "Skipping {:?}, distance {}, collected {:?}",
                    //     p.square,
                    //     p.distance,
                    //     p.collected,
                    // );
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
                for p in queue.iter().take(20) {
                    let mut collected: Vec<char> = p.collected.iter().copied().collect();
                    collected.sort();
                    let collected: String = collected.iter().copied().collect();
                    let path: String = p.seen.iter().copied().map(char::from).collect();
                    log::warn!(
                        "    {} {}, collected {}, path {}",
                        char::from(p.square),
                        p.distance,
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
                if p.since_collected.contains(&next) {
                    // We've been to this square since the last time we
                    // collected a key. That is pointless.
                    continue;
                }
                let mut collected = p.collected.clone();
                let mut just_collected = false;
                if let Square::Key(c) = next {
                    if !collected.contains(&c) {
                        collected.insert(c);
                        just_collected = true;
                    }
                }

                let since_collected = if just_collected {
                    HashSet::new()
                } else {
                    let mut since = p.since_collected.clone();
                    since.insert(next);
                    since
                };

                let mut seen = p.seen.clone();
                seen.push(next);

                // if i % 1 == 0 {
                //     log::debug!(
                //         "Went from {:?} to {:?}, distance {} -> {}, collected {:?}",
                //         p.square,
                //         next,
                //         p.distance,
                //         p.distance + dist,
                //         collected
                //     );
                // }

                queue.push(Progress {
                    distance: p.distance + dist,
                    square: next,
                    collected,
                    since_collected,
                    seen,
                });

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
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    for line in buf_reader.lines() {
        println!("{}", line?)
    }

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
        // Some progress
        // a -2> b -3> B
        let p1 = Progress {
            collected: vec!['a', 'b'].iter().copied().collect(),
            distance: 5,
            square: Square::Door('B'),
            seen: vec!['a', 'b', 'B']
                .iter()
                .map(|&c| Square::try_from(c).unwrap())
                .collect(),
            since_collected: HashSet::new(), // I'm ignoring this in this test
        };

        // More progress
        // a -2> b -3> B -5> c
        let p2 = Progress {
            collected: vec!['a', 'b', 'c'].iter().copied().collect(),
            distance: 10,
            square: Square::Key('c'),

            seen: vec!['a', 'b', 'B', 'c']
                .iter()
                .map(|&c| Square::try_from(c).unwrap())
                .collect(),
            since_collected: HashSet::new(), // I'm ignoring this in this test
        };

        // Less progress
        // a -2> b -3> A -2> B
        let p3 = Progress {
            collected: vec!['a', 'b'].iter().copied().collect(),
            distance: 7,
            square: Square::Door('B'),

            seen: vec!['a', 'b', 'A', 'B']
                .iter()
                .map(|&c| Square::try_from(c).unwrap())
                .collect(),
            since_collected: HashSet::new(), // I'm ignoring this in this test
        };

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
