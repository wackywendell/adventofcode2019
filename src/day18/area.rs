use std::cmp::{Ord, Ordering, PartialOrd, Reverse};
use std::collections::{hash_map::Entry, BinaryHeap, HashMap, HashSet};
use std::convert::TryFrom;
use std::fmt;
use std::str::FromStr;
use thiserror::Error;

use crate::grid::{Map, Token};

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
    SubEntrance(u8),
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
            Square::SubEntrance(n) => (b'0' + n) as char,
            Square::Key(c) => c,
            Square::Door(c) => c,
        }
    }
}

#[derive(Clone)]
pub struct Distances<'a> {
    area: &'a Area,
    pub distances: HashMap<Square, HashMap<Square, Value>>,
    shortests: HashMap<(Square, Vec<char>), ShortestPath>,
    shortests4: HashMap<([Square; 4], Vec<char>), ShortestPath>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct Progress {
    distance: Value,
    square: Square,
    collected: Vec<char>,
    path: Vec<Square>,
}

impl Progress {
    fn start<S: Into<Square>>(location: S) -> Self {
        let sq = location.into();
        let mut start = Progress {
            distance: 0,
            square: sq,
            collected: Default::default(),
            path: vec![sq],
        };
        if let Square::Key(c) = sq {
            start.collected.push(c);
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
            new.collected.push(c);
            new.collected.sort_unstable();
            new.collected.dedup();
        }

        new
    }

    fn ordering_key(&self) -> impl Ord + '_ {
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

#[derive(Clone, Eq, PartialEq, Debug)]
struct Progress4 {
    distance: Value,
    // None when it is finished
    square: [Square; 4],
    collected: Vec<char>,
    path: Vec<Square>,
    current_bot: Option<usize>,
}

impl Progress4 {
    fn start(location: [Square; 4]) -> Self {
        let mut start = Progress4 {
            distance: 0,
            square: location,
            collected: Default::default(),
            path: location.to_vec(),
            current_bot: None,
        };
        for &sq in &location {
            if let Square::Key(c) = sq {
                start.collected.push(c);
            }
        }
        start
    }

    fn step(&self, ix: usize, location: Square, dist: Value) -> Self {
        let mut new = Progress4 {
            distance: self.distance + dist,
            square: self.square,
            collected: self.collected.clone(),
            path: self.path.clone(),
            current_bot: Some(ix),
        };
        new.square[ix] = location;
        new.path.push(location);

        if let Square::Key(c) = location {
            match new.collected.binary_search(&c) {
                Ok(_) => {}
                Err(cix) => {
                    // This is a new key, so the current_bot can change
                    new.collected.insert(cix, c);
                    new.current_bot = None;
                }
            }
        }

        new
    }

    fn ordering_key(&self) -> impl Ord + '_ {
        // The less distance gone, and the more collected, the better
        (
            Reverse(self.distance),
            self.collected.len(),
            self.square,
            &self.path,
        )
    }
}

impl Ord for Progress4 {
    fn cmp(&self, other: &Self) -> Ordering {
        self.ordering_key().cmp(&other.ordering_key())
    }
}

impl PartialOrd for Progress4 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for Progress4 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Progress4[{} ", self.distance)?;

        for &loc in &self.square {
            write!(f, "{}", char::from(loc))?;
        }

        write!(f, " (")?;
        for &c in &self.collected {
            write!(f, "{}", c)?;
        }
        write!(f, "): ")?;

        for &sq in &self.path {
            write!(f, "{}", char::from(sq))?;
        }
        write!(f, "]")
    }
}

#[derive(Clone, Debug)]
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

impl From<Progress4> for ShortestPath {
    fn from(p: Progress4) -> Self {
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
            }
        }

        None
    }

    fn step4(&mut self, p: &Progress4, ix: usize, next: Square, dist: Value) -> Option<Progress4> {
        let p = p.step(ix, next, dist);
        let graph_key = (p.square, p.collected.clone());
        match self.shortests4.entry(graph_key) {
            Entry::Occupied(o) if o.get().dist <= p.distance => {
                // log::info!("Skipping {}", p,);
                return None;
            }
            Entry::Occupied(mut o) => {
                o.insert(p.clone().into());
            }
            Entry::Vacant(e) => {
                e.insert(p.clone().into());
            }
        }

        Some(p)
    }

    pub fn shortest_subs(&mut self) -> Option<ShortestPath> {
        let initial = Progress4::start([
            Square::SubEntrance(0),
            Square::SubEntrance(1),
            Square::SubEntrance(2),
            Square::SubEntrance(3),
        ]);

        let mut queue = BinaryHeap::from(vec![initial]);

        let mut progress_counter = 0;
        while let Some(p) = queue.pop() {
            progress_counter += 1;
            log::debug!("{}: Queue size {}: {}", progress_counter, queue.len(), p);

            if p.collected.len() == self.area.keys.len() {
                return Some(p.into());
            }

            let ixs = match p.current_bot {
                Some(ix) => vec![ix],
                None => vec![0, 1, 2, 3],
            };

            for ix in ixs {
                let current_square = p.square[ix];

                // log::debug!("  Trying {}", i
                let possibilities: Vec<(Square, Value)> = self
                    .distances
                    .get(&current_square)
                    .unwrap()
                    .iter()
                    .map(|(&s, &d)| (s, d))
                    .collect();
                for &(next, dist) in &possibilities {
                    // log::debug!("    {} -> {}", char::from(p.square[ix]), char::from(next));
                    if let Square::Door(d) = next {
                        if !p.collected.contains(&d.to_ascii_lowercase()) {
                            // log::debug!("      No key, skipping");
                            // We've made it to a door, but have no keys. That is not progress.
                            continue;
                        }
                    };

                    if let Some(next_progress) = self.step4(&p, ix, next, dist) {
                        queue.push(next_progress);
                    }
                }
            }
        }

        None
    }
}

#[derive(Clone)]
pub struct Area {
    pub map: Map<Square>,
    pub keys: HashSet<char>,
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

        let mut map = Map::from_str(s, tokenize)?;

        let mut entrances: Vec<_> = map
            .grid
            .iter()
            .filter_map(|(&p, &sq)| {
                if sq == Square::Entrance {
                    Some(p)
                } else {
                    None
                }
            })
            .collect();

        if entrances.len() > 1 {
            // Multiple entrances, so label them as sub-entrances
            entrances.sort();
            for (ix, &p) in entrances.iter().enumerate() {
                map.grid.insert(p, Square::SubEntrance(ix as u8));
            }
        }

        Ok(Area { map, keys })
    }
}

impl Area {
    pub fn split_entrance(&mut self) {
        let entrances: Vec<_> = self
            .map
            .grid
            .iter()
            .filter_map(|(&p, &sq)| {
                if sq == Square::Entrance {
                    Some(p)
                } else {
                    None
                }
            })
            .collect();
        if entrances.len() > 1 {
            panic!("Found multiple entrances: {:?}", entrances);
        }
        if entrances.is_empty() {
            panic!("No entrance found");
        }

        let entrance_position = entrances[0];

        let changes: [((Value, Value), Square); 9] = [
            ((-1, -1), Square::SubEntrance(0)),
            ((-1, 0), Square::Wall),
            ((-1, 1), Square::SubEntrance(1)),
            ((0, -1), Square::Wall),
            ((0, 0), Square::Wall),
            ((0, 1), Square::Wall),
            ((1, -1), Square::SubEntrance(2)),
            ((1, 0), Square::Wall),
            ((1, 1), Square::SubEntrance(3)),
        ];

        for &(dxy, sq) in &changes {
            self.map.grid.insert(entrance_position + dxy, sq);
        }
    }

    pub fn distances(&self) -> Distances {
        let mut starts = Vec::with_capacity(self.keys.len() * 2 + 1);

        for (&pos, &item) in &self.map.grid {
            match item {
                Square::Entrance => {}
                Square::SubEntrance(_) => {}
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
                // We can go straight through empty square
                Square::Empty => true,
                // No passing through walls
                Square::Wall => false,
                // Entrances, SubEntrances, Keys, and Doors are destinations so
                // we don't "pass" through them
                Square::Entrance => false,
                Square::SubEntrance(_) => false,
                Square::Key(_) => false,
                Square::Door(_) => false,
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
            area: self,
            shortests: HashMap::new(),
            shortests4: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use test_log::test;

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
    fn test_progress_order() {
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

    const EXAMPLE4_1: &str = r#"
        #######
        #a.#Cd#
        ##...##
        ##.@.##
        ##...##
        #cB#Ab#
        #######
    "#;

    const EXAMPLE4_2: &str = r#"
        ###############
        #d.ABC.#.....a#
        ######@#@######
        ###############
        ######@#@######
        #b.....#.....c#
        ###############
    "#;

    #[test]
    fn test_shortest4_easy() -> anyhow::Result<()> {
        let mut area = Area::from_str(EXAMPLE4_1)?;
        area.split_entrance();
        println!("area:\n{}", area.map);

        let shortest = area.distances().shortest_subs().unwrap();

        println!("{}", shortest);
        assert_eq!(shortest.dist, 8);

        let area = Area::from_str(EXAMPLE4_2)?;
        let shortest = area.distances().shortest_subs().unwrap();

        println!("{}", shortest);
        assert_eq!(shortest.dist, 24);

        Ok(())
    }

    const EXAMPLE4_3: &str = r#"
        #############
        #DcBa.#.GhKl#
        #.###@#@#I###
        #e#d#####j#k#
        ###C#@#@###J#
        #fEbA.#.FgHi#
        #############
    "#;

    const EXAMPLE4_4: &str = r#"
        #############
        #g#f.D#..h#l#
        #F###e#E###.#
        #dCba@#@BcIJ#
        #############
        #nK.L@#@G...#
        #M###N#H###.#
        #o#m..#i#jk.#
        #############
    "#;

    #[test]
    fn test_shortest4_more() -> anyhow::Result<()> {
        let area = Area::from_str(EXAMPLE4_3)?;
        let shortest = area.distances().shortest_subs().unwrap();
        println!("{}", shortest);
        assert_eq!(shortest.dist, 32);

        let area = Area::from_str(EXAMPLE4_4)?;
        let shortest = area.distances().shortest_subs().unwrap();

        println!("{}", shortest);
        assert_eq!(shortest.dist, 72);

        Ok(())
    }
}
