use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::str::FromStr;

use clap::{App, Arg};
use log::debug;
use thiserror::Error;

use aoc::parse::parse_err_iter;

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Orbit {
    orbiter: String,
    center: String,
}

impl fmt::Display for Orbit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Orbit[{}({}]", self.orbiter, self.center)
    }
}

#[derive(Error, Debug)]
#[error("Orbit not found: {}", _0)]
pub struct OrbitNotFound(String);

impl FromStr for Orbit {
    type Err = OrbitNotFound;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pieces: Vec<_> = s.trim().splitn(2, ')').collect();

        if pieces.len() != 2 {
            log::warn!("Found {} pieces: {:?}", pieces.len(), pieces);
            return Err(OrbitNotFound(s.to_owned()));
        }

        Ok(Orbit {
            orbiter: pieces[1].to_owned(),
            center: pieces[0].to_owned(),
        })
    }
}

pub struct Orbits {
    // Orbiter -> Center
    orbiting: HashMap<String, String>,
    // Center -> [Orbiter]
    orbiters: HashMap<String, HashSet<String>>,
}

impl Orbits {
    pub fn depth_sum(&self) -> usize {
        let mut queue: VecDeque<(String, usize)> = VecDeque::new();
        let mut depths: HashMap<String, usize> = HashMap::with_capacity(self.orbiting.len());
        let mut depth_sum = 0;

        queue.push_back(("COM".to_owned(), 0));

        while let Some((next, d)) = queue.pop_front() {
            log::debug!("Found {} with depth {}", next, d);
            depths.insert(next.clone(), d);
            depth_sum += d;

            let deeper = match self.orbiters.get(&next) {
                None => continue,
                Some(v) => v,
            };

            for o in deeper {
                queue.push_back((o.to_string(), d + 1));
            }
        }

        depth_sum
    }

    pub fn parents(&self, object: &str) -> Vec<&str> {
        let mut v = Vec::new();

        let mut current = object;

        while let Some(parent) = self.orbiting.get(current) {
            v.push(parent.as_ref());
            current = parent;
        }

        v
    }

    pub fn transfer(&self, start: &str, end: &str) -> usize {
        let p1s = self.parents(start);
        let p2s = self.parents(end);

        let seen: HashSet<&str> = p1s.iter().copied().collect();

        let mut doubled = 0;
        for &v in &p2s {
            if seen.contains(v) {
                doubled += 1
            }
        }

        (p1s.len() - doubled) + (p2s.len() - doubled)
    }
}

impl From<&[Orbit]> for Orbits {
    fn from(slice: &[Orbit]) -> Self {
        let mut orbiting = HashMap::with_capacity(slice.len());
        let mut orbiters: HashMap<String, HashSet<String>> = HashMap::with_capacity(slice.len());
        for o in slice {
            orbiting.insert(o.orbiter.clone(), o.center.clone());
            let v = orbiters.entry(o.center.clone()).or_default();
            v.insert(o.orbiter.clone());
        }

        Orbits { orbiting, orbiters }
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 6")
        .arg(
            Arg::with_name("input")
                .short('i')
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day6.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let parsed: Vec<Orbit> = parse_err_iter(buf_reader.lines())?;
    let orbits = Orbits::from(parsed.as_ref());

    let d = orbits.depth_sum();

    println!("Found depth sum {}", d);

    let ts = orbits.transfer("YOU", "SAN");

    println!("Found {} transfers between YOU and SAN", ts);

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_log::test;

    use aoc::parse::parse_iter;

    use super::*;

    #[test]
    fn test_depths() -> Result<(), Box<dyn std::error::Error>> {
        let s = r#"
            COM)B
            B)C
            C)D
            D)E
            E)F
            B)G
            G)H
            D)I
            E)J
            J)K
            K)L"#;
        let parsed: Vec<Orbit> = parse_iter(s.lines())?;
        let orbits = Orbits::from(parsed.as_ref());
        let d = orbits.depth_sum();

        assert_eq!(d, 42);

        Ok(())
    }

    #[test]
    fn test_transfers() -> Result<(), Box<dyn std::error::Error>> {
        let s = r#"
            COM)B
            B)C
            C)D
            D)E
            E)F
            B)G
            G)H
            D)I
            E)J
            J)K
            K)L
            K)YOU
            I)SAN"#;
        let parsed: Vec<Orbit> = parse_iter(s.lines())?;
        let orbits = Orbits::from(parsed.as_ref());
        let ts = orbits.transfer("YOU", "SAN");

        assert_eq!(ts, 4);

        Ok(())
    }
}
