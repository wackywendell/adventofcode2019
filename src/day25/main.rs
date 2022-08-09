use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::str::FromStr;

use anyhow::{anyhow, Error as AnyErr, Result as AnyResult};
use clap::{App, Arg};
use log::debug;
use slotmap::SlotMap;

use aoc::grid::{Compass, Turn};
use aoc::intcomp::{IntComp, OutputVec, Stopped};

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Room {
    pub name: String,
    pub message: String,
    pub items: BTreeSet<String>,
    pub directions: Vec<Compass>,
}

impl Display for Room {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "Room[{}, doors=", self.name)?;
        if !self.directions.is_empty() {
            write!(f, "doors=")?;
            for &d in &self.directions {
                write!(f, "{}", d)?;
            }
            write!(f, ", ")?;
        }

        for (ix, item) in self.items.iter().enumerate() {
            if ix == 0 {
                write!(f, "items={}", item)?;
            } else {
                write!(f, ",{}", item)?;
            }

            if ix == self.items.len() - 1 {
                write!(f, ", ")?;
            }
        }

        write!(f, "message='{}']", self.message)?;

        Ok(())
    }
}

fn str_to_compass(s: &str) -> AnyResult<Compass> {
    Ok(match s {
        "north" => Compass::North,
        "south" => Compass::South,
        "east" => Compass::East,
        "west" => Compass::West,
        _ => return Err(anyhow!("'{}' is not a compoass direction")),
    })
}

impl FromStr for Room {
    type Err = AnyErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines = s.lines();
        let mut first = lines.next().ok_or_else(|| anyhow!("No first line"))?.trim();
        while first.is_empty() {
            first = lines
                .next()
                .ok_or_else(|| anyhow!("No non-empty first line"))?
                .trim();
        }
        assert!(first.starts_with("== "));
        assert!(first.ends_with(" =="));
        let name = first
            .trim_end_matches(" ==")
            .trim_start_matches("== ")
            .to_owned();

        let message = lines
            .next()
            .ok_or_else(|| anyhow!("No second line"))?
            .trim()
            .to_owned();

        assert!(!message.is_empty(), "Expected non-empty message");

        let next = lines.next().ok_or_else(|| anyhow!("No third line"))?.trim();
        assert!(next.is_empty(), "Expected third line to be empty");

        let next = lines
            .next()
            .ok_or_else(|| anyhow!("No fourth line"))?
            .trim();
        assert!(
            next == "Doors here lead:",
            "Expected third line to be 'Doors here lead:'"
        );

        let mut directions = Vec::new();
        let mut next = lines
            .next()
            .ok_or_else(|| anyhow!("No line after doors"))?
            .trim();
        while next.starts_with("- ") {
            let dir = next.trim_start_matches("- ");
            let dir = str_to_compass(dir)?;
            directions.push(dir);
            next = lines
                .next()
                .ok_or_else(|| anyhow!("No line after directions"))?
                .trim();
        }

        assert!(
            next.is_empty(),
            "Expected line after directions to be empty"
        );

        next = lines
            .next()
            .ok_or_else(|| anyhow!("No line after directions + empty"))?
            .trim();

        let mut items = BTreeSet::new();
        if next == "Items here:" {
            next = lines
                .next()
                .ok_or_else(|| anyhow!("No line after items"))?
                .trim();
            while next.starts_with("- ") {
                let item = next.trim_start_matches("- ");
                items.insert(item.to_owned());
                next = lines
                    .next()
                    .ok_or_else(|| anyhow!("No line after items"))?
                    .trim();
            }

            assert!(
                next.is_empty(),
                "Expected line after items to be empty, got '{}'",
                next
            );

            next = lines
                .next()
                .ok_or_else(|| anyhow!("No line after items + empty"))?
                .trim();
        }

        assert_eq!(
            next, "Command?",
            "Expected line after items to be 'Command?'"
        );

        assert!(lines.next().is_none());

        Ok(Room {
            name,
            message,
            items,
            directions,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Explorer {
    comp: IntComp,
}

impl Explorer {
    fn new(comp: IntComp) -> AnyResult<(Self, String)> {
        let mut exp = Explorer { comp };
        let mut output = OutputVec::new();
        exp.comp.run_to_input(&mut output)?;
        let out = output.as_string()?;

        Ok((exp, out))
    }

    fn process_input_str(&mut self, output: &mut OutputVec, input: &str) -> anyhow::Result<String> {
        log::info!("Process 1: '{}'", input);
        self.comp
            .process_ascii(input, output)?
            .expect(Stopped::Input)?;
        log::info!("Process 2: '\\n'");
        self.comp
            .process_ascii("\n", output)?
            .expect(Stopped::Input)?;
        log::info!("Processed: '\\n'");

        Ok(output.as_string()?)
    }

    fn process_str(&mut self, input: &str) -> anyhow::Result<String> {
        let mut out = OutputVec::new();

        match self.process_input_str(&mut out, input) {
            Ok(v) => Ok(v),
            Err(e) => {
                let output = out.as_string()?;
                log::warn!("process_str failure, output: {}", output);
                Err(e)
            }
        }
    }

    pub fn north(&mut self) -> anyhow::Result<String> {
        self.process_str("north")
    }

    pub fn south(&mut self) -> anyhow::Result<String> {
        self.process_str("south")
    }

    pub fn east(&mut self) -> anyhow::Result<String> {
        self.process_str("east")
    }

    pub fn west(&mut self) -> anyhow::Result<String> {
        self.process_str("west")
    }

    pub fn step(&mut self, direction: Compass) -> anyhow::Result<Room> {
        let input = match direction {
            Compass::East => "east",
            Compass::North => "north",
            Compass::South => "south",
            Compass::West => "west",
        };
        log::info!("Taking step {}", input);
        let output = self.process_str(input)?;
        log::info!("Took step:\n{}\n", output);
        Room::from_str(&output)
    }

    pub fn take(&mut self, item: &str) -> anyhow::Result<String> {
        log::info!("Taking {}", item);
        let mut s = String::from("take ");
        s.push_str(item);
        let result = self.process_str(&s)?;
        log::info!("took {}", item);
        Ok(result)
    }

    pub fn drop(&mut self, item: &str) -> anyhow::Result<String> {
        let mut s = String::from("drop ");
        s.push_str(item);
        self.process_str(&s)
    }

    pub fn inventory(&mut self) -> anyhow::Result<String> {
        self.process_str("inv")
    }
}

type Key = slotmap::DefaultKey;

#[derive(Default, Debug)]
pub struct Map {
    rooms_by_name: HashMap<String, Key>,
    rooms: SlotMap<Key, Room>,
    doors: HashMap<Key, BTreeMap<Compass, Key>>,
}

impl Map {
    fn add_room(&mut self, room: Room) -> Key {
        if let Some(&key) = self.rooms_by_name.get(&room.name) {
            return key;
        }

        let name = room.name.clone();
        let key = self.rooms.insert(room);
        self.rooms_by_name.insert(name, key);
        key
    }

    fn add_door(&mut self, first: Key, direction: Compass, second: Key) {
        self.doors
            .entry(first)
            .or_default()
            .insert(direction, second);
        self.doors
            .entry(second)
            .or_default()
            .insert(direction + Turn::Reverse, first);
    }

    fn len(&self) -> usize {
        self.rooms.len()
    }

    // fn is_empty(&self) -> bool {
    //     self.rooms.is_empty()
    // }

    fn contains(&self, room: &Room) -> bool {
        self.rooms_by_name.contains_key(&room.name)
    }

    fn get(&self, key: Key) -> &Room {
        self.rooms.get(key).unwrap()
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 25")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day25.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let line: String = buf_reader
        .lines()
        .next()
        .ok_or_else(|| anyhow::format_err!("No line found"))??;

    // Pick-up-able:
    // - food ration
    // - candy cane
    // - mouse
    // - mug
    // - coin
    // - ornament
    // - semiconductor
    // - mutex

    let to_take: BTreeSet<String> = [
        "food ration",
        "candy cane",
        "mouse",
        "mug",
        "coin",
        "ornament",
        "semiconductor",
    ]
    .iter()
    .map(|&s| s.to_owned())
    .collect();

    let cp: IntComp = str::parse(&line)?;
    let (mut explorer, initial_output) = Explorer::new(cp)?;
    let room = Room::from_str(&initial_output)?;

    let mut dir = Compass::North;
    let mut map = Map::default();
    println!("+ ({}) {}: {}", map.len() + 1, dir, room);
    let mut last = map.add_room(room);

    for _ in 0..40 {
        let mut room = explorer.step(dir)?;

        let overlap = to_take.intersection(&room.items);

        for item in overlap {
            let output = explorer.take(&item)?;
            println!("Took {}, output {}", item, output);
        }

        let seen = map.contains(&room);
        let key = map.add_room(room);
        map.add_door(last, dir, key);
        last = key;
        let room = map.get(key);
        if !seen {
            println!("({}) {}: {}", map.len(), dir, room);
        } else {
            log::info!("({}) {}: {}", map.len(), dir, room);
        };
        if room.name == "Security Checkpoint" {
            println!("inv: {}", explorer.inventory()?);
            //     dir = dir + Turn::Left + Turn::Left;
            // } else {
        }
        dir = dir + Turn::Left;
        for _ in 0..4 {
            if room.directions.contains(&dir) {
                break;
            }
            dir = dir + Turn::Right;
        }
        // }
        assert!(room.directions.contains(&dir));
    }
    // for &d in &dirs {
    //     let room = explorer.step(d)?;
    //     println!("{}", room);
    // }

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_thing() -> anyhow::Result<()> {
        Ok(())
    }
}
