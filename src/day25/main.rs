use std::collections::hash_map::Entry::Occupied;
use std::collections::{BTreeMap, BTreeSet, HashMap};
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

#[derive(Debug, Clone)]
pub struct Explorer {
    comp: IntComp,
    room: Key,
    direction: Compass,
    carrying: BTreeSet<String>,
    map: Map,
}

impl Explorer {
    fn new(mut comp: IntComp) -> AnyResult<(Self, String)> {
        let mut output = OutputVec::new();
        comp.run_to_input(&mut output)?;
        let out = output.as_string()?;

        let room = Room::from_str(&out)?;
        let mut map: Map = Default::default();
        let key = map.add_room(room);
        let exp = Explorer {
            comp,
            room: key,
            direction: Compass::North,
            carrying: Default::default(),
            map,
        };

        Ok((exp, out))
    }

    fn see_room(&self) -> &Room {
        self.map.get(self.room)
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

    // fn north(&mut self) -> anyhow::Result<String> {
    //     self.process_str("north")
    // }

    // fn south(&mut self) -> anyhow::Result<String> {
    //     self.process_str("south")
    // }

    // fn east(&mut self) -> anyhow::Result<String> {
    //     self.process_str("east")
    // }

    // fn west(&mut self) -> anyhow::Result<String> {
    //     self.process_str("west")
    // }

    pub fn step(&mut self, direction: Compass) -> anyhow::Result<()> {
        let input = match direction {
            Compass::East => "east",
            Compass::North => "north",
            Compass::South => "south",
            Compass::West => "west",
        };
        log::info!("Taking step {}", input);
        let output = self.process_str(input)?;
        log::info!("Took step:\n{}\n", output);
        let room = Room::from_str(&output)?;
        let new = self.map.add_room(room);
        self.map.add_door(self.room, direction, new);
        self.room = new;
        self.direction = direction;
        Ok(())
    }

    pub fn take(&mut self, item: &str) -> anyhow::Result<String> {
        log::info!("Taking {}", item);
        let mut s = String::from("take ");
        s.push_str(item);
        let result = self.process_str(&s)?;
        let new = self.carrying.insert(item.to_string());
        let room = self.map.rooms.get_mut(self.room).unwrap();
        room.items.remove(item);
        assert!(new, "Expected to add {}", item);
        log::info!("took {}", item);
        Ok(result)
    }

    pub fn drop(&mut self, item: &str) -> anyhow::Result<String> {
        let found = self.carrying.remove(item);
        assert!(!found, "Expected to drop {}", item);

        let mut s = String::from("drop ");
        s.push_str(item);
        self.process_str(&s)
    }

    pub fn inventory(&mut self) -> anyhow::Result<String> {
        self.process_str("inv")
    }
}

type Key = slotmap::DefaultKey;

#[derive(Default, Debug, Clone)]
pub struct Map {
    rooms_by_name: HashMap<String, Key>,
    rooms: SlotMap<Key, Room>,
    doors: HashMap<Key, BTreeMap<Compass, Key>>,
    unvisited: HashMap<Key, BTreeSet<Compass>>,
}

impl Map {
    fn add_room(&mut self, room: Room) -> Key {
        if let Some(&key) = self.rooms_by_name.get(&room.name) {
            return key;
        }

        let name = room.name.clone();
        let directions = room.directions.clone();
        let key = self.rooms.insert(room);
        self.rooms_by_name.insert(name, key);
        let unvisited = self.unvisited.insert(key, Default::default());
        assert!(unvisited.is_none());

        let unvisited = self.unvisited.get_mut(&key).unwrap();
        for dir in directions {
            unvisited.insert(dir);
        }
        key
    }

    fn visit(&mut self, room: Key, direction: Compass) {
        if let Occupied(mut o) = self.unvisited.entry(room) {
            o.get_mut().remove(&direction);
            if o.get().is_empty() {
                o.remove();
            }
        }
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

        self.visit(first, direction);
        self.visit(second, direction + Turn::Reverse);
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

fn explore_and_take(explorer: &mut Explorer, items: BTreeSet<String>) -> AnyResult<()> {
    let start = explorer.room;
    loop {
        let mut dir = explorer.direction + Turn::Left;
        log::info!("Starting {} -> {}", explorer.direction, dir);
        for _ in 0..4 {
            log::info!("Checking {} -> {}", explorer.direction, dir);
            if explorer.see_room().directions.contains(&dir) {
                break;
            }
            dir = dir + Turn::Right;
        }
        if explorer.see_room().name == "Security Checkpoint" {
            println!("inv: {}", explorer.inventory()?);
            log::info!("Turning around at security checkpoint");
            dir = explorer.direction + Turn::Reverse;
        }
        assert!(explorer.see_room().directions.contains(&dir));
        explorer.step(dir)?;
        log::info!(
            "Stepped {}, entering room {}",
            dir,
            explorer.see_room().name
        );

        let overlap: BTreeSet<String> = items
            .intersection(&explorer.see_room().items)
            .map(|s| s.to_owned())
            .collect();

        for item in overlap {
            let output = explorer.take(&item)?;
            println!("Took {}, output: {}", item, output.trim());
        }

        if explorer.room == start {
            break;
        }
    }

    Ok(())
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

    explore_and_take(&mut explorer, to_take.clone())?;
    println!(
        "Visited, back to start. Unvisited: {}",
        explorer.map.unvisited.len()
    );

    return Ok(());

    for _ in 0..40 {
        explorer.step(dir)?;

        let overlap: BTreeSet<String> = to_take
            .intersection(&explorer.see_room().items)
            .map(|s| s.to_owned())
            .collect();

        for item in overlap {
            let output = explorer.take(&item)?;
            println!("Took {}, output: {}", item, output.trim());
        }

        let seen = map.contains(&explorer.see_room());
        let key = map.add_room(explorer.see_room().clone());
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
