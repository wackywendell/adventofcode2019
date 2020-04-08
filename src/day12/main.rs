use std::cmp::Ordering::*;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::str::FromStr;
use std::{fmt, ops};

use clap::{App, Arg};
use log::debug;
use num::integer::lcm;
use text_io::try_scan;

use aoc::parse::parse_err_iter;

type Value = i64;

fn sign(value: Value) -> Value {
    match value.cmp(&0) {
        Greater => 1,
        Equal => 0,
        Less => -1,
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Vector(Value, Value, Value);

impl Vector {
    pub fn normed(self) -> Self {
        Vector(sign(self.0), sign(self.1), sign(self.2))
    }

    pub fn norm(self) -> Value {
        self.0.abs() + self.1.abs() + self.2.abs()
    }
}

impl ops::Sub for Vector {
    type Output = Self;

    fn sub(self: Self, rhs: Self) -> Self {
        Vector(self.0 - rhs.0, self.1 - rhs.1, self.2 - rhs.2)
    }
}

impl ops::Add for Vector {
    type Output = Self;

    fn add(self: Self, rhs: Self) -> Self {
        Vector(self.0 + rhs.0, self.1 + rhs.1, self.2 + rhs.2)
    }
}

impl fmt::Display for Vector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{}, {}, {}>", self.0, self.1, self.2)
    }
}

impl FromStr for Vector {
    type Err = anyhow::Error;
    // type Err = Box<dyn std::error::Error>;
    // type Err = text_io::Error;

    fn from_str(l: &str) -> Result<Self, Self::Err> {
        // let (x, y, z): (Value, Value, Value);
        let (xs, ys, zs): (String, String, String);
        try_scan!(l.bytes() => "<x={}, y={}, z={}>", xs,ys,zs);
        let x = xs.trim().parse()?;
        let y = ys.trim().parse()?;
        let z = zs.trim().parse()?;

        Ok(Vector(x, y, z))
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Moon {
    r: Vector,
    v: Vector,
}

impl FromStr for Moon {
    type Err = <Vector as FromStr>::Err;

    fn from_str(l: &str) -> Result<Self, Self::Err> {
        // let (r, v): (Vector, Vector);
        let mut vars: Vec<String> = (0..6).map(|_| String::new()).collect();
        try_scan!(l.bytes() => "pos=<x={}, y={}, z={}>, vel=<x={}, y={}, z={}>",
            vars[0], vars[1], vars[2], vars[3], vars[4], vars[5]);

        let r = Vector(
            vars[0].trim().parse()?,
            vars[1].trim().parse()?,
            vars[2].trim().parse()?,
        );
        let v = Vector(
            vars[3].trim().parse()?,
            vars[4].trim().parse()?,
            vars[5].trim().parse()?,
        );

        Ok(Moon::new(r, v))
    }
}

impl Moon {
    pub fn new(r: Vector, v: Vector) -> Self {
        Moon { r, v }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct System {
    moons: Vec<Moon>,
}

impl System {
    fn new(moons: Vec<Moon>) -> Self {
        System { moons }
    }

    fn forces(&self) -> Vec<Vector> {
        let mut fs: Vec<Vector> = self.moons.iter().map(|_| Vector::default()).collect();

        for (i, m1) in self.moons.iter().enumerate() {
            for (j, m2) in self.moons[..i].iter().enumerate() {
                let df = (m2.r - m1.r).normed();
                fs[i] = fs[i] + df;
                fs[j] = fs[j] - df;
            }
        }

        fs
    }

    pub fn step(&mut self) {
        let fs = self.forces();

        for (m, f) in self.moons.iter_mut().zip(fs) {
            m.v = m.v + f;
            m.r = m.r + m.v;
        }
    }

    pub fn energy(&self) -> Value {
        self.moons.iter().map(|m| m.r.norm() * m.v.norm()).sum()
    }

    pub fn dim_matches(&self, other: &Self) -> (bool, bool, bool) {
        if self.moons.len() != other.moons.len() {
            panic!("Can only compare systems with the same number of moons");
        }

        let (mut xm, mut ym, mut zm) = (true, true, true);

        for (m1, m2) in self.moons.iter().zip(&other.moons) {
            xm &= m1.r.0 == m2.r.0;
            xm &= m1.v.0 == m2.v.0;
            ym &= m1.r.1 == m2.r.1;
            ym &= m1.v.1 == m2.v.1;
            zm &= m1.r.2 == m2.r.2;
            zm &= m1.v.2 == m2.v.2;
        }

        (xm, ym, zm)
    }
}

#[derive(Debug, Clone)]
pub struct Repeats {
    start: System,
    system: System,
    steps: isize,
    repeated: (Option<isize>, Option<isize>, Option<isize>),
}

impl Repeats {
    pub fn new(system: System) -> Self {
        Repeats {
            start: system.clone(),
            system,
            steps: 0,
            repeated: Default::default(),
        }
    }

    pub fn step(&mut self) {
        self.system.step();
        self.steps += 1;

        let (xm, ym, zm) = self.system.dim_matches(&self.start);

        if xm && self.repeated.0.is_none() {
            self.repeated.0 = Some(self.steps);
        }
        if ym && self.repeated.1.is_none() {
            self.repeated.1 = Some(self.steps);
        }
        if zm && self.repeated.2.is_none() {
            self.repeated.2 = Some(self.steps);
        }
    }

    pub fn run(&mut self) -> (isize, isize, isize) {
        while self.repeated.0.is_none() || self.repeated.1.is_none() || self.repeated.2.is_none() {
            self.step();
            if self.steps % 10_000 == 0 {
                log::info!("Simulated {} steps: {:?}", self.steps, self.repeated);
            }
        }

        (
            self.repeated.0.unwrap(),
            self.repeated.1.unwrap(),
            self.repeated.2.unwrap(),
        )
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 12")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day12.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let parsed: Vec<Vector> = parse_err_iter(buf_reader.lines())?;
    let moons: Vec<Moon> = parsed
        .iter()
        .map(|&v| Moon::new(v, Vector::default()))
        .collect();

    let mut system = System::new(moons);

    log::info!("Parsed {} moons", system.moons.len());

    for _ in 0..1000 {
        system.step();
    }

    println!("Energy: {}", system.energy());

    let mut repeats = Repeats::new(system);
    let (rx, ry, rz) = repeats.run();

    let cycle = lcm(lcm(rx, ry), rz);

    println!(
        "Found repeats after {},{},{} cycles, for a total of {}",
        rx, ry, rz, cycle
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    use super::*;

    use aoc::parse::parse_iter;

    const EXAMPLE: &str = r#"
        <x=-1, y=0, z=2>
        <x=2, y=-10, z=-7>
        <x=4, y=-8, z=8>
        <x=3, y=5, z=-1>    
    "#;

    fn parse_system(s: &str) -> Result<System, <Vector as FromStr>::Err> {
        let parsed: Vec<Vector> = parse_iter(s.lines())?;
        let moons: Vec<Moon> = parsed
            .iter()
            .map(|&v| Moon::new(v, Vector::default()))
            .collect();

        Ok(System::new(moons))
    }

    #[test]
    fn test_parse() -> anyhow::Result<()> {
        let system = parse_system(EXAMPLE)?;

        assert_eq!(system.moons[0].r, Vector(-1, 0, 2));
        assert_eq!(system.moons[0].v, Vector(0, 0, 0));
        assert_eq!(system.moons[1].r, Vector(2, -10, -7));
        assert_eq!(system.moons[1].v, Vector(0, 0, 0));
        assert_eq!(system.moons[2].r, Vector(4, -8, 8));
        assert_eq!(system.moons[2].v, Vector(0, 0, 0));
        assert_eq!(system.moons[3].r, Vector(3, 5, -1));
        assert_eq!(system.moons[3].v, Vector(0, 0, 0));

        Ok(())
    }

    #[test]
    fn test_moon_parse() -> anyhow::Result<()> {
        log::info!("Parsing 0-1");
        let moon: Moon = "pos=<x=0, y=0, z=0>, vel=<x=1, y=1, z=1>".parse()?;
        assert_eq!(moon.r, Vector(0, 0, 0));
        log::info!("Parsing 2-3");
        let moon: Moon = "pos=<x=0, y=  2, z=0>, vel=<x=3, y=1, z= 3>".parse()?;
        assert_eq!(moon.r, Vector(0, 2, 0));
        assert_eq!(moon.v, Vector(3, 1, 3));

        Ok(())
    }

    #[test]
    fn test_run() -> anyhow::Result<()> {
        let mut system = parse_system(EXAMPLE)?;

        let stages: Vec<&str> = vec![
            r#"
                pos=<x=-1, y=  0, z= 2>, vel=<x= 0, y= 0, z= 0>
                pos=<x= 2, y=-10, z=-7>, vel=<x= 0, y= 0, z= 0>
                pos=<x= 4, y= -8, z= 8>, vel=<x= 0, y= 0, z= 0>
                pos=<x= 3, y=  5, z=-1>, vel=<x= 0, y= 0, z= 0>
            "#,
            r#"
                pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>
                pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>
                pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>
                pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>
            "#,
            r#"
                pos=<x= 5, y=-3, z=-1>, vel=<x= 3, y=-2, z=-2>
                pos=<x= 1, y=-2, z= 2>, vel=<x=-2, y= 5, z= 6>
                pos=<x= 1, y=-4, z=-1>, vel=<x= 0, y= 3, z=-6>
                pos=<x= 1, y=-4, z= 2>, vel=<x=-1, y=-6, z= 2>
            "#,
            r#"
                pos=<x= 5, y=-6, z=-1>, vel=<x= 0, y=-3, z= 0>
                pos=<x= 0, y= 0, z= 6>, vel=<x=-1, y= 2, z= 4>
                pos=<x= 2, y= 1, z=-5>, vel=<x= 1, y= 5, z=-4>
                pos=<x= 1, y=-8, z= 2>, vel=<x= 0, y=-4, z= 0>
            "#,
            r#"
                pos=<x= 2, y=-8, z= 0>, vel=<x=-3, y=-2, z= 1>
                pos=<x= 2, y= 1, z= 7>, vel=<x= 2, y= 1, z= 1>
                pos=<x= 2, y= 3, z=-6>, vel=<x= 0, y= 2, z=-1>
                pos=<x= 2, y=-9, z= 1>, vel=<x= 1, y=-1, z=-1>
            "#,
            r#"
                pos=<x=-1, y=-9, z= 2>, vel=<x=-3, y=-1, z= 2>
                pos=<x= 4, y= 1, z= 5>, vel=<x= 2, y= 0, z=-2>
                pos=<x= 2, y= 2, z=-4>, vel=<x= 0, y=-1, z= 2>
                pos=<x= 3, y=-7, z=-1>, vel=<x= 1, y= 2, z=-2>
            "#,
            r#"
                pos=<x=-1, y=-7, z= 3>, vel=<x= 0, y= 2, z= 1>
                pos=<x= 3, y= 0, z= 0>, vel=<x=-1, y=-1, z=-5>
                pos=<x= 3, y=-2, z= 1>, vel=<x= 1, y=-4, z= 5>
                pos=<x= 3, y=-4, z=-2>, vel=<x= 0, y= 3, z=-1>
            "#,
            r#"
                pos=<x= 2, y=-2, z= 1>, vel=<x= 3, y= 5, z=-2>
                pos=<x= 1, y=-4, z=-4>, vel=<x=-2, y=-4, z=-4>
                pos=<x= 3, y=-7, z= 5>, vel=<x= 0, y=-5, z= 4>
                pos=<x= 2, y= 0, z= 0>, vel=<x=-1, y= 4, z= 2> 
            "#,
            r#"
                pos=<x= 5, y= 2, z=-2>, vel=<x= 3, y= 4, z=-3>
                pos=<x= 2, y=-7, z=-5>, vel=<x= 1, y=-3, z=-1>
                pos=<x= 0, y=-9, z= 6>, vel=<x=-3, y=-2, z= 1>
                pos=<x= 1, y= 1, z= 3>, vel=<x=-1, y= 1, z= 3>
            "#,
            r#"
                pos=<x= 5, y= 3, z=-4>, vel=<x= 0, y= 1, z=-2>
                pos=<x= 2, y=-9, z=-3>, vel=<x= 0, y=-2, z= 2>
                pos=<x= 0, y=-8, z= 4>, vel=<x= 0, y= 1, z=-2>
                pos=<x= 1, y= 1, z= 5>, vel=<x= 0, y= 0, z= 2>
            "#,
            r#"
                pos=<x= 2, y= 1, z=-3>, vel=<x=-3, y=-2, z= 1>
                pos=<x= 1, y=-8, z= 0>, vel=<x=-1, y= 1, z= 3>
                pos=<x= 3, y=-6, z= 1>, vel=<x= 3, y= 2, z=-3>
                pos=<x= 2, y= 0, z= 4>, vel=<x= 1, y=-1, z=-1>
            "#,
        ];

        for (i, s) in stages.iter().enumerate() {
            log::info!("Stage {}", i);
            if i > 0 {
                system.step();
            }
            let moons: Vec<Moon> = parse_iter(s.lines())?;
            assert_eq!(system.moons, moons);
        }

        assert_eq!(system.energy(), 179);

        Ok(())
    }

    const EXAMPLE2: &str = r#"
        <x=-8, y=-10, z=0>
        <x=5, y=5, z=10>
        <x=2, y=-7, z=3>
        <x=9, y=-8, z=-3>
    "#;

    #[test]
    fn test_example2() -> anyhow::Result<()> {
        let mut system = parse_system(EXAMPLE2)?;

        for _ in 0..100 {
            system.step()
        }

        assert_eq!(system.energy(), 1940);

        Ok(())
    }

    #[test]
    fn test_repeats() -> anyhow::Result<()> {
        let system = parse_system(EXAMPLE)?;
        let mut repeats = Repeats::new(system);
        let (rx, ry, rz) = repeats.run();

        let cycle = lcm(lcm(rx, ry), rz);
        assert_eq!(cycle, 2772);

        Ok(())
    }

    const EXAMPLE3: &str = r#"
        <x=-8, y=-10, z=0>
        <x=5, y=5, z=10>
        <x=2, y=-7, z=3>
        <x=9, y=-8, z=-3>
    "#;

    #[test]
    fn test_many_repeats() -> anyhow::Result<()> {
        let system = parse_system(EXAMPLE3)?;
        let mut repeats = Repeats::new(system);
        let (rx, ry, rz) = repeats.run();

        log::info!("Found repeats after {},{},{} cycles", rx, ry, rz);
        let cycle = lcm(lcm(rx, ry), rz);
        assert_eq!(cycle, 4_686_774_924);

        Ok(())
    }
}
