use std::cmp::Ordering::{Equal, Greater, Less};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::iter::FromIterator;
use std::str::FromStr;

use clap::{App, Arg};
use log::debug;

use aoc::parse::parse_err_iter;

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Operand {
    quantity: i64,
    chemical: String,
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.quantity, self.chemical)
    }
}

impl Operand {
    pub fn new<S: Into<String>>(quantity: i64, chemical: S) -> Operand {
        Operand {
            quantity,
            chemical: chemical.into(),
        }
    }
}

impl std::ops::Mul<i64> for &Operand {
    type Output = Operand;

    fn mul(self, rhs: i64) -> Operand {
        Operand {
            quantity: self.quantity * rhs,
            chemical: self.chemical.clone(),
        }
    }
}

impl FromStr for Operand {
    type Err = failure::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let splits: Vec<_> = s.split(' ').collect();
        match splits.as_slice() {
            &[ns, cs] => {
                let n: i64 = ns.parse()?;
                return Ok(Operand {
                    quantity: n,
                    chemical: cs.to_string(),
                });
            }
            _ => return Err(failure::format_err!("Cannot convert '{}' to Operand", s)),
        };
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Reaction {
    input: Vec<Operand>,
    output: Operand,
}

impl fmt::Display for Reaction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (n, i) in self.input.iter().enumerate() {
            if n == 0 {
                write!(f, "{}", i)?;
                continue;
            }
            write!(f, ", {}", i)?;
        }

        write!(f, " => {}", self.output)
    }
}

impl FromStr for Reaction {
    type Err = failure::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let splits: Vec<_> = s.split(" => ").collect();
        let (inputs, output) = match splits.as_slice() {
            &[inp, outp] => (inp, outp),
            _ => return Err(failure::format_err!("Cannot convert '{}' to Reactions", s)),
        };

        let output: Operand = str::parse(output)?;
        let inputs: Result<Vec<Operand>, failure::Error> =
            inputs.split(", ").map(|rs| str::parse(rs)).collect();

        Ok(Reaction {
            input: inputs?,
            output: output,
        })
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Reactions {
    // output.chemical -> (output.quantity, inputs)
    values: HashMap<String, (i64, Vec<Operand>)>,
}

impl Reactions {
    pub fn sources(&self, output: Operand) -> Vec<Operand> {
        let mut queue = VecDeque::new();
        queue.push_back(output);
        let mut sourced: HashMap<String, i64> = HashMap::new();
        let mut extras: HashMap<String, i64> = HashMap::new();

        while let Some(Operand { quantity, chemical }) = queue.pop_front() {
            log::info!("Popped {} {}", quantity, chemical);
            let needed = match extras.entry(chemical.clone()) {
                Occupied(mut o) => {
                    let have = *o.get();
                    match have.cmp(&quantity) {
                        // We have not enough
                        Less => {
                            log::info!(
                                "  Have not enough {}. have: {}, quantity: {}",
                                chemical,
                                have,
                                quantity
                            );
                            quantity - o.remove()
                        }
                        Equal => {
                            log::info!(
                                "  Have exactly enough {}. have: {}, quantity: {}",
                                chemical,
                                have,
                                quantity
                            );
                            // We have exactly enough left over
                            o.remove();
                            continue;
                        }
                        Greater => {
                            log::info!(
                                "  Have more than enough {}. have: {}, quantity: {}",
                                chemical,
                                have,
                                quantity
                            );
                            // We have more than enough left over
                            o.insert(have - quantity);
                            continue;
                        }
                    }
                }
                Vacant(_) => {
                    log::info!(
                        "  Have no {}. have: {}, quantity: {}",
                        chemical,
                        0,
                        quantity
                    );
                    quantity
                }
            };

            log::info!("  Still need {}/{} {}", needed, quantity, chemical);

            let &(outn, ref inputs) = match self.values.get(&chemical) {
                None => {
                    let source_entry = sourced.entry(chemical.clone()).or_default();
                    log::info!(
                        "  Source element {}: {} + {} = {}",
                        chemical,
                        *source_entry,
                        needed,
                        *source_entry + needed
                    );

                    *source_entry += needed;
                    continue;
                }
                Some(oi) => oi,
            };

            let runs = (needed + outn - 1) / outn;
            let leftover = runs * outn - needed;
            log::info!(
                "  Need {}, out {} -> {} runs + {} leftover",
                needed,
                outn,
                runs,
                leftover
            );

            log::info!(
                "  Running {} of reaction: {}",
                runs,
                Reaction {
                    output: Operand::new(outn, chemical.clone()),
                    input: inputs.clone(),
                }
            );

            if leftover > 0 {
                let entry = extras.entry(chemical.clone()).or_default();
                log::info!(
                    "  Leftovers of {}: {} + {} => {}",
                    chemical,
                    *entry,
                    leftover,
                    *entry + leftover
                );
                *entry += leftover;
            }

            for op in inputs {
                log::info!("    pushing {}*{} = {}", op, runs, op * runs);
                queue.push_back(op * runs);
            }
        }

        sourced.iter().map(|(c, &n)| Operand::new(n, c)).collect()
    }
}

impl FromIterator<Reaction> for Reactions {
    fn from_iter<I: IntoIterator<Item = Reaction>>(iter: I) -> Self {
        let values: HashMap<String, (i64, Vec<Operand>)> = iter
            .into_iter()
            .map(|r| (r.output.chemical, (r.output.quantity, r.input)))
            .collect();
        Reactions { values }
    }
}

fn main() -> Result<(), failure::Error> {
    env_logger::init();

    let matches = App::new("Day 14")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day14.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let reactions: Reactions = parse_err_iter(buf_reader.lines())?;
    println!("Found {} reactions", reactions.values.len());

    let sourced = reactions.sources(Operand::new(1, "FUEL"));
    for s in &sourced {
        println!("  {}", s);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    use aoc::parse::parse_iter;

    use super::*;

    #[test]
    fn test_reaction() -> Result<(), failure::Error> {
        let r: Reaction = str::parse("1 A, 2 BB => 3 CCC")?;
        let expected = Reaction {
            input: vec![Operand::new(1, "A"), Operand::new(2, "BB")],
            output: Operand::new(3, "CCC"),
        };

        assert_eq!(r, expected);

        Ok(())
    }

    #[test]
    fn test_reactions() -> Result<(), failure::Error> {
        const EXAMPLE1: &str = "
            10 ORE => 10 A
            1 ORE => 1 B
            7 A, 1 B => 1 C
            7 A, 1 C => 1 D
            7 A, 1 D => 1 E
            7 A, 1 E => 1 FUEL
        ";
        let r: Reactions = parse_iter(EXAMPLE1.lines())?;

        assert_eq!(r.values.len(), 6);
        let sources = r.sources(Operand::new(1, "FUEL"));

        assert_eq!(sources, vec![Operand::new(31, "ORE")]);

        Ok(())
    }

    #[test]
    fn test_reactions2() -> Result<(), failure::Error> {
        const EXAMPLE: &str = "
            9 ORE => 2 A
            8 ORE => 3 B
            7 ORE => 5 C
            3 A, 4 B => 1 AB
            5 B, 7 C => 1 BC
            4 C, 1 A => 1 CA
            2 AB, 3 BC, 4 CA => 1 FUEL
        ";
        let r: Reactions = parse_iter(EXAMPLE.lines())?;

        assert_eq!(r.values.len(), 7);
        let sources = r.sources(Operand::new(1, "FUEL"));

        assert_eq!(sources, vec![Operand::new(165, "ORE")]);

        Ok(())
    }

    #[test]
    fn test_reactions3() -> Result<(), failure::Error> {
        const EXAMPLE: &str = "
            157 ORE => 5 NZVS
            165 ORE => 6 DCFZ
            44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
            12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
            179 ORE => 7 PSHF
            177 ORE => 5 HKGWZ
            7 DCFZ, 7 PSHF => 2 XJWVT
            165 ORE => 2 GPVTF
            3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT
        ";
        let r: Reactions = parse_iter(EXAMPLE.lines())?;

        let sources = r.sources(Operand::new(1, "FUEL"));

        assert_eq!(sources, vec![Operand::new(13312, "ORE")]);

        Ok(())
    }

    #[test]
    fn test_reactions4() -> Result<(), failure::Error> {
        const EXAMPLE: &str = "
            2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
            17 NVRVD, 3 JNWZP => 8 VPVL
            53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
            22 VJHF, 37 MNCFX => 5 FWMGM
            139 ORE => 4 NVRVD
            144 ORE => 7 JNWZP
            5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
            5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
            145 ORE => 6 MNCFX
            1 NVRVD => 8 CXFTF
            1 VJHF, 6 MNCFX => 4 RFSQX
            176 ORE => 6 VJHF
        ";
        let r: Reactions = parse_iter(EXAMPLE.lines())?;

        let sources = r.sources(Operand::new(1, "FUEL"));

        assert_eq!(sources, vec![Operand::new(180697, "ORE")]);

        Ok(())
    }

    #[test]
    fn test_reactions5() -> Result<(), failure::Error> {
        const EXAMPLE: &str = "
            171 ORE => 8 CNZTR
            7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
            114 ORE => 4 BHXH
            14 VRPVC => 6 BMBT
            6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
            6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
            15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
            13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
            5 BMBT => 4 WPTQ
            189 ORE => 9 KTJDG
            1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
            12 VRPVC, 27 CNZTR => 2 XDBXC
            15 KTJDG, 12 BHXH => 5 XCVML
            3 BHXH, 2 VRPVC => 7 MZWV
            121 ORE => 7 VRPVC
            7 XCVML => 6 RJRHP
            5 BHXH, 4 VRPVC => 5 LTCX
        ";
        let r: Reactions = parse_iter(EXAMPLE.lines())?;

        let sources = r.sources(Operand::new(1, "FUEL"));

        assert_eq!(sources, vec![Operand::new(2210736, "ORE")]);

        Ok(())
    }
}
