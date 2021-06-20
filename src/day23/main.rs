use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use itertools::Itertools;
use log::debug;

use aoc::intcomp::{IntComp, InvalidInstruction, OutputVec, Stopped};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Nic(IntComp);

impl Nic {
    pub fn initialize(&mut self, addr: i64) -> Result<(), InvalidInstruction> {
        let mut output = OutputVec::new();
        let Nic(ref mut cp) = self;
        cp.run_to_input(&mut output)?;
        for v in output.0.drain(..) {
            println!("Output from {}: {}", addr, v)
        }
        cp.process_input(addr)?;
        Ok(())
    }

    pub fn process_queue<I: IntoIterator<Item = (i64, i64)>>(
        &mut self,
        pkts: I,
    ) -> Result<Vec<(i64, i64, i64)>, InvalidInstruction> {
        let mut output = OutputVec::new();
        let Nic(ref mut cp) = self;
        let state = cp.run_to_input(&mut output)?;
        assert_eq!(state, Stopped::Input);

        for (x, y) in pkts {
            assert_eq!(cp.run_to_io()?, Stopped::Input);
            cp.process_input(x)?;
            assert_eq!(cp.run_to_io()?, Stopped::Input);
            cp.process_input(y)?;

            log::info!("Received: ({}, {})", x, y);
        }

        let mut tuples = output.0.iter().copied().filter(|&n| n != -1).tuples();
        let mut packets = Vec::new();

        loop {
            if let Some((n, x, y)) = tuples.next() {
                packets.push((n, x, y));
                continue;
            }

            break;
        }

        assert!(tuples.into_buffer().len() == 0);

        Ok(packets)
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 23")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day23.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let line: String = buf_reader
        .lines()
        .next()
        .ok_or_else(|| anyhow::format_err!("No line found"))??;

    let orig_cp: IntComp = str::parse(&line)?;
    let n_cps = 50;

    let mut cps: Vec<IntComp> = (0..n_cps)
        .map(|n| {
            let mut cp = orig_cp.clone();
            let mut output = OutputVec::new();
            cp.run_to_input(&mut output)?;
            for v in output.0.drain(..) {
                println!("Output from {}: {}", n, v)
            }
            cp.process_input(n)?;
            Ok(cp)
        })
        .collect::<Result<Vec<IntComp>, InvalidInstruction>>()?;

    let mut packet_queue: HashMap<usize, VecDeque<(i64, i64)>> =
        (0..n_cps).map(|n| (n as usize, VecDeque::new())).collect();

    let mut blanks = 3;
    let mut did_something = false;
    let mut runs = 0;

    let mut nat_packet: Option<(i64, i64)> = None;

    while blanks > 0 {
        log::info!("Starting run {}", runs);
        runs += 1;
        if did_something {
            blanks = 3;
        } else {
            blanks -= 1;
        }
        let mut idling = 0;
        for (ix, cp) in cps.iter_mut().enumerate() {
            let mut output = OutputVec::new();
            let state = cp.run_to_input(&mut output)?;

            let mut tuples = output.0.iter().copied().filter(|&n| n != -1).tuples();

            loop {
                if let Some((n, x, y)) = tuples.next() {
                    did_something = true;
                    log::info!("Packet {} -> {}: ({}, {})", ix, n, x, y);
                    if n >= 0 && n < n_cps {
                        packet_queue
                            .get_mut(&(n as usize))
                            .expect("Expected n to exist")
                            .push_back((x, y));
                    } else if n == 255 {
                        println!("NAT packet: {} -> {}: ({}, {})", ix, n, x, y);
                        nat_packet = Some((x, y));
                    } else {
                        println!("Unknown packet: {} -> {}: ({}, {})", ix, n, x, y);
                    }
                    continue;
                }

                break;
            }

            assert!(tuples.into_buffer().len() == 0);

            match state {
                Stopped::Halted => {
                    panic!("IntComp {} Halted", ix);
                }
                Stopped::Output => {
                    panic!("Did not expect more output");
                    // let n = cp.consume_output().expect("Expected first output");
                    // cp.run_to_io()?;
                    // let x = cp.consume_output().expect("Expected second output");
                    // cp.run_to_io()?;
                    // let y = cp.consume_output().expect("Expected third output");
                    // println!("Output {}: {}, {}, {}", ix, n, x, y);
                    // assert!(n >= 0);
                    // assert!(n < n_cps as i64);
                    // packet_queue
                    //     .get_mut(&(n as usize))
                    //     .expect("Expected n to exist")
                    //     .push_back((x, y));
                }
                Stopped::Input => {
                    let packets = packet_queue
                        .get_mut(&(ix as usize))
                        .expect("Expected packet queue to contain all ixs");
                    match packets.pop_front() {
                        None => {
                            assert!(cp.process_input(-1)?);
                            idling += 1;
                        }
                        Some((x, y)) => {
                            did_something = true;
                            assert_eq!(cp.run_to_io()?, Stopped::Input);
                            cp.process_input(x)?;
                            assert_eq!(cp.run_to_io()?, Stopped::Input);
                            cp.process_input(y)?;

                            log::info!("Received ? -> {}: ({}, {})", ix, x, y);
                        }
                    }
                }
            }
        }

        if idling != n_cps {
            continue;
        }
    }

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
