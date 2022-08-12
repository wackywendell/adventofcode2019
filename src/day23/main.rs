use std::collections::VecDeque;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use itertools::Itertools;
use log::debug;

use aoc::intcomp::{IntComp, InvalidInstruction, OutputVec, Stopped};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Nic {
    addr: i64,
    comp: IntComp,
    inputs: VecDeque<(i64, i64)>,
}

impl Nic {
    pub fn new(mut comp: IntComp, addr: i64) -> Result<Self, InvalidInstruction> {
        let mut output = OutputVec::new();
        comp.run_to_input(&mut output)?;
        for v in output.0.drain(..) {
            panic!(
                "Unexpected output during initialization from {}: {}",
                addr, v
            )
        }
        comp.process_input(addr)?;
        Ok(Nic {
            addr,
            comp,
            inputs: Default::default(),
        })
    }

    pub fn queue_packet(&mut self, packet: (i64, i64)) {
        self.inputs.push_back(packet);
    }

    // pub fn fetch_outputs(&mut self) -> Result<Vec<(i64, i64, i64)>, InvalidInstruction> {
    //     let mut output = OutputVec::new();
    //     let state = self.comp.run_to_input(&mut output)?;
    //     assert_eq!(state, Stopped::Input);

    fn process_output(output: OutputVec) -> Vec<(i64, i64, i64)> {
        if output.0.len() % 3 != 0 {
            panic!("Unexpected output of length {}", output.0.len());
        }

        output.0.iter().copied().tuples().collect()
    }

    pub fn queue_len(&self) -> usize {
        self.inputs.len()
    }

    pub fn process(&mut self) -> Result<Vec<(i64, i64, i64)>, InvalidInstruction> {
        let mut output = OutputVec::new();
        assert_eq!(self.comp.run_to_input(&mut output)?, Stopped::Input);

        for (x, y) in self.inputs.drain(..) {
            self.comp.process_input(x)?;
            assert_eq!(self.comp.run_to_input(&mut output)?, Stopped::Input);
            self.comp.process_input(y)?;
            assert_eq!(self.comp.run_to_input(&mut output)?, Stopped::Input);

            log::info!("Received: ({}, {})", x, y);
        }

        // Tell it the queue is now empty
        self.comp.process_input(-1)?;
        assert_eq!(self.comp.run_to_input(&mut output)?, Stopped::Input);

        let packets = Self::process_output(output);
        for &(n, x, y) in &packets {
            log::info!("Sending {} -> {}: ({}, {})", self.addr, n, x, y);
        }

        Ok(packets)
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 23")
        .arg(
            Arg::with_name("input")
                .short('i')
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

    let mut cps: Vec<Nic> = (0..n_cps)
        .map(|n| Nic::new(orig_cp.clone(), n))
        .collect::<Result<Vec<Nic>, InvalidInstruction>>()?;

    let mut nat_packet: Option<(i64, i64)> = None;
    let mut first_nat_packet = None;
    let mut last_nat_packet = None;

    loop {
        let mut traffic = 0;
        for ix in 0..cps.len() {
            let nic = &mut cps[ix];
            traffic += nic.queue_len();
            let out = nic.process()?;
            traffic += out.len();
            for (n, x, y) in out {
                if n == 255 {
                    log::info!("Sending to NAT {} -> {}: ({}, {})", ix, n, x, y);
                    nat_packet = Some((x, y));
                    if first_nat_packet.is_none() {
                        println!("Got first packet to 255: ({}, {})", x, y);
                        first_nat_packet = nat_packet;
                    }
                    continue;
                }
                if n < 0 || n > n_cps {
                    panic!("Unexpected address: {}", n);
                }

                log::info!("Sending {} -> {}: ({}, {})", ix, n, x, y);

                let receiver = &mut cps[n as usize];
                receiver.queue_packet((x, y));
            }
        }

        if traffic == 0 {
            let (x, y) = match nat_packet.take() {
                None => {
                    let (x, y) = last_nat_packet.unwrap();
                    println!("No nat packet left, last sent ({}, {})", x, y);
                    break;
                }
                Some(pkt) => pkt,
            };
            if last_nat_packet == Some((x, y)) {
                println!("NAT sending y={} again", y);
            }
            let receiver = &mut cps[0usize];

            log::info!("NAT Sending: ({}, {})", x, y);
            receiver.queue_packet((x, y));
            last_nat_packet = Some((x, y));
        }
    }

    Ok(())
}
/*
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
*/

#[cfg(test)]
mod tests {
    use test_log::test;

    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_thing() -> anyhow::Result<()> {
        Ok(())
    }
}
