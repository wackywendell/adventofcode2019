use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::str::FromStr;

use clap::{App, Arg};
use log::debug;

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub struct Signal {
    values: Vec<i64>,
}

impl Signal {
    pub fn new(values: Vec<i64>) -> Signal {
        Signal { values }
    }
}

impl FromStr for Signal {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let values: anyhow::Result<Vec<i64>> = s
            .chars()
            .map(|c| {
                c.to_digit(10)
                    .map(|n| n as i64)
                    .ok_or_else(|| anyhow::format_err!("Cannot parse character {} as int", c))
            })
            .collect();

        Ok(Signal::new(values?))
    }
}

fn ones_digit(n: i64) -> i64 {
    (n % 10).abs()
}

#[derive(Copy, Clone)]
pub struct BasicPattern {
    form: i64,
}

impl BasicPattern {
    pub fn new(form: i64) -> Self {
        if form < 1 {
            panic!("Form {} must be >= 1", form);
        }
        BasicPattern { form }
    }

    pub fn apply(self, signal: &Signal) -> i64 {
        ones_digit(
            signal
                .values
                .iter()
                .zip(self.into_iter())
                .map(|(s, v)| s * v)
                .sum::<i64>(),
        )
    }

    pub fn transform(signal: &Signal) -> Signal {
        let len = signal.values.len();
        Signal::new(
            (1..=len)
                .map(|n| BasicPattern::new(n as i64).apply(signal))
                .collect(),
        )
    }

    pub fn repeated_transform(signal: &Signal, n: isize) -> Signal {
        let mut signal: Signal = signal.clone();
        for _ in 0..n {
            signal = BasicPattern::transform(&signal);
        }

        signal
    }
}

impl IntoIterator for BasicPattern {
    type Item = i64;
    type IntoIter = BasicPatternIter;

    fn into_iter(self) -> Self::IntoIter {
        BasicPatternIter {
            form: self.form,
            index: 0,
        }
    }
}

#[derive(Copy, Clone)]
pub struct BasicPatternIter {
    form: i64,
    index: i64,
}

impl Iterator for BasicPatternIter {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        let ix = ((self.index + 1) / self.form) % 4;
        self.index += 1;
        let values = [0i64, 1, 0, -1];
        Some(values[ix as usize])
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 16")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day16.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    for maybe_line in buf_reader.lines() {
        let line = maybe_line?;
        let signal: Signal = line.parse()?;
        let output = BasicPattern::repeated_transform(&signal, 100);
        for n in &output.values[..8] {
            print!("{}", n);
        }
        println!();
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    use super::*;

    #[test]
    fn test_pattern() -> anyhow::Result<()> {
        let p1 = BasicPattern::new(1);
        let iter = p1.into_iter();
        let expected = vec![1, 0, -1, 0, 1, 0, -1, 0];
        let values: Vec<i64> = iter.take(expected.len()).collect();
        assert_eq!(values, expected);

        let p2 = BasicPattern::new(2);
        let iter = p2.into_iter();
        let expected = vec![0, 1, 1, 0, 0, -1, -1, 0, 0, 1, 1, 0, 0, -1, -1, 0, 0];
        let values: Vec<i64> = iter.take(expected.len()).collect();
        assert_eq!(values, expected);

        Ok(())
    }

    #[test]
    fn test_pattern_apply() -> anyhow::Result<()> {
        let signal: Signal = str::parse("12345678")?;
        assert_eq!(BasicPattern::new(1).apply(&signal), 4);
        assert_eq!(BasicPattern::new(2).apply(&signal), 8);
        assert_eq!(BasicPattern::new(3).apply(&signal), 2);
        assert_eq!(BasicPattern::new(4).apply(&signal), 2);
        assert_eq!(BasicPattern::new(5).apply(&signal), 6);
        assert_eq!(BasicPattern::new(6).apply(&signal), 1);
        assert_eq!(BasicPattern::new(7).apply(&signal), 5);
        assert_eq!(BasicPattern::new(8).apply(&signal), 8);

        let outputs = [
            str::parse("12345678")?,
            str::parse("48226158")?,
            str::parse("34040438")?,
            str::parse("03415518")?,
            str::parse("01029498")?,
        ];

        for (n, row) in outputs.iter().enumerate() {
            let mut found_output: Signal = signal.clone();
            for _ in 0..n {
                found_output = BasicPattern::transform(&found_output)
            }
            assert_eq!(&found_output, row);
        }

        Ok(())
    }

    #[test]
    fn test_simple_applies() -> anyhow::Result<()> {
        let signal: Signal = str::parse("80871224585914546619083218645595")?;
        let output = BasicPattern::repeated_transform(&signal, 100);
        let expected: Signal = str::parse("24176176")?;
        assert_eq!(output.values[..8], expected.values[..]);

        let signal: Signal = str::parse("19617804207202209144916044189917")?;
        let output = BasicPattern::repeated_transform(&signal, 100);
        let expected: Signal = str::parse("73745418")?;
        assert_eq!(output.values[..8], expected.values[..]);

        let signal: Signal = str::parse("69317163492948606335995924319873")?;
        let output = BasicPattern::repeated_transform(&signal, 100);
        let expected: Signal = str::parse("52432133")?;
        assert_eq!(output.values[..8], expected.values[..]);

        Ok(())
    }
}
