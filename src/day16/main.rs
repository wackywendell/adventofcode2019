use std::fmt;
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

    pub fn repeated(values: Vec<i64>, count: usize) -> Signal {
        let total = values.len() * count;
        let repeated_values = values.iter().copied().cycle().take(total).collect();
        Signal {
            values: repeated_values,
        }
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

impl fmt::Display for Signal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<")?;
        for &value in &self.values {
            write!(f, "{}", value)?;
        }
        write!(f, ">")
    }
}

pub struct RepeatedSignalPortion {
    pub values: Vec<i64>,
    buf: Vec<i64>,
    pub offset: usize,
    pub len: usize,
}

impl RepeatedSignalPortion {
    pub fn repeated(values: Vec<i64>, repeats: usize, offset: usize, len: usize) -> Self {
        let total = values.len() * repeats;
        assert!(offset * 2 > total, "Offset must be more than half");
        RepeatedSignalPortion {
            values: values
                .iter()
                .copied()
                .cycle()
                .skip(offset)
                .take(total - offset)
                .collect(),
            buf: Vec::new(),
            offset,
            len,
        }
    }

    pub fn transform(&mut self) {
        let sz = self.values.len();
        self.buf.clear();
        self.buf.resize(sz, 0);
        self.buf[sz - 1] = self.values[sz - 1];
        for ix in (0..sz - 1).rev() {
            self.buf[ix] = ones_digit(self.values[ix] + self.buf[ix + 1]);
        }

        std::mem::swap(&mut self.buf, &mut self.values);
    }

    pub fn from_line(
        input: &str,
        repeats: usize,
        offset_digits: usize,
        len: usize,
    ) -> anyhow::Result<Self> {
        let offset_str: String = input.chars().take(offset_digits).collect();
        let offset: usize = str::parse(&offset_str)?;
        let signal = Signal::from_str(input)?;
        Ok(Self::repeated(signal.values, repeats, offset, len))
    }
}

impl fmt::Display for RepeatedSignalPortion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for &value in &self.values[..self.len] {
            write!(f, "{}", value)?;
        }
        write!(f, "]")
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
    let line = buf_reader
        .lines()
        .next()
        .ok_or_else(|| anyhow::format_err!("No line found!"))??;
    let signal: Signal = line.parse()?;
    let output = BasicPattern::repeated_transform(&signal, 100);
    for n in &output.values[..8] {
        print!("{}", n);
    }
    println!();
    println!("-- Part Two --");
    let mut repeated = RepeatedSignalPortion::from_line(&line, 10000, 7, 8)?;
    for _ in 0..100 {
        repeated.transform();
    }
    println!("{}", repeated);

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    use super::*;

    #[test]
    fn test_pattern() {
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

    #[test]
    fn test_repeated_simple() -> anyhow::Result<()> {
        let input: Signal = str::parse("12345678")?;
        let repeats = 10;
        let offset = 64;
        let len = 8;
        let mut output: Signal = Signal::repeated(input.values.clone(), repeats);
        let mut repeated = RepeatedSignalPortion::repeated(input.values, repeats, offset, len);

        for i in 0..=20 {
            println!(
                "{:2}: {}\n    {}",
                i,
                Signal::new(output.values[offset..offset + len].to_owned()),
                repeated
            );
            output = BasicPattern::transform(&output);
            repeated.transform();

            assert_eq!(output.values[offset..offset + len], repeated.values[..len]);
        }

        Ok(())
    }

    #[test]
    fn test_repeated() -> anyhow::Result<()> {
        let mut signal =
            RepeatedSignalPortion::from_line("03036732577212944063491565474664", 10000, 7, 8)?;
        for _ in 0..100 {
            signal.transform();
        }
        let expected: Signal = str::parse("84462026")?;
        assert_eq!(signal.values[..8], expected.values[..]);

        let mut signal =
            RepeatedSignalPortion::from_line("02935109699940807407585447034323", 10000, 7, 8)?;
        for _ in 0..100 {
            signal.transform();
        }
        let expected: Signal = str::parse("78725270")?;
        assert_eq!(signal.values[..8], expected.values[..]);

        let mut signal =
            RepeatedSignalPortion::from_line("03081770884921959731165446850517", 10000, 7, 8)?;
        for _ in 0..100 {
            signal.transform();
        }
        let expected: Signal = str::parse("53553731")?;
        assert_eq!(signal.values[..8], expected.values[..]);

        Ok(())
    }
}
