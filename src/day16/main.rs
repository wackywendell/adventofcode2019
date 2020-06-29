use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use log::debug;

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
        println!(
            "ix: {}, self.index: {}, self.form: {}",
            ix, self.index, self.form
        );
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

    for line in buf_reader.lines() {
        println!("{}", line?)
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
}
