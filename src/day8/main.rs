use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use log::debug;

pub struct Image {
    pixels: Vec<Vec<Vec<u8>>>,
}

impl Image {
    pub fn new(
        input: Vec<u8>,
        layers: usize,
        width: usize,
        height: usize,
    ) -> Result<Image, failure::Error> {
        let size = width * height;
        if input.len() != layers * size {
            return Err(failure::err_msg(format!(
                "Found {} pixels; expected {}*{}*{}={}",
                input.len(),
                layers,
                height,
                width,
                layers * size
            )));
        }

        let mut pixels: Vec<Vec<Vec<u8>>> = Vec::with_capacity(layers);
        for layer in input.chunks(size) {
            let mut rows: Vec<Vec<u8>> = Vec::with_capacity(height);
            for row in layer.chunks(width) {
                rows.push(row.to_owned())
            }
            pixels.push(rows);
        }

        Ok(Image { pixels })
    }
}

fn parse_digits(line: &str) -> Result<Vec<u8>, failure::Error> {
    let mut ints = Vec::new();

    for c in line.chars() {
        let n = match c.to_digit(10) {
            None => {
                return Err(failure::err_msg(format!("Expected digit, found '{}'", c)));
            }
            Some(n) => n,
        };
        ints.push(n as u8);
    }

    Ok(ints)
}

fn main() -> Result<(), failure::Error> {
    env_logger::init();

    let matches = App::new("Day 8")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day8.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let mut ints: Vec<u8> = Vec::new();

    for line in buf_reader.lines() {
        let line = line?;
        let mut v = parse_digits(line.trim())?;
        ints.append(&mut v);
    }

    println!("Found {} ints", ints.len());

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    // use super::*;

    #[test]
    fn test_thing() -> Result<(), failure::Error> {
        Ok(())
    }
}
