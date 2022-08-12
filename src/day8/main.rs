use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use log::debug;

pub struct Rendered {
    pixels: Vec<Vec<u8>>,
}

impl Rendered {
    pub fn show(&self) -> impl Iterator<Item = String> + '_ {
        self.pixels.iter().map(|row| {
            row.iter()
                .map(|p| match p {
                    0 => ' ',
                    1 => '█',
                    _ => '?',
                })
                .collect()
        })
    }
}

pub struct Image {
    pixels: Vec<Vec<Vec<u8>>>,
}

impl Image {
    pub fn new(
        input: Vec<u8>,
        layers: usize,
        height: usize,
        width: usize,
    ) -> anyhow::Result<Image> {
        log::debug!("layers: {}, height: {}, width: {}", layers, height, width);

        let size = width * height;
        if input.len() != layers * size {
            return Err(anyhow::format_err!(
                "Found {} pixels; expected {}*{}*{}={}",
                input.len(),
                layers,
                height,
                width,
                layers * size
            ));
        }

        let mut pixels: Vec<Vec<Vec<u8>>> = Vec::with_capacity(layers);
        for layer in input.chunks(size) {
            let mut rows: Vec<Vec<u8>> = Vec::with_capacity(height);
            for row in layer.chunks(width) {
                log::debug!("Pushing row of size {} (width {})", row.len(), width);
                rows.push(row.to_owned())
            }
            pixels.push(rows);
        }

        Ok(Image { pixels })
    }

    pub fn shape(&self) -> (usize, usize, usize) {
        (
            self.pixels.len(),
            self.pixels[0].len(),
            self.pixels[0][0].len(),
        )
    }

    pub fn layer_count(&self, layer: usize) -> HashMap<u8, usize> {
        let mut counts = HashMap::new();
        for row in &self.pixels[layer] {
            for &p in row {
                let count = counts.entry(p).or_default();
                *count += 1;
            }
        }

        counts
    }

    pub fn layer_counts(&self) -> Vec<HashMap<u8, usize>> {
        let mut v = Vec::with_capacity(self.pixels.len());
        for layer in &self.pixels {
            let mut counts = HashMap::new();
            for row in layer {
                for &p in row {
                    let count = counts.entry(p).or_default();
                    *count += 1;
                }
            }
            v.push(counts)
        }

        v
    }

    pub fn render(&self) -> Rendered {
        // Start with upper record
        let mut rendered = self.pixels[0].clone();

        for layer in &self.pixels[1..] {
            for (i, row) in layer.iter().enumerate() {
                for (j, &p) in row.iter().enumerate() {
                    let rp = rendered[i][j];
                    if rp != 2 {
                        continue;
                    }
                    rendered[i][j] = p;
                }
            }
        }

        Rendered { pixels: rendered }
    }
}

pub fn parse_image(
    line: &str,
    layers: usize,
    height: usize,
    width: usize,
) -> anyhow::Result<Image> {
    let digits = parse_digits(line)?;
    Image::new(digits, layers, height, width)
}

fn parse_digits(line: &str) -> anyhow::Result<Vec<u8>> {
    let mut ints = Vec::new();

    for c in line.chars() {
        let n = match c.to_digit(10) {
            None => {
                return Err(anyhow::format_err!("Expected digit, found '{}'", c));
            }
            Some(n) => n,
        };
        ints.push(n as u8);
    }

    Ok(ints)
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 8")
        .arg(
            Arg::with_name("input")
                .short('i')
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day8.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let mut image = None;

    for line in buf_reader.lines() {
        let line = line?;
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        };

        let n_layers = trimmed.len() / (25 * 6);

        image = Some(parse_image(trimmed, n_layers, 6, 25)?);
        break;
    }

    let image = image.unwrap();
    let (layers, height, width) = image.shape();

    log::info!(
        "Found image with {} layers ({} * {})",
        layers,
        height,
        width
    );

    let counts = image.layer_counts();

    let ns: Vec<_> = counts
        .iter()
        .map(|c| {
            let (z, o, t) = (
                c.get(&0).copied().unwrap_or_default(),
                c.get(&1).copied().unwrap_or_default(),
                c.get(&2).copied().unwrap_or_default(),
            );
            log::debug!("Found counts: {}, {}, {}", z, o, t);
            (z, o, t)
        })
        .collect();

    let &(z, o, t) = ns.iter().min().unwrap();

    println!("Found min: {}, {}, {} -> {}", z, o, t, o * t);

    let rendered = image.render();

    for s in rendered.show() {
        println!("{}", s);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    use super::*;

    #[test]
    fn test_parse() -> anyhow::Result<()> {
        let img = parse_image("123456789012", 2, 2, 3)?;

        assert_eq!(
            img.pixels,
            vec![
                vec![vec![1, 2, 3], vec![4, 5, 6]],
                vec![vec![7, 8, 9], vec![0, 1, 2]],
            ]
        );

        Ok(())
    }

    #[test]
    fn test_counts() -> anyhow::Result<()> {
        let img = parse_image("123456789012", 2, 2, 3)?;

        let counts0 = img.layer_count(0);
        let exp0: Vec<(u8, usize)> = vec![(1, 1), (2, 1), (3, 1), (4, 1), (5, 1), (6, 1)];

        assert_eq!(counts0, exp0.iter().copied().collect());

        Ok(())
    }

    #[test]
    fn test_render() -> anyhow::Result<()> {
        let img = parse_image("0222112222120000", 4, 2, 2)?;

        let rendered = img.render();

        assert_eq!(rendered.pixels, vec![vec![0, 1], vec![1, 0]]);

        Ok(())
    }
}
