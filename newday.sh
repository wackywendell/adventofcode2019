#!/bin/bash

day="$1";

[ "$day" -eq "$day" ] 2>/dev/null
if [ $? -ne 0 ]; then
    echo "Expected a number"
    exit 1
fi

if [ "$day" -gt 25 ] 2>/dev/null; then
    echo "Expected a number <= 25"
    exit 2
fi

touch inputs/day"$day".txt
mkdir -p src/day"$day"
touch src/day"$day"/README.txt

main="src/day$day/main.rs"
if [[ -f "$main" ]]; then
    echo "Main already exists; leaving untouched."
else
    cat >"$main" <<EOF
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use clap::{App, Arg};
use log::debug;

fn main() -> Result<(), failure::Error> {
    env_logger::init();

    let matches = App::new("Day $day")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day$day.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    for line in buf_reader.lines() {
        println!("{}", line?)
    }

    Ok(())
}
EOF

    cat >>Cargo.toml <<EOF


[[bin]]
name = "day$day"
path = "src/day$day/main.rs"
EOF
fi

code . inputs/day"$day".txt src/day"$day"/README.txt "$main"