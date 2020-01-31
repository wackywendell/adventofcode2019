use clap::{App, Arg};
use log::debug;

fn main() -> Result<(), failure::Error> {
    env_logger::init();

    let matches = App::new("Day 4")
        .arg(
            Arg::with_name("start")
                .short("s")
                .long("start")
                .value_name("START")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("end")
                .short("e")
                .long("end")
                .value_name("END")
                .takes_value(true),
        )
        .get_matches();

    // My input was
    // 134564-585159
    let start = matches
        .value_of("START")
        .map(str::parse::<i64>)
        .transpose()?
        .unwrap_or(134_564);
    let end = matches
        .value_of("END")
        .map(str::parse::<i64>)
        .transpose()?
        .unwrap_or(585_159);

    debug!("Using input {}-{}", start, end);

    Ok(())
}

#[cfg(test)]
mod tests {
    // use test_env_log::test;

    // use super::*;
}
