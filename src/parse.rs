use std::error::Error;
use std::fmt::{Debug, Display};
use std::iter::FromIterator;
use std::str::FromStr;

use log::debug;
use log::warn;

pub fn trimmed_lines<'a, I>(iter: I) -> impl Iterator<Item = &'a str>
where
    I: IntoIterator<Item = &'a str>,
{
    iter.into_iter().filter_map(|l| {
        let trimmed = l.trim();
        if trimmed.is_empty() {
            None
        } else {
            Some(trimmed)
        }
    })
}

pub fn trimmed_err_lines<'a, I, E>(iter: I) -> impl Iterator<Item = Result<&'a str, E>>
where
    I: IntoIterator<Item = Result<&'a str, E>>,
    E: Error + Send + Sync + Sized + 'static,
{
    iter.into_iter().filter_map(|rl: Result<&str, _>| match rl {
        Err(e) => Some(Err(e)),
        Ok(l) => {
            let trimmed = l.trim();
            if trimmed.is_empty() {
                None
            } else {
                Some(Ok(trimmed))
            }
        }
    })
}

// Parse a series of items from iterator.
pub fn parse_iter<S, T, Item, F>(iter: T) -> Result<F, Item::Err>
where
    S: AsRef<str>,
    T: IntoIterator<Item = S>,
    Item: Debug + FromStr,
    Item::Err: Display,
    F: FromIterator<Item>,
{
    iter.into_iter()
        .filter_map(|l| {
            let trimmed = l.as_ref().trim();
            if trimmed.is_empty() {
                None
            } else {
                let fd = Item::from_str(trimmed);
                match fd {
                    Ok(ref i) => debug!("  Parsed line '{}' -> {:?}", trimmed, i),
                    Err(ref e) => warn!("  Error parsing line '{}': {}", trimmed, e),
                }
                Some(fd)
            }
        })
        .collect()
}

// Parse a series of items from iterator.
pub fn parse_err_iter<E, S, T, Item, F>(iter: T) -> anyhow::Result<F>
where
    E: Into<anyhow::Error> + Display,
    S: AsRef<str>,
    T: IntoIterator<Item = Result<S, E>>,
    Item: Debug + FromStr,
    Item::Err: Into<anyhow::Error> + Display,
    F: FromIterator<Item>,
{
    iter.into_iter()
        .filter_map(|rl| match rl {
            Err(e) => {
                warn!("  Error getting line: {}", e);
                Some(Err(e.into()))
            }
            Ok(l) => {
                let trimmed = l.as_ref().trim();
                if trimmed.is_empty() {
                    None
                } else {
                    let fd = Item::from_str(trimmed);
                    match fd {
                        Ok(ref i) => debug!("  Parsed line '{}' -> {:?}", trimmed, i),
                        Err(ref e) => warn!("  Error parsing line '{}': {}", trimmed, e),
                    }
                    Some(fd.map_err(|e| e.into()))
                }
            }
        })
        .collect()
}
