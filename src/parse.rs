use std::error::Error;
use std::fmt::Debug;
use std::str::FromStr;

use log::debug;
use log::warn;

// Parse a series of items from iterator.
pub fn parse_iter<E, S, T, Item>(iter: T) -> Result<Vec<Item>, Item::Err>
where
    S: AsRef<str>,
    T: IntoIterator<Item = S>,
    Item: Debug + FromStr,
    Item::Err: Error + 'static,
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
pub fn parse_err_iter<E, S, T, Item>(iter: T) -> Result<Vec<Item>, failure::Error>
where
    E: Error + Send + Sync + Sized + 'static,
    S: AsRef<str>,
    T: IntoIterator<Item = Result<S, E>>,
    Item: Debug + FromStr,
    Item::Err: Error + Send + Sync + Sized + 'static,
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
