use std::error::Error;
use std::fmt::Debug;
use std::str::FromStr;

use log::debug;
use log::warn;

// Parse a series of items from iterator.
pub fn parse_iter<E, S, T, F, Item>(iter: T) -> Result<Vec<Item>, Item::Err>
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
pub fn parse_err_iter<E, S, T, F, Item>(iter: T) -> Result<Vec<Item>, Box<dyn Error>>
where
    E: Error + 'static,
    S: AsRef<str>,
    T: IntoIterator<Item = Result<S, E>>,
    Item: Debug + FromStr,
    Item::Err: Error + 'static,
{
    iter.into_iter()
        .filter_map(|rl| match rl {
            Err(e) => {
                warn!("  Error getting line: {}", e);
                Some(Err(Box::new(e) as Box<dyn Error>))
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
                    Some(fd.map_err(|e| Box::new(e) as Box<dyn Error>))
                }
            }
        })
        .collect()
}
