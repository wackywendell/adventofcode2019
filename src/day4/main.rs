use clap::{App, Arg};
use log::debug;

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Password {
    digits: Vec<u8>,
}

impl Password {
    pub fn from_number(n: i64) -> Password {
        let mut digits = Vec::new();
        let mut m = n;
        while m > 0 {
            digits.push((m % 10) as u8);
            m /= 10;
        }

        digits.reverse();

        Password { digits }
    }

    pub fn valid(&self) -> bool {
        log::debug!("Testing validity of {:?}", self.digits);
        if self.digits.len() != 6 {
            log::debug!("  FAIL: Length {} != 6", self.digits.len());
            return false;
        }

        let mut repeats = false;
        for (l, n) in self.digits.iter().zip(&self.digits[1..]) {
            match n.cmp(l) {
                std::cmp::Ordering::Less => {
                    log::debug!("  FAIL: Out of order {} < {}", n, l);
                    return false;
                }
                std::cmp::Ordering::Equal => repeats = true,
                std::cmp::Ordering::Greater => {}
            }
        }

        if !repeats {
            log::debug!("  FAIL: No repeats");
        }

        repeats
    }

    pub fn extra_valid(&self) -> bool {
        log::debug!("Testing validity of {:?}", self.digits);
        if self.digits.len() != 6 {
            log::debug!("  FAIL: Length {} != 6", self.digits.len());
            return false;
        }

        let mut rep_count = 1;
        let mut doublet = false;
        for (l, n) in self.digits.iter().zip(&self.digits[1..]) {
            if n < l {
                return false;
            }
            match n.cmp(l) {
                std::cmp::Ordering::Less => {
                    log::debug!("  FAIL: Out of order {} < {}", n, l);
                    return false;
                }
                std::cmp::Ordering::Equal => rep_count += 1,
                std::cmp::Ordering::Greater if rep_count == 2 => {
                    doublet = true;
                    rep_count = 1;
                }
                std::cmp::Ordering::Greater => {
                    rep_count = 1;
                }
            }
        }

        doublet |= rep_count == 2;

        if !doublet {
            log::debug!("  FAIL: No doublet");
        }

        doublet
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 4")
        .arg(
            Arg::with_name("start")
                .short('s')
                .long("start")
                .value_name("START")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("end")
                .short('e')
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

    let (mut pws, mut eps) = (0, 0);
    for n in start..=end {
        let p = Password::from_number(n);
        if p.valid() {
            pws += 1;
        }
        if p.extra_valid() {
            eps += 1;
        }
    }

    println!("Found {} or {} valid passwords", pws, eps);

    Ok(())
}

#[cfg(test)]
mod tests {
    use test_log::test;

    use super::*;

    #[test]
    fn test_validity() {
        // Decreasing
        assert!(!Password::from_number(124_435).valid());
        // No repeat
        assert!(!Password::from_number(124_589).valid());

        // valid
        assert!(Password::from_number(122_345).valid());
        assert!(Password::from_number(135_669).valid());

        // From assignment
        assert!(Password::from_number(111_111).valid());
        assert!(!Password::from_number(223_450).valid());
        assert!(!Password::from_number(123_789).valid());
    }

    #[test]
    fn test_more_validity() {
        // Decreasing
        assert!(!Password::from_number(124_435).extra_valid());
        // No repeat
        assert!(!Password::from_number(124_589).extra_valid());
        // Repeat but no doublet
        assert!(!Password::from_number(124_449).extra_valid());

        // valid
        assert!(Password::from_number(122_345).extra_valid());
        assert!(Password::from_number(135_669).extra_valid());

        // From assignment part 1
        assert!(!Password::from_number(111_111).extra_valid());
        assert!(!Password::from_number(223_450).extra_valid());
        assert!(!Password::from_number(123_789).extra_valid());

        // From assignment part 2
        assert!(Password::from_number(112_233).extra_valid());
        assert!(!Password::from_number(123_444).extra_valid());
        assert!(Password::from_number(111_122).extra_valid());
    }
}
