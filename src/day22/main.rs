use std::collections::VecDeque;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::str::FromStr;

use clap::{App, Arg};
use log::debug;
use text_io::try_scan;

use aoc::parse::parse_err_iter;

pub type Card = i64;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Action {
    DealNew,
    Cut(isize),
    // bool for inverse
    DealIncrement(usize, bool),
}

impl Action {
    pub fn invert(self) -> Self {
        match self {
            Action::DealNew => Action::DealNew,
            Action::Cut(n) => Action::Cut(-n),
            Action::DealIncrement(n, inverse) => Action::DealIncrement(n, !inverse),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    index: usize,
    len: usize,
}

impl Position {
    pub fn apply(self, action: Action) -> anyhow::Result<Position> {
        let index = match action {
            Action::DealNew => self.len - self.index - 1,
            Action::Cut(n) => {
                let shift = ((self.len as isize) - n) as usize;
                (self.index + shift) % self.len
            }
            Action::DealIncrement(n, false) => (self.index * n) % self.len,
            Action::DealIncrement(n, true) => {
                let inv = multiplicative_inverse(n as u128, self.len as u128);
                let inv = inv.ok_or_else(|| {
                    anyhow::anyhow!("Cannot take the inverse of {} in base {}", n, self.len)
                })? as usize;
                (self.index * inv) % self.len
            }
        };

        Ok(Position { index, ..self })
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ActionSet {
    length: i128,
    mul: i128,
    add: i128,
}

impl ActionSet {
    pub fn new(length: usize) -> Self {
        ActionSet {
            length: length as i128,
            mul: 1,
            add: 0,
        }
    }

    pub fn from_actions<I: IntoIterator<Item = Action>>(length: usize, actions: I) -> Self {
        let mut set = ActionSet::new(length);
        for a in actions {
            set.append(a).unwrap();
        }

        set
    }

    pub fn append(&mut self, action: Action) -> anyhow::Result<()> {
        match action {
            Action::DealNew => {
                self.mul *= -1;
                self.add = -self.add + self.length - 1;
            }
            Action::Cut(n) => {
                self.add = (self.add - n as i128) % self.length;
            }
            Action::DealIncrement(n, false) => {
                self.mul = (self.mul * n as i128) % self.length;
                self.add = (self.add * n as i128) % self.length;
            }
            Action::DealIncrement(n, true) => {
                let inv = multiplicative_inverse(n as u128, self.length as u128);
                let inv = inv.ok_or_else(|| {
                    anyhow::anyhow!("Cannot take the inverse of {} in base {}", n, self.length)
                })? as u128;

                self.mul = (self.mul * inv as i128) % self.length;
                self.add = (self.add * inv as i128) % self.length;
            }
        };

        self.mul %= self.length;
        self.add %= self.length;

        Ok(())
    }

    pub fn invert(&self) -> anyhow::Result<Self> {
        if self.mul == 1 {
            return Ok(ActionSet {
                mul: 1,
                add: (-self.add).rem_euclid(self.length),
                length: self.length,
            });
        }

        let mul = self.mul.rem_euclid(self.length);

        let inv = multiplicative_inverse(mul as u128, self.length as u128);
        let inv = inv.ok_or_else(|| {
            anyhow::anyhow!(
                "Cannot take the inverse of {} in base {}",
                self.mul,
                self.length
            )
        })? as i128;

        let add = (inv * (-self.add)).rem_euclid(self.length);

        Ok(ActionSet {
            mul: inv,
            add,
            length: self.length,
        })
    }

    pub fn pow(&self, pow: usize) -> anyhow::Result<Self> {
        // See https://github.com/Aidiakapi/advent_of_code_2019/blob/7a3057e0a9fc754868698203f668b1db95a26b55/src/day22.rs#L138
        // let f(x) = m x + a
        // Then f^n(x) = pm^x + (am^x - a) / (m - 1)

        // m^x
        if pow == 1 {
            return Ok(self.clone());
        }
        if pow == 0 {
            return Ok(ActionSet {
                mul: 1,
                add: 0,
                length: self.length,
            });
        }
        if self.mul == 1 {
            return Ok(ActionSet {
                mul: 1,
                add: (self.add * (pow as i128)).rem_euclid(self.length),
                length: self.length,
            });
        }
        println!("{:?} ^ {}", self, pow);
        let mul = self.mul.rem_euclid(self.length);
        let add = self.add.rem_euclid(self.length);
        println!("{:?} ^ {}", self, pow);
        let m_x = modular_exponentiate(mul as u128, pow as u128, self.length as u128) as i128;

        println!("Done 1");

        let denom = (mul - 1).rem_euclid(self.length);
        let inv = multiplicative_inverse(denom as u128, self.length as u128);
        let inv = inv.ok_or_else(|| {
            anyhow::anyhow!(
                "Cannot take the inverse of {} in base {}",
                denom,
                self.length
            )
        })? as i128;
        println!("Done 2");

        Ok(ActionSet {
            mul: m_x,
            add: ((add * m_x - add).rem_euclid(self.length) * inv).rem_euclid(self.length),
            length: self.length,
        })
    }

    pub fn apply(&self, index: usize) -> usize {
        let ix = ((index as i128) * self.mul).rem_euclid(self.length);
        let ix = (ix + self.add).rem_euclid(self.length);
        ix as usize
    }
}

// impl Action {
//     fn apply_reverse(self, len: usize, index: usize) ->usize{
//         match self {
//             Action::DealNew => len - index,
//             Action::Cut(n) if n > 0 {
//                 (index + (n as usize)) & len,
//             }
//         }
//     }
// }

impl FromStr for Action {
    type Err = anyhow::Error;

    fn from_str(l: &str) -> Result<Self, Self::Err> {
        let l = l.trim();
        if l.trim() == "deal into new stack" {
            return Ok(Action::DealNew);
        }
        if l.starts_with("deal with increment ") {
            let var: String;
            try_scan!(l.bytes() => "deal with increment {}", var);
            return Ok(Action::DealIncrement(var.parse()?, false));
        }
        if l.starts_with("cut ") {
            let var: String;
            try_scan!(l.bytes() => "cut {}", var);
            return Ok(Action::Cut(var.parse()?));
        }

        return Err(anyhow::anyhow!("Can't interpret line {}", l));
    }
}

/// From: https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Computing_multiplicative_inverses_in_modular_structures
fn multiplicative_inverse(n: u128, base: u128) -> Option<u128> {
    let mut t = 0i128;
    let mut newt = 1i128;
    let mut r = base as i128;
    let mut newr = n as i128;

    while newr != 0 {
        let quotient = r / newr;
        let nextt = t - quotient * newt;
        let nextr = r - quotient * newr;
        t = newt;
        newt = nextt;
        r = newr;
        newr = nextr;
    }

    if r > 1 {
        None
    } else if t < 0 {
        Some((t + base as i128) as u128)
    } else {
        Some(t as u128)
    }
}

// From: https://en.wikipedia.org/wiki/Modular_exponentiation#Pseudocode
pub fn modular_exponentiate(base: u128, exponent: u128, modulus: u128) -> u128 {
    if modulus == 1 {
        return 0;
    }

    println!("{}^{} % {}", base, exponent, modulus);
    let m = modulus as u128;
    let mut b = base as u128;
    let mut e = exponent as u128;
    assert!((m - 1) < std::u64::MAX as u128);

    let mut result = 1u128;
    while e > 0 {
        println!("{}^{} % {}", b, e, m);
        if (e % 2) == 1 {
            result = (result * b) % m;
        }

        // b^(2e) = (b^2) * e
        e >>= 1;
        b = (b * b) % m;
    }

    result
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Deck {
    cards: VecDeque<Card>,
}

impl Deck {
    pub fn new<I: IntoIterator<Item = Card>>(cards: I) -> Self {
        Deck {
            cards: cards.into_iter().collect(),
        }
    }

    pub fn reverse(&mut self) {
        let slice = self.cards.make_contiguous();
        slice.reverse();
    }

    pub fn cut(&mut self, position: isize) {
        let ilen = self.cards.len() as isize;
        assert!(position < ilen);
        assert!(position > -ilen);
        let at = if position < 0 {
            (ilen + position) as usize
        } else {
            position as usize
        };

        let mut latter_half = self.cards.split_off(at);
        std::mem::swap(&mut self.cards, &mut latter_half);
        self.cards.append(&mut latter_half);
    }

    pub fn deal(&mut self, increment: usize) {
        let len = self.cards.len();
        let mut new_deck: Vec<Option<Card>> = self.cards.iter().map(|_| None).collect();
        for (i, card) in self.cards.drain(..).enumerate() {
            let spot = (i * increment) % len;
            assert!(new_deck[spot].is_none());
            new_deck[spot] = Some(card);
        }

        let deck = new_deck.iter().map(|&val| val.unwrap()).collect();
        self.cards = deck;
    }

    pub fn shuffle(&mut self, action: Action) -> Result<(), anyhow::Error> {
        match action {
            Action::DealNew => self.reverse(),
            Action::Cut(n) => self.cut(n),
            Action::DealIncrement(n, false) => self.deal(n as usize),
            Action::DealIncrement(n, true) => {
                let inv = multiplicative_inverse(n as u128, self.cards.len() as u128);
                let inv = inv.ok_or_else(|| {
                    anyhow::anyhow!(
                        "Cannot take the inverse of {} in base {}",
                        n,
                        self.cards.len()
                    )
                })? as u128;
                self.deal(inv as usize)
            }
        };

        Ok(())
    }
}

impl From<Vec<Card>> for Deck {
    fn from(cards: Vec<Card>) -> Deck {
        Deck {
            cards: cards.into(),
        }
    }
}

impl From<Deck> for Vec<Card> {
    fn from(deck: Deck) -> Vec<Card> {
        deck.cards.into()
    }
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let matches = App::new("Day 22")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("INPUT")
                .takes_value(true),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap_or("inputs/day22.txt");

    debug!("Using input {}", input_path);
    let file = File::open(input_path)?;
    let buf_reader = BufReader::new(file);

    let actions: Vec<Action> = parse_err_iter(buf_reader.lines())?;
    println!("Found {} actions", actions.len());

    let mut deck = Deck::new(0..=10006);
    let aset = ActionSet::from_actions(deck.cards.len(), actions.iter().copied());
    for &action in &actions {
        deck.shuffle(action)?;
    }

    println!("Expect card 2019 at {}", aset.apply(2019));

    for (i, &card) in deck.cards.iter().enumerate() {
        if card == 2019 {
            println!("Found card 2019 at {}", i);
        }
    }

    // Part 2
    let big_deck_size = 119315717514047;
    let n_reps = 101741582076661;
    let aset2 = ActionSet::from_actions(big_deck_size, actions.iter().copied())
        .invert()?
        .pow(n_reps)?;

    println!("Expect position 2020 to contain {}", aset2.apply(2020));

    Ok(())
}

// #[allow(clippy::clippy::unnecessary_wraps)]
#[cfg(test)]
mod tests {
    use test_env_log::test;

    #[allow(unused_imports)]
    use super::*;

    use aoc::parse::parse_iter;

    const INSTRUCTIONS1: &str = "\
        deal with increment 7
        deal into new stack
        deal into new stack";

    #[test]
    fn test_first() -> anyhow::Result<()> {
        let mut deck = Deck::new(0..=9);
        let expected = Deck::from(vec![0, 3, 6, 9, 2, 5, 8, 1, 4, 7]);
        deck.deal(7);
        deck.reverse();
        deck.reverse();

        assert_eq!(deck, expected);

        let actions: Vec<Action> = parse_iter(INSTRUCTIONS1.lines())?;
        assert_eq!(
            actions,
            vec![
                Action::DealIncrement(7, false),
                Action::DealNew,
                Action::DealNew
            ]
        );

        deck = Deck::new(0..=9);
        for action in actions {
            deck.shuffle(action)?;
        }
        assert_eq!(deck, expected);

        Ok(())
    }

    const INSTRUCTIONS2: &str = "\
    cut 6
    deal with increment 7
    deal into new stack";

    #[test]
    fn test_second() -> anyhow::Result<()> {
        let mut deck = Deck::new(0..=9);
        let actions: Vec<Action> = parse_iter(INSTRUCTIONS2.lines())?;
        let expected = Deck::from(vec![3, 0, 7, 4, 1, 8, 5, 2, 9, 6]);
        for action in actions {
            deck.shuffle(action)?;
        }
        assert_eq!(deck, expected);

        Ok(())
    }

    const INSTRUCTIONS3: &str = "\
    deal with increment 7
    deal with increment 9
    cut -2";

    #[test]
    fn test_third() -> anyhow::Result<()> {
        let mut deck = Deck::new(0..=9);
        let actions: Vec<Action> = parse_iter(INSTRUCTIONS3.lines())?;
        let expected = Deck::from(vec![6, 3, 0, 7, 4, 1, 8, 5, 2, 9]);
        for action in actions {
            let start = deck.clone();
            deck.shuffle(action)?;
            println!("Action: {:?} {:?} -> {:?}", action, start, deck);
        }
        assert_eq!(deck, expected);

        Ok(())
    }

    const INSTRUCTIONS4: &str = "\
    deal into new stack
    cut -2
    deal with increment 7
    cut 8
    cut -4
    deal with increment 7
    cut 3
    deal with increment 9
    deal with increment 3
    cut -1";

    #[test]
    fn test_fourth() -> anyhow::Result<()> {
        let mut deck = Deck::new(0..=9);
        let actions: Vec<Action> = parse_iter(INSTRUCTIONS4.lines())?;
        let expected = Deck::from(vec![9, 2, 5, 8, 1, 4, 7, 0, 3, 6]);
        for action in actions {
            deck.shuffle(action)?;
        }
        assert_eq!(deck, expected);

        Ok(())
    }

    #[test]
    fn test_multiplicative_inverse() {
        // (n, base)
        let examples: Vec<(usize, usize)> = vec![(7, 15), (213, 391), (37, 480)];

        for &(n, length) in &examples {
            let m = multiplicative_inverse(n as u128, length as u128).unwrap() as usize;
            assert_eq!(
                (n * m) % length,
                1,
                "{}*{} % {} = {}",
                n,
                m,
                length,
                (n * m) % length
            );
        }
    }

    #[test]
    fn test_fourth_inverse() -> anyhow::Result<()> {
        let mut deck = Deck::new(0..=9);
        let actions: Vec<Action> = parse_iter(INSTRUCTIONS4.lines())?;
        let expected = Deck::from(vec![9, 2, 5, 8, 1, 4, 7, 0, 3, 6]);
        for action in actions {
            let start = deck.clone();
            deck.shuffle(action)?;
            println!("Shuffled {:?}: {:?} -> {:?}", action, start, deck);
            for (ix, &val) in start.cards.iter().enumerate() {
                let pos = Position {
                    index: ix,
                    len: deck.cards.len(),
                };
                let next = pos.apply(action)?;
                println!(
                    "  {} -> {}, but {} != {}",
                    pos.index, next.index, val, deck.cards[next.index]
                );
                assert_eq!(val, deck.cards[next.index]);
                let rev = next.apply(action.invert())?;
                assert_eq!(rev, pos);
            }

            deck.shuffle(action.invert())?;
            assert_eq!(start, deck);
            deck.shuffle(action)?;
        }
        assert_eq!(deck, expected);

        Ok(())
    }

    const INSTRUCTIONS5: &str = "\
    deal into new stack
    cut -3
    deal with increment 7
    cut 7
    cut -3
    deal with increment 7
    cut 3
    deal with increment 9
    deal with increment 3
    cut -1
    ";

    #[test]
    fn test_fifth_pow() -> anyhow::Result<()> {
        let start = Deck::new(0..=12);
        let mut deck = start.clone();
        let actions: Vec<Action> = parse_iter(INSTRUCTIONS5.lines())?;
        let action_set = ActionSet::from_actions(deck.cards.len(), actions.iter().copied());
        let power = 33;

        let mut successes = 0;
        for p in 1..=power {
            println!("Pow: {}", p);
            for &action in &actions {
                deck.shuffle(action)?;
            }

            let set = match action_set.pow(p) {
                Err(e) => {
                    println!("Skipping pow {}: {}", p, e);
                    continue;
                }
                Ok(s) => s,
            };
            println!("  Actions: {:?}^{} = {:?}", action_set, p, set);

            let inverse = set.invert()?;
            for (ix, &card) in start.cards.iter().enumerate() {
                let new_ix = set.apply(ix);
                assert_eq!(card, deck.cards[new_ix]);
                assert_eq!(
                    ix,
                    inverse.apply(new_ix),
                    "{:?} => {:?} :: {} => {} != {}",
                    set,
                    inverse,
                    new_ix,
                    inverse.apply(new_ix),
                    ix
                );
            }
            successes += 1;
        }

        assert!(successes > power / 3, "{} successes / {}", successes, power);
        assert!(successes > 2, "{} successes / {}", successes, power);

        Ok(())
    }

    const INSTRUCTIONS6: &str = "\
    deal into new stack
    cut -34801
    deal with increment 78188108
    deal into new stack
    cut 71188192
    cut -318881181
    deal with increment 71181888880008192
    cut 861610
    deal with increment 597109981
    deal into new stack
    deal with increment 171798
    cut -40098101
    ";

    #[test]
    fn test_big_pow() -> anyhow::Result<()> {
        let n_cards = 119315717514047;
        let n_reps = 101741582076661;
        let actions: Vec<Action> = parse_iter(INSTRUCTIONS6.lines())?;
        let set = ActionSet::from_actions(n_cards, actions.iter().copied());
        let inv = set.invert()?;

        let rep = set.pow(n_reps)?;
        let rep_inv = rep.invert()?;
        assert_eq!(inv.pow(n_reps)?, rep_inv);

        for ix in 0..1000 {
            let new_ix = set.apply(ix);
            let rev = inv.apply(new_ix);
            assert_eq!(ix, rev);

            let new_ix = inv.apply(ix);
            let rev = set.apply(new_ix);
            assert_eq!(ix, rev);

            let new_ix = rep.apply(ix);
            let rev = rep_inv.apply(new_ix);
            assert_eq!(ix, rev);

            let new_ix = rep_inv.apply(ix);
            let rev = rep.apply(new_ix);
            assert_eq!(ix, rev);
        }

        Ok(())
    }
}
