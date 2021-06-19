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
                let n = (n % (self.len as isize)) as usize;
                (self.index + n) % self.len
            }
            Action::DealIncrement(n, false) => (self.index * n) % self.len,
            Action::DealIncrement(n, true) => {
                let inv = multiplicative_inverse(n, self.len);
                let inv = inv.ok_or_else(|| {
                    anyhow::anyhow!("Cannot take the inverse of {} in base {}", n, self.len)
                })?;
                (self.index * inv) % self.len
            }
        };

        Ok(Position { index, ..self })
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

fn multiplicative_inverse(n: usize, base: usize) -> Option<usize> {
    let mut t = 0isize;
    let mut newt = 1isize;
    let mut r = base as isize;
    let mut newr = n as isize;

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
        Some((t + base as isize) as usize)
    } else {
        Some(t as usize)
    }
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
                let inv = multiplicative_inverse(n, self.cards.len());
                let inv = inv.ok_or_else(|| {
                    anyhow::anyhow!(
                        "Cannot take the inverse of {} in base {}",
                        n,
                        self.cards.len()
                    )
                })?;
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
    for action in actions {
        deck.shuffle(action)?;
    }

    for (i, &card) in deck.cards.iter().enumerate() {
        if card == 2019 {
            println!("Found card 2019 at {}", i);
        }
    }

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
            let m = multiplicative_inverse(n, length).unwrap();
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
            deck.shuffle(action.invert())?;
            assert_eq!(start, deck);
            deck.shuffle(action)?;
        }
        assert_eq!(deck, expected);

        Ok(())
    }
}
