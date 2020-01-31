use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

use log::{debug, info};

type Value = i64;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum Code {
    Add,
    Multiply,
    Input,
    Output,
    Halt,
}

impl Code {
    fn registers_needed(self) -> usize {
        match self {
            Code::Add => 3,
            Code::Multiply => 3,
            Code::Input => 1,
            Code::Output => 1,
            Code::Halt => 0,
        }
    }
}

impl fmt::Display for Code {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Code::Add => "Add",
            Code::Multiply => "Mul",
            Code::Input => "In",
            Code::Output => "Out",
            Code::Halt => "Halt",
        };

        f.write_str(s)
    }
}

struct CodeFindError {
    value: Value,
}

impl fmt::Display for CodeFindError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error converting {} to code", self.value)
    }
}

impl fmt::Debug for CodeFindError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error converting {} to code", self.value)
    }
}

impl TryFrom<Value> for Code {
    type Error = CodeFindError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Code::Add),
            2 => Ok(Code::Multiply),
            3 => Ok(Code::Input),
            4 => Ok(Code::Output),
            99 => Ok(Code::Halt),
            v => Err(CodeFindError { value: v }),
        }
    }
}

impl Error for CodeFindError {}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum Mode {
    Position,
    Immediate,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Instruction {
    code: Code,
    modes: [Mode; 3],
}

impl TryFrom<Value> for Instruction {
    type Error = CodeFindError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let code = Code::try_from(value % 100)?;

        fn mode_from_num(n: Value) -> Option<Mode> {
            match n {
                0 => Some(Mode::Immediate),
                1 => Some(Mode::Position),
                _ => None,
            }
        }

        let mode1 = mode_from_num((value / 100) % 10).ok_or(CodeFindError { value })?;
        let mode2 = mode_from_num((value / 1000) % 10).ok_or(CodeFindError { value })?;
        let mode3 = mode_from_num((value / 10_000) % 10).ok_or(CodeFindError { value })?;

        Ok(Instruction {
            code,
            modes: [mode1, mode2, mode3],
        })
    }
}
