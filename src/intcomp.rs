use std::collections::VecDeque;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

use failure::Fail;

pub type Value = i64;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Code {
    Add,
    Multiply,
    Input,
    Output,
    Halt,
}

impl Code {
    fn parameters(self) -> usize {
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

pub struct CodeFindError {
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

impl Error for CodeFindError {}

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

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mode {
    Position,
    Immediate,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Instruction {
    code: Code,
    modes: [Mode; 3],
}

impl TryFrom<Value> for Instruction {
    type Error = CodeFindError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let code = Code::try_from(value % 100)?;

        fn mode_from_num(n: Value) -> Option<Mode> {
            match n {
                0 => Some(Mode::Position),
                1 => Some(Mode::Immediate),
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

impl Instruction {
    pub fn parameters(self, ix: usize, registers: &[Value]) -> Vec<Value> {
        let rn = self.code.parameters();
        let mut params = Vec::with_capacity(rn);
        for j in 0..rn {
            let value = registers[ix + j + 1];
            let value = match self.modes[j] {
                Mode::Immediate => value,
                Mode::Position => registers[value as usize],
            };
            params.push(value);
        }

        params
    }
}

#[derive(Fail, Debug)]
pub enum InvalidInstruction {
    #[fail(
        display = "Pointer {}: No Code found with the value {}",
        position, code
    )]
    InvalidCode { position: usize, code: Value },

    #[fail(display = "Pointer {}: Invalid access at {}", position, loc)]
    InvalidAddress { position: usize, loc: usize },

    #[fail(display = "Pointer {}: Expected input, none found", position)]
    MissingInput { position: usize },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntComp {
    pub position: Option<usize>,
    pub values: Vec<Value>,
    pub inputs: VecDeque<Value>,
    pub outputs: VecDeque<Value>,
}

impl IntComp {
    pub fn new(values: Vec<Value>) -> Self {
        IntComp {
            position: Some(0),
            values,
            inputs: Default::default(),
            outputs: Default::default(),
        }
    }

    fn get(&self, loc: usize) -> Result<Value, InvalidInstruction> {
        let v = self
            .values
            .get(loc)
            .ok_or_else(|| InvalidInstruction::InvalidAddress {
                position: self.position.unwrap_or(0),
                loc,
            })?;

        Ok(*v)
    }

    fn get_mut(&mut self, loc: usize) -> Result<&mut Value, InvalidInstruction> {
        let opted = self.values.get_mut(loc);
        let pos = self.position.unwrap_or(0);
        opted.ok_or(InvalidInstruction::InvalidAddress { position: pos, loc })
    }

    // Returns true if there's more work to do, false if finished
    pub fn step(&mut self) -> Result<bool, InvalidInstruction> {
        let pos = match self.position {
            None => return Ok(false),
            Some(p) => p,
        };

        let cv = self.get(pos)?;

        let instruction =
            Instruction::try_from(cv).map_err(|_| InvalidInstruction::InvalidCode {
                position: pos,
                code: cv,
            })?;
        let rn = instruction.code.parameters();
        if self.values.len() <= pos + rn {
            return Err(InvalidInstruction::InvalidAddress {
                position: pos,
                loc: pos + rn,
            });
        }
        let params = instruction.parameters(pos, &self.values);

        log::debug!(
            "Apply {:?} at position {} with registers {:?}",
            instruction,
            pos,
            &self.values[pos..=pos + rn],
        );
        log::debug!("  Parameters: {:?}", params);

        let adv = match instruction.code {
            Code::Add => {
                let out_ix = self.get(pos + 3)? as usize;
                let out_loc = self.get_mut(out_ix)?;
                log::debug!(
                    "  Adding {}: {} -> {}",
                    out_ix,
                    *out_loc,
                    params[0] + params[1],
                );
                *out_loc = params[0] + params[1];
                Some(4)
            }
            Code::Multiply => {
                let out_ix = self.get(pos + 3)? as usize;
                let out_loc = self.get_mut(out_ix)?;
                log::debug!(
                    "  Multiplying {}: {} -> {}",
                    out_ix,
                    *out_loc,
                    params[0] * params[1],
                );

                *out_loc = params[0] * params[1];
                Some(4)
            }
            Code::Input => {
                let input = self
                    .inputs
                    .pop_front()
                    .ok_or(InvalidInstruction::MissingInput { position: pos })?;
                let out_ix = self.get(pos + 3)? as usize;
                let out_loc = self.get_mut(out_ix)?;
                *out_loc = input;
                Some(2)
            }
            Code::Output => {
                self.outputs.push_back(params[0]);
                Some(2)
            }
            Code::Halt => None,
        };

        self.position = match adv {
            None => None,
            Some(a) => Some(pos + a),
        };

        Ok(adv.is_some())
    }

    pub fn run(&mut self) -> Result<(), failure::Error> {
        while self.step()? {}

        Ok(())
    }
}

impl FromStr for IntComp {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pieces = s.trim().split(',');

        let v: Result<Vec<Value>, ParseIntError> = pieces.map(str::parse).collect();

        Ok(IntComp::new(v?))
    }
}

#[cfg(test)]
mod tests {
    use test_env_log::test;

    use super::*;

    #[test]
    fn test_intcodes() -> Result<(), failure::Error> {
        let start = vec![1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50];
        let mut cp = IntComp::new(start.clone());

        assert_eq!(cp.step()?, true);
        assert_eq!(cp.values[0..4], [1, 9, 10, 70]);
        assert_eq!(cp.position, Some(4));

        assert_eq!(cp.step()?, true);
        assert_eq!(cp.values[0], 3500);

        assert_eq!(cp.step()?, false);

        assert_eq!(
            cp.values,
            vec![3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
        );

        cp = IntComp::new(start);
        cp.run()?;
        assert_eq!(
            cp.values,
            vec![3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
        );

        Ok(())
    }

    #[test]
    fn test_more_intcodes() -> Result<(), failure::Error> {
        let mut cp = IntComp::new(vec![1, 0, 0, 0, 99]);
        cp.run()?;
        assert_eq!(cp.values, vec![2, 0, 0, 0, 99]);

        cp = IntComp::new(vec![2, 3, 0, 3, 99]);
        cp.run()?;
        assert_eq!(cp.values, vec![2, 3, 0, 6, 99]);

        cp = IntComp::new(vec![2, 4, 4, 5, 99, 0]);
        cp.run()?;
        assert_eq!(cp.values, vec![2, 4, 4, 5, 99, 9801]);

        cp = IntComp::new(vec![1, 1, 1, 4, 99, 5, 6, 0, 99]);
        cp.run()?;
        assert_eq!(cp.values, vec![30, 1, 1, 4, 2, 5, 6, 0, 99]);

        Ok(())
    }

    #[test]
    fn test_modes() -> Result<(), failure::Error> {
        let i = Instruction::try_from(1002)?;
        assert_eq!(
            i,
            Instruction {
                code: Code::Multiply,
                modes: [Mode::Position, Mode::Immediate, Mode::Position],
            }
        );

        Ok(())
    }

    #[test]
    fn test_comp_modes() -> Result<(), failure::Error> {
        let mut cp = IntComp::from_str("1002,4,3,4,33")?;

        assert_eq!(cp.step()?, true);
        assert_eq!(cp.position, Some(4));
        assert_eq!(cp.values, vec![1002, 4, 3, 4, 99]);

        assert_eq!(cp.step()?, false);
        assert_eq!(cp.position, None);
        assert_eq!(cp.values, vec![1002, 4, 3, 4, 99]);

        Ok(())
    }
}
