use std::collections::VecDeque;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

use failure::Fail;

pub type Value = i64;

const MAX_SIZE: i64 = 100_000;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Code {
    Add,
    Multiply,
    Input,
    Output,
    TrueJump,
    FalseJump,
    LessThan,
    Equals,
    Relative,
    Halt,
}

impl Code {
    fn parameters(self) -> usize {
        match self {
            Code::Add => 3,
            Code::Multiply => 3,
            Code::Input => 1,
            Code::Output => 1,
            Code::TrueJump => 2,
            Code::FalseJump => 2,
            Code::LessThan => 3,
            Code::Equals => 3,
            Code::Relative => 1,
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
            Code::TrueJump => "TJmp",
            Code::FalseJump => "FJmp",
            Code::LessThan => "Less",
            Code::Equals => "Eq",
            Code::Relative => "Rel",
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
            5 => Ok(Code::TrueJump),
            6 => Ok(Code::FalseJump),
            7 => Ok(Code::LessThan),
            8 => Ok(Code::Equals),
            9 => Ok(Code::Relative),
            99 => Ok(Code::Halt),
            v => Err(CodeFindError { value: v }),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mode {
    Position,
    Immediate,
    Relative,
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Mode::Position => write!(f, "Mode::Position"),
            Mode::Immediate => write!(f, "Mode::Immediate"),
            Mode::Relative => write!(f, "Mode::Relative"),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Parameter {
    relative: usize,
    mode: Mode,
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
                2 => Some(Mode::Relative),
                _ => None,
            }
        }

        let mode1 = mode_from_num((value / 100) % 10).ok_or(CodeFindError { value })?;
        let mode2 = mode_from_num((value / 1000) % 10).ok_or(CodeFindError { value })?;
        let mode3 = mode_from_num((value / 10_000) % 10).ok_or(CodeFindError { value })?;

        log::debug!(
            "  Parsed: {} -> Code {} -> {},{},{}",
            value,
            code,
            mode1,
            mode2,
            mode3,
        );

        Ok(Instruction {
            code,
            modes: [mode1, mode2, mode3],
        })
    }
}

impl Instruction {
    pub fn parameters(self) -> Vec<Parameter> {
        let rn = self.code.parameters();
        let mut v = Vec::with_capacity(rn);
        for i in 0..rn {
            v.push(Parameter {
                mode: self.modes[i],
                relative: i,
            })
        }

        v
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

    #[fail(display = "Pointer {}: Surprisingly large access at {}", position, loc)]
    HugeAddress { position: usize, loc: usize },

    #[fail(display = "Pointer {}: Expected input, none found", position)]
    MissingInput { position: usize },

    #[fail(display = "Cannot operated on completed program")]
    Completed,

    #[fail(
        display = "Pointer {}: Asked for mutable value for immediate mode",
        position
    )]
    MutImmediate { position: usize },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntComp {
    pub stepped: Value,
    pub position: Option<usize>,
    pub relative_base: Value,
    pub values: Vec<Value>,
    pub inputs: VecDeque<Value>,
    pub outputs: VecDeque<Value>,
}

impl IntComp {
    pub fn new(values: Vec<Value>) -> Self {
        IntComp {
            stepped: 0,
            position: Some(0),
            relative_base: 0,
            values,
            inputs: Default::default(),
            outputs: Default::default(),
        }
    }

    fn value(&self, param: Parameter) -> Result<Value, InvalidInstruction> {
        let ix = match self.position {
            None => return Err(InvalidInstruction::Completed),
            Some(p) => p,
        };

        let value = self.get(ix + param.relative + 1)?;
        Ok(match param.mode {
            Mode::Immediate => value,
            Mode::Position => {
                let v = self.get(value as usize)?;
                log::debug!("      {} Position {}: {}", param.relative, value, v);
                v
            }
            Mode::Relative => {
                let v = self.get((value + self.relative_base) as usize)?;
                log::debug!(
                    "      {} Relative {} + {}: {} -> {}",
                    param.relative,
                    value,
                    self.relative_base,
                    value + self.relative_base,
                    v
                );
                v
            }
        })
    }

    fn value_mut(&mut self, param: Parameter) -> Result<&mut Value, InvalidInstruction> {
        let ix = match self.position {
            None => return Err(InvalidInstruction::Completed),
            Some(p) => p,
        };

        Ok(match param.mode {
            Mode::Immediate => self.get_mut(ix + param.relative + 1)?,
            Mode::Position => {
                let value = self.get(ix + param.relative + 1)?;
                let v = self.get_mut(value as usize)?;
                log::debug!("      {} Position {}: {}", param.relative, value, v);
                v
            }
            Mode::Relative => {
                let relative_base = self.relative_base;
                let value = self.get(ix + param.relative + 1)?;
                let v = self.get_mut((value + relative_base) as usize)?;
                log::debug!(
                    "      {} Relative {} + {}: {} -> {}",
                    param.relative,
                    value,
                    relative_base,
                    value + relative_base,
                    v
                );
                v
            }
        })
    }

    fn get(&self, loc: usize) -> Result<Value, InvalidInstruction> {
        if loc as i64 >= MAX_SIZE {
            return Err(InvalidInstruction::HugeAddress {
                position: self.position.unwrap_or(0),
                loc,
            });
        }

        Ok(match self.values.get(loc) {
            Some(&n) => n,
            None if loc > 0 => {
                // Memory is allowed to extend to 'infinity'
                0
            }
            _ => {
                return Err(InvalidInstruction::InvalidAddress {
                    position: self.position.unwrap_or(0),
                    loc,
                })
            }
        })
    }

    fn get_mut(&mut self, loc: usize) -> Result<&mut Value, InvalidInstruction> {
        // if loc < 0 {
        //     let pos = self.position.unwrap_or(0);
        //     return Err(InvalidInstruction::InvalidAddress { position: pos, loc })
        // }
        if loc as i64 >= MAX_SIZE {
            return Err(InvalidInstruction::HugeAddress {
                position: self.position.unwrap_or(0),
                loc,
            });
        }

        if loc >= self.values.len() {
            self.values.resize_with(loc + 1, Default::default);
            // self.values.extend_with(loc - self.values.len() + 1, 0)
        }

        Ok(self
            .values
            .get_mut(loc)
            .expect("This should be extended to incorporate"))
    }

    // Returns true if there's more work to do, false if finished
    pub fn step(&mut self) -> Result<bool, InvalidInstruction> {
        let pos = match self.position {
            None => return Ok(false),
            Some(p) => p,
        };

        let cv = self.get(pos)?;

        log::debug!("{} Apply Instruction at {}: {}", self.stepped, pos, cv);

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
        let params = instruction.parameters();

        log::debug!(
            "  Apply {}: {:?} at position {} with registers {:?}",
            cv,
            instruction,
            pos,
            &self.values[pos..=pos + rn],
        );
        log::debug!("    Parameters: {:?}", params);

        self.position = match instruction.code {
            Code::Add => {
                let v1 = self.value(params[0])?;
                let v2 = self.value(params[1])?;
                let out_loc = self.value_mut(params[2])?;
                log::debug!("  Adding {} <- {} + {} = {}", *out_loc, v1, v2, v1 + v2);
                *out_loc = v1 + v2;
                Some(pos + 4)
            }
            Code::Multiply => {
                let v1 = self.value(params[0])?;
                let v2 = self.value(params[1])?;
                let out_loc = self.value_mut(params[2])?;
                log::debug!(
                    "  Multiplying {} <- {} + {} = {}",
                    *out_loc,
                    v1,
                    v2,
                    v1 * v2
                );

                *out_loc = v1 * v2;
                Some(pos + 4)
            }
            Code::Input => {
                let input = self
                    .inputs
                    .pop_front()
                    .ok_or(InvalidInstruction::MissingInput { position: pos })?;
                let out_loc = self.value_mut(params[0])?;
                log::debug!("  Input {} <- {}", *out_loc, input);
                *out_loc = input;
                Some(pos + 2)
            }
            Code::Output => {
                let val = self.value(params[0])?;
                self.outputs.push_back(val);
                log::debug!("  Output {}", val);
                Some(pos + 2)
            }
            Code::TrueJump => {
                let truthy = self.value(params[0])?;
                if truthy != 0 {
                    let address = self.value(params[1])?; // self.get(pos + 2)? as usize;
                    log::debug!("  TrueJump Jump: {} != 0: Jump -> {}", truthy, address);
                    Some(address as usize)
                } else {
                    log::debug!("  TrueJump Advance: {} == 0: Jump -> {}", truthy, pos + 3);
                    Some(pos + 3)
                }
            }
            Code::FalseJump => {
                let truthy = self.value(params[0])?;
                if truthy == 0 {
                    let address = self.value(params[1])?; //self.get(pos + 2)? as usize;
                    log::debug!("  FalseJump Jump: {} == 0: Jump -> {}", truthy, address);
                    Some(address as usize)
                } else {
                    log::debug!("  FalseJump Advance: {} != 0: Jump -> {}", truthy, pos + 3,);
                    Some(pos + 3)
                }
            }
            Code::LessThan => {
                let v1 = self.value(params[0])?;
                let v2 = self.value(params[1])?;
                let out_loc = self.value_mut(params[2])?;
                let out = if v1 < v2 { 1 } else { 0 };
                log::debug!("  LessThan: {} <- {} < {} = {}", *out_loc, v1, v2, out);
                *out_loc = out;
                Some(pos + 4)
            }
            Code::Equals => {
                let v1 = self.value(params[0])?;
                let v2 = self.value(params[1])?;
                let out_loc = self.value_mut(params[2])?;
                let out = if v1 == v2 { 1 } else { 0 };
                log::debug!("  Equals: {} <- {} == {} = {}", *out_loc, v1, v2, out);
                *out_loc = out;
                Some(pos + 4)
            }
            Code::Relative => {
                let val = self.value(params[0])?;
                log::debug!(
                    "  Adjust Relative Base {} + {} : {}",
                    self.relative_base,
                    val,
                    self.relative_base + val,
                );
                self.relative_base += val;
                Some(pos + 2)
            }
            Code::Halt => None,
        };

        self.stepped += 1;

        Ok(self.position.is_some())
    }

    pub fn run(&mut self) -> Result<(), failure::Error> {
        while self.step()? {}

        Ok(())
    }

    pub fn run_to_output(&mut self) -> Result<Option<Value>, failure::Error> {
        while self.outputs.is_empty() && self.step()? {}

        Ok(self.outputs.pop_front())
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

    #[test]
    fn test_equals() -> Result<(), failure::Error> {
        log::debug!("-- Test 1 --");
        let orig_cp = IntComp::from_str("3,9,8,9,10,9,4,9,99,-1,8")?;
        let mut cp = orig_cp.clone();
        cp.inputs.push_back(8);
        cp.run()?;
        assert_eq!(cp.outputs.pop_front(), Some(1));

        log::debug!("-- Test 2 --");
        let mut cp = orig_cp.clone();
        cp.inputs.push_back(3);
        cp.run()?;
        assert_eq!(cp.outputs.pop_front(), Some(0));

        log::debug!("-- Test 3 --");
        let mut cp = orig_cp;
        cp.inputs.push_back(2);
        cp.run()?;
        assert_eq!(cp.outputs.pop_front(), Some(0));

        log::debug!("-- Test 4 --");
        let orig_cp = IntComp::from_str("3,3,1108,-1,8,3,4,3,99")?;
        let mut cp = orig_cp.clone();
        cp.inputs.push_back(8);
        cp.run()?;
        assert_eq!(cp.outputs.pop_front(), Some(1));

        log::debug!("-- Test 5 --");
        let mut cp = orig_cp.clone();
        cp.inputs.push_back(3);
        cp.run()?;
        assert_eq!(cp.outputs.pop_front(), Some(0));

        log::debug!("-- Test 6 --");
        let mut cp = orig_cp;
        cp.inputs.push_back(2);
        cp.run()?;
        assert_eq!(cp.outputs.pop_front(), Some(0));

        Ok(())
    }

    #[test]
    fn test_less() -> Result<(), failure::Error> {
        let orig_cp = IntComp::from_str("3,9,7,9,10,9,4,9,99,-1,8")?;
        let mut cp = orig_cp.clone();
        cp.inputs.push_back(3);
        cp.run()?;
        assert_eq!(cp.outputs.pop_front(), Some(1));

        let mut cp = orig_cp.clone();
        cp.inputs.push_back(8);
        cp.run()?;
        assert_eq!(cp.outputs.pop_front(), Some(0));

        let mut cp = orig_cp;
        cp.inputs.push_back(20);
        cp.run()?;
        assert_eq!(cp.outputs.pop_front(), Some(0));

        let orig_cp = IntComp::from_str("3,3,1107,-1,8,3,4,3,99")?;
        let mut cp = orig_cp.clone();
        cp.inputs.push_back(3);
        cp.run()?;
        assert_eq!(cp.outputs.pop_front(), Some(1));

        let mut cp = orig_cp.clone();
        cp.inputs.push_back(8);
        cp.run()?;
        assert_eq!(cp.outputs.pop_front(), Some(0));

        let mut cp = orig_cp;
        cp.inputs.push_back(20);
        cp.run()?;
        assert_eq!(cp.outputs.pop_front(), Some(0));

        Ok(())
    }

    #[test]
    fn test_jump() -> Result<(), failure::Error> {
        let cps = [
            IntComp::from_str("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")?,
            IntComp::from_str("3,3,1105,-1,9,1101,0,0,12,4,12,99,1")?,
        ];
        for (n, ocp) in cps.iter().enumerate() {
            log::info!("==== CP {} ====", n);
            let mut cp = ocp.clone();
            cp.inputs.push_back(0);
            log::info!("-- Test 1 --");
            cp.run()?;
            assert_eq!(cp.outputs.pop_front(), Some(0));

            let mut cp = ocp.clone();
            cp.inputs.push_back(8);
            log::info!("-- Test 2 --");
            cp.run()?;
            assert_eq!(cp.outputs.pop_front(), Some(1));

            let mut cp = ocp.clone();
            cp.inputs.push_back(-20);
            log::info!("-- Test 3 --");
            cp.run()?;
            assert_eq!(cp.outputs.pop_front(), Some(1));
        }
        Ok(())
    }

    #[test]
    fn test_jump_equals_long() -> Result<(), failure::Error> {
        let orig_cp = IntComp::from_str(
            "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99",
        )?;
        // The above example program uses an input instruction to ask for a
        // single number. The program will then output 999 if the input value
        // is below 8, output 1000 if the input value is equal to 8, or output
        // 1001 if the input value is greater than 8.

        let mut cp = orig_cp.clone();
        cp.inputs.push_back(1);
        log::info!("-- Test 1 --");
        cp.run()?;
        assert_eq!(cp.outputs.pop_front(), Some(999));

        let mut cp = orig_cp.clone();
        cp.inputs.push_back(8);
        log::info!("-- Test 2 --");
        cp.run()?;
        assert_eq!(cp.outputs.pop_front(), Some(1000));

        let mut cp = orig_cp;
        cp.inputs.push_back(29);
        log::info!("-- Test 3 --");
        cp.run()?;
        assert_eq!(cp.outputs.pop_front(), Some(1001));
        Ok(())
    }

    #[test]
    fn test_relative_mode() -> Result<(), failure::Error> {
        let orig_cp =
            IntComp::from_str("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")?;

        let mut cp = orig_cp.clone();
        cp.run()?;

        // Produces a copy of itself as output
        assert_eq!(cp.outputs, orig_cp.values);
        Ok(())
    }

    #[test]
    fn test_relative_mode2() -> Result<(), failure::Error> {
        let mut cp = IntComp::from_str("1102,34915192,34915192,7,4,7,99,0")?;

        cp.run()?;

        assert_eq!(cp.outputs.len(), 1);

        assert!(
            (cp.outputs[0] >= 1_000_000_000_000_000) && (cp.outputs[0] < 10_000_000_000_000_000)
        );

        Ok(())
    }

    #[test]
    fn test_relative_mode3() -> Result<(), failure::Error> {
        let mut cp = IntComp::from_str("104,1125899906842624,99")?;

        cp.run()?;

        assert_eq!(cp.outputs.len(), 1);

        assert_eq!(cp.outputs[0], 1_125_899_906_842_624);

        Ok(())
    }
}
