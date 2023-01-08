use crate::{
    instructions::{self, Arithmetic, Instruction},
    memory::Memory,
    registers::{Registers, CARRY_POS},
};

struct CPU {
    reg: Registers,
    pc: u16,
    mem: Memory,
}

impl CPU {
    fn run(&mut self) {
        let ins = self.fetch();

        self.execute(ins);
    }

    fn decode(&self, ins: u8) -> Instruction {
        match ins {
            _ => return Instruction::NOP,
        }
    }

    fn fetch(&self) -> Instruction {
        let ins = self.mem.readByte(self.pc);

        self.decode(ins)
    }

    fn execute(&mut self, ins: Instruction) {
        match ins {
            Instruction::ADD(target) => match target {
                Arithmetic::A => {
                    let lhs = self.reg.a;
                    self.reg.a = self.add_without_carry(lhs);
                }
                Arithmetic::B => {
                    let lhs = self.reg.b;
                    self.reg.a = self.add_without_carry(lhs);
                }
                Arithmetic::C => {
                    let lhs = self.reg.c;
                    self.reg.a = self.add_without_carry(lhs);
                }
                Arithmetic::D => {
                    let lhs = self.reg.d;
                    self.reg.a = self.add_without_carry(lhs);
                }
                Arithmetic::E => {
                    let lhs = self.reg.e;
                    self.reg.a = self.add_without_carry(lhs);
                }
                Arithmetic::H => {
                    let lhs = self.reg.h;
                    self.reg.a = self.add_without_carry(lhs);
                }
                Arithmetic::L => {
                    let lhs = self.reg.l;
                    self.reg.a = self.add_without_carry(lhs);
                }
            },
            Instruction::NOP => {}
        }
    }

    fn add_without_carry(&mut self, lhs: u8) -> u8 {
        let (sum, overflow) = self.reg.a.overflowing_add(lhs);

        self.reg.f.zero = sum == 0;
        self.reg.f.sub = false;
        self.reg.f.carry = overflow;
        self.reg.f.half_carry = (self.reg.a & 0xF) + (lhs & 0xF) > 0xF;

        sum
    }
}
