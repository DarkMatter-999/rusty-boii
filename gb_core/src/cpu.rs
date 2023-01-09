use crate::{
    instructions::{
        Arithmetic, IncDecTarget, Indirect, Instruction, LoadByteSource, LoadByteTarget, LoadType,
        LoadWordTarget,
    },
    memory::Memory,
    registers::{self, FlagsReg, Registers},
};

struct CPU {
    reg: Registers,
    pc: u16,
    sp: u16,
    mem: Memory,
}

impl CPU {
    fn run(&mut self) -> u8 {
        let ins = self.fetch();

        let (pc, mut cycles) = self.execute(ins);

        cycles
    }

    fn decode(&self, ins: u8) -> Instruction {
        match ins {
            _ => return Instruction::NOP,
        }
    }

    fn fetch(&self) -> Instruction {
        let ins = self.mem.read_byte(self.pc);

        self.decode(ins)
    }

    fn execute(&mut self, ins: Instruction) -> (u16, u8) {
        match ins {
            Instruction::NOP => (self.pc.wrapping_add(1), 4),
            Instruction::LD(load_type) => match load_type {
                LoadType::Byte(target, source) => {
                    let source_value = match source {
                        LoadByteSource::A => self.reg.a,
                        LoadByteSource::B => self.reg.b,
                        LoadByteSource::C => self.reg.c,
                        LoadByteSource::D => self.reg.d,
                        LoadByteSource::E => self.reg.e,
                        LoadByteSource::H => self.reg.h,
                        LoadByteSource::L => self.reg.l,
                        LoadByteSource::D8 => self.read_next_byte(),
                        LoadByteSource::HLI => self.mem.read_byte(self.reg.get_hl()),
                    };
                    match target {
                        LoadByteTarget::A => self.reg.a = source_value,
                        LoadByteTarget::B => self.reg.b = source_value,
                        LoadByteTarget::C => self.reg.c = source_value,
                        LoadByteTarget::D => self.reg.d = source_value,
                        LoadByteTarget::E => self.reg.e = source_value,
                        LoadByteTarget::H => self.reg.h = source_value,
                        LoadByteTarget::L => self.reg.l = source_value,
                        LoadByteTarget::HLI => self.mem.write_byte(self.reg.get_hl(), source_value),
                    };
                    match source {
                        LoadByteSource::D8 => (self.pc.wrapping_add(2), 8),
                        LoadByteSource::HLI => (self.pc.wrapping_add(1), 8),
                        _ => (self.pc.wrapping_add(1), 4),
                    }
                }
                LoadType::Word(target) => {
                    let word = self.read_next_word();
                    match target {
                        LoadWordTarget::BC => self.reg.set_bc(word),
                        LoadWordTarget::DE => self.reg.set_de(word),
                        LoadWordTarget::HL => self.reg.set_hl(word),
                        LoadWordTarget::SP => self.sp = word,
                    };
                    (self.pc.wrapping_add(3), 12)
                }
                LoadType::AFromIndirect(source) => {
                    self.reg.a = match source {
                        Indirect::BCIndirect => self.mem.read_byte(self.reg.get_bc()),
                        Indirect::DEIndirect => self.mem.read_byte(self.reg.get_de()),
                        Indirect::HLIndirectMinus => {
                            let hl = self.reg.get_hl();
                            self.reg.set_hl(hl.wrapping_sub(1));
                            self.mem.read_byte(hl)
                        }
                        Indirect::HLIndirectPlus => {
                            let hl = self.reg.get_hl();
                            self.reg.set_hl(hl.wrapping_add(1));
                            self.mem.read_byte(hl)
                        }
                        Indirect::WordIndirect => self.mem.read_byte(self.read_next_word()),
                        Indirect::LastByteIndirect => {
                            self.mem.read_byte(0xFF00 + self.reg.c as u16)
                        }
                    };

                    match source {
                        Indirect::WordIndirect => (self.pc.wrapping_add(3), 16),
                        _ => (self.pc.wrapping_add(1), 8),
                    }
                }
                LoadType::IndirectFromA(target) => {
                    let a = self.reg.a;
                    match target {
                        Indirect::BCIndirect => {
                            let bc = self.reg.get_bc();
                            self.mem.write_byte(bc, a)
                        }
                        Indirect::DEIndirect => {
                            let de = self.reg.get_de();
                            self.mem.write_byte(de, a)
                        }
                        Indirect::HLIndirectMinus => {
                            let hl = self.reg.get_hl();
                            self.reg.set_hl(hl.wrapping_sub(1));
                            self.mem.write_byte(hl, a);
                        }
                        Indirect::HLIndirectPlus => {
                            let hl = self.reg.get_hl();
                            self.reg.set_hl(hl.wrapping_add(1));
                            self.mem.write_byte(hl, a);
                        }
                        Indirect::WordIndirect => {
                            let word = self.read_next_word();
                            self.mem.write_byte(word, a);
                        }
                        Indirect::LastByteIndirect => {
                            let c = self.reg.c as u16;
                            self.mem.write_byte(0xFF00 + c, a);
                        }
                    };

                    match target {
                        Indirect::WordIndirect => (self.pc.wrapping_add(3), 16),
                        _ => (self.pc.wrapping_add(1), 8),
                    }
                }
                LoadType::ByteAddressFromA => {
                    let offset = self.read_next_byte() as u16;
                    self.mem.write_byte(0xFF00 + offset, self.reg.a);
                    (self.pc.wrapping_add(2), 12)
                }
                LoadType::AFromByteAddress => {
                    let offset = self.read_next_byte() as u16;
                    self.reg.a = self.mem.read_byte(0xFF00 + offset);
                    (self.pc.wrapping_add(2), 12)
                }
                LoadType::SPFromHL => {
                    self.sp = self.reg.get_hl();
                    (self.pc.wrapping_add(1), 8)
                }
                LoadType::IndirectFromSP => {
                    let address = self.read_next_word();
                    let sp = self.sp;
                    self.mem.write_byte(address, (sp & 0xFF) as u8);
                    self.mem
                        .write_byte(address.wrapping_add(1), ((sp & 0xFF00) >> 8) as u8);
                    (self.pc.wrapping_add(3), 20)
                }
                LoadType::HLFromSPN => {
                    let value = self.read_next_byte() as i8 as i16 as u16;
                    let result = self.sp.wrapping_add(value);
                    self.reg.set_hl(result);
                    self.reg.f.zero = false;
                    self.reg.f.sub = false;
                    self.reg.f.half_carry = (self.sp & 0xF) + (value & 0xF) > 0xF;
                    self.reg.f.carry = (self.sp & 0xFF) + (value & 0xFF) > 0xFF;
                    (self.pc.wrapping_add(2), 12)
                }
            },
            Instruction::INC(target) => {
                match target {
                    IncDecTarget::A => {
                        self.reg.a = self.reg.a.wrapping_add(1);
                    }
                    IncDecTarget::B => {
                        self.reg.b = self.reg.b.wrapping_add(1);
                    }
                    IncDecTarget::C => {
                        self.reg.c = self.reg.c.wrapping_add(1);
                    }
                    IncDecTarget::D => {
                        self.reg.d = self.reg.d.wrapping_add(1);
                    }
                    IncDecTarget::E => {
                        self.reg.e = self.reg.e.wrapping_add(1);
                    }
                    IncDecTarget::F => {
                        self.reg.f = FlagsReg::from(u8::from(self.reg.f).wrapping_add(1));
                    }
                    IncDecTarget::H => {
                        self.reg.h = self.reg.h.wrapping_add(1);
                    }
                    IncDecTarget::L => {
                        self.reg.l = self.reg.l.wrapping_add(1);
                    }
                    IncDecTarget::HLI => {
                        let hl = self.reg.get_hl();
                        let res = self.mem.read_byte(hl).wrapping_add(1);
                        self.mem.write_byte(hl, res);
                    }
                    IncDecTarget::BC => {
                        let bc = self.reg.get_bc();
                        self.reg.set_bc(bc.wrapping_add(1));
                    }
                    IncDecTarget::DE => {
                        let de = self.reg.get_de();
                        self.reg.set_de(de.wrapping_add(1));
                    }
                    IncDecTarget::HL => {
                        let hl = self.reg.get_hl();
                        self.reg.set_hl(hl.wrapping_add(1));
                    }
                    IncDecTarget::SP => {
                        let sp = self.sp;
                        self.sp = sp.wrapping_add(1);
                    }
                }
                let cycles = match target {
                    IncDecTarget::BC | IncDecTarget::DE | IncDecTarget::HL | IncDecTarget::SP => 8,
                    IncDecTarget::HLI => 12,
                    _ => 4,
                };
                (self.pc.wrapping_add(1), cycles)
            }
            Instruction::DEC(target) => {
                match target {
                    IncDecTarget::A => {
                        self.reg.a = self.reg.a.wrapping_sub(1);
                    }
                    IncDecTarget::B => {
                        self.reg.b = self.reg.b.wrapping_sub(1);
                    }
                    IncDecTarget::C => {
                        self.reg.c = self.reg.c.wrapping_sub(1);
                    }
                    IncDecTarget::D => {
                        self.reg.d = self.reg.d.wrapping_sub(1);
                    }
                    IncDecTarget::E => {
                        self.reg.e = self.reg.e.wrapping_sub(1);
                    }
                    IncDecTarget::F => {
                        self.reg.f = FlagsReg::from(u8::from(self.reg.f).wrapping_sub(1));
                    }
                    IncDecTarget::H => {
                        self.reg.h = self.reg.h.wrapping_sub(1);
                    }
                    IncDecTarget::L => {
                        self.reg.l = self.reg.l.wrapping_sub(1);
                    }
                    IncDecTarget::HLI => {
                        let hl = self.reg.get_hl();
                        let res = self.mem.read_byte(hl).wrapping_sub(1);
                        self.mem.write_byte(hl, res);
                    }
                    IncDecTarget::BC => {
                        let bc = self.reg.get_bc();
                        self.reg.set_bc(bc.wrapping_sub(1));
                    }
                    IncDecTarget::DE => {
                        let de = self.reg.get_de();
                        self.reg.set_de(de.wrapping_sub(1));
                    }
                    IncDecTarget::HL => {
                        let hl = self.reg.get_hl();
                        self.reg.set_hl(hl.wrapping_sub(1));
                    }
                    IncDecTarget::SP => {
                        let sp = self.sp;
                        self.sp = sp.wrapping_sub(1);
                    }
                }
                let cycles = match target {
                    IncDecTarget::BC | IncDecTarget::DE | IncDecTarget::HL | IncDecTarget::SP => 8,
                    IncDecTarget::HLI => 12,
                    _ => 4,
                };
                (self.pc.wrapping_add(1), cycles)
            }

            Instruction::ADD(target) => match target {
                Arithmetic::A => {
                    self.reg.a = self.add_without_carry(self.reg.a);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::B => {
                    self.reg.a = self.add_without_carry(self.reg.b);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::C => {
                    self.reg.a = self.add_without_carry(self.reg.c);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::D => {
                    self.reg.a = self.add_without_carry(self.reg.d);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::E => {
                    self.reg.a = self.add_without_carry(self.reg.e);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::F => {
                    self.reg.a = self.add_without_carry(u8::from(self.reg.f));
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::H => {
                    self.reg.a = self.add_without_carry(self.reg.h);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::L => {
                    self.reg.a = self.add_without_carry(self.reg.l);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::D8 => {
                    self.reg.a = self.add_without_carry(self.read_next_byte());
                    (self.pc.wrapping_add(2), 8)
                }
                Arithmetic::HLI => {
                    self.reg.a = self.add_without_carry(self.mem.read_byte(self.reg.get_hl()));
                    (self.pc.wrapping_add(1), 8)
                }
            },
        }
    }

    fn read_next_word(&self) -> u16 {
        ((self.mem.read_byte(self.pc + 2) as u16) << 8) | (self.mem.read_byte(self.pc + 1) as u16)
    }

    fn read_next_byte(&self) -> u8 {
        self.mem.read_byte(self.pc)
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
