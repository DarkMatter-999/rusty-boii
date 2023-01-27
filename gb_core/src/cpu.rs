use std::{thread::sleep, time::Duration};

use crate::{
    controller::Key,
    instructions::{
        ADDHLTarget, Arithmetic, BitPosition, IncDecTarget, Indirect, Instruction, JumpTest,
        LoadByteSource, LoadByteTarget, LoadType, LoadWordTarget, PreFixTarget, StackTarget,
    },
    memory::{Memory, LCDSTAT_VECTOR, TIMER_VECTOR, VBLANK_VECTOR},
    registers::{FlagsReg, Registers},
};

pub struct CPU {
    reg: Registers,
    pub pc: u16,
    sp: u16,
    pub mem: Memory,
    is_halted: bool,
    interrupts_enabled: bool,
}

impl CPU {
    pub fn new(boot_rom: Option<Vec<u8>>, game_rom: Vec<u8>) -> CPU {
        CPU {
            reg: Registers::new(),
            pc: 0x0,
            sp: 0x00,
            mem: Memory::new(boot_rom, game_rom),
            is_halted: false,
            interrupts_enabled: true,
        }
    }

    pub fn run(&mut self) -> u8 {
        let ins = self.fetch();
        let (nextpc, mut cycles) = self.execute(ins);

        self.mem.step(cycles);

        if self.mem.has_interrupt() {
            self.is_halted = false;
        }

        if !self.is_halted {
            self.pc = nextpc;
        }

        let mut interrupted = false;
        if self.interrupts_enabled {
            if self.mem.interrupt_enable.vblank && self.mem.interrupt_flag.vblank {
                interrupted = true;
                self.mem.interrupt_flag.vblank = false;
                self.interrupt(VBLANK_VECTOR)
            }
            if self.mem.interrupt_enable.lcdstat && self.mem.interrupt_flag.lcdstat {
                interrupted = true;
                self.mem.interrupt_flag.lcdstat = false;
                self.interrupt(LCDSTAT_VECTOR)
            }
            if self.mem.interrupt_enable.timer && self.mem.interrupt_flag.timer {
                interrupted = true;
                self.mem.interrupt_flag.timer = false;
                self.interrupt(TIMER_VECTOR)
            }
        }

        if interrupted {
            cycles += 12;
        }

        cycles
    }

    fn decode(&self, ins: u8, prefix: bool) -> Instruction {
        if let Some(instruction) = Instruction::from_byte(ins, prefix) {
            if ins > 0xff {
                println!(
                    "0x{:X}\t pc: 0x{:X} \t f:{:X} \t{:?}",
                    ins,
                    self.pc,
                    u8::from(self.reg.f),
                    instruction
                );
            }

            // sleep(Duration::from_millis(100));
            instruction
        } else {
            panic!("Invalid instruction recieved at 0x{:x}", ins);
        }
    }

    fn fetch(&self) -> Instruction {
        let mut ins = self.mem.read_byte(self.pc);

        let prefix = ins == 0xcb;

        if prefix {
            ins = self.mem.read_byte(self.pc + 1);
        }

        self.decode(ins, prefix)
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
                        self.reg.a = self.inc8bit(self.reg.a);
                    }
                    IncDecTarget::B => {
                        self.reg.b = self.inc8bit(self.reg.b);
                    }
                    IncDecTarget::C => {
                        self.reg.c = self.inc8bit(self.reg.c);
                    }
                    IncDecTarget::D => {
                        self.reg.d = self.inc8bit(self.reg.d);
                    }
                    IncDecTarget::E => {
                        self.reg.e = self.inc8bit(self.reg.e);
                    }
                    IncDecTarget::H => {
                        self.reg.h = self.inc8bit(self.reg.h);
                    }
                    IncDecTarget::L => {
                        self.reg.l = self.inc8bit(self.reg.l);
                    }
                    IncDecTarget::HLI => {
                        let hl = self.reg.get_hl();
                        let amount = self.mem.read_byte(hl);
                        let result = self.inc8bit(amount);
                        self.mem.write_byte(hl, result);
                    }
                    IncDecTarget::BC => {
                        let bc = self.inc16bit(self.reg.get_bc());
                        self.reg.set_bc(bc);
                    }
                    IncDecTarget::DE => {
                        let de = self.inc16bit(self.reg.get_de());
                        self.reg.set_de(de);
                    }
                    IncDecTarget::HL => {
                        let hl = self.inc16bit(self.reg.get_hl());
                        self.reg.set_hl(hl);
                    }
                    IncDecTarget::SP => {
                        let amount = self.sp;
                        let result = self.inc16bit(amount);
                        self.sp = result;
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
                        self.reg.a = self.dec8bit(self.reg.a);
                    }
                    IncDecTarget::B => {
                        self.reg.b = self.dec8bit(self.reg.b);
                    }
                    IncDecTarget::C => {
                        self.reg.c = self.dec8bit(self.reg.c);
                    }
                    IncDecTarget::D => {
                        self.reg.d = self.dec8bit(self.reg.d);
                    }
                    IncDecTarget::E => {
                        self.reg.e = self.dec8bit(self.reg.e);
                    }
                    IncDecTarget::H => {
                        self.reg.h = self.dec8bit(self.reg.h);
                    }
                    IncDecTarget::L => {
                        self.reg.l = self.dec8bit(self.reg.l);
                    }
                    IncDecTarget::HLI => {
                        let hl = self.reg.get_hl();
                        let amount = self.mem.read_byte(hl);
                        let result = self.dec8bit(amount);
                        self.mem.write_byte(hl, result);
                    }
                    IncDecTarget::BC => {
                        let bc = self.dec16bit(self.reg.get_bc());
                        self.reg.set_bc(bc);
                    }
                    IncDecTarget::DE => {
                        let de = self.dec16bit(self.reg.get_de());
                        self.reg.set_de(de);
                    }
                    IncDecTarget::HL => {
                        let hl = self.dec16bit(self.reg.get_hl());
                        self.reg.set_hl(hl);
                    }
                    IncDecTarget::SP => {
                        let amount = self.sp;
                        let result = self.dec16bit(amount);
                        self.sp = result;
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
            Instruction::ADDHL(target) => {
                let value = match target {
                    ADDHLTarget::BC => self.reg.get_bc(),
                    ADDHLTarget::DE => self.reg.get_de(),
                    ADDHLTarget::HL => self.reg.get_hl(),
                    ADDHLTarget::SP => self.sp,
                };
                let hl = self.reg.get_hl();
                let (result, carry) = hl.overflowing_add(value);

                self.reg.f.carry = carry;
                self.reg.f.sub = false;
                let mask = 0b111_1111_1111;
                self.reg.f.half_carry = (value & mask) + (hl & mask) > mask;

                self.reg.set_hl(result);
                (self.pc.wrapping_add(1), 8)
            }
            Instruction::ADDSP => {
                let value = self.read_next_byte() as i8 as i16 as u16;
                let res = self.sp.wrapping_add(value);

                self.reg.f.half_carry = (self.sp & 0xf) + (value & 0xf) > 0xf;

                self.reg.f.carry = (self.sp & 0xff) + (value & 0xff) > 0xff;
                self.reg.f.zero = false;
                self.reg.f.sub = false;

                self.sp = res;

                (self.pc.wrapping_add(2), 16)
            }
            Instruction::ADC(target) => match target {
                Arithmetic::A => {
                    self.reg.a = self.add_with_carry(self.reg.a);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::B => {
                    self.reg.a = self.add_with_carry(self.reg.b);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::C => {
                    self.reg.a = self.add_with_carry(self.reg.c);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::D => {
                    self.reg.a = self.add_with_carry(self.reg.d);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::E => {
                    self.reg.a = self.add_with_carry(self.reg.e);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::F => {
                    self.reg.a = self.add_with_carry(u8::from(self.reg.f));
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::H => {
                    self.reg.a = self.add_with_carry(self.reg.h);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::L => {
                    self.reg.a = self.add_with_carry(self.reg.l);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::D8 => {
                    self.reg.a = self.add_with_carry(self.read_next_byte());
                    (self.pc.wrapping_add(2), 8)
                }
                Arithmetic::HLI => {
                    self.reg.a = self.add_with_carry(self.mem.read_byte(self.reg.get_hl()));
                    (self.pc.wrapping_add(1), 8)
                }
            },
            Instruction::SUB(target) => match target {
                Arithmetic::A => {
                    self.reg.a = self.sub_without_carry(self.reg.a);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::B => {
                    self.reg.a = self.sub_without_carry(self.reg.b);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::C => {
                    self.reg.a = self.sub_without_carry(self.reg.c);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::D => {
                    self.reg.a = self.sub_without_carry(self.reg.d);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::E => {
                    self.reg.a = self.sub_without_carry(self.reg.e);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::F => {
                    self.reg.a = self.sub_without_carry(u8::from(self.reg.f));
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::H => {
                    self.reg.a = self.sub_without_carry(self.reg.h);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::L => {
                    self.reg.a = self.sub_without_carry(self.reg.l);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::D8 => {
                    self.reg.a = self.sub_without_carry(self.read_next_byte());
                    (self.pc.wrapping_add(2), 8)
                }
                Arithmetic::HLI => {
                    self.reg.a = self.sub_without_carry(self.mem.read_byte(self.reg.get_hl()));
                    (self.pc.wrapping_add(1), 8)
                }
            },
            Instruction::SBC(target) => match target {
                Arithmetic::A => {
                    self.reg.a = self.sub_with_carry(self.reg.a);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::B => {
                    self.reg.a = self.sub_with_carry(self.reg.b);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::C => {
                    self.reg.a = self.sub_with_carry(self.reg.c);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::D => {
                    self.reg.a = self.sub_with_carry(self.reg.d);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::E => {
                    self.reg.a = self.sub_with_carry(self.reg.e);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::F => {
                    self.reg.a = self.sub_with_carry(u8::from(self.reg.f));
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::H => {
                    self.reg.a = self.sub_with_carry(self.reg.h);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::L => {
                    self.reg.a = self.sub_with_carry(self.reg.l);
                    (self.pc.wrapping_add(1), 4)
                }
                Arithmetic::D8 => {
                    self.reg.a = self.sub_with_carry(self.read_next_byte());
                    (self.pc.wrapping_add(2), 8)
                }
                Arithmetic::HLI => {
                    self.reg.a = self.sub_with_carry(self.mem.read_byte(self.reg.get_hl()));
                    (self.pc.wrapping_add(1), 8)
                }
            },
            Instruction::RLCA => {
                let carry = (self.reg.a & 0x80) >> 7;
                let new_value = self.reg.a.rotate_left(1) | carry;
                self.reg.f.zero = false;
                self.reg.f.sub = false;
                self.reg.f.half_carry = false;
                self.reg.f.carry = carry == 0x01;
                self.reg.a = new_value;
                (self.pc.wrapping_add(1), 4)
            }
            Instruction::RRCA => {
                let new_value = self.reg.a.rotate_right(1);
                self.reg.f.zero = false;
                self.reg.f.sub = false;
                self.reg.f.half_carry = false;
                self.reg.f.carry = self.reg.a & 0b1 == 0b1;
                self.reg.a = new_value;
                (self.pc.wrapping_add(1), 4)
            }
            Instruction::STOP => {
                self.is_halted = true;
                (self.pc.wrapping_add(2), 4)
            }

            Instruction::CCF => {
                self.reg.f.sub = false;
                self.reg.f.half_carry = false;
                self.reg.f.carry = !self.reg.f.carry;
                (self.pc.wrapping_add(1), 4)
            }
            Instruction::SCF => {
                self.reg.f.sub = false;
                self.reg.f.half_carry = false;
                self.reg.f.carry = true;
                (self.pc.wrapping_add(1), 4)
            }
            Instruction::RRA => {
                let carry_bit = if self.reg.f.carry { 1 } else { 0 } << 7;
                let new_value = carry_bit | (self.reg.a >> 1);
                self.reg.f.zero = false;
                self.reg.f.sub = false;
                self.reg.f.half_carry = false;
                self.reg.f.carry = self.reg.a & 0b1 == 0b1;
                self.reg.a = new_value;
                (self.pc.wrapping_add(1), 4)
            }
            Instruction::RLA => {
                let carry_bit = if self.reg.f.carry { 1 } else { 0 };
                let new_value = (self.reg.a << 1) | carry_bit;
                self.reg.f.zero = false;
                self.reg.f.sub = false;
                self.reg.f.half_carry = false;
                self.reg.f.carry = (self.reg.a & 0x80) == 0x80;
                self.reg.a = new_value;
                (self.pc.wrapping_add(1), 4)
            }
            Instruction::CPL => {
                self.reg.a = !self.reg.a;
                self.reg.f.sub = true;
                self.reg.f.half_carry = true;
                (self.pc.wrapping_add(1), 4)
            }
            Instruction::DAA => {
                let flags = self.reg.f;
                let mut carry = false;

                let result = if !flags.sub {
                    let mut result = self.reg.a;
                    if flags.carry || self.reg.a > 0x99 {
                        carry = true;
                        result = result.wrapping_add(0x60);
                    }
                    if flags.half_carry || self.reg.a & 0x0F > 0x09 {
                        result = result.wrapping_add(0x06);
                    }
                    result
                } else if flags.carry {
                    carry = true;
                    let add = if flags.half_carry { 0x9A } else { 0xA0 };
                    self.reg.a.wrapping_add(add)
                } else if flags.half_carry {
                    self.reg.a.wrapping_add(0xFA)
                } else {
                    self.reg.a
                };

                self.reg.f.zero = result == 0;
                self.reg.f.half_carry = false;
                self.reg.f.carry = carry;

                self.reg.a = result;
                (self.pc.wrapping_add(1), 4)
            }
            Instruction::JP(target) => {
                let jumpcondition = match target {
                    JumpTest::NotZero => !self.reg.f.zero,
                    JumpTest::NotCarry => !self.reg.f.carry,
                    JumpTest::Zero => self.reg.f.zero,
                    JumpTest::Carry => self.reg.f.carry,
                    JumpTest::Always => true,
                };
                self.jump(jumpcondition)
            }
            Instruction::JR(target) => {
                let jump_condition = match target {
                    JumpTest::NotZero => !self.reg.f.zero,
                    JumpTest::NotCarry => !self.reg.f.carry,
                    JumpTest::Zero => self.reg.f.zero,
                    JumpTest::Carry => self.reg.f.carry,
                    JumpTest::Always => true,
                };

                self.jump_rel(jump_condition)
            }
            Instruction::JPI => (self.reg.get_hl(), 4),
            Instruction::AND(target) => {
                let value = match target {
                    Arithmetic::A => self.reg.a,
                    Arithmetic::B => self.reg.b,
                    Arithmetic::C => self.reg.c,
                    Arithmetic::D => self.reg.d,
                    Arithmetic::E => self.reg.e,
                    Arithmetic::F => u8::from(self.reg.f),
                    Arithmetic::H => self.reg.h,
                    Arithmetic::L => self.reg.l,
                    Arithmetic::D8 => self.read_next_byte(),
                    Arithmetic::HLI => self.mem.read_byte(self.reg.get_hl()),
                };
                let n = self.reg.a & value;
                self.reg.f.zero = n == 0;
                self.reg.f.sub = false;
                self.reg.f.half_carry = true;
                self.reg.f.carry = false;
                self.reg.a = n;
                match target {
                    Arithmetic::D8 => (self.pc.wrapping_add(2), 8),
                    Arithmetic::HLI => (self.pc.wrapping_add(1), 8),
                    _ => (self.pc.wrapping_add(1), 4),
                }
            }
            Instruction::OR(target) => {
                let value = match target {
                    Arithmetic::A => self.reg.a,
                    Arithmetic::B => self.reg.b,
                    Arithmetic::C => self.reg.c,
                    Arithmetic::D => self.reg.d,
                    Arithmetic::E => self.reg.e,
                    Arithmetic::F => u8::from(self.reg.f),
                    Arithmetic::H => self.reg.h,
                    Arithmetic::L => self.reg.l,
                    Arithmetic::D8 => self.read_next_byte(),
                    Arithmetic::HLI => self.mem.read_byte(self.reg.get_hl()),
                };
                let n = self.reg.a | value;
                self.reg.f.zero = n == 0;
                self.reg.f.sub = false;
                self.reg.f.half_carry = false;
                self.reg.f.carry = false;
                self.reg.a = n;
                match target {
                    Arithmetic::D8 => (self.pc.wrapping_add(2), 8),
                    Arithmetic::HLI => (self.pc.wrapping_add(1), 8),
                    _ => (self.pc.wrapping_add(1), 4),
                }
            }
            Instruction::XOR(target) => {
                let value = match target {
                    Arithmetic::A => self.reg.a,
                    Arithmetic::B => self.reg.b,
                    Arithmetic::C => self.reg.c,
                    Arithmetic::D => self.reg.d,
                    Arithmetic::E => self.reg.e,
                    Arithmetic::F => u8::from(self.reg.f),
                    Arithmetic::H => self.reg.h,
                    Arithmetic::L => self.reg.l,
                    Arithmetic::D8 => self.read_next_byte(),
                    Arithmetic::HLI => self.mem.read_byte(self.reg.get_hl()),
                };
                let n = self.reg.a ^ value;
                self.reg.f.zero = n == 0;
                self.reg.f.sub = false;
                self.reg.f.half_carry = false;
                self.reg.f.carry = false;
                self.reg.a = n;
                match target {
                    Arithmetic::D8 => (self.pc.wrapping_add(2), 8),
                    Arithmetic::HLI => (self.pc.wrapping_add(1), 8),
                    _ => (self.pc.wrapping_add(1), 4),
                }
            }
            Instruction::CP(target) => {
                let value = match target {
                    Arithmetic::A => self.reg.a,
                    Arithmetic::B => self.reg.b,
                    Arithmetic::C => self.reg.c,
                    Arithmetic::D => self.reg.d,
                    Arithmetic::E => self.reg.e,
                    Arithmetic::F => u8::from(self.reg.f),
                    Arithmetic::H => self.reg.h,
                    Arithmetic::L => self.reg.l,
                    Arithmetic::D8 => self.read_next_byte(),
                    Arithmetic::HLI => self.mem.read_byte(self.reg.get_hl()),
                };

                self.reg.f.zero = self.reg.a == value;
                self.reg.f.sub = true;
                self.reg.f.half_carry = (self.reg.a & 0xF) < (value & 0xF);
                self.reg.f.carry = self.reg.a < value;

                match target {
                    Arithmetic::D8 => (self.pc.wrapping_add(2), 8),
                    Arithmetic::HLI => (self.pc.wrapping_add(1), 8),
                    _ => (self.pc.wrapping_add(1), 4),
                }
            }
            Instruction::PUSH(target) => {
                let value = match target {
                    StackTarget::AF => self.reg.get_af(),
                    StackTarget::BC => self.reg.get_bc(),
                    StackTarget::DE => self.reg.get_de(),
                    StackTarget::HL => self.reg.get_hl(),
                };
                self.push(value);
                (self.pc.wrapping_add(1), 16)
            }
            Instruction::POP(target) => {
                let result = self.pop();
                match target {
                    StackTarget::AF => self.reg.set_af(result),
                    StackTarget::BC => self.reg.set_bc(result),
                    StackTarget::DE => self.reg.set_de(result),
                    StackTarget::HL => self.reg.set_hl(result),
                };
                (self.pc.wrapping_add(1), 12)
            }
            Instruction::CALL(target) => {
                let jumpcondition = match target {
                    JumpTest::NotZero => !self.reg.f.zero,
                    JumpTest::NotCarry => !self.reg.f.carry,
                    JumpTest::Zero => self.reg.f.zero,
                    JumpTest::Carry => self.reg.f.carry,
                    JumpTest::Always => true,
                    _ => panic!("Invalid call value recieved"),
                };
                self.call(jumpcondition)
            }
            Instruction::RET(target) => {
                let jumpcondition = match target {
                    JumpTest::NotZero => !self.reg.f.zero,
                    JumpTest::NotCarry => !self.reg.f.carry,
                    JumpTest::Zero => self.reg.f.zero,
                    JumpTest::Carry => self.reg.f.carry,
                    JumpTest::Always => true,
                    _ => panic!("Invalid ret value recieved"),
                };
                let next_pc = self.ret(jumpcondition);

                let cycles = if jumpcondition && target == JumpTest::Always {
                    16
                } else if jumpcondition {
                    20
                } else {
                    8
                };
                (next_pc, cycles)
            }
            Instruction::RETI => {
                self.interrupts_enabled = true;
                (self.pop(), 16)
            }
            Instruction::RST(target) => {
                self.push(self.pc.wrapping_add(1));
                (target.to_hex(), 24)
            }
            Instruction::HALT => {
                self.is_halted = true;
                (self.pc.wrapping_add(1), 4)
            }
            Instruction::DI => {
                self.interrupts_enabled = false;
                (self.pc.wrapping_add(1), 4)
            }
            Instruction::EI => {
                self.interrupts_enabled = true;
                (self.pc.wrapping_add(1), 4)
            }
            Instruction::BIT(target, bit_position) => {
                match target {
                    PreFixTarget::A => self.bit_test(self.reg.a, bit_position),
                    PreFixTarget::B => self.bit_test(self.reg.b, bit_position),
                    PreFixTarget::C => self.bit_test(self.reg.c, bit_position),
                    PreFixTarget::D => self.bit_test(self.reg.d, bit_position),
                    PreFixTarget::E => self.bit_test(self.reg.e, bit_position),
                    PreFixTarget::H => self.bit_test(self.reg.h, bit_position),
                    PreFixTarget::L => self.bit_test(self.reg.l, bit_position),
                    PreFixTarget::HLI => {
                        let hl = self.reg.get_hl();
                        let value = self.mem.read_byte(hl);
                        self.bit_test(value, bit_position);
                    }
                }
                match target {
                    PreFixTarget::HLI => (self.pc.wrapping_add(2), 16),
                    _ => (self.pc.wrapping_add(2), 8),
                }
            }
            Instruction::RES(target, bit_position) => {
                match target {
                    PreFixTarget::A => self.reg.a = self.reset_bit(self.reg.a, bit_position),
                    PreFixTarget::B => self.reg.b = self.reset_bit(self.reg.b, bit_position),
                    PreFixTarget::C => self.reg.c = self.reset_bit(self.reg.c, bit_position),
                    PreFixTarget::D => self.reg.d = self.reset_bit(self.reg.d, bit_position),
                    PreFixTarget::E => self.reg.e = self.reset_bit(self.reg.e, bit_position),
                    PreFixTarget::H => self.reg.h = self.reset_bit(self.reg.h, bit_position),
                    PreFixTarget::L => self.reg.l = self.reset_bit(self.reg.l, bit_position),
                    PreFixTarget::HLI => {
                        let hl = self.reg.get_hl();
                        let value = self.mem.read_byte(hl);
                        let result = self.reset_bit(value, bit_position);
                        self.mem.write_byte(hl, result);
                    }
                }
                match target {
                    PreFixTarget::HLI => (self.pc.wrapping_add(2), 16),
                    _ => (self.pc.wrapping_add(2), 8),
                }
            }
            Instruction::SET(target, bit_position) => {
                match target {
                    PreFixTarget::A => self.reg.a = self.set_bit(self.reg.a, bit_position),
                    PreFixTarget::B => self.reg.b = self.set_bit(self.reg.b, bit_position),
                    PreFixTarget::C => self.reg.c = self.set_bit(self.reg.c, bit_position),
                    PreFixTarget::D => self.reg.d = self.set_bit(self.reg.d, bit_position),
                    PreFixTarget::E => self.reg.e = self.set_bit(self.reg.e, bit_position),
                    PreFixTarget::H => self.reg.h = self.set_bit(self.reg.h, bit_position),
                    PreFixTarget::L => self.reg.l = self.set_bit(self.reg.l, bit_position),
                    PreFixTarget::HLI => {
                        let hl = self.reg.get_hl();
                        let value = self.mem.read_byte(hl);
                        let result = self.set_bit(value, bit_position);
                        self.mem.write_byte(hl, result);
                    }
                }
                match target {
                    PreFixTarget::HLI => (self.pc.wrapping_add(2), 16),
                    _ => (self.pc.wrapping_add(2), 8),
                }
            }
            Instruction::SRL(target) => {
                match target {
                    PreFixTarget::A => self.reg.a = self.shift_right_logical(self.reg.a),
                    PreFixTarget::B => self.reg.b = self.shift_right_logical(self.reg.b),
                    PreFixTarget::C => self.reg.c = self.shift_right_logical(self.reg.c),
                    PreFixTarget::D => self.reg.d = self.shift_right_logical(self.reg.d),
                    PreFixTarget::E => self.reg.e = self.shift_right_logical(self.reg.e),
                    PreFixTarget::H => self.reg.h = self.shift_right_logical(self.reg.h),
                    PreFixTarget::L => self.reg.l = self.shift_right_logical(self.reg.l),
                    PreFixTarget::HLI => {
                        let hl = self.reg.get_hl();
                        let value = self.mem.read_byte(hl);
                        let result = self.shift_right_logical(value);
                        self.mem.write_byte(hl, result);
                    }
                }
                match target {
                    PreFixTarget::HLI => (self.pc.wrapping_add(2), 16),
                    _ => (self.pc.wrapping_add(2), 8),
                }
            }
            Instruction::RR(target) => {
                match target {
                    PreFixTarget::A => {
                        self.reg.a = self.rotate_right_through_carry_set_zero(self.reg.a)
                    }
                    PreFixTarget::B => {
                        self.reg.b = self.rotate_right_through_carry_set_zero(self.reg.b)
                    }
                    PreFixTarget::C => {
                        self.reg.c = self.rotate_right_through_carry_set_zero(self.reg.c)
                    }
                    PreFixTarget::D => {
                        self.reg.d = self.rotate_right_through_carry_set_zero(self.reg.d)
                    }
                    PreFixTarget::E => {
                        self.reg.e = self.rotate_right_through_carry_set_zero(self.reg.e)
                    }
                    PreFixTarget::H => {
                        self.reg.h = self.rotate_right_through_carry_set_zero(self.reg.h)
                    }
                    PreFixTarget::L => {
                        self.reg.l = self.rotate_right_through_carry_set_zero(self.reg.l)
                    }
                    PreFixTarget::HLI => {
                        let hl = self.reg.get_hl();
                        let value = self.mem.read_byte(hl);
                        let result = self.rotate_right_through_carry_set_zero(value);
                        self.mem.write_byte(hl, result);
                    }
                }
                match target {
                    PreFixTarget::HLI => (self.pc.wrapping_add(2), 16),
                    _ => (self.pc.wrapping_add(2), 8),
                }
            }
            Instruction::RL(target) => {
                match target {
                    PreFixTarget::A => {
                        self.reg.a = self.rotate_left_through_carry_set_zero(self.reg.a)
                    }
                    PreFixTarget::B => {
                        self.reg.b = self.rotate_left_through_carry_set_zero(self.reg.b)
                    }
                    PreFixTarget::C => {
                        self.reg.c = self.rotate_left_through_carry_set_zero(self.reg.c)
                    }
                    PreFixTarget::D => {
                        self.reg.d = self.rotate_left_through_carry_set_zero(self.reg.d)
                    }
                    PreFixTarget::E => {
                        self.reg.e = self.rotate_left_through_carry_set_zero(self.reg.e)
                    }
                    PreFixTarget::H => {
                        self.reg.h = self.rotate_left_through_carry_set_zero(self.reg.h)
                    }
                    PreFixTarget::L => {
                        self.reg.l = self.rotate_left_through_carry_set_zero(self.reg.l)
                    }
                    PreFixTarget::HLI => {
                        let hl = self.reg.get_hl();
                        let value = self.mem.read_byte(hl);
                        let result = self.rotate_left_through_carry_set_zero(value);
                        self.mem.write_byte(hl, result);
                    }
                }
                match target {
                    PreFixTarget::HLI => (self.pc.wrapping_add(2), 16),
                    _ => (self.pc.wrapping_add(2), 8),
                }
            }
            Instruction::RRC(target) => {
                match target {
                    PreFixTarget::A => self.reg.a = self.rotate_right_set_zero(self.reg.a),
                    PreFixTarget::B => self.reg.b = self.rotate_right_set_zero(self.reg.b),
                    PreFixTarget::C => self.reg.c = self.rotate_right_set_zero(self.reg.c),
                    PreFixTarget::D => self.reg.d = self.rotate_right_set_zero(self.reg.d),
                    PreFixTarget::E => self.reg.e = self.rotate_right_set_zero(self.reg.e),
                    PreFixTarget::H => self.reg.h = self.rotate_right_set_zero(self.reg.h),
                    PreFixTarget::L => self.reg.l = self.rotate_right_set_zero(self.reg.l),
                    PreFixTarget::HLI => {
                        let hl = self.reg.get_hl();
                        let value = self.mem.read_byte(hl);
                        let result = self.rotate_right_set_zero(value);
                        self.mem.write_byte(hl, result);
                    }
                }
                match target {
                    PreFixTarget::HLI => (self.pc.wrapping_add(2), 16),
                    _ => (self.pc.wrapping_add(2), 8),
                }
            }
            Instruction::RLC(target) => {
                match target {
                    PreFixTarget::A => self.reg.a = self.rotate_left_set_zero(self.reg.a),
                    PreFixTarget::B => self.reg.b = self.rotate_left_set_zero(self.reg.b),
                    PreFixTarget::C => self.reg.c = self.rotate_left_set_zero(self.reg.c),
                    PreFixTarget::D => self.reg.d = self.rotate_left_set_zero(self.reg.d),
                    PreFixTarget::E => self.reg.e = self.rotate_left_set_zero(self.reg.e),
                    PreFixTarget::H => self.reg.h = self.rotate_left_set_zero(self.reg.h),
                    PreFixTarget::L => self.reg.l = self.rotate_left_set_zero(self.reg.l),
                    PreFixTarget::HLI => {
                        let hl = self.reg.get_hl();
                        let value = self.mem.read_byte(hl);
                        let result = self.rotate_left_set_zero(value);
                        self.mem.write_byte(hl, result);
                    }
                }
                match target {
                    PreFixTarget::HLI => (self.pc.wrapping_add(2), 16),
                    _ => (self.pc.wrapping_add(2), 8),
                }
            }
            Instruction::SRA(target) => {
                match target {
                    PreFixTarget::A => self.reg.a = self.shift_right_arithmetic(self.reg.a),
                    PreFixTarget::B => self.reg.b = self.shift_right_arithmetic(self.reg.b),
                    PreFixTarget::C => self.reg.c = self.shift_right_arithmetic(self.reg.c),
                    PreFixTarget::D => self.reg.d = self.shift_right_arithmetic(self.reg.d),
                    PreFixTarget::E => self.reg.e = self.shift_right_arithmetic(self.reg.e),
                    PreFixTarget::H => self.reg.h = self.shift_right_arithmetic(self.reg.h),
                    PreFixTarget::L => self.reg.l = self.shift_right_arithmetic(self.reg.l),
                    PreFixTarget::HLI => {
                        let hl = self.reg.get_hl();
                        let value = self.mem.read_byte(hl);
                        let result = self.shift_right_arithmetic(value);
                        self.mem.write_byte(hl, result);
                    }
                }
                match target {
                    PreFixTarget::HLI => (self.pc.wrapping_add(2), 16),
                    _ => (self.pc.wrapping_add(2), 8),
                }
            }
            Instruction::SLA(target) => {
                match target {
                    PreFixTarget::A => self.reg.a = self.shift_left_arithmetic(self.reg.a),
                    PreFixTarget::B => self.reg.b = self.shift_left_arithmetic(self.reg.b),
                    PreFixTarget::C => self.reg.c = self.shift_left_arithmetic(self.reg.c),
                    PreFixTarget::D => self.reg.d = self.shift_left_arithmetic(self.reg.d),
                    PreFixTarget::E => self.reg.e = self.shift_left_arithmetic(self.reg.e),
                    PreFixTarget::H => self.reg.h = self.shift_left_arithmetic(self.reg.h),
                    PreFixTarget::L => self.reg.l = self.shift_left_arithmetic(self.reg.l),
                    PreFixTarget::HLI => {
                        let hl = self.reg.get_hl();
                        let value = self.mem.read_byte(hl);
                        let result = self.shift_left_arithmetic(value);
                        self.mem.write_byte(hl, result);
                    }
                }
                match target {
                    PreFixTarget::HLI => (self.pc.wrapping_add(2), 16),
                    _ => (self.pc.wrapping_add(2), 8),
                }
            }
            Instruction::SWAP(target) => {
                match target {
                    PreFixTarget::A => self.reg.a = self.swap(self.reg.a),
                    PreFixTarget::B => self.reg.b = self.swap(self.reg.b),
                    PreFixTarget::C => self.reg.c = self.swap(self.reg.c),
                    PreFixTarget::D => self.reg.d = self.swap(self.reg.d),
                    PreFixTarget::E => self.reg.e = self.swap(self.reg.e),
                    PreFixTarget::H => self.reg.h = self.swap(self.reg.h),
                    PreFixTarget::L => self.reg.l = self.swap(self.reg.l),
                    PreFixTarget::HLI => {
                        let hl = self.reg.get_hl();
                        let value = self.mem.read_byte(hl);
                        let result = self.swap(value);
                        self.mem.write_byte(hl, result);
                    }
                }
                match target {
                    PreFixTarget::HLI => (self.pc.wrapping_add(2), 16),
                    _ => (self.pc.wrapping_add(2), 8),
                }
            }
        }
    }

    fn read_next_word(&self) -> u16 {
        ((self.mem.read_byte(self.pc + 2) as u16) << 8) | (self.mem.read_byte(self.pc + 1) as u16)
    }

    fn read_next_byte(&self) -> u8 {
        self.mem.read_byte(self.pc + 1)
    }

    fn add_without_carry(&mut self, lhs: u8) -> u8 {
        let (sum, overflow) = self.reg.a.overflowing_add(lhs);

        self.reg.f.zero = sum == 0;
        self.reg.f.sub = false;
        self.reg.f.carry = overflow;
        self.reg.f.half_carry = (self.reg.a & 0xF) + (lhs & 0xF) > 0xF;

        sum
    }
    fn add_with_carry(&mut self, lhs: u8) -> u8 {
        let (add1, carry) = self.reg.a.overflowing_add(lhs);
        let (add2, carry2) = add1.overflowing_add(self.reg.f.carry as u8);
        self.reg.f.zero = add2 == 0;
        self.reg.f.sub = false;
        self.reg.f.carry = carry || carry2;
        self.reg.f.half_carry = ((self.reg.a & 0xF) + (lhs & 0xF) + (self.reg.f.carry as u8)) > 0xF;
        add2
    }
    fn sub_without_carry(&mut self, lhs: u8) -> u8 {
        let (value, overflow) = self.reg.a.overflowing_sub(lhs);
        self.reg.f.zero = value == 0;
        self.reg.f.sub = true;
        self.reg.f.carry = overflow;
        self.reg.f.half_carry = (self.reg.a & 0xF) < (value & 0xF);

        value
    }
    fn sub_with_carry(&mut self, lhs: u8) -> u8 {
        let (sub, carry) = self.reg.a.overflowing_sub(lhs);
        let (sub2, carry2) = sub.overflowing_sub(self.reg.f.carry as u8);
        self.reg.f.zero = sub2 == 0;
        self.reg.f.sub = true;
        self.reg.f.carry = carry || carry2;
        self.reg.f.half_carry = (self.reg.a & 0xF) < (lhs & 0xF) + (self.reg.f.carry as u8);
        sub2
    }
    fn jump(&mut self, jump: bool) -> (u16, u8) {
        if jump {
            let lsb = self.mem.read_byte(self.pc + 1) as u16;
            let msb = self.mem.read_byte(self.pc + 2) as u16;
            (((msb << 8) | lsb), 16)
        } else {
            (self.pc.wrapping_add(3), 12)
        }
    }
    fn jump_rel(&self, should_jump: bool) -> (u16, u8) {
        let next_step = self.pc.wrapping_add(2);
        if should_jump {
            let offset = self.read_next_byte() as i8;
            let pc = if offset >= 0 {
                next_step.wrapping_add(offset as u16)
            } else {
                next_step.wrapping_sub(offset.abs() as u16)
            };
            (pc, 16)
        } else {
            (next_step, 12)
        }
    }
    fn push(&mut self, value: u16) {
        self.sp = self.sp.wrapping_sub(1);
        self.mem.write_byte(self.sp, ((value & 0xFF00) >> 8) as u8);

        self.sp = self.sp.wrapping_sub(1);
        self.mem.write_byte(self.sp, (value & 0xFF) as u8);
    }

    fn pop(&mut self) -> u16 {
        let lsb = self.mem.read_byte(self.sp) as u16;
        self.sp = self.sp.wrapping_add(1);

        let msb = self.mem.read_byte(self.sp) as u16;
        self.sp = self.sp.wrapping_add(1);

        (msb << 8) | lsb
    }

    fn call(&mut self, jump: bool) -> (u16, u8) {
        let nextpc = self.pc.wrapping_add(3);
        if jump {
            self.push(nextpc);
            (self.read_next_word(), 24)
        } else {
            (nextpc, 12)
        }
    }

    fn ret(&mut self, jump: bool) -> u16 {
        if jump {
            self.pop()
        } else {
            self.pc.wrapping_add(1)
        }
    }

    fn interrupt(&mut self, location: u16) {
        self.interrupts_enabled = false;
        self.push(self.pc);
        self.pc = location;
        self.mem.step(12);
    }

    fn bit_test(&mut self, value: u8, bit_position: BitPosition) {
        let bit_position: u8 = bit_position.into();
        let result = (value >> bit_position) & 0b1;
        self.reg.f.zero = result == 0;
        self.reg.f.sub = false;
        self.reg.f.half_carry = true;
    }
    fn reset_bit(&mut self, value: u8, bit_position: BitPosition) -> u8 {
        let bit_position: u8 = bit_position.into();
        value & !(1 << bit_position)
    }
    fn set_bit(&mut self, value: u8, bit_position: BitPosition) -> u8 {
        let bit_position: u8 = bit_position.into();
        value | (1 << bit_position)
    }
    fn shift_right_logical(&mut self, value: u8) -> u8 {
        let new_value = value >> 1;
        self.reg.f.zero = new_value == 0;
        self.reg.f.sub = false;
        self.reg.f.half_carry = false;
        self.reg.f.carry = value & 0b1 == 0b1;
        new_value
    }
    fn rotate_right_through_carry_set_zero(&mut self, value: u8) -> u8 {
        let carry_bit = if self.reg.f.carry { 1 } else { 0 } << 7;
        let new_value = carry_bit | (value >> 1);
        self.reg.f.zero = new_value == 0;
        self.reg.f.sub = false;
        self.reg.f.half_carry = false;
        self.reg.f.carry = value & 0b1 == 0b1;
        new_value
    }
    fn rotate_left_through_carry_set_zero(&mut self, value: u8) -> u8 {
        let carry_bit = if self.reg.f.carry { 1 } else { 0 };
        let new_value = (value << 1) | carry_bit;
        self.reg.f.zero = new_value == 0;
        self.reg.f.sub = false;
        self.reg.f.half_carry = false;
        self.reg.f.carry = (value & 0x80) == 0x80;
        new_value
    }
    fn rotate_right_set_zero(&mut self, value: u8) -> u8 {
        let new_value = value.rotate_right(1);
        self.reg.f.zero = new_value == 0;
        self.reg.f.sub = false;
        self.reg.f.half_carry = false;
        self.reg.f.carry = value & 0b1 == 0b1;
        new_value
    }
    fn rotate_left_set_zero(&mut self, value: u8) -> u8 {
        let carry = (value & 0x80) >> 7;
        let new_value = value.rotate_left(1) | carry;
        self.reg.f.zero = new_value == 0;
        self.reg.f.sub = false;
        self.reg.f.half_carry = false;
        self.reg.f.carry = carry == 0x01;
        new_value
    }
    fn shift_right_arithmetic(&mut self, value: u8) -> u8 {
        let msb = value & 0x80;
        let new_value = msb | (value >> 1);
        self.reg.f.zero = new_value == 0;
        self.reg.f.sub = false;
        self.reg.f.half_carry = false;
        self.reg.f.carry = value & 0b1 == 0b1;
        new_value
    }
    fn shift_left_arithmetic(&mut self, value: u8) -> u8 {
        let new_value = value << 1;
        self.reg.f.zero = new_value == 0;
        self.reg.f.sub = false;
        self.reg.f.half_carry = false;
        self.reg.f.carry = value & 0x80 == 0x80;
        new_value
    }
    fn swap(&mut self, value: u8) -> u8 {
        let new_value = ((value & 0xf) << 4) | ((value & 0xf0) >> 4);
        self.reg.f.zero = new_value == 0;
        self.reg.f.sub = false;
        self.reg.f.half_carry = false;
        self.reg.f.carry = false;
        new_value
    }

    fn inc8bit(&mut self, value: u8) -> u8 {
        let new_value = value.wrapping_add(1);
        self.reg.f.zero = new_value == 0;
        self.reg.f.sub = false;
        self.reg.f.half_carry = value & 0xF == 0xF;
        new_value
    }

    fn inc16bit(&mut self, value: u16) -> u16 {
        value.wrapping_add(1)
    }

    fn dec8bit(&mut self, value: u8) -> u8 {
        let new_value = value.wrapping_sub(1);
        self.reg.f.zero = new_value == 0;
        self.reg.f.sub = true;
        self.reg.f.half_carry = value & 0xF == 0x0;
        new_value
    }
    fn dec16bit(&mut self, value: u16) -> u16 {
        value.wrapping_sub(1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn initcpu() -> CPU {
        let mut cpu = CPU::new(None, vec![0; 0xFFFF]);
        cpu
    }

    fn checkflags(cpu: &CPU, zero: bool, sub: bool, half_carry: bool, carry: bool) {
        let flags = cpu.reg.f;
        assert_eq!(flags.zero, zero);
        assert_eq!(flags.sub, sub);
        assert_eq!(flags.half_carry, half_carry);
        assert_eq!(flags.carry, carry);
    }

    #[test]
    fn test_nop() {
        let mut cpu = initcpu();
        cpu.execute(Instruction::NOP);
        checkflags(&cpu, false, false, false, false);
    }

    #[test]
    fn test_ld_a_indirect() {
        let mut cpu = initcpu();
        cpu.reg.set_bc(0xF9);
        cpu.mem.write_byte(0xF9, 0x4);
        cpu.execute(Instruction::LD(LoadType::AFromIndirect(
            Indirect::BCIndirect,
        )));

        assert_eq!(cpu.reg.a, 0x04);

        cpu.reg.set_hl(0xA1);
        cpu.mem.write_byte(0xA1, 0x9);
        cpu.execute(Instruction::LD(LoadType::AFromIndirect(
            Indirect::HLIndirectPlus,
        )));

        assert_eq!(cpu.reg.a, 0x09);
        assert_eq!(cpu.reg.get_hl(), 0xA2);
    }

    #[test]
    fn test_ld_byte() {
        let mut cpu = initcpu();
        cpu.reg.b = 0x4;
        cpu.execute(Instruction::LD(LoadType::Byte(
            LoadByteTarget::D,
            LoadByteSource::B,
        )));

        assert_eq!(cpu.reg.b, 0x4);
        assert_eq!(cpu.reg.d, 0x4);
    }

    #[test]
    fn test_inc_8bit_non_overflow() {
        let mut cpu = initcpu();
        cpu.reg.a = 0x7;
        cpu.execute(Instruction::INC(IncDecTarget::A));
        assert_eq!(cpu.reg.a, 0x8);
        checkflags(&cpu, false, false, false, false);
    }

    #[test]
    fn test_inc_8bit_half_carry() {
        let mut cpu = initcpu();
        cpu.reg.a = 0xF;
        cpu.execute(Instruction::INC(IncDecTarget::A));
        assert_eq!(cpu.reg.a, 0x10);
        checkflags(&cpu, false, false, true, false);
    }

    #[test]
    fn test_inc_8bit_overflow() {
        let mut cpu = initcpu();
        cpu.reg.a = 0xFF;
        cpu.execute(Instruction::INC(IncDecTarget::A));
        assert_eq!(cpu.reg.a, 0x0);
        checkflags(&cpu, true, false, true, false);
    }

    #[test]
    fn test_inc_16bit_byte_overflow() {
        let mut cpu = initcpu();
        cpu.reg.set_bc(0xFF);
        cpu.execute(Instruction::INC(IncDecTarget::BC));

        assert_eq!(cpu.reg.get_bc(), 0x0100);
        assert_eq!(cpu.reg.b, 0x01);
        assert_eq!(cpu.reg.c, 0x00);
        checkflags(&cpu, false, false, false, false);
    }

    #[test]
    fn test_inc_16bit_overflow() {
        let mut cpu = initcpu();
        cpu.reg.set_bc(0xFFFF);
        cpu.execute(Instruction::INC(IncDecTarget::BC));

        assert_eq!(cpu.reg.get_bc(), 0x0);
        assert_eq!(cpu.reg.b, 0x00);
        assert_eq!(cpu.reg.c, 0x00);
        checkflags(&cpu, false, false, false, false);
    }
}
