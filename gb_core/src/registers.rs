pub struct FlagsReg {
    pub zero: bool,
    pub sub: bool,
    pub half_carry: bool,
    pub carry: bool,
}

pub const ZERO_POS: u8 = 7;
pub const SUB_POS: u8 = 6;
pub const HCARRY_POS: u8 = 5;
pub const CARRY_POS: u8 = 4;

impl std::convert::From<FlagsReg> for u8 {
    fn from(flag: FlagsReg) -> Self {
        (if flag.carry { 1 } else { 0 }) << CARRY_POS
            | (if flag.half_carry { 1 } else { 0 }) << HCARRY_POS
            | (if flag.sub { 1 } else { 0 }) << SUB_POS
            | (if flag.zero { 1 } else { 0 }) << ZERO_POS
    }
}

impl std::convert::From<u8> for FlagsReg {
    fn from(value: u8) -> Self {
        let zero = (value >> ZERO_POS) & 0b1 != 0;
        let sub = (value >> SUB_POS) & 0b1 != 0;
        let half_carry = (value >> HCARRY_POS) & 0b1 != 0;
        let carry = (value >> CARRY_POS) & 0b1 != 0;

        FlagsReg {
            zero,
            sub,
            half_carry,
            carry,
        }
    }
}

impl FlagsReg {
    pub fn set_bit(&mut self, offset: u8, val: bool) {
        if offset == ZERO_POS {
            self.zero = val
        } else if offset == SUB_POS {
            self.sub = val
        } else if offset == HCARRY_POS {
            self.half_carry = val
        } else if offset == CARRY_POS {
            self.carry = val
        }
    }
}

pub struct Registers {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub f: FlagsReg,
    pub h: u8,
    pub l: u8,
}

impl Registers {
    fn get_bc(&self) -> u16 {
        (self.b as u16) << 8 | self.c as u16
    }

    fn set_bc(&mut self, val: u16) {
        self.b = (val & 0xFF00 >> 8) as u8;
        self.c = (val & 0xFF) as u8;
    }
}
