struct FlagsReg {
    zero: bool,
    sub: bool,
    half_carry: bool,
    carry: bool,
}

const ZERO_POS: u8 = 7;
const SUB_POS: u8 = 6;
const HCARRY_POS: u8 = 5;
const CARRY_POS: u8 = 4;

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

struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
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
