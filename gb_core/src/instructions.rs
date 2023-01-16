pub enum Instruction {
    NOP,
    LD(LoadType),

    INC(IncDecTarget),
    DEC(IncDecTarget),

    ADD(Arithmetic),
    ADC(Arithmetic),
    ADDHL(ADDHLTarget),
    ADDSP,

    SUB(Arithmetic),
    SBC(Arithmetic),

    RLCA,
    RRCA,

    STOP,

    CCF,
    SCF,

    RRA,
    RLA,
    CPL,
    DAA,

    JP(JumpTest),
    JR(JumpTest),
    JPI,

    AND(Arithmetic),
    OR(Arithmetic),
    XOR(Arithmetic),
    CP(Arithmetic),

    PUSH(StackTarget),
    POP(StackTarget),
    CALL(JumpTest),
    RET(JumpTest),
    RETI,
    RST(RSTLocation),
}

pub enum LoadByteTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    HLI,
}

pub enum LoadByteSource {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    HLI,
    D8,
}
pub enum LoadWordTarget {
    BC,
    DE,
    HL,
    SP,
}
pub enum Indirect {
    BCIndirect,
    DEIndirect,
    HLIndirectMinus,
    HLIndirectPlus,
    WordIndirect,
    LastByteIndirect,
}

pub enum LoadType {
    Byte(LoadByteTarget, LoadByteSource),
    Word(LoadWordTarget),
    AFromIndirect(Indirect),
    IndirectFromA(Indirect),
    AFromByteAddress,
    ByteAddressFromA,
    SPFromHL,
    HLFromSPN,
    IndirectFromSP,
}

pub enum IncDecTarget {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
    HLI,
    BC,
    DE,
    HL,
    SP,
}

pub enum Arithmetic {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
    D8,
    HLI,
}

pub enum ADDHLTarget {
    BC,
    DE,
    HL,
    SP,
}

#[derive(Copy, Clone, PartialEq)]
pub enum JumpTest {
    NotZero,
    Zero,
    NotCarry,
    Carry,
    Always,
}

pub enum StackTarget {
    AF,
    BC,
    DE,
    HL,
}

pub enum RSTLocation {
    x00,
    x08,
    x10,
    x18,
    x20,
    x28,
    x30,
    x38,
}

impl RSTLocation {
    pub fn to_hex(&self) -> u16 {
        match self {
            RSTLocation::x00 => 0x00,
            RSTLocation::x08 => 0x08,
            RSTLocation::x10 => 0x10,
            RSTLocation::x18 => 0x18,
            RSTLocation::x20 => 0x20,
            RSTLocation::x28 => 0x28,
            RSTLocation::x30 => 0x30,
            RSTLocation::x38 => 0x38,
        }
    }
}
