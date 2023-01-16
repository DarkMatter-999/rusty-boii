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
