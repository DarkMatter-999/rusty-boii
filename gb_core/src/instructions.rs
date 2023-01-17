#[derive(Debug)]
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

    HALT,
    DI,
    EI,

    BIT(PreFixTarget, BitPosition),
    RES(PreFixTarget, BitPosition),
    SET(PreFixTarget, BitPosition),
    SRL(PreFixTarget),
    RR(PreFixTarget),
    RL(PreFixTarget),
    RRC(PreFixTarget),
    RLC(PreFixTarget),
    SRA(PreFixTarget),
    SLA(PreFixTarget),
    SWAP(PreFixTarget),
}

impl Instruction {
    pub fn from_byte(byte: u8, prefixed: bool) -> Option<Instruction> {
        if prefixed {
            Instruction::from_byte_prefixed(byte)
        } else {
            Instruction::from_byte_not_prefixed(byte)
        }
    }

    fn from_byte_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            0x00 => Some(Instruction::RLC(PreFixTarget::B)),
            0x01 => Some(Instruction::RLC(PreFixTarget::C)),
            0x02 => Some(Instruction::RLC(PreFixTarget::D)),
            0x03 => Some(Instruction::RLC(PreFixTarget::E)),
            0x04 => Some(Instruction::RLC(PreFixTarget::H)),
            0x05 => Some(Instruction::RLC(PreFixTarget::L)),
            0x06 => Some(Instruction::RLC(PreFixTarget::HLI)),
            0x07 => Some(Instruction::RLC(PreFixTarget::A)),

            0x08 => Some(Instruction::RRC(PreFixTarget::B)),
            0x09 => Some(Instruction::RRC(PreFixTarget::C)),
            0x0a => Some(Instruction::RRC(PreFixTarget::D)),
            0x0b => Some(Instruction::RRC(PreFixTarget::E)),
            0x0c => Some(Instruction::RRC(PreFixTarget::H)),
            0x0d => Some(Instruction::RRC(PreFixTarget::L)),
            0x0e => Some(Instruction::RRC(PreFixTarget::HLI)),
            0x0f => Some(Instruction::RRC(PreFixTarget::A)),

            0x10 => Some(Instruction::RL(PreFixTarget::B)),
            0x11 => Some(Instruction::RL(PreFixTarget::C)),
            0x12 => Some(Instruction::RL(PreFixTarget::D)),
            0x13 => Some(Instruction::RL(PreFixTarget::E)),
            0x14 => Some(Instruction::RL(PreFixTarget::H)),
            0x15 => Some(Instruction::RL(PreFixTarget::L)),
            0x16 => Some(Instruction::RL(PreFixTarget::HLI)),
            0x17 => Some(Instruction::RL(PreFixTarget::A)),

            0x18 => Some(Instruction::RR(PreFixTarget::B)),
            0x19 => Some(Instruction::RR(PreFixTarget::C)),
            0x1a => Some(Instruction::RR(PreFixTarget::D)),
            0x1b => Some(Instruction::RR(PreFixTarget::E)),
            0x1c => Some(Instruction::RR(PreFixTarget::H)),
            0x1d => Some(Instruction::RR(PreFixTarget::L)),
            0x1e => Some(Instruction::RR(PreFixTarget::HLI)),
            0x1f => Some(Instruction::RR(PreFixTarget::A)),

            0x20 => Some(Instruction::SLA(PreFixTarget::B)),
            0x21 => Some(Instruction::SLA(PreFixTarget::C)),
            0x22 => Some(Instruction::SLA(PreFixTarget::D)),
            0x23 => Some(Instruction::SLA(PreFixTarget::E)),
            0x24 => Some(Instruction::SLA(PreFixTarget::H)),
            0x25 => Some(Instruction::SLA(PreFixTarget::L)),
            0x26 => Some(Instruction::SLA(PreFixTarget::HLI)),
            0x27 => Some(Instruction::SLA(PreFixTarget::A)),

            0x28 => Some(Instruction::SRA(PreFixTarget::B)),
            0x29 => Some(Instruction::SRA(PreFixTarget::C)),
            0x2a => Some(Instruction::SRA(PreFixTarget::D)),
            0x2b => Some(Instruction::SRA(PreFixTarget::E)),
            0x2c => Some(Instruction::SRA(PreFixTarget::H)),
            0x2d => Some(Instruction::SRA(PreFixTarget::L)),
            0x2e => Some(Instruction::SRA(PreFixTarget::HLI)),
            0x2f => Some(Instruction::SRA(PreFixTarget::A)),

            0x30 => Some(Instruction::SWAP(PreFixTarget::B)),
            0x31 => Some(Instruction::SWAP(PreFixTarget::C)),
            0x32 => Some(Instruction::SWAP(PreFixTarget::D)),
            0x33 => Some(Instruction::SWAP(PreFixTarget::E)),
            0x34 => Some(Instruction::SWAP(PreFixTarget::H)),
            0x35 => Some(Instruction::SWAP(PreFixTarget::L)),
            0x36 => Some(Instruction::SWAP(PreFixTarget::HLI)),
            0x37 => Some(Instruction::SWAP(PreFixTarget::A)),

            0x38 => Some(Instruction::SRL(PreFixTarget::B)),
            0x39 => Some(Instruction::SRL(PreFixTarget::C)),
            0x3a => Some(Instruction::SRL(PreFixTarget::D)),
            0x3b => Some(Instruction::SRL(PreFixTarget::E)),
            0x3c => Some(Instruction::SRL(PreFixTarget::H)),
            0x3d => Some(Instruction::SRL(PreFixTarget::L)),
            0x3e => Some(Instruction::SRL(PreFixTarget::HLI)),
            0x3f => Some(Instruction::SRL(PreFixTarget::A)),

            0x40 => Some(Instruction::BIT(PreFixTarget::B, BitPosition::B0)),
            0x41 => Some(Instruction::BIT(PreFixTarget::C, BitPosition::B0)),
            0x42 => Some(Instruction::BIT(PreFixTarget::D, BitPosition::B0)),
            0x43 => Some(Instruction::BIT(PreFixTarget::E, BitPosition::B0)),
            0x44 => Some(Instruction::BIT(PreFixTarget::H, BitPosition::B0)),
            0x45 => Some(Instruction::BIT(PreFixTarget::L, BitPosition::B0)),
            0x46 => Some(Instruction::BIT(PreFixTarget::HLI, BitPosition::B0)),
            0x47 => Some(Instruction::BIT(PreFixTarget::A, BitPosition::B0)),

            0x48 => Some(Instruction::BIT(PreFixTarget::B, BitPosition::B1)),
            0x49 => Some(Instruction::BIT(PreFixTarget::C, BitPosition::B1)),
            0x4a => Some(Instruction::BIT(PreFixTarget::D, BitPosition::B1)),
            0x4b => Some(Instruction::BIT(PreFixTarget::E, BitPosition::B1)),
            0x4c => Some(Instruction::BIT(PreFixTarget::H, BitPosition::B1)),
            0x4d => Some(Instruction::BIT(PreFixTarget::L, BitPosition::B1)),
            0x4e => Some(Instruction::BIT(PreFixTarget::HLI, BitPosition::B1)),
            0x4f => Some(Instruction::BIT(PreFixTarget::A, BitPosition::B1)),

            0x50 => Some(Instruction::BIT(PreFixTarget::B, BitPosition::B2)),
            0x51 => Some(Instruction::BIT(PreFixTarget::C, BitPosition::B2)),
            0x52 => Some(Instruction::BIT(PreFixTarget::D, BitPosition::B2)),
            0x53 => Some(Instruction::BIT(PreFixTarget::E, BitPosition::B2)),
            0x54 => Some(Instruction::BIT(PreFixTarget::H, BitPosition::B2)),
            0x55 => Some(Instruction::BIT(PreFixTarget::L, BitPosition::B2)),
            0x56 => Some(Instruction::BIT(PreFixTarget::HLI, BitPosition::B2)),
            0x57 => Some(Instruction::BIT(PreFixTarget::A, BitPosition::B2)),

            0x58 => Some(Instruction::BIT(PreFixTarget::B, BitPosition::B3)),
            0x59 => Some(Instruction::BIT(PreFixTarget::C, BitPosition::B3)),
            0x5a => Some(Instruction::BIT(PreFixTarget::D, BitPosition::B3)),
            0x5b => Some(Instruction::BIT(PreFixTarget::E, BitPosition::B3)),
            0x5c => Some(Instruction::BIT(PreFixTarget::H, BitPosition::B3)),
            0x5d => Some(Instruction::BIT(PreFixTarget::L, BitPosition::B3)),
            0x5e => Some(Instruction::BIT(PreFixTarget::HLI, BitPosition::B3)),
            0x5f => Some(Instruction::BIT(PreFixTarget::A, BitPosition::B3)),

            0x60 => Some(Instruction::BIT(PreFixTarget::B, BitPosition::B4)),
            0x61 => Some(Instruction::BIT(PreFixTarget::C, BitPosition::B4)),
            0x62 => Some(Instruction::BIT(PreFixTarget::D, BitPosition::B4)),
            0x63 => Some(Instruction::BIT(PreFixTarget::E, BitPosition::B4)),
            0x64 => Some(Instruction::BIT(PreFixTarget::H, BitPosition::B4)),
            0x65 => Some(Instruction::BIT(PreFixTarget::L, BitPosition::B4)),
            0x66 => Some(Instruction::BIT(PreFixTarget::HLI, BitPosition::B4)),
            0x67 => Some(Instruction::BIT(PreFixTarget::A, BitPosition::B4)),

            0x68 => Some(Instruction::BIT(PreFixTarget::B, BitPosition::B5)),
            0x69 => Some(Instruction::BIT(PreFixTarget::C, BitPosition::B5)),
            0x6a => Some(Instruction::BIT(PreFixTarget::D, BitPosition::B5)),
            0x6b => Some(Instruction::BIT(PreFixTarget::E, BitPosition::B5)),
            0x6c => Some(Instruction::BIT(PreFixTarget::H, BitPosition::B5)),
            0x6d => Some(Instruction::BIT(PreFixTarget::L, BitPosition::B5)),
            0x6e => Some(Instruction::BIT(PreFixTarget::HLI, BitPosition::B5)),
            0x6f => Some(Instruction::BIT(PreFixTarget::A, BitPosition::B5)),

            0x70 => Some(Instruction::BIT(PreFixTarget::B, BitPosition::B6)),
            0x71 => Some(Instruction::BIT(PreFixTarget::C, BitPosition::B6)),
            0x72 => Some(Instruction::BIT(PreFixTarget::D, BitPosition::B6)),
            0x73 => Some(Instruction::BIT(PreFixTarget::E, BitPosition::B6)),
            0x74 => Some(Instruction::BIT(PreFixTarget::H, BitPosition::B6)),
            0x75 => Some(Instruction::BIT(PreFixTarget::L, BitPosition::B6)),
            0x76 => Some(Instruction::BIT(PreFixTarget::HLI, BitPosition::B6)),
            0x77 => Some(Instruction::BIT(PreFixTarget::A, BitPosition::B6)),

            0x78 => Some(Instruction::BIT(PreFixTarget::B, BitPosition::B7)),
            0x79 => Some(Instruction::BIT(PreFixTarget::C, BitPosition::B7)),
            0x7a => Some(Instruction::BIT(PreFixTarget::D, BitPosition::B7)),
            0x7b => Some(Instruction::BIT(PreFixTarget::E, BitPosition::B7)),
            0x7c => Some(Instruction::BIT(PreFixTarget::H, BitPosition::B7)),
            0x7d => Some(Instruction::BIT(PreFixTarget::L, BitPosition::B7)),
            0x7e => Some(Instruction::BIT(PreFixTarget::HLI, BitPosition::B7)),
            0x7f => Some(Instruction::BIT(PreFixTarget::A, BitPosition::B7)),

            0x80 => Some(Instruction::RES(PreFixTarget::B, BitPosition::B0)),
            0x81 => Some(Instruction::RES(PreFixTarget::C, BitPosition::B0)),
            0x82 => Some(Instruction::RES(PreFixTarget::D, BitPosition::B0)),
            0x83 => Some(Instruction::RES(PreFixTarget::E, BitPosition::B0)),
            0x84 => Some(Instruction::RES(PreFixTarget::H, BitPosition::B0)),
            0x85 => Some(Instruction::RES(PreFixTarget::L, BitPosition::B0)),
            0x86 => Some(Instruction::RES(PreFixTarget::HLI, BitPosition::B0)),
            0x87 => Some(Instruction::RES(PreFixTarget::A, BitPosition::B0)),

            0x88 => Some(Instruction::RES(PreFixTarget::B, BitPosition::B1)),
            0x89 => Some(Instruction::RES(PreFixTarget::C, BitPosition::B1)),
            0x8a => Some(Instruction::RES(PreFixTarget::D, BitPosition::B1)),
            0x8b => Some(Instruction::RES(PreFixTarget::E, BitPosition::B1)),
            0x8c => Some(Instruction::RES(PreFixTarget::H, BitPosition::B1)),
            0x8d => Some(Instruction::RES(PreFixTarget::L, BitPosition::B1)),
            0x8e => Some(Instruction::RES(PreFixTarget::HLI, BitPosition::B1)),
            0x8f => Some(Instruction::RES(PreFixTarget::A, BitPosition::B1)),

            0x90 => Some(Instruction::RES(PreFixTarget::B, BitPosition::B2)),
            0x91 => Some(Instruction::RES(PreFixTarget::C, BitPosition::B2)),
            0x92 => Some(Instruction::RES(PreFixTarget::D, BitPosition::B2)),
            0x93 => Some(Instruction::RES(PreFixTarget::E, BitPosition::B2)),
            0x94 => Some(Instruction::RES(PreFixTarget::H, BitPosition::B2)),
            0x95 => Some(Instruction::RES(PreFixTarget::L, BitPosition::B2)),
            0x96 => Some(Instruction::RES(PreFixTarget::HLI, BitPosition::B2)),
            0x97 => Some(Instruction::RES(PreFixTarget::A, BitPosition::B2)),

            0x98 => Some(Instruction::RES(PreFixTarget::B, BitPosition::B3)),
            0x99 => Some(Instruction::RES(PreFixTarget::C, BitPosition::B3)),
            0x9a => Some(Instruction::RES(PreFixTarget::D, BitPosition::B3)),
            0x9b => Some(Instruction::RES(PreFixTarget::E, BitPosition::B3)),
            0x9c => Some(Instruction::RES(PreFixTarget::H, BitPosition::B3)),
            0x9d => Some(Instruction::RES(PreFixTarget::L, BitPosition::B3)),
            0x9e => Some(Instruction::RES(PreFixTarget::HLI, BitPosition::B3)),
            0x9f => Some(Instruction::RES(PreFixTarget::A, BitPosition::B3)),

            0xa0 => Some(Instruction::RES(PreFixTarget::B, BitPosition::B4)),
            0xa1 => Some(Instruction::RES(PreFixTarget::C, BitPosition::B4)),
            0xa2 => Some(Instruction::RES(PreFixTarget::D, BitPosition::B4)),
            0xa3 => Some(Instruction::RES(PreFixTarget::E, BitPosition::B4)),
            0xa4 => Some(Instruction::RES(PreFixTarget::H, BitPosition::B4)),
            0xa5 => Some(Instruction::RES(PreFixTarget::L, BitPosition::B4)),
            0xa6 => Some(Instruction::RES(PreFixTarget::HLI, BitPosition::B4)),
            0xa7 => Some(Instruction::RES(PreFixTarget::A, BitPosition::B4)),

            0xa8 => Some(Instruction::RES(PreFixTarget::B, BitPosition::B5)),
            0xa9 => Some(Instruction::RES(PreFixTarget::C, BitPosition::B5)),
            0xaa => Some(Instruction::RES(PreFixTarget::D, BitPosition::B5)),
            0xab => Some(Instruction::RES(PreFixTarget::E, BitPosition::B5)),
            0xac => Some(Instruction::RES(PreFixTarget::H, BitPosition::B5)),
            0xad => Some(Instruction::RES(PreFixTarget::L, BitPosition::B5)),
            0xae => Some(Instruction::RES(PreFixTarget::HLI, BitPosition::B5)),
            0xaf => Some(Instruction::RES(PreFixTarget::A, BitPosition::B5)),

            0xb0 => Some(Instruction::RES(PreFixTarget::B, BitPosition::B6)),
            0xb1 => Some(Instruction::RES(PreFixTarget::C, BitPosition::B6)),
            0xb2 => Some(Instruction::RES(PreFixTarget::D, BitPosition::B6)),
            0xb3 => Some(Instruction::RES(PreFixTarget::E, BitPosition::B6)),
            0xb4 => Some(Instruction::RES(PreFixTarget::H, BitPosition::B6)),
            0xb5 => Some(Instruction::RES(PreFixTarget::L, BitPosition::B6)),
            0xb6 => Some(Instruction::RES(PreFixTarget::HLI, BitPosition::B6)),
            0xb7 => Some(Instruction::RES(PreFixTarget::A, BitPosition::B6)),

            0xb8 => Some(Instruction::RES(PreFixTarget::B, BitPosition::B7)),
            0xb9 => Some(Instruction::RES(PreFixTarget::C, BitPosition::B7)),
            0xba => Some(Instruction::RES(PreFixTarget::D, BitPosition::B7)),
            0xbb => Some(Instruction::RES(PreFixTarget::E, BitPosition::B7)),
            0xbc => Some(Instruction::RES(PreFixTarget::H, BitPosition::B7)),
            0xbd => Some(Instruction::RES(PreFixTarget::L, BitPosition::B7)),
            0xbe => Some(Instruction::RES(PreFixTarget::HLI, BitPosition::B7)),
            0xbf => Some(Instruction::RES(PreFixTarget::A, BitPosition::B7)),

            0xc0 => Some(Instruction::SET(PreFixTarget::B, BitPosition::B0)),
            0xc1 => Some(Instruction::SET(PreFixTarget::C, BitPosition::B0)),
            0xc2 => Some(Instruction::SET(PreFixTarget::D, BitPosition::B0)),
            0xc3 => Some(Instruction::SET(PreFixTarget::E, BitPosition::B0)),
            0xc4 => Some(Instruction::SET(PreFixTarget::H, BitPosition::B0)),
            0xc5 => Some(Instruction::SET(PreFixTarget::L, BitPosition::B0)),
            0xc6 => Some(Instruction::SET(PreFixTarget::HLI, BitPosition::B0)),
            0xc7 => Some(Instruction::SET(PreFixTarget::A, BitPosition::B0)),

            0xc8 => Some(Instruction::SET(PreFixTarget::B, BitPosition::B1)),
            0xc9 => Some(Instruction::SET(PreFixTarget::C, BitPosition::B1)),
            0xca => Some(Instruction::SET(PreFixTarget::D, BitPosition::B1)),
            0xcb => Some(Instruction::SET(PreFixTarget::E, BitPosition::B1)),
            0xcc => Some(Instruction::SET(PreFixTarget::H, BitPosition::B1)),
            0xcd => Some(Instruction::SET(PreFixTarget::L, BitPosition::B1)),
            0xce => Some(Instruction::SET(PreFixTarget::HLI, BitPosition::B1)),
            0xcf => Some(Instruction::SET(PreFixTarget::A, BitPosition::B1)),

            0xd0 => Some(Instruction::SET(PreFixTarget::B, BitPosition::B2)),
            0xd1 => Some(Instruction::SET(PreFixTarget::C, BitPosition::B2)),
            0xd2 => Some(Instruction::SET(PreFixTarget::D, BitPosition::B2)),
            0xd3 => Some(Instruction::SET(PreFixTarget::E, BitPosition::B2)),
            0xd4 => Some(Instruction::SET(PreFixTarget::H, BitPosition::B2)),
            0xd5 => Some(Instruction::SET(PreFixTarget::L, BitPosition::B2)),
            0xd6 => Some(Instruction::SET(PreFixTarget::HLI, BitPosition::B2)),
            0xd7 => Some(Instruction::SET(PreFixTarget::A, BitPosition::B2)),

            0xd8 => Some(Instruction::SET(PreFixTarget::B, BitPosition::B3)),
            0xd9 => Some(Instruction::SET(PreFixTarget::C, BitPosition::B3)),
            0xda => Some(Instruction::SET(PreFixTarget::D, BitPosition::B3)),
            0xdb => Some(Instruction::SET(PreFixTarget::E, BitPosition::B3)),
            0xdc => Some(Instruction::SET(PreFixTarget::H, BitPosition::B3)),
            0xdd => Some(Instruction::SET(PreFixTarget::L, BitPosition::B3)),
            0xde => Some(Instruction::SET(PreFixTarget::HLI, BitPosition::B3)),
            0xdf => Some(Instruction::SET(PreFixTarget::A, BitPosition::B3)),

            0xe0 => Some(Instruction::SET(PreFixTarget::B, BitPosition::B4)),
            0xe1 => Some(Instruction::SET(PreFixTarget::C, BitPosition::B4)),
            0xe2 => Some(Instruction::SET(PreFixTarget::D, BitPosition::B4)),
            0xe3 => Some(Instruction::SET(PreFixTarget::E, BitPosition::B4)),
            0xe4 => Some(Instruction::SET(PreFixTarget::H, BitPosition::B4)),
            0xe5 => Some(Instruction::SET(PreFixTarget::L, BitPosition::B4)),
            0xe6 => Some(Instruction::SET(PreFixTarget::HLI, BitPosition::B4)),
            0xe7 => Some(Instruction::SET(PreFixTarget::A, BitPosition::B4)),

            0xe8 => Some(Instruction::SET(PreFixTarget::B, BitPosition::B5)),
            0xe9 => Some(Instruction::SET(PreFixTarget::C, BitPosition::B5)),
            0xea => Some(Instruction::SET(PreFixTarget::D, BitPosition::B5)),
            0xeb => Some(Instruction::SET(PreFixTarget::E, BitPosition::B5)),
            0xec => Some(Instruction::SET(PreFixTarget::H, BitPosition::B5)),
            0xed => Some(Instruction::SET(PreFixTarget::L, BitPosition::B5)),
            0xee => Some(Instruction::SET(PreFixTarget::HLI, BitPosition::B5)),
            0xef => Some(Instruction::SET(PreFixTarget::A, BitPosition::B5)),

            0xf0 => Some(Instruction::SET(PreFixTarget::B, BitPosition::B6)),
            0xf1 => Some(Instruction::SET(PreFixTarget::C, BitPosition::B6)),
            0xf2 => Some(Instruction::SET(PreFixTarget::D, BitPosition::B6)),
            0xf3 => Some(Instruction::SET(PreFixTarget::E, BitPosition::B6)),
            0xf4 => Some(Instruction::SET(PreFixTarget::H, BitPosition::B6)),
            0xf5 => Some(Instruction::SET(PreFixTarget::L, BitPosition::B6)),
            0xf6 => Some(Instruction::SET(PreFixTarget::HLI, BitPosition::B6)),
            0xf7 => Some(Instruction::SET(PreFixTarget::A, BitPosition::B6)),

            0xf8 => Some(Instruction::SET(PreFixTarget::B, BitPosition::B7)),
            0xf9 => Some(Instruction::SET(PreFixTarget::C, BitPosition::B7)),
            0xfa => Some(Instruction::SET(PreFixTarget::D, BitPosition::B7)),
            0xfb => Some(Instruction::SET(PreFixTarget::E, BitPosition::B7)),
            0xfc => Some(Instruction::SET(PreFixTarget::H, BitPosition::B7)),
            0xfd => Some(Instruction::SET(PreFixTarget::L, BitPosition::B7)),
            0xfe => Some(Instruction::SET(PreFixTarget::HLI, BitPosition::B7)),
            0xff => Some(Instruction::SET(PreFixTarget::A, BitPosition::B7)),
            _ => None,
        }
    }

    fn from_byte_not_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            0x3c => Some(Instruction::INC(IncDecTarget::A)),
            0x04 => Some(Instruction::INC(IncDecTarget::B)),
            0x14 => Some(Instruction::INC(IncDecTarget::D)),
            0x24 => Some(Instruction::INC(IncDecTarget::H)),
            0x0c => Some(Instruction::INC(IncDecTarget::C)),
            0x1c => Some(Instruction::INC(IncDecTarget::E)),
            0x2c => Some(Instruction::INC(IncDecTarget::L)),
            0x34 => Some(Instruction::INC(IncDecTarget::HLI)),
            0x03 => Some(Instruction::INC(IncDecTarget::BC)),
            0x13 => Some(Instruction::INC(IncDecTarget::DE)),
            0x23 => Some(Instruction::INC(IncDecTarget::HL)),
            0x33 => Some(Instruction::INC(IncDecTarget::SP)),

            0x3d => Some(Instruction::DEC(IncDecTarget::A)),
            0x05 => Some(Instruction::DEC(IncDecTarget::B)),
            0x0d => Some(Instruction::DEC(IncDecTarget::C)),
            0x15 => Some(Instruction::DEC(IncDecTarget::D)),
            0x1d => Some(Instruction::DEC(IncDecTarget::E)),
            0x25 => Some(Instruction::DEC(IncDecTarget::H)),
            0x2d => Some(Instruction::DEC(IncDecTarget::L)),
            0x35 => Some(Instruction::DEC(IncDecTarget::HLI)),
            0x0b => Some(Instruction::DEC(IncDecTarget::BC)),
            0x1b => Some(Instruction::DEC(IncDecTarget::DE)),
            0x2b => Some(Instruction::DEC(IncDecTarget::HL)),
            0x3b => Some(Instruction::DEC(IncDecTarget::SP)),

            0x87 => Some(Instruction::ADD(Arithmetic::A)),
            0x80 => Some(Instruction::ADD(Arithmetic::B)),
            0x81 => Some(Instruction::ADD(Arithmetic::C)),
            0x82 => Some(Instruction::ADD(Arithmetic::D)),
            0x83 => Some(Instruction::ADD(Arithmetic::E)),
            0x84 => Some(Instruction::ADD(Arithmetic::H)),
            0x85 => Some(Instruction::ADD(Arithmetic::L)),
            0x86 => Some(Instruction::ADD(Arithmetic::HLI)),
            0xc6 => Some(Instruction::ADD(Arithmetic::D8)),

            0x09 => Some(Instruction::ADDHL(ADDHLTarget::BC)),
            0x19 => Some(Instruction::ADDHL(ADDHLTarget::DE)),
            0x29 => Some(Instruction::ADDHL(ADDHLTarget::HL)),
            0x39 => Some(Instruction::ADDHL(ADDHLTarget::SP)),

            0x8f => Some(Instruction::ADC(Arithmetic::A)),
            0x88 => Some(Instruction::ADC(Arithmetic::B)),
            0x89 => Some(Instruction::ADC(Arithmetic::C)),
            0x8a => Some(Instruction::ADC(Arithmetic::D)),
            0x8b => Some(Instruction::ADC(Arithmetic::E)),
            0x8c => Some(Instruction::ADC(Arithmetic::H)),
            0x8d => Some(Instruction::ADC(Arithmetic::L)),
            0x8e => Some(Instruction::ADC(Arithmetic::HLI)),
            0xce => Some(Instruction::ADC(Arithmetic::D8)),

            0x97 => Some(Instruction::SUB(Arithmetic::A)),
            0x90 => Some(Instruction::SUB(Arithmetic::B)),
            0x91 => Some(Instruction::SUB(Arithmetic::C)),
            0x92 => Some(Instruction::SUB(Arithmetic::D)),
            0x93 => Some(Instruction::SUB(Arithmetic::E)),
            0x94 => Some(Instruction::SUB(Arithmetic::H)),
            0x95 => Some(Instruction::SUB(Arithmetic::L)),
            0x96 => Some(Instruction::SUB(Arithmetic::HLI)),
            0xd6 => Some(Instruction::SUB(Arithmetic::D8)),

            0x9f => Some(Instruction::SBC(Arithmetic::A)),
            0x98 => Some(Instruction::SBC(Arithmetic::B)),
            0x99 => Some(Instruction::SBC(Arithmetic::C)),
            0x9a => Some(Instruction::SBC(Arithmetic::D)),
            0x9b => Some(Instruction::SBC(Arithmetic::E)),
            0x9c => Some(Instruction::SBC(Arithmetic::H)),
            0x9d => Some(Instruction::SBC(Arithmetic::L)),
            0x9e => Some(Instruction::SBC(Arithmetic::HLI)),
            0xde => Some(Instruction::SBC(Arithmetic::D8)),

            0xa7 => Some(Instruction::AND(Arithmetic::A)),
            0xa0 => Some(Instruction::AND(Arithmetic::B)),
            0xa1 => Some(Instruction::AND(Arithmetic::C)),
            0xa2 => Some(Instruction::AND(Arithmetic::D)),
            0xa3 => Some(Instruction::AND(Arithmetic::E)),
            0xa4 => Some(Instruction::AND(Arithmetic::H)),
            0xa5 => Some(Instruction::AND(Arithmetic::L)),
            0xa6 => Some(Instruction::AND(Arithmetic::HLI)),
            0xe6 => Some(Instruction::AND(Arithmetic::D8)),

            0xb7 => Some(Instruction::OR(Arithmetic::A)),
            0xb0 => Some(Instruction::OR(Arithmetic::B)),
            0xb1 => Some(Instruction::OR(Arithmetic::C)),
            0xb2 => Some(Instruction::OR(Arithmetic::D)),
            0xb3 => Some(Instruction::OR(Arithmetic::E)),
            0xb4 => Some(Instruction::OR(Arithmetic::H)),
            0xb5 => Some(Instruction::OR(Arithmetic::L)),
            0xb6 => Some(Instruction::OR(Arithmetic::HLI)),
            0xf6 => Some(Instruction::OR(Arithmetic::D8)),

            0xaf => Some(Instruction::XOR(Arithmetic::A)),
            0xa8 => Some(Instruction::XOR(Arithmetic::B)),
            0xa9 => Some(Instruction::XOR(Arithmetic::C)),
            0xaa => Some(Instruction::XOR(Arithmetic::D)),
            0xab => Some(Instruction::XOR(Arithmetic::E)),
            0xac => Some(Instruction::XOR(Arithmetic::H)),
            0xad => Some(Instruction::XOR(Arithmetic::L)),
            0xae => Some(Instruction::XOR(Arithmetic::HLI)),
            0xee => Some(Instruction::XOR(Arithmetic::D8)),

            0xbf => Some(Instruction::CP(Arithmetic::A)),
            0xb8 => Some(Instruction::CP(Arithmetic::B)),
            0xb9 => Some(Instruction::CP(Arithmetic::C)),
            0xba => Some(Instruction::CP(Arithmetic::D)),
            0xbb => Some(Instruction::CP(Arithmetic::E)),
            0xbc => Some(Instruction::CP(Arithmetic::H)),
            0xbd => Some(Instruction::CP(Arithmetic::L)),
            0xbe => Some(Instruction::CP(Arithmetic::HLI)),
            0xfe => Some(Instruction::CP(Arithmetic::D8)),

            0xe8 => Some(Instruction::ADDSP),

            0x3f => Some(Instruction::CCF),
            0x37 => Some(Instruction::SCF),
            0x1f => Some(Instruction::RRA),
            0x17 => Some(Instruction::RLA),
            0x0f => Some(Instruction::RRCA),
            0x07 => Some(Instruction::RLCA),
            0x2f => Some(Instruction::CPL),

            0x27 => Some(Instruction::DAA),

            0xc3 => Some(Instruction::JP(JumpTest::Always)),
            0xc2 => Some(Instruction::JP(JumpTest::NotZero)),
            0xd2 => Some(Instruction::JP(JumpTest::NotCarry)),
            0xca => Some(Instruction::JP(JumpTest::Zero)),
            0xda => Some(Instruction::JP(JumpTest::Carry)),

            0x18 => Some(Instruction::JR(JumpTest::Always)),
            0x28 => Some(Instruction::JR(JumpTest::Zero)),
            0x38 => Some(Instruction::JR(JumpTest::Carry)),
            0x20 => Some(Instruction::JR(JumpTest::NotZero)),
            0x30 => Some(Instruction::JR(JumpTest::NotCarry)),

            0xe9 => Some(Instruction::JPI),

            0xf2 => Some(Instruction::LD(LoadType::AFromIndirect(
                Indirect::LastByteIndirect,
            ))),
            0x0a => Some(Instruction::LD(LoadType::AFromIndirect(
                Indirect::BCIndirect,
            ))),
            0x1a => Some(Instruction::LD(LoadType::AFromIndirect(
                Indirect::DEIndirect,
            ))),
            0x2a => Some(Instruction::LD(LoadType::AFromIndirect(
                Indirect::HLIndirectPlus,
            ))),
            0x3a => Some(Instruction::LD(LoadType::AFromIndirect(
                Indirect::HLIndirectMinus,
            ))),
            0xfa => Some(Instruction::LD(LoadType::AFromIndirect(
                Indirect::WordIndirect,
            ))),

            0xe2 => Some(Instruction::LD(LoadType::IndirectFromA(
                Indirect::LastByteIndirect,
            ))),
            0x02 => Some(Instruction::LD(LoadType::IndirectFromA(
                Indirect::BCIndirect,
            ))),
            0x12 => Some(Instruction::LD(LoadType::IndirectFromA(
                Indirect::DEIndirect,
            ))),
            0x22 => Some(Instruction::LD(LoadType::IndirectFromA(
                Indirect::HLIndirectPlus,
            ))),
            0x32 => Some(Instruction::LD(LoadType::IndirectFromA(
                Indirect::HLIndirectMinus,
            ))),
            0xea => Some(Instruction::LD(LoadType::IndirectFromA(
                Indirect::WordIndirect,
            ))),

            0x01 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::BC))),
            0x11 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::DE))),
            0x21 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::HL))),
            0x31 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::SP))),

            0x40 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::B,
            ))),
            0x41 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::C,
            ))),
            0x42 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::D,
            ))),
            0x43 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::E,
            ))),
            0x44 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::H,
            ))),
            0x45 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::L,
            ))),
            0x46 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::HLI,
            ))),
            0x47 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::A,
            ))),

            0x48 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::B,
            ))),
            0x49 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::C,
            ))),
            0x4a => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::D,
            ))),
            0x4b => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::E,
            ))),
            0x4c => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::H,
            ))),
            0x4d => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::L,
            ))),
            0x4e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::HLI,
            ))),
            0x4f => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::A,
            ))),

            0x50 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::B,
            ))),
            0x51 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::C,
            ))),
            0x52 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::D,
            ))),
            0x53 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::E,
            ))),
            0x54 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::H,
            ))),
            0x55 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::L,
            ))),
            0x56 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::HLI,
            ))),
            0x57 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::A,
            ))),

            0x58 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::B,
            ))),
            0x59 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::C,
            ))),
            0x5a => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::D,
            ))),
            0x5b => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::E,
            ))),
            0x5c => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::H,
            ))),
            0x5d => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::L,
            ))),
            0x5e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::HLI,
            ))),
            0x5f => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::A,
            ))),

            0x60 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::B,
            ))),
            0x61 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::C,
            ))),
            0x62 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::D,
            ))),
            0x63 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::E,
            ))),
            0x64 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::H,
            ))),
            0x65 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::L,
            ))),
            0x66 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::HLI,
            ))),
            0x67 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::A,
            ))),

            0x68 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::B,
            ))),
            0x69 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::C,
            ))),
            0x6a => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::D,
            ))),
            0x6b => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::E,
            ))),
            0x6c => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::H,
            ))),
            0x6d => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::L,
            ))),
            0x6e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::HLI,
            ))),
            0x6f => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::A,
            ))),

            0x70 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::B,
            ))),
            0x71 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::C,
            ))),
            0x72 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::D,
            ))),
            0x73 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::E,
            ))),
            0x74 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::H,
            ))),
            0x75 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::L,
            ))),
            0x77 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::A,
            ))),

            0x78 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::B,
            ))),
            0x79 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::C,
            ))),
            0x7a => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::D,
            ))),
            0x7b => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::E,
            ))),
            0x7c => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::H,
            ))),
            0x7d => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::L,
            ))),
            0x7e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::HLI,
            ))),
            0x7f => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::A,
            ))),

            0x3e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::D8,
            ))),
            0x06 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::D8,
            ))),
            0x0e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::D8,
            ))),
            0x16 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::D8,
            ))),
            0x1e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::D8,
            ))),
            0x26 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::D8,
            ))),
            0x2e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::D8,
            ))),
            0x36 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::D8,
            ))),

            0xe0 => Some(Instruction::LD(LoadType::ByteAddressFromA)),
            0xf0 => Some(Instruction::LD(LoadType::AFromByteAddress)),

            0x08 => Some(Instruction::LD(LoadType::IndirectFromSP)),
            0xf9 => Some(Instruction::LD(LoadType::SPFromHL)),
            0xf8 => Some(Instruction::LD(LoadType::HLFromSPN)),

            0xc5 => Some(Instruction::PUSH(StackTarget::BC)),
            0xd5 => Some(Instruction::PUSH(StackTarget::DE)),
            0xe5 => Some(Instruction::PUSH(StackTarget::HL)),
            0xf5 => Some(Instruction::PUSH(StackTarget::AF)),

            0xc1 => Some(Instruction::POP(StackTarget::BC)),
            0xd1 => Some(Instruction::POP(StackTarget::DE)),
            0xe1 => Some(Instruction::POP(StackTarget::HL)),
            0xf1 => Some(Instruction::POP(StackTarget::AF)),

            0xc4 => Some(Instruction::CALL(JumpTest::NotZero)),
            0xd4 => Some(Instruction::CALL(JumpTest::NotCarry)),
            0xcc => Some(Instruction::CALL(JumpTest::Zero)),
            0xdc => Some(Instruction::CALL(JumpTest::Carry)),
            0xcd => Some(Instruction::CALL(JumpTest::Always)),

            0xc0 => Some(Instruction::RET(JumpTest::NotZero)),
            0xd0 => Some(Instruction::RET(JumpTest::NotCarry)),
            0xc8 => Some(Instruction::RET(JumpTest::Zero)),
            0xd8 => Some(Instruction::RET(JumpTest::Carry)),
            0xc9 => Some(Instruction::RET(JumpTest::Always)),
            0xd9 => Some(Instruction::RETI),

            0xc7 => Some(Instruction::RST(RSTLocation::x00)),
            0xd7 => Some(Instruction::RST(RSTLocation::x10)),
            0xe7 => Some(Instruction::RST(RSTLocation::x20)),
            0xf7 => Some(Instruction::RST(RSTLocation::x30)),
            0xcf => Some(Instruction::RST(RSTLocation::x08)),
            0xdf => Some(Instruction::RST(RSTLocation::x18)),
            0xef => Some(Instruction::RST(RSTLocation::x28)),
            0xff => Some(Instruction::RST(RSTLocation::x38)),

            0x00 => Some(Instruction::NOP),
            0x76 => Some(Instruction::HALT),
            0xf3 => Some(Instruction::DI),
            0xfb => Some(Instruction::EI),
            _ => None,
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum LoadWordTarget {
    BC,
    DE,
    HL,
    SP,
}

#[derive(Debug)]
pub enum Indirect {
    BCIndirect,
    DEIndirect,
    HLIndirectMinus,
    HLIndirectPlus,
    WordIndirect,
    LastByteIndirect,
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum ADDHLTarget {
    BC,
    DE,
    HL,
    SP,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum JumpTest {
    NotZero,
    Zero,
    NotCarry,
    Carry,
    Always,
}

#[derive(Debug)]
pub enum StackTarget {
    AF,
    BC,
    DE,
    HL,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum PreFixTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    HLI,
}

#[derive(Debug)]
pub enum BitPosition {
    B0,
    B1,
    B2,
    B3,
    B4,
    B5,
    B6,
    B7,
}
impl std::convert::From<BitPosition> for u8 {
    fn from(position: BitPosition) -> u8 {
        match position {
            BitPosition::B0 => 0,
            BitPosition::B1 => 1,
            BitPosition::B2 => 2,
            BitPosition::B3 => 3,
            BitPosition::B4 => 4,
            BitPosition::B5 => 5,
            BitPosition::B6 => 6,
            BitPosition::B7 => 7,
        }
    }
}
