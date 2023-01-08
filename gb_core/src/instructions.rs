pub enum Arithmetic {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

pub enum Instruction {
    ADD(Arithmetic),
    NOP,
}
