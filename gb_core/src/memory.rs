pub struct Memory {
    memory: [u8; 0xFFFF],
}

impl Memory {
    pub fn readByte(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }
}
