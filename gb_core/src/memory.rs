pub struct Memory {
    memory: [u8; 0xFFFF],
}

impl Memory {
    pub fn read_byte(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        // println!("{:x}", addr);
        let addr = addr as usize;
        self.memory[addr] = val;
    }
}
