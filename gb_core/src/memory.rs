use crate::interrupts::InterruptFlags;

pub const VBLANK_VECTOR: u16 = 0x40;
pub const LCDSTAT_VECTOR: u16 = 0x48;
pub const TIMER_VECTOR: u16 = 0x50;

pub struct Memory {
    memory: [u8; 0xFFFF],
    pub interrupt_enable: InterruptFlags,
    pub interrupt_flag: InterruptFlags,
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
    pub fn has_interrupt(&self) -> bool {
        (self.interrupt_enable.vblank && self.interrupt_flag.vblank)
            || (self.interrupt_enable.lcdstat && self.interrupt_flag.lcdstat)
            || (self.interrupt_enable.timer && self.interrupt_flag.timer)
            || (self.interrupt_enable.serial && self.interrupt_flag.serial)
            || (self.interrupt_enable.joypad && self.interrupt_flag.joypad)
    }

    pub fn step(&mut self, cycles: u8) {}
}
