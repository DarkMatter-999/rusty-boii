use crate::{gpu::GPU, interrupts::InterruptFlags};

const RAM_SIZE: usize = 0xFFFF;

pub const BOOT_ROM_BEGIN: usize = 0x00;
pub const BOOT_ROM_END: usize = 0xFF;
pub const BOOT_ROM_SIZE: usize = BOOT_ROM_END - BOOT_ROM_BEGIN + 1;

pub const ROM_BANK_0_BEGIN: usize = 0x0000;
pub const ROM_BANK_0_END: usize = 0x3FFF;
pub const ROM_BANK_0_SIZE: usize = ROM_BANK_0_END - ROM_BANK_0_BEGIN + 1;

pub const ROM_BANK_N_BEGIN: usize = 0x4000;
pub const ROM_BANK_N_END: usize = 0x7FFF;
pub const ROM_BANK_N_SIZE: usize = ROM_BANK_N_END - ROM_BANK_N_BEGIN + 1;

pub const WORKING_RAM_BEGIN: usize = 0xC000;
pub const WORKING_RAM_END: usize = 0xDFFF;
pub const WORKING_RAM_SIZE: usize = WORKING_RAM_END - WORKING_RAM_BEGIN + 1;

pub const ECHO_RAM_BEGIN: usize = 0xE000;
pub const ECHO_RAM_END: usize = 0xFDFF;

pub const UNUSED_BEGIN: usize = 0xFEA0;
pub const UNUSED_END: usize = 0xFEFF;

pub const IO_REGISTERS_BEGIN: usize = 0xFF00;
pub const IO_REGISTERS_END: usize = 0xFF7F;

pub const ZERO_PAGE_BEGIN: usize = 0xFF80;
pub const ZERO_PAGE_END: usize = 0xFFFE;
pub const ZERO_PAGE_SIZE: usize = ZERO_PAGE_END - ZERO_PAGE_BEGIN + 1;

pub const EXTERNAL_RAM_BEGIN: usize = 0xA000;
pub const EXTERNAL_RAM_END: usize = 0xBFFF;
pub const EXTERNAL_RAM_SIZE: usize = EXTERNAL_RAM_END - EXTERNAL_RAM_BEGIN + 1;

pub const VRAM_BEGIN: usize = 0x8000;
pub const VRAM_END: usize = 0x9FFF;
pub const VRAM_SIZE: usize = VRAM_END - VRAM_BEGIN + 1;

pub const OAM_BEGIN: usize = 0xFE00;
pub const OAM_END: usize = 0xFE9F;
pub const OAM_SIZE: usize = OAM_END - OAM_BEGIN + 1;

pub const INTERRUPT_ENABLE_REGISTER: usize = 0xFFFF;

pub const VBLANK_VECTOR: u16 = 0x40;
pub const LCDSTAT_VECTOR: u16 = 0x48;
pub const TIMER_VECTOR: u16 = 0x50;

pub struct Memory {
    pub bootrom: Option<[u8; BOOT_ROM_SIZE]>,
    rom_bank_0: [u8; ROM_BANK_0_SIZE],
    rom_bank_n: [u8; ROM_BANK_N_SIZE],
    working_ram: [u8; WORKING_RAM_SIZE],
    zero_page: [u8; ZERO_PAGE_SIZE],
    external_ram: [u8; EXTERNAL_RAM_SIZE],
    pub interrupt_enable: InterruptFlags,
    pub interrupt_flag: InterruptFlags,
    pub gpu: GPU,
}

impl Memory {
    pub fn new(boot_rom_buffer: Option<Vec<u8>>, game_rom: Vec<u8>) -> Memory {
        let bootrom = boot_rom_buffer.map(|boot_rom_buffer| {
            if boot_rom_buffer.len() != BOOT_ROM_SIZE {
                panic!(
                    "Supplied boot ROM is the wrong size. Is {} bytes but should be {} bytes",
                    boot_rom_buffer.len(),
                    BOOT_ROM_SIZE
                );
            }
            let mut boot_rom = [0; BOOT_ROM_SIZE];
            boot_rom.copy_from_slice(&boot_rom_buffer);
            boot_rom
        });

        let mut rom_bank_0 = [0; ROM_BANK_0_SIZE];
        for i in 0..ROM_BANK_0_SIZE {
            rom_bank_0[i] = game_rom[i];
        }
        let mut rom_bank_n = [0; ROM_BANK_N_SIZE];
        for i in 0..ROM_BANK_N_SIZE {
            rom_bank_n[i] = game_rom[ROM_BANK_0_SIZE + i];
        }
        Memory {
            bootrom,
            rom_bank_0,
            rom_bank_n,
            external_ram: [0; EXTERNAL_RAM_SIZE],
            working_ram: [0; WORKING_RAM_SIZE],
            zero_page: [0; ZERO_PAGE_SIZE],
            interrupt_enable: InterruptFlags::new(),
            interrupt_flag: InterruptFlags::new(),
            gpu: GPU::new(),
        }
    }
    pub fn read_byte(&self, addr: u16) -> u8 {
        // self.memory[addr as usize]
        let addr = addr as usize;
        match addr {
            BOOT_ROM_BEGIN..=BOOT_ROM_END => {
                // return self.rom_bank_0[addr];
                if let Some(boot_rom) = self.bootrom {
                    boot_rom[addr]
                } else {
                    self.rom_bank_0[addr]
                }
            }
            ROM_BANK_0_BEGIN..=ROM_BANK_0_END => self.rom_bank_0[addr],
            ROM_BANK_N_BEGIN..=ROM_BANK_N_END => self.rom_bank_n[addr - ROM_BANK_N_BEGIN],
            VRAM_BEGIN..=VRAM_END => self.gpu.vram[addr - VRAM_BEGIN],
            EXTERNAL_RAM_BEGIN..=EXTERNAL_RAM_END => self.external_ram[addr - EXTERNAL_RAM_BEGIN],
            WORKING_RAM_BEGIN..=WORKING_RAM_END => self.working_ram[addr - WORKING_RAM_BEGIN],
            ECHO_RAM_BEGIN..=ECHO_RAM_END => self.working_ram[addr - ECHO_RAM_BEGIN],
            OAM_BEGIN..=OAM_END => 0, // TODO gpu.oam[addr - OAM_BEGIN],
            UNUSED_BEGIN..=UNUSED_END => 0,
            IO_REGISTERS_BEGIN..=IO_REGISTERS_END => 0, // TODO read io register,
            ZERO_PAGE_BEGIN..=ZERO_PAGE_END => self.zero_page[addr - ZERO_PAGE_BEGIN],
            INTERRUPT_ENABLE_REGISTER => self.interrupt_enable.to_byte(),
            _ => panic!("Cannot read mem 0x{:x}", addr),
        }
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        // println!("{:x}", addr);
        // let addr = addr as usize;
        // self.memory[addr] = val;

        let address = addr as usize;
        match address {
            ROM_BANK_0_BEGIN..=ROM_BANK_0_END => {
                self.rom_bank_0[address] = val;
            }
            VRAM_BEGIN..=VRAM_END => {
                self.gpu.write_vram(address - VRAM_BEGIN, val);
            }
            EXTERNAL_RAM_BEGIN..=EXTERNAL_RAM_END => {
                self.external_ram[address - EXTERNAL_RAM_BEGIN] = val;
            }
            WORKING_RAM_BEGIN..=WORKING_RAM_END => {
                self.working_ram[address - WORKING_RAM_BEGIN] = val;
            }
            OAM_BEGIN..=OAM_END => {
                // TODO gpu.write_oam(address - OAM_BEGIN, val);
            }
            IO_REGISTERS_BEGIN..=IO_REGISTERS_END => {
                // TODO write io register
            }
            UNUSED_BEGIN..=UNUSED_END => {
                // do nothing
            }
            ZERO_PAGE_BEGIN..=ZERO_PAGE_END => {
                self.zero_page[address - ZERO_PAGE_BEGIN] = val;
            }
            INTERRUPT_ENABLE_REGISTER => {
                self.interrupt_enable.from_byte(val);
            }
            _ => {
                panic!(
                    "Error writing to unkown part of memory at address 0x{:x}",
                    address
                );
            }
        }
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
