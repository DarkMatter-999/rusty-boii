use crate::{
    controller::{self, Controller},
    gpu::{BackgroundAndWindowDataSelect, InterruptRequest, ObjectSize, TileMap, GPU},
    interrupts::InterruptFlags,
};

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

fn bit(condition: bool) -> u8 {
    if condition {
        1
    } else {
        0
    }
}

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
    pub controller: Controller,
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
            controller: Controller::new(),
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
            OAM_BEGIN..=OAM_END => self.gpu.oam[addr - OAM_BEGIN],
            UNUSED_BEGIN..=UNUSED_END => 0,
            IO_REGISTERS_BEGIN..=IO_REGISTERS_END => self.read_io_register(addr),
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
            ECHO_RAM_BEGIN..=ECHO_RAM_END => {
                self.working_ram[address - ECHO_RAM_BEGIN] = val;
            }
            OAM_BEGIN..=OAM_END => {
                self.gpu.write_oam(address - OAM_BEGIN, val);
            }
            IO_REGISTERS_BEGIN..=IO_REGISTERS_END => {
                self.write_io_register(address, val);
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

    pub fn step(&mut self, cycles: u8) {
        // self.gpu.step(cycles);

        let (vblank, lcd) = match self.gpu.step(cycles) {
            InterruptRequest::Both => (true, true),
            InterruptRequest::VBlank => (true, false),
            InterruptRequest::LCDStat => (false, true),
            InterruptRequest::None => (false, false),
        };

        if vblank {
            self.interrupt_flag.vblank = true;
        }
        if lcd {
            self.interrupt_flag.lcdstat = true;
        }
    }

    fn read_io_register(&self, addr: usize) -> u8 {
        match addr {
            0xFF00 => self.controller.read(),
            0xFF01 => 0, // TODO: serial
            0xFF02 => 0, // TODO: serial
            0xFF04 => 0,
            0xFF0F => 0,
            0xFF40 => {
                // LCD Control
                bit(self.gpu.lcd_display_enabled) << 7
                    | bit(self.gpu.window_tile_map == TileMap::X9C00) << 6
                    | bit(self.gpu.window_display_enabled) << 5
                    | bit(self.gpu.background_and_window_data_select
                        == BackgroundAndWindowDataSelect::X8000)
                        << 4
                    | bit(self.gpu.background_tile_map == TileMap::X9C00) << 3
                    | bit(self.gpu.object_size == ObjectSize::OS8X16) << 2
                    | bit(self.gpu.object_display_enabled) << 1
                    | bit(self.gpu.background_display_enabled)
            }
            0xFF41 => {
                // LCD Controller Status
                let mode: u8 = self.gpu.mode.into();

                0b10000000
                    | bit(self.gpu.line_equals_line_check_interrupt_enabled) << 6
                    | bit(self.gpu.oam_interrupt_enabled) << 5
                    | bit(self.gpu.vblank_interrupt_enabled) << 4
                    | bit(self.gpu.hblank_interrupt_enabled) << 3
                    | bit(self.gpu.line_equals_line_check) << 2
                    | mode
            }

            0xFF42 => {
                // Scroll Y Position
                self.gpu.viewport_y_offset
            }
            0xFF44 => {
                // Current Line
                self.gpu.line
            }
            0xFF47 => 0b11111111,
            _ => panic!("Reading from an unknown I/O register {:x}", addr),
        }
    }

    fn write_io_register(&mut self, addr: usize, value: u8) {
        match addr {
            0xFF00 => self.controller.write(value),
            0xFF01 => { /* Serial Transfer */ }
            0xFF02 => { /* Serial Transfer Control */ }
            0xFF04 => { /* TODO */ }
            0xFF05 => { /* TODO */ }
            0xFF06 => { /* TODO */ }
            0xFF07 => { /* TODO */ }
            0xFF0F => { /* TODO */ }
            0xFF10 => { /* Channel 1 Sweep register */ }
            0xFF11 => { /* Channel 1 Sound Length and Wave */ }
            0xFF12 => { /* Channel 1 Sound Control */ }
            0xFF13 => { /* Channel 1 Frequency lo */ }
            0xFF14 => { /* Channel 1 Control */ }
            0xFF16 => { /* Channel 2 Sound Control */ }
            0xFF17 => { /* Channel 2 Sound Control */ }
            0xFF18 => { /* Channel 2 Sound Control */ }
            0xFF19 => { /* Channel 2 Frequency hi data*/ }
            0xFF1A => { /* Channel 3 Sound on/off */ }
            0xFF1B => { /* Channel 3 Sound on/off */ }
            0xFF1C => { /* Channel 3 Sound on/off */ }
            0xFF1D => { /* Channel 3 Sound on/off */ }
            0xFF1E => { /* Channel 3 Sound on/off */ }
            0xFF20 => { /* Channel 4 Volume */ }
            0xFF21 => { /* Channel 4 Volume */ }
            0xFF22 => { /* Channel 4 Volume */ }
            0xFF23 => { /* Channel 4 Counter/consecutive */ }
            0xFF24 => { /* Sound  Volume */ }
            0xFF25 => { /* Sound output terminal selection */ }
            0xFF26 => { /* Sound on/off */ }
            0xff30 | 0xff31 | 0xff32 | 0xff33 | 0xff34 | 0xff35 | 0xff36 | 0xff37 | 0xff38
            | 0xff39 | 0xff3a | 0xff3b | 0xff3c | 0xff3d | 0xff3e | 0xff3f => {
                //Wave Pattern RAM
            }
            0xFF40 => {
                // LCD Control
                self.gpu.lcd_display_enabled = (value >> 7) == 1;
                self.gpu.window_tile_map = if ((value >> 6) & 0b1) == 1 {
                    TileMap::X9C00
                } else {
                    TileMap::X9800
                };
                self.gpu.window_display_enabled = ((value >> 5) & 0b1) == 1;
                self.gpu.background_and_window_data_select = if ((value >> 4) & 0b1) == 1 {
                    BackgroundAndWindowDataSelect::X8000
                } else {
                    BackgroundAndWindowDataSelect::X8800
                };
                self.gpu.background_tile_map = if ((value >> 3) & 0b1) == 1 {
                    TileMap::X9C00
                } else {
                    TileMap::X9800
                };
                self.gpu.object_size = if ((value >> 2) & 0b1) == 1 {
                    ObjectSize::OS8X16
                } else {
                    ObjectSize::OS8X8
                };
                self.gpu.object_display_enabled = ((value >> 1) & 0b1) == 1;
                self.gpu.background_display_enabled = (value & 0b1) == 1;
            }
            0xFF41 => {
                // LCD Controller Status
                self.gpu.line_equals_line_check_interrupt_enabled =
                    (value & 0b1000000) == 0b1000000;
                self.gpu.oam_interrupt_enabled = (value & 0b100000) == 0b100000;
                self.gpu.vblank_interrupt_enabled = (value & 0b10000) == 0b10000;
                self.gpu.hblank_interrupt_enabled = (value & 0b1000) == 0b1000;
            }
            0xFF42 => {
                // Viewport Y Offset
                self.gpu.viewport_y_offset = value;
            }
            0xFF43 => {
                // Viewport X Offset
                self.gpu.viewport_x_offset = value;
            }
            0xFF45 => {
                self.gpu.line_check = value;
            }
            0xFF46 => {
                // TODO: account for the fact this takes 160 microseconds
                let dma_source = (value as u16) << 8;
                let dma_destination = 0xFE00;
                for offset in 0..150 {
                    self.write_byte(
                        dma_destination + offset,
                        self.read_byte(dma_source + offset),
                    )
                }
            }
            0xFF47 => {
                // Background Colors Setting
                self.gpu.background_colors = value.into();
            }
            0xFF48 => {
                self.gpu.obj_0_color_3 = (value >> 6).into();
                self.gpu.obj_0_color_2 = ((value >> 4) & 0b11).into();
                self.gpu.obj_0_color_1 = ((value >> 2) & 0b11).into();
            }
            0xFF49 => {
                self.gpu.obj_1_color_3 = (value >> 6).into();
                self.gpu.obj_1_color_2 = ((value >> 4) & 0b11).into();
                self.gpu.obj_1_color_1 = ((value >> 2) & 0b11).into();
            }
            0xFF4A => {
                self.gpu.window.y = value;
            }
            0xFF4B => {
                self.gpu.window.x = value;
            }
            0xFF50 => {
                // Unmap boot ROM
                self.bootrom = None;
            }
            0xFF70..=0xFF7f => {
                // Writing to here does nothing
            }
            _ => panic!(
                "Writting '0b{:b}' to an unknown I/O register {:x}",
                value, addr
            ),
        }
    }
}
