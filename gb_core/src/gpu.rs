use crate::memory::{OAM_SIZE, VRAM_SIZE};

pub enum Color {
    White = 255,
    LightGray = 192,
    DarkGray = 96,
    Black = 0,
}
impl std::convert::From<u8> for Color {
    fn from(n: u8) -> Self {
        match n {
            0 => Color::White,
            1 => Color::LightGray,
            2 => Color::DarkGray,
            3 => Color::Black,
            _ => panic!("Failed to covert {} to a Color", n),
        }
    }
}

pub struct BackgroundColors(Color, Color, Color, Color);

impl BackgroundColors {
    fn new() -> BackgroundColors {
        BackgroundColors(
            Color::White,
            Color::LightGray,
            Color::DarkGray,
            Color::Black,
        )
    }
}

impl std::convert::From<u8> for BackgroundColors {
    fn from(value: u8) -> Self {
        BackgroundColors(
            (value & 0b11).into(),
            ((value >> 2) & 0b11).into(),
            ((value >> 4) & 0b11).into(),
            (value >> 6).into(),
        )
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum TilePixelValue {
    Zero,
    One,
    Two,
    Three,
}
impl Default for TilePixelValue {
    fn default() -> Self {
        TilePixelValue::Zero
    }
}

type TileRow = [TilePixelValue; 8];
type Tile = [TileRow; 8];
fn empty_tile() -> Tile {
    [[Default::default(); 8]; 8]
}

pub enum Mode {
    HorizontalBlank,
    VerticalBlank,
    OAMAccess,
    VRAMAccess,
}
impl std::convert::From<Mode> for u8 {
    fn from(value: Mode) -> Self {
        match value {
            Mode::HorizontalBlank => 0,
            Mode::VerticalBlank => 1,
            Mode::OAMAccess => 2,
            Mode::VRAMAccess => 3,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum ObjectPalette {
    Zero,
    One,
}

impl Default for ObjectPalette {
    fn default() -> Self {
        ObjectPalette::Zero
    }
}

// OAM

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ObjectData {
    x: i16,
    y: i16,
    tile: u8,
    palette: ObjectPalette,
    xflip: bool,
    yflip: bool,
    priority: bool,
}

impl Default for ObjectData {
    fn default() -> Self {
        ObjectData {
            x: -16,
            y: -8,
            tile: Default::default(),
            palette: Default::default(),
            xflip: Default::default(),
            yflip: Default::default(),
            priority: Default::default(),
        }
    }
}

#[derive(Eq, PartialEq)]
pub enum InterruptRequest {
    None,
    VBlank,
    LCDStat,
    Both,
}

impl InterruptRequest {
    fn add(&mut self, other: InterruptRequest) {
        match self {
            InterruptRequest::None => *self = other,
            InterruptRequest::VBlank if other == InterruptRequest::LCDStat => {
                *self = InterruptRequest::Both
            }
            InterruptRequest::LCDStat if other == InterruptRequest::VBlank => {
                *self = InterruptRequest::Both
            }
            _ => {}
        };
    }
}

pub struct Window {
    pub x: u8,
    pub y: u8,
}

const NUMBER_OF_OBJECTS: usize = 40;
pub const SCREEN_WIDTH: usize = 160;
pub const SCREEN_HEIGHT: usize = 144;

pub struct GPU {
    pub canvas_buffer: [u8; SCREEN_WIDTH * SCREEN_HEIGHT * 4],
    pub tile_set: [Tile; 384],
    pub object_data: [ObjectData; NUMBER_OF_OBJECTS],
    pub vram: [u8; VRAM_SIZE],
    pub oam: [u8; OAM_SIZE],
    pub background_colors: BackgroundColors,
    pub viewport_x_offset: u8,
    pub viewport_y_offset: u8,
    pub lcd_display_enabled: bool,
    pub window_display_enabled: bool,
    pub background_display_enabled: bool,
    pub object_display_enabled: bool,
    pub line_equals_line_check_interrupt_enabled: bool,
    pub oam_interrupt_enabled: bool,
    pub vblank_interrupt_enabled: bool,
    pub hblank_interrupt_enabled: bool,
    pub line_check: u8,
    pub line_equals_line_check: bool,
    pub obj_0_color_1: Color,
    pub obj_0_color_2: Color,
    pub obj_0_color_3: Color,
    pub obj_1_color_1: Color,
    pub obj_1_color_2: Color,
    pub obj_1_color_3: Color,
    pub window: Window,
    pub line: u8,
    pub mode: Mode,
    cycles: u16,
}

impl GPU {
    pub fn new() -> GPU {
        GPU {
            canvas_buffer: [0; SCREEN_WIDTH * SCREEN_HEIGHT * 4],
            tile_set: [empty_tile(); 384],
            object_data: [Default::default(); NUMBER_OF_OBJECTS],
            vram: [0; VRAM_SIZE],
            oam: [0; OAM_SIZE],
            background_colors: BackgroundColors::new(),
            viewport_x_offset: 0,
            viewport_y_offset: 0,
            lcd_display_enabled: false,
            window_display_enabled: false,
            background_display_enabled: false,
            object_display_enabled: false,
            line_equals_line_check_interrupt_enabled: false,
            oam_interrupt_enabled: false,
            vblank_interrupt_enabled: false,
            hblank_interrupt_enabled: false,
            line_check: 0,
            line_equals_line_check: false,
            obj_0_color_1: Color::LightGray,
            obj_0_color_2: Color::DarkGray,
            obj_0_color_3: Color::Black,
            obj_1_color_1: Color::LightGray,
            obj_1_color_2: Color::DarkGray,
            obj_1_color_3: Color::Black,
            window: Window { x: 0, y: 0 },
            line: 0,
            cycles: 0,
            mode: Mode::HorizontalBlank,
        }
    }

    pub fn write_vram(&mut self, index: usize, value: u8) {
        self.vram[index] = value;
        if index >= 0x1800 {
            return;
        }

        let normalized_index = index & 0xFFFE;
        let byte1 = self.vram[normalized_index];
        let byte2 = self.vram[normalized_index + 1];

        let tile_index = index / 16;
        let row_index = (index % 16) / 2;

        for pixel_index in 0..8 {
            let mask = 1 << (7 - pixel_index);
            let lsb = byte1 & mask;
            let msb = byte2 & mask;

            let value = match (lsb != 0, msb != 0) {
                (true, true) => TilePixelValue::Three,
                (false, true) => TilePixelValue::Two,
                (true, false) => TilePixelValue::One,
                (false, false) => TilePixelValue::Zero,
            };

            self.tile_set[tile_index][row_index][pixel_index] = value;
        }
    }
}
