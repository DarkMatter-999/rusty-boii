pub struct Controller {
    row0: u8,
    row1: u8,
    data: u8,
    pub interrupt: u8,
}

#[derive(Copy, Clone)]
pub enum Key {
    Right,
    Left,
    Up,
    Down,
    A,
    B,
    Select,
    Start,
}

impl Controller {
    pub fn new() -> Controller {
        Controller {
            row0: 0x0F,
            row1: 0x0F,
            data: 0xFF,
            interrupt: 0,
        }
    }

    pub fn read(&self) -> u8 {
        self.data
    }

    pub fn write(&mut self, value: u8) {
        self.data = (self.data & 0xCF) | (value & 0x30);
        self.update();
    }

    fn update(&mut self) {
        let current_values = self.data & 0xF;
        let mut new_values = 0xF;

        if self.data & 0x10 == 0x00 {
            new_values &= self.row0;
        }
        if self.data & 0x20 == 0x00 {
            new_values &= self.row1;
        }

        if current_values == 0xF && new_values != 0xF {
            self.interrupt |= 0x10;
        }

        self.data = (self.data & 0xF0) | new_values;
    }

    pub fn keydown(&mut self, key: Key) {
        match key {
            Key::Right => self.row0 &= !(1 << 0),
            Key::Left => self.row0 &= !(1 << 1),
            Key::Up => self.row0 &= !(1 << 2),
            Key::Down => self.row0 &= !(1 << 3),
            Key::A => self.row1 &= !(1 << 0),
            Key::B => self.row1 &= !(1 << 1),
            Key::Select => self.row1 &= !(1 << 2),
            Key::Start => self.row1 &= !(1 << 3),
        }
        self.update();
    }

    pub fn keyup(&mut self, key: Key) {
        match key {
            Key::Right => self.row0 |= 1 << 0,
            Key::Left => self.row0 |= 1 << 1,
            Key::Up => self.row0 |= 1 << 2,
            Key::Down => self.row0 |= 1 << 3,
            Key::A => self.row1 |= 1 << 0,
            Key::B => self.row1 |= 1 << 1,
            Key::Select => self.row1 |= 1 << 2,
            Key::Start => self.row1 |= 1 << 3,
        }
        self.update();
    }
}
