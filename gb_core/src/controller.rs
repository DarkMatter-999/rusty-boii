#[derive(PartialEq, Debug)]
pub enum Column {
    One,
    Two,
}

#[derive(Debug)]
pub enum Key {
    Start,
    Select,
    A,
    B,
    Up,
    Down,
    Right,
    Left,
}

pub struct Controller {
    pub column: Column,
    pub start: bool,
    pub select: bool,
    pub b: bool,
    pub a: bool,
    pub down: bool,
    pub up: bool,
    pub left: bool,
    pub right: bool,
}

impl Controller {
    pub fn new() -> Controller {
        Controller {
            column: Column::One,
            start: false,
            select: false,
            b: false,
            a: false,
            down: false,
            up: false,
            left: false,
            right: false,
        }
    }

    pub fn to_byte(&self) -> u8 {
        println!("{:?}", self.column);
        let columnbit = if self.column == Column::One {
            1 << 4
        } else {
            1 << 5
        };

        let bit4 = (if self.col_select() {
            !self.down
        } else {
            self.start
        } as u8)
            << 3;

        let bit3 = (if self.col_select() {
            !self.up
        } else {
            self.select
        } as u8)
            << 2;

        let bit2 = (if self.col_select() {
            !self.left
        } else {
            self.b
        } as u8)
            << 2;

        let bit1 = (if self.col_select() {
            !self.right
        } else {
            self.a
        } as u8);

        let rowbit = bit1 | bit2 | bit3 | bit4;

        columnbit | rowbit
    }

    fn col_select(&self) -> bool {
        if self.column == Column::One {
            true
        } else {
            false
        }
    }
}
