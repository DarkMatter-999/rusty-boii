use gb_core::cpu::CPU;
use std::io::Read;
fn main() {
    let boot_buffer = buffer_from_file("./roms/dmg0_boot.bin");
    let game_buffer = buffer_from_file("./roms/Tetris (World) (Rev A).gb");

    let mut cpu = CPU::new(Some(boot_buffer), game_buffer);

    run(&mut cpu);
}

fn buffer_from_file(path: &str) -> Vec<u8> {
    let mut file = std::fs::File::open(path).expect("File not there");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Could not read file");
    buffer
}

fn run(cpu: &mut CPU) {
    loop {
        cpu.run();
    }
}
