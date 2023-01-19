use gb_core::{controller::Key, cpu::CPU};
use sdl2::{
    event::Event, keyboard::Keycode, pixels::Color, rect::Rect, render::Canvas, sys::KeyCode,
    video::Window,
};
use std::{io::Read, time::Instant};

pub const SCREEN_WIDTH: u32 = 160;
pub const SCREEN_HEIGHT: u32 = 144;
const SCALE: u32 = 4;

const TICKS_PER_FRAME: usize = 70224;

const ONE_SECOND_IN_MICROS: usize = 1000000000;
const ONE_SECOND_IN_CYCLES: usize = 4190000;
const NUMBER_OF_PIXELS: usize = 23040;

fn main() {
    let boot_buffer = buffer_from_file("./roms/dmg_boot.bin");
    let game_buffer = buffer_from_file("./roms/Tetris (World) (Rev A).gb");

    let mut cpu = CPU::new(Some(boot_buffer), game_buffer);

    // run(&mut cpu);

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window(
            "GameBoy Emulator",
            SCREEN_WIDTH * SCALE,
            SCREEN_HEIGHT * SCALE,
        )
        .position_centered()
        .opengl()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    canvas.clear();
    canvas.present();

    let mut event_pump = sdl_context.event_pump().unwrap();

    let mut cycles_elapsed_in_frame = 0usize;
    let mut now = Instant::now();

    'running: loop {
        for evt in event_pump.poll_iter() {
            match evt {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => {
                    break 'running;
                }
                Event::KeyDown {
                    keycode: Some(Keycode::R),
                    ..
                } => {
                    cpu.pc = 0x00;
                }

                Event::KeyDown {
                    keycode: Some(key), ..
                } => {
                    if let Some(k) = get_input(key) {
                        cpu.keypress(k, true);
                    }
                }
                Event::KeyUp {
                    keycode: Some(key), ..
                } => {
                    if let Some(k) = get_input(key) {
                        cpu.keypress(k, false);
                    }
                }
                _ => (),
            }
        }
        let time_delta = now.elapsed().subsec_nanos();
        now = Instant::now();
        let delta = time_delta as f64 / ONE_SECOND_IN_MICROS as f64;
        let cycles_to_run = delta * ONE_SECOND_IN_CYCLES as f64;

        let mut cycles_elapsed = 0;
        while cycles_elapsed <= cycles_to_run as usize {
            cycles_elapsed += cpu.run() as usize;
            // ins = get_mem(&cpu);
            // draw_debug(&mut canvas2, &mut font, ins);
            // sleep(Duration::from_millis(100));
        }
        draw_screen(&cpu, &mut canvas);

        cycles_elapsed_in_frame += cycles_elapsed;

        // TODO: Consider updating buffer after every line is rendered.
        if cycles_elapsed_in_frame >= TICKS_PER_FRAME {
            draw_screen(&cpu, &mut canvas);
        }
    }
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

fn draw_screen(cpu: &CPU, canvas: &mut Canvas<Window>) {
    for (i, pixel) in cpu.mem.gpu.canvas_buffer.chunks(4).enumerate() {
        // Convert our 1D array's index into a 2D (x,y) position
        let x = (i as u32 % SCREEN_WIDTH) as i32;
        let y = (i as u32 / SCREEN_WIDTH) as i32;

        canvas.set_draw_color(Color::RGBA(pixel[0], pixel[1], pixel[2], pixel[3]));

        // Draw a rectangle at (x,y), scaled up by our SCALE value
        let rect = Rect::new(x * SCALE as i32, y * SCALE as i32, SCALE, SCALE);
        canvas.fill_rect(rect).unwrap();
    }
    canvas.present();
}

fn get_input(key: Keycode) -> Option<Key> {
    match key {
        Keycode::W => Some(Key::Up),
        Keycode::A => Some(Key::Left),
        Keycode::S => Some(Key::Down),
        Keycode::D => Some(Key::Right),
        Keycode::H => Some(Key::Select),
        Keycode::J => Some(Key::Start),
        Keycode::K => Some(Key::B),
        Keycode::L => Some(Key::A),
        _ => None,
    }
}
