use gb_core::{controller::Key, cpu::CPU};
use sdl2::{
    event::Event,
    keyboard::Keycode,
    pixels::Color,
    rect::Rect,
    render::{Canvas, TextureQuery},
    ttf::Font,
    video::Window,
};

use std::{
    env,
    io::Read,
    thread::sleep,
    time::{Duration, Instant},
};

pub const SCREEN_WIDTH: u32 = 160;
pub const SCREEN_HEIGHT: u32 = 144;
const SCALE: u32 = 4;

const TICKS_PER_FRAME: usize = 70224;

const ONE_SECOND_IN_MICROS: usize = 1000000000;
const ONE_SECOND_IN_CYCLES: usize = 4190000;
// const NUMBER_OF_PIXELS: usize = 23040;

fn main() {
    let args: Vec<String> = env::args().collect();

    let boot_buffer = buffer_from_file(&args[1]);
    let game_buffer = buffer_from_file(&args[2]);

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

    // Debug
    // let window2 = video_subsystem
    //     .window("MemView", 500, 500)
    //     .position_centered()
    //     .opengl()
    //     .build()
    //     .unwrap();

    // let ttf_context = sdl2::ttf::init().unwrap();
    // let mut canvas2 = window2.into_canvas().build().unwrap();
    // let texture_creator = canvas2.texture_creator();
    // let mut font = ttf_context.load_font("./pixeboy.ttf", 128).unwrap();
    // font.set_style(sdl2::ttf::FontStyle::BOLD);
    // let mut ins: [u8; 10] = [0; 10];

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
                        cpu.mem.controller.keydown(k);
                    }
                }
                Event::KeyUp {
                    keycode: Some(key), ..
                } => {
                    if let Some(k) = get_input(key) {
                        cpu.mem.controller.keyup(k);
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

            // sleep(Duration::from_millis(100));
        }

        // ins = get_mem(&cpu);
        // ins[1] = (cpu.pc & 0xff) as u8;
        // ins[0] = (cpu.pc >> 2) as u8;
        // draw_debug(&mut canvas2, &mut font, ins);

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

fn draw_debug(canvas2: &mut Canvas<Window>, font: &mut Font, ins: [u8; 10]) {
    let strs: Vec<String> = ins.iter().map(|b| format!("0x{:x}", b)).collect();
    let strs = strs.join(" ");

    let texture_creator = canvas2.texture_creator();

    font.set_style(sdl2::ttf::FontStyle::BOLD);

    // render a surface, and convert it to a texture bound to the canvas
    let mut surface = font
        .render(&strs)
        .blended(Color::RGBA(255, 0, 0, 255))
        .unwrap();
    let mut texture = texture_creator
        .create_texture_from_surface(&surface)
        .unwrap();

    canvas2.set_draw_color(Color::RGBA(195, 217, 255, 255));
    canvas2.clear();

    let TextureQuery { width, height, .. } = texture.query();

    canvas2
        .copy(&texture, None, Some(Rect::new(1, 1, 500, 50)))
        .unwrap();
    canvas2.present();
}

fn get_mem(cpu: &CPU) -> [u8; 10] {
    let mut data: [u8; 10] = [0; 10];

    for x in 0..10 {
        data[x] = cpu.mem.read_byte(cpu.pc + (x as u16));
    }

    data
}
