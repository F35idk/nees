#[macro_use]
mod util;
mod apu;
mod controller;
mod cpu;
mod memory_map;
mod parse;
mod ppu;
mod win;

use mem::{CpuMemoryMap, PpuMemoryMap};
use memory_map as mem;

use pixel_renderer;
use pixel_renderer::xcb;
use pixel_renderer::PixelRenderer;
use xcb_util::keysyms;

fn main() {
    let rom = std::fs::read("some_rom.nes").unwrap();
    assert!(parse::is_valid(&rom));
    logln!("{}", std::str::from_utf8(&rom[0..=3]).unwrap());
    logln!("is nes 2.0: {}", parse::is_nes_2_format(&rom));
    logln!("has trainer: {}", parse::has_trainer(&rom));
    logln!("mirroring type: {:?}", parse::get_mirroring_type(&rom));
    logln!("mapper number: {}", parse::get_mapper_num(&rom));
    logln!("prg rom size: {}KB", parse::get_prg_size(&rom) as u32 * 16);
    logln!("chr rom size: {}KB", parse::get_chr_size(&rom) as u32 * 8);
    logln!(
        "has battery-backed RAM: {}",
        parse::has_persistent_mem(&rom)
    );

    let mut win = win::XcbWindowWrapper::new("mynes", 1200, 600).unwrap();
    let renderer = PixelRenderer::new(&mut win.connection, win.win, 256, 240).unwrap();
    let key_syms = keysyms::KeySymbols::new(&win.connection);
    let mut ppu_memory = mem::NromPpuMemory::new();

    let prg_size = 0x4000 * (parse::get_prg_size(&rom) as usize);
    let chr_size = 0x2000 * (parse::get_chr_size(&rom) as usize);

    match parse::get_mirroring_type(&rom) {
        parse::MirroringType::Hor => ppu_memory.hor_mirroring = true,
        parse::MirroringType::Vert => ppu_memory.hor_mirroring = false,
        _ => (),
    }

    ppu_memory.load_chr_ram(&rom[0x10 + prg_size..=prg_size + chr_size + 0xf]);

    let ppu = ppu::Ppu::new(renderer, &mut ppu_memory);
    let apu = apu::Apu {};
    let controller = controller::Controller::default();
    let mut cpu_memory = mem::Nrom256CpuMemory::new(ppu, apu, controller);
    let mut cpu = cpu::Cpu::default();

    cpu_memory.load_prg_rom(&rom[0x10..=prg_size + 0xf]);

    // set pc = reset vector
    cpu.pc = u16::from_le_bytes([
        cpu_memory.read(0xfffc, &mut cpu),
        cpu_memory.read(0xfffd, &mut cpu),
    ]);

    cpu.cycle_count = 0;
    cpu.p = 04;
    cpu.sp = 0xfd;

    win.map_and_flush();

    let mut is_paused = false;

    loop {
        let mut current_event = win.connection.poll_for_event();
        while let Some(e) = current_event {
            match e.response_type() & !0x80 {
                xcb::KEY_PRESS => {
                    let key_press: &xcb::KeyPressEvent = unsafe { xcb::cast_event(&e) };
                    let key_sym = key_syms.press_lookup_keysym(key_press, 0);

                    if key_sym == win::Keys::ESC {
                        is_paused = !is_paused;

                        // if paused, call 'wait_for_event()' until an 'ESC'
                        // key press is received
                        while is_paused {
                            match win
                                .connection
                                .wait_for_event()
                                .map(|e| (e.response_type() & !0x80, e))
                            {
                                Some((xcb::KEY_PRESS, e)) => {
                                    let press = unsafe { xcb::cast_event(&e) };
                                    let sym = key_syms.press_lookup_keysym(press, 0);

                                    if sym == win::Keys::ESC {
                                        // unpause
                                        is_paused = false;
                                    }
                                }
                                Some((xcb::CONFIGURE_NOTIFY, _)) => {
                                    // make sure to re-render frame on resize events
                                    let idx = cpu_memory.ppu.renderer.render_frame();
                                    cpu_memory.ppu.renderer.present(idx);
                                }
                                _ => (),
                            }
                        }
                    } else {
                        cpu_memory.controller.set_key(key_sym);
                    }
                }
                xcb::KEY_RELEASE => {
                    let key_release: &xcb::KeyReleaseEvent = unsafe { xcb::cast_event(&e) };
                    let next_event = win.connection.poll_for_queued_event();

                    // check if the next event is a key press
                    if let Some(next_key_press) = next_event
                        .as_ref()
                        .filter(|e| e.response_type() & !0x80 == xcb::KEY_PRESS)
                        .map::<&xcb::KeyPressEvent, _>(|e| unsafe { xcb::cast_event(e) })
                    {
                        if key_release.time() == next_key_press.time()
                            && key_release.detail() == next_key_press.detail()
                        {
                            // ignore key release event if next event is a key press that
                            // occured at the exact same time (this means autorepeat has
                            // kicked in)
                            current_event = win.connection.poll_for_queued_event();
                            continue;
                        }
                    }

                    let key_sym = key_syms.release_lookup_keysym(key_release, 0);
                    cpu_memory.controller.unset_key(key_sym);

                    current_event = next_event;
                    continue;
                }
                _ => (),
            }

            current_event = win.connection.poll_for_queued_event();
        }

        let start_of_frame = std::time::Instant::now();
        let pre_vblank_cycles = ((242i32 * 341i32) / 3) as i16 - 7;

        // run the cpu until vblank
        while cpu.cycle_count < pre_vblank_cycles {
            // NOTE: 'exec_instruction()' ticks the ppu whenever it
            // interacts with it (through writes to 'cpu_memory')
            cpu.exec_instruction(&mut cpu_memory);
        }

        assert!(cpu_memory.ppu.current_scanline < 241);

        // run cpu and ppu side by side until frame is done
        while !cpu_memory.ppu.is_frame_done() {
            cpu.exec_instruction(&mut cpu_memory);
            cpu_memory.ppu.catch_up(&mut cpu);
        }

        // reset counters
        cpu_memory.ppu.set_cycle_count(0);
        cpu.cycle_count -= 29781;
        cpu_memory.ppu.set_frame_done(false);

        let idx = cpu_memory.ppu.renderer.render_frame();
        let elapsed = start_of_frame.elapsed();
        let frame_time_left = std::time::Duration::from_nanos(16_666_677)
            .checked_sub(elapsed)
            .unwrap_or_default();

        std::thread::sleep(frame_time_left);
        cpu_memory.ppu.renderer.present(idx);
    }
}
