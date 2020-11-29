#[macro_use]
mod util;
mod address_bus;
mod apu;
mod controller;
mod cpu;
mod parse;
mod ppu;
mod test;
mod win;

use address_bus as bus;
use bus::{CpuAddressBus, PpuAddressBus};
use controller as ctrl;

use pixel_renderer;
use pixel_renderer::xcb;
use pixel_renderer::PixelRenderer;
use xcb_util::keysyms;

use std::io::Read;

struct Nes<'a> {
    cpu: cpu::Cpu,
    bus: &'a mut dyn CpuAddressBus,
}

impl<'a> Nes<'a> {
    fn new(framebuffer: &mut [u32; 256 * 240], rom_file: &mut std::fs::File) -> Self {
        let mut rom = vec![0; rom_file.metadata().unwrap().len() as usize];
        rom_file.read(&mut rom).unwrap();

        let prg_size = 0x4000 * (parse::get_prg_size(&rom) as usize);
        let chr_size = 0x2000 * (parse::get_chr_size(&rom) as usize);
        let mirroring = parse::get_mirroring_type(&rom);

        if !parse::is_valid(&rom) {
            error_exit!("Failed to load rom file: invalid ines header information");
        }

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

        let ppu = ppu::Ppu::new();
        let apu = apu::Apu {};
        let controller = ctrl::Controller::default();

        let cpu = cpu::Cpu::default();
        // NOTE: 'bus' stores the 'framebuffer' pointer as a raw pointer and keeps it
        // until the emulator exits. in the context of this program, this is totally
        // safe, as the pointer stays valid at all times. the 'PixelRenderer' that
        // provides the framebuffer pointer is guaranteed to live for the entire
        // duration of the program, and the framebuffer itself is never moved in memory.
        let bus: &mut dyn CpuAddressBus = match (parse::get_mapper_num(&rom), cfg!(test)) {
            (num, false) => match num {
                // mapper 0 => nrom
                0 => Box::leak(Box::new(bus::NromCpuAddressBus::new(
                    &rom[0x10..=prg_size + 0xf],
                    &rom[0x10 + prg_size..=prg_size + chr_size + 0xf],
                    mirroring,
                    ppu,
                    apu,
                    controller,
                    framebuffer,
                ))),
                // mapper 4 => mmc3
                4 => Box::leak(Box::new(bus::Mmc3CpuAddressBus::new(
                    &rom[0x10..=prg_size + 0xf],
                    &rom[0x10 + prg_size..=prg_size + chr_size + 0xf],
                    mirroring,
                    ppu,
                    apu,
                    controller,
                    framebuffer,
                ))),
                n => error_exit!(
                    "Failed to load rom file: ines mapper {} is not supported",
                    n
                ),
            },
            // use custom address bus structs in tests
            (num, true) => match num {
                0 => Box::leak(Box::new(
                    test::TestCpuAddressBus::<bus::NromCpuAddressBus>::new(
                        &rom[0x10..=prg_size + 0xf],
                        &rom[0x10 + prg_size..=prg_size + chr_size + 0xf],
                        mirroring,
                        ppu,
                        apu,
                        controller,
                        framebuffer,
                    ),
                )),
                4 => Box::leak(Box::new(
                    test::TestCpuAddressBus::<bus::Mmc3CpuAddressBus>::new(
                        &rom[0x10..=prg_size + 0xf],
                        &rom[0x10 + prg_size..=prg_size + chr_size + 0xf],
                        mirroring,
                        ppu,
                        apu,
                        controller,
                        framebuffer,
                    ),
                )),
                _ => panic!(),
            },
        };

        Self { cpu, bus }
    }

    #[cfg(test)]
    fn reset_state(&mut self) {
        self.cpu = cpu::Cpu::default();
        let base = self.bus.base().0;
        base.ppu.reset_state();
        base.controller = ctrl::Controller::default();
        base.apu = apu::Apu {};
        // TODO: rest of state
    }

    #[cfg(test)]
    fn new_test(framebuffer: &mut [u32; 256 * 240]) -> Self {
        let ppu = ppu::Ppu::new();
        let apu = apu::Apu {};
        let cpu = cpu::Cpu::default();
        let controller = ctrl::Controller::default();
        let bus = Box::leak(Box::new(bus::NromCpuAddressBus::new_empty(
            0x4000,
            ppu,
            apu,
            controller,
            framebuffer,
        )));

        Self { cpu, bus }
    }
}

fn main() {
    let mut args = std::env::args();
    if args.len() < 2 {
        error_exit!("Failed to parse commandline arguments: too few arguments were provided");
    }
    let rom_path = args.nth(1).unwrap();
    let mut rom_file = match std::fs::File::open(rom_path) {
        Ok(f) => f,
        Err(e) => error_exit!("Failed to open rom file: {}", e),
    };

    let win = match win::XcbWindowWrapper::new("mynes", 1200, 600) {
        Ok(w) => w,
        Err(e) => error_exit!("Failed to create XCB window: {}", e),
    };
    let mut renderer = match PixelRenderer::new(&win.connection, win.win, 256, 240) {
        Ok(r) => r,
        Err(e) => error_exit!("Failed to initialize renderer: {}", e),
    };
    let key_syms = keysyms::KeySymbols::new(&win.connection);

    let Nes { mut cpu, bus } = Nes::new(util::pixels_to_u32(&mut renderer), &mut rom_file);
    // NOTE: raw pointers are used to avoid repeated virtual function calls
    // SAFETY AND RATIONALE: all of the '(*base_raw)' and '(*ppu_bus_raw)'
    // dereferences in the main loop below are equivalent to simply
    // accessing the 'base' and 'ppu_bus' fields on 'bus', which borrow
    // checks just fine. however, accessing these fields directly is only
    // possible when 'bus' isn't hidden behind a dyn pointer. when 'bus'
    // is behind a dyn pointer, the only way to access the fields is
    // through the 'CpuAddressBus::base()' virtual method. to avoid
    // repeated calls to 'base()' in the main loop', 'base' and 'ppu_bus'
    // are instead stored as raw pointers and accessed directly when needed.
    let (base_raw, ppu_bus_raw): (*mut bus::CpuAddressBusBase, *mut dyn bus::PpuAddressBus) =
        unsafe { (bus.base().0, std::mem::transmute(bus.base().1)) };

    cpu.p = 4;
    cpu.sp = 0xfd;
    // set pc = reset vector
    cpu.pc = u16::from_le_bytes([bus.read(0xfffc, &mut cpu), bus.read(0xfffd, &mut cpu)]);

    win.map_and_flush();

    let mut is_paused = false;

    loop {
        let start_of_frame = std::time::Instant::now();

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
                                Some((xcb::CONFIGURE_NOTIFY, _)) | Some((xcb::EXPOSE, _)) => {
                                    // make sure to re-render frame on resize/expose events
                                    let idx = renderer.render_frame();
                                    renderer.present(idx);
                                }
                                _ => (),
                            }
                        }
                    } else {
                        unsafe { (*base_raw).controller.set_key(key_sym) };
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
                    unsafe { (*base_raw).controller.unset_key(key_sym) };

                    current_event = next_event;
                    continue;
                }
                _ => (),
            }

            current_event = win.connection.poll_for_queued_event();
        }

        // run cpu and ppu side by side until frame is done
        // OPTIMIZE: no need to constantly catch the ppu up
        unsafe {
            while !(*base_raw).ppu.is_frame_done() {
                cpu.exec_instruction(bus);
                (*base_raw).ppu.catch_up(
                    &mut cpu,
                    &mut *ppu_bus_raw,
                    util::pixels_to_u32(&mut renderer),
                );
            }
        }

        // reset counters
        unsafe {
            (*base_raw).ppu.sub_cycle_count(cpu.cycle_count as i32 * 3);
            (*base_raw).ppu.set_frame_done(false);
        }
        cpu.cycle_count = 0;

        let idx = renderer.render_frame();
        let elapsed = start_of_frame.elapsed();
        let frame_time_left = std::time::Duration::from_nanos(16_666_677 - 200_000)
            .checked_sub(elapsed)
            .unwrap_or_default();

        // sleep until slightly less than 16.67 ms have passed
        std::thread::sleep(frame_time_left);
        renderer.present(idx);
    }
}
