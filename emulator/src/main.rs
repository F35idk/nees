#[macro_use]
mod log;
mod apu;
mod cpu;
mod memory_map;
mod parse;
mod ppu;
mod win;

use memory_map as mmap;
use mmap::MemoryMap;

use pixel_renderer;
use pixel_renderer::PixelRenderer;

// convenience/helper struct for passing around important pointers,
// to reduce amt. of function arguments everywhere, etc.
pub struct PtrsWrapper<'a, 'b, 'c> {
    pub ppu: &'a mut ppu::Ppu,
    pub apu: &'b mut apu::Apu,
    pub cpu_cycles: &'c mut u64,
}

pub fn pixels_to_u32<'a>(pixel_renderer: &'a mut PixelRenderer) -> &'a mut [u32] {
    unsafe {
        std::slice::from_raw_parts_mut(
            pixel_renderer.get_pixels().as_mut_ptr() as *mut u32,
            pixel_renderer.get_pixels().len() / 4,
        )
    }
}

fn main() {
    let rom = std::fs::read("Super_Mario_Bros.nes").unwrap();
    assert!(parse::is_valid(&rom));

    let mut win = win::XcbWindowWrapper::new("mynes", 1200, 600).unwrap();
    let mut renderer = PixelRenderer::new(&mut win.connection, win.win, 256, 240).unwrap();

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

    let mut memory = mmap::Nrom128MemoryMap::new();
    let ref mut ppu = ppu::Ppu::default();

    win.map_and_flush();

    let prg_size = 0x4000 * (parse::get_prg_size(&rom) as usize);
    let chr_size = 0x2000 * (parse::get_chr_size(&rom) as usize);
    memory.load_chr_ram(&rom[0x10 + prg_size..=prg_size + chr_size + 0xf]);
    memory.fill_nametables();

    // set nametable mirroring mask = vertical mirroring
    memory.nametable_mirroring_mask = !0x800;
    // set background pattern table addr = 0x1000 and base nametable addr = 0x2400
    ppu.write_register_by_index(0, 0b10001, &mut memory);
    // set coarse x scroll = 15 (offset by 15 in the nametable)
    ppu.temp_vram_addr.addr |= 0b01111;

    let mut i = 0;
    loop {
        if i % 32 == 0 {
            let i = renderer.render_frame();
            renderer.present(i);
            std::thread::sleep(std::time::Duration::from_millis(5));
        }

        if i < 960 * 8 {
            ppu.draw_tile_row(&mut memory, &mut renderer);
        }

        i += 1;
        // TODO:
    }

    // match (parse::get_mapper_num(&rom), parse::get_prg_size(&rom)) {
    //     (0, 1) => {
    //         // nrom-128 (maybe?)
    //     }
    //     (0, 2) => {
    //         // nrom-256
    //     }
    //     _ => (),
    // }

    // if false {
    //     let mut cpu = cpu::Cpu::new_nestest();
    //     let mut memory = mmap::Nrom128MemoryMap::new();
    //     let ref mut ppu = ppu::Ppu::default();
    //     let ref mut apu = apu::Apu {};
    //     let ref mut cpu_cycles = 7; // for whatever reason

    //     let ref mut ptrs = PtrsWrapper {
    //         ppu,
    //         apu,
    //         cpu_cycles,
    //     };

    //     let prg_size = 0x4000 * (parse::get_prg_size(&rom) as usize);
    //     memory.load_prg_rom(&rom[0x10..=prg_size + 0xf]);

    //     loop {
    //         cpu.log_register_values(ptrs);
    //         cpu.exec_instruction(&mut memory, ptrs);
    //     }
    // }

    // let mut cpu = cpu::Cpu::default();
    // let ref mut memory = mmap::Nrom128MemoryMap::new();
    // let ref mut ppu = ppu::Ppu::default();
    // let ref mut apu = apu::Apu {};
    // let ref mut cpu_cycles = 0;

    // let ref mut ptrs = PtrsWrapper {
    //     ppu,
    //     apu,
    //     cpu_cycles,
    // };

    // // LDA #ff
    // memory.write_cpu(ptrs, 0u16, 0xa9);
    // memory.write_cpu(ptrs, 1u16, 0xff);
    // // STA $2000
    // memory.write_cpu(ptrs, 2u16, 0x8d);
    // memory.write_cpu(ptrs, 3u16, 00);
    // memory.write_cpu(ptrs, 4u16, 0x20);

    // cpu.pc = 0;
    // for _ in 0..2 {
    //     cpu.exec_instruction(memory, ptrs);
    // }
}
