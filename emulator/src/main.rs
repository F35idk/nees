#[macro_use]
mod util;
mod apu;
mod context;
mod cpu;
mod memory_map;
mod parse;
mod ppu;
mod win;

use memory_map as mmap;
use mmap::MemoryMap;

use pixel_renderer;
use pixel_renderer::PixelRenderer;

#[repr(C)]
pub struct Nes {
    cpu: cpu::Cpu,
    ppu: ppu::Ppu,
    apu: apu::Apu,
    memory: mmap::Nrom128MemoryMap,
    renderer: PixelRenderer,
}

fn main() {
    let rom = std::fs::read("Super_Mario_Bros.nes").unwrap();
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
    let mut renderer = PixelRenderer::new(&mut win.connection, win.win, 256, 240).unwrap();
    let mut memory = mmap::Nrom128MemoryMap::new();
    let mut ppu = ppu::Ppu::default();

    match (parse::get_mapper_num(&rom), parse::get_prg_size(&rom)) {
        (0, 1) => {
            // nrom-128 (maybe?)
        }
        (0, 2) => {
            // nrom-256
        }
        _ => (),
    }

    let nestest = false;
    if nestest {
        let rom = std::fs::read("nestest.nes").unwrap();
        let mut nes = Nes {
            cpu: cpu::Cpu::new_nestest(),
            ppu: ppu::Ppu::default(),
            apu: apu::Apu {},
            memory: mmap::Nrom128MemoryMap::new(),
            renderer,
        };

        let ctx = context::NesContext::new(&mut nes);
        let prg_size = 0x4000 * (parse::get_prg_size(&rom) as usize);
        ctx.memory.load_prg_rom(&rom[0x10..=prg_size + 0xf]);

        loop {
            ctx.cpu.log_register_values();
            ctx.cpu.exec_instruction(&mut ctx.memory, &mut ctx.cpu_ctx);
        }
    }
}
