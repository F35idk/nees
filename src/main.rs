#[macro_use]
mod log;
mod apu;
mod cpu;
mod memory_map;
mod parse;
mod ppu;

use memory_map as mmap;

fn main() {
    let rom = std::fs::read("nestest.nes").unwrap();
    assert!(parse::is_valid(&rom));

    if false {
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
    }

    match (parse::get_mapper_num(&rom), parse::get_prg_size(&rom)) {
        (0, 1) => {
            // nrom-128 (maybe?)
        }
        (0, 2) => {
            // nrom-256
        }
        _ => (),
    }

    let mut cpu = cpu::Cpu::new_nestest();
    let mut memory = mmap::Nrom128MemoryMap::new();
    let ref mut ppu = ppu::Ppu::default();
    let ref mut apu = apu::Apu {};
    let ref mut ptrs = mmap::MemoryMapPtrs { ppu, apu };

    if true {
        let prg_size = 0x4000 * (parse::get_prg_size(&rom) as usize);
        memory.load_prg_rom(&rom[0x10..=prg_size + 0xf]);

        loop {
            cpu.log_register_values();
            cpu.exec_instruction(&mut memory, ptrs);
        }
    }
}
