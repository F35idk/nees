mod nrom;

pub use nrom::{Nrom128CpuMemory, Nrom256CpuMemory, NromPpuMemory};

use super::{apu, cpu, ppu};

// traits to represent operations on the cpu and ppu memory maps/
// address spaces. allows implementing custom memory read/write
// behavior for the various 'mappers' used by nes games/cartridges

pub trait CpuMemoryMap {
    fn read(&mut self, addr: u16, cpu: &mut cpu::Cpu) -> u8;
    fn write(&mut self, addr: u16, val: u8, cpu: &mut cpu::Cpu);
    fn get_ppu(&mut self) -> (&mut ppu::Ppu, &mut dyn PpuMemoryMap);
    fn get_apu(&mut self) -> &mut apu::Apu;
    // provided method for writing to the 'oamdma' register on the
    // ppu (0x4014). requires 'read()' to be implemented. intented
    // to be used by 'write()' implementations.
    fn write_oamdma(&mut self, val: u8, cpu: &mut cpu::Cpu) {
        self.get_ppu().0.set_ppustatus_low_bits(val);

        // if 'val' is $XX, start address should be $XX00
        let start_addr = (val as u16) << 8;

        for (i, addr) in ((start_addr)..=(start_addr + 0xff)).enumerate() {
            let byte = self.read(addr, cpu);

            let (ppu, ppu_memory) = self.get_ppu();
            ppu.write_to_oam_and_increment_addr(byte);

            cpu.cycle_count += 2;

            // catch the ppu up to the cpu on every 4th iteration
            if i % 4 == 0 {
                // NOTE: the ppu may finish the current frame while being caught up here, in
                // which case subsequent calls to 'catch_up()' will have no effect. when this
                // happens, the cpu could end up running quite far ahead of the ppu before it
                // is ultimately caught up in the next frame. realistically, this shouldn't
                // be a problem if it ever happens, but it may be worth keeping in mind
                ppu.catch_up(cpu, ppu_memory);
            }
        }

        // set 'Cpu.nmi' = false in case a call to 'Ppu.catch_up()' set it to true
        // TODO: less hacky solution (pass 'do_nmi' param to 'catch_up()' or something)
        cpu.nmi = false;

        // in total, dma should take 513 cpu cycles (or 514 if on an odd cpu cycle)
        cpu.cycle_count += 1 + (cpu.cycle_count % 2);
    }
}

pub trait PpuMemoryMap {
    fn read(&self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, val: u8);
    fn get_pattern_tables<'a>(&'a self) -> &'a [u8; 0x2000];
}

// convenience functions for address calculation. to be
// used by memory map implementations

fn is_0_to_1fff(addr: u16) -> bool {
    // address lines a13-a15 = 000 => (0-0x1fff, internal cpu ram)
    (addr >> 13) == 0
}

fn is_2000_to_3fff(addr: u16) -> bool {
    // address lines a13-a15 = 001 => (0x2000-0x3fff, ppu registers)
    (addr >> 13) == 1
}

fn is_6000_to_7fff(addr: u16) -> bool {
    // address lines a13-a15 = 011 => (0x6000-0x7fff, prg ram/wram)
    (addr >> 13) == 3
}
