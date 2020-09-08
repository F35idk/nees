mod nrom;

pub use nrom::{Nrom128CpuMemory, Nrom256CpuMemory, NromPpuMemory};

use super::{apu, cpu, ppu};

// traits to represent operations on the cpu and ppu memory maps/
// address spaces. allows implementing custom memory read/write
// behavior for the various 'mappers' used by nes games/cartridges

pub trait CpuMemoryMap<'a> {
    fn read(&mut self, addr: u16, cpu: &mut cpu::Cpu) -> u8;
    fn write(&mut self, addr: u16, val: u8, cpu: &mut cpu::Cpu);
    fn get_ppu(&mut self) -> &mut ppu::Ppu<'a>;
    fn get_apu(&mut self) -> &mut apu::Apu;

    // provided method for writing to the 'oamdma' register on the ppu
    // (0x4014). intented to be used by 'write()' implementations
    fn write_oamdma(&mut self, val: u8, cpu: &mut cpu::Cpu) {
        // FIXME: should set low bits of 'ppustatus' to low
        // bits of 'val' (properly emulate open bus behavior)

        // if 'val' is $XX, start address should be $XX00
        let start_addr = (val as u16) << 8;

        for (i, addr) in ((start_addr)..=(start_addr + 0xff)).enumerate() {
            let byte = self.read(addr, cpu);
            // write to 'oamdata' register (0x2004)
            self.get_ppu().write_register_by_index(4, byte, cpu);
            cpu.cycle_count += 2;

            // catch the ppu up to the cpu on every 4th iteration
            if i % 4 == 0 {
                self.get_ppu().catch_up(cpu);
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
    fn get_pattern_tables<'a>(&'a mut self) -> &'a mut [u8; 0x2000];
}
