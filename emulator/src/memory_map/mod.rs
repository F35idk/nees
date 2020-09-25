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

    // provided method for writing to the 'oamdma' register on the
    // ppu (0x4014). requires 'read()' to be implemented. intented
    // to be used by 'write()' implementations.
    fn write_oamdma(&mut self, val: u8, cpu: &mut cpu::Cpu) {
        self.get_ppu().set_ppustatus_low_bits(val);

        // if 'val' is $XX, start address should be $XX00
        let start_addr = (val as u16) << 8;

        for (i, addr) in ((start_addr)..=(start_addr + 0xff)).enumerate() {
            let byte = self.read(addr, cpu);

            // write 'byte' to oam.primary[oamaddr]
            let ppu = self.get_ppu();
            ppu.oam.primary.set_byte(ppu.oamaddr, byte);
            ppu.oamaddr = ppu.oamaddr.wrapping_add(1);

            cpu.cycle_count += 2;

            // catch the ppu up to the cpu on every 4th iteration
            if i % 4 == 0 {
                // NOTE: the ppu may finish the current frame while being caught up here, in
                // which case subsequent calls to 'catch_up()' will have no effect. when this
                // happens, the cpu could end up running quite far ahead of the ppu before it
                // is ultimately caught up in the next frame. realistically, this shouldn't
                // be a problem if it ever happens, but it may be worth keeping in mind
                ppu.catch_up(cpu);
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
