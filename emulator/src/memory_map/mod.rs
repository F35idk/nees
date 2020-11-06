mod nrom;

pub use nrom::{Nrom128CpuMemory, Nrom256CpuMemory, NromPpuMemory};

use crate::{apu, controller as ctrl, cpu, ppu, util, PixelRenderer};

// the base struct that all 'CpuMemoryMap' implementations should inherit
// from. can be accessed through the 'CpuMemoryMap::base()' trait method
pub struct CpuMemoryMapBase {
    pub apu: apu::Apu,
    pub ppu: ppu::Ppu,
    pub renderer: PixelRenderer,
    pub controller: ctrl::Controller,
}

impl CpuMemoryMapBase {
    pub fn new(
        ppu: ppu::Ppu,
        apu: apu::Apu,
        controller: ctrl::Controller,
        renderer: PixelRenderer,
    ) -> Self {
        Self {
            ppu,
            apu,
            controller,
            renderer,
        }
    }
}

// trait to represent operations on the cpu memory map/address space.
// allows implementing custom cpu memory read/write behavior for the
// various 'mappers' used by nes games/cartridges. the 'CpuMemoryMap'
// implementations own all devices and other nes state, except the cpu
// itself. a separate 'PpuMemoryMap' trait is used to implement mapping
// functionality for the ppu. the 'CpuMemoryMap' implementor owns this
// as well (it can be accessed through the 'base()' method)

pub trait CpuMemoryMap {
    fn base(&mut self) -> (&mut CpuMemoryMapBase, &mut dyn PpuMemoryMap);
    fn read(&mut self, addr: u16, cpu: &mut cpu::Cpu) -> u8;
    fn write(&mut self, addr: u16, val: u8, cpu: &mut cpu::Cpu);
    // provided method for writing to the 'oamdma' register on the
    // ppu (0x4014). requires 'read()' to be implemented. intented
    // to be used by 'write()' implementations.
    fn write_oamdma(&mut self, val: u8, cpu: &mut cpu::Cpu) {
        self.base().0.ppu.set_ppustatus_low_bits(val);

        // if 'val' is $XX, start address should be $XX00
        let start_addr = (val as u16) << 8;

        for (i, addr) in ((start_addr)..=(start_addr + 0xff)).enumerate() {
            let byte = self.read(addr, cpu);
            self.base().0.ppu.write_to_oam_and_increment_addr(byte);

            cpu.cycle_count += 2;

            // catch the ppu up to the cpu on every 4th iteration
            if i % 4 == 0 {
                let (base, ppu_memory) = self.base();
                base.ppu
                    .catch_up(cpu, ppu_memory, util::pixels_to_u32(&mut base.renderer));
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
