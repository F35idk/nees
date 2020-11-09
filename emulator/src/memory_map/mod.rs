mod nrom;

pub use nrom::{NromCpuMemory, NromPpuMemory};

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
}

pub trait PpuMemoryMap {
    fn read(&self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, val: u8);
}

// utility function for writing to the 'oamdma' register on the ppu
// (0x4014). only requires 'CpuMemoryMap::read()' to be implemented.
// intented to be used by 'CpuMemoryMap::write()' implementations.
fn write_oamdma<M: CpuMemoryMap>(memory: &mut M, val: u8, cpu: &mut cpu::Cpu) {
    memory.base().0.ppu.set_ppustatus_low_bits(val);

    // if 'val' is $XX, start address should be $XX00
    let start_addr = (val as u16) << 8;

    for (i, addr) in ((start_addr)..=(start_addr + 0xff)).enumerate() {
        let byte = memory.read(addr, cpu);
        memory.base().0.ppu.write_to_oam_and_increment_addr(byte);

        cpu.cycle_count += 2;

        // catch the ppu up to the cpu on every 4th iteration
        if i % 4 == 0 {
            let (base, ppu_memory) = memory.base();
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

fn is_8000_to_9fff(addr: u16) -> bool {
    (addr >> 13) == 4
}

fn is_a000_to_bfff(addr: u16) -> bool {
    (addr >> 13) == 5
}

fn is_c000_to_dfff(addr: u16) -> bool {
    (addr >> 13) == 6
}

fn is_e000_to_ffff(addr: u16) -> bool {
    (addr >> 13) == 7
}

// assumes 'addr' points to ppu palette memory and applies
// proper mirroring to it. returns a 5-bit index
fn calc_ppu_palette_addr(mut addr: u16) -> u8 {
    debug_assert!(addr >= 0x3f00);

    // ignore all but lowest 5 bits (32 palettes)
    addr &= 0b11111;

    // ensure 0x10, 0x14, 0x18, 0x1c are mirrored down
    if matches!(addr, 0x10 | 0x14 | 0x18 | 0x1c) {
        addr &= 0xf;
    }

    addr as u8
}

// assumes 'addr' points to a nametable in ppu memory
// (0x2000-0x3eff) and applies proper mirroring to it.
// returns a value in the range 0-0x7ff.
// NOTE: this assumes either horizontal or vertical
// mirroring, not neither
fn calc_ppu_nametable_addr_with_mirroring(mut addr: u16, hor_mirroring: bool) -> u16 {
    debug_assert!(matches!(addr, 0x2000..=0x3eff));

    addr &= !0x3000;

    // apply horizontal nametable mirroring
    if hor_mirroring {
        let high_bits = addr & 0b110000000000;
        addr -= (high_bits.count_ones() as u16) << 10;
    }

    addr & 0x7ff
}
