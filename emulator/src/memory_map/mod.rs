mod nrom;

pub use nrom::Nrom128MemoryMap;

use super::{apu, context, cpu, ppu};

// trait to represent operations on the cpu and ppu memory maps/
// address spaces. allows implementing custom memory read/write
// behavior for the various 'mappers' used by nes games/cartridges
pub trait MemoryMap {
    fn read_cpu(&mut self, addr: u16, ctx: &mut MemoryMapContext) -> u8;
    fn write_cpu(&mut self, addr: u16, val: u8, ctx: &mut MemoryMapContext);
    fn read_ppu(&self, addr: u16) -> u8;
    fn write_ppu(&mut self, addr: u16, val: u8);
    fn get_pattern_tables<'a>(&'a mut self) -> &'a mut [u8; 0x2000];
    // TODO: default methods for loading into rom/ram/etc.
}

// pointers needed by the memory map
pub struct MemoryMapContext<'c, 'p, 'a, 'r> {
    pub cpu: context::ContextPtr<'c, cpu::Cpu>,
    pub ppu: context::ContextPtr<'p, ppu::Ppu>,
    pub apu: context::ContextPtr<'a, apu::Apu>,
    pub renderer: context::ContextPtr<'r, super::PixelRenderer>,
}
