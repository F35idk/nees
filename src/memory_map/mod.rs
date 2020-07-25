mod nrom;

pub use nrom::Nrom128MemoryMap;

use super::{apu, ppu};

// trait to represent operations on the cpu and ppu memory maps/
// address spaces. allows implementing custom memory read/write
// behavior for the various 'mappers' used by nes games/cartridges
pub trait MemoryMap {
    fn read_cpu(&self, ptrs: &mut super::PtrsWrapper, addr: u16) -> u8;
    fn write_cpu(&mut self, ptrs: &mut super::PtrsWrapper, addr: u16, val: u8);
    fn read_ppu(&self, addr: u16) -> u8;
    fn write_ppu(&mut self, addr: u16, val: u8);
    unsafe fn get_pattern_tables_raw(&mut self) -> *mut [u8; 0x2000];
    // TODO: default methods for loading into rom/ram/etc.
}
