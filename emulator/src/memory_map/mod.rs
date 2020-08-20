mod nrom;

pub use nrom::{Nrom128CpuMemory, Nrom256CpuMemory, NromPpuMemory};

use super::cpu;

// traits to represent operations on the cpu and ppu memory maps/
// address spaces. allows implementing custom memory read/write
// behavior for the various 'mappers' used by nes games/cartridges

pub trait CpuMemoryMap {
    fn read(&mut self, addr: u16, cpu: &mut cpu::Cpu) -> u8;
    fn write(&mut self, addr: u16, val: u8, cpu: &mut cpu::Cpu);
}

pub trait PpuMemoryMap {
    fn read(&self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, val: u8);
    fn get_pattern_tables<'a>(&'a mut self) -> &'a mut [u8; 0x2000];
}
