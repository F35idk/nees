mod nrom;

pub use nrom::Nrom128MemoryMap;

use super::{apu, ppu};

// trait to represent operations on the cpu and ppu memory maps/
// address spaces. allows implementing custom memory read/write
// behavior for the various 'mappers' used by nes games/cartridges
pub trait MemoryMap {
    fn read_cpu<A: AddrInt>(&self, ppu: &mut ppu::Ppu, apu: &mut apu::Apu, addr: A) -> u8;
    fn write_cpu<A: AddrInt>(&mut self, ppu: &mut ppu::Ppu, apu: &mut apu::Apu, addr: A, val: u8);
    fn read_ppu<A: AddrInt>(&self, addr: A) -> u8;
    fn write_ppu<A: AddrInt>(&mut self, addr: A, val: u8);
    // TODO: default methods for loading into rom/ram/etc.
}

// helper trait needed so that the read and write functions of
// the 'MemoryMap' trait can be generic over u16s and u8s.
// these ints are small enough that unchecked indexing is safe.
// NOTE: though the above holds true for the cpu address space,
// u16::MAX is still way out of range of the ppu address space.
// when ppu memory maps are implemented in the future, this
// needs to be taken into account
pub trait AddrInt: Copy {
    fn to_usize(self) -> usize;
    fn to_u16(self) -> u16;
}

impl AddrInt for u8 {
    #[inline]
    fn to_usize(self) -> usize {
        self as usize
    }

    #[inline]
    fn to_u16(self) -> u16 {
        self as u16
    }
}

impl AddrInt for u16 {
    #[inline]
    fn to_usize(self) -> usize {
        self as usize
    }
    #[inline]
    fn to_u16(self) -> u16 {
        self as u16
    }
}
