// helper trait needed so that the 'read()' and 'write()' functions from the
// 'MemoryMap' trait can be generic over u16s and u8s. these ints are small
// enough that unchecked indexing is safe (they can't hold values larger
// than the highest address in the address space represented by a 'MemoryMap')
pub trait SmallInt {
    fn to_usize(self) -> usize;
}

impl SmallInt for u8 {
    #[inline]
    fn to_usize(self) -> usize {
        self as usize
    }
}

impl SmallInt for u16 {
    #[inline]
    fn to_usize(self) -> usize {
        self as usize
    }
}

// trait to represent operations on the cpu memory map/address space. allows implementing
// custom memory read/write behavior for the various 'mappers' used by nes games/cartridges
pub trait MemoryMap {
    fn read<I: SmallInt>(&self, addr: I) -> u8;
    fn write<I: SmallInt>(&mut self, addr: I, val: u8);
}

// the cpu memory map for games that use the 'NROM' cartridge/mapper (ines mapper 0)
pub struct Nrom128MemoryMap {
    pub memory: [u8; 0x10000],
}

impl MemoryMap for Nrom128MemoryMap {
    #[inline]
    fn read<I: SmallInt>(&self, addr: I) -> u8 {
        // TODO: mirror 0x8000-0xbfff in 0xc000-0xffff
        unsafe { *self.memory.get_unchecked(addr.to_usize()) }
    }

    #[inline]
    fn write<I: SmallInt>(&mut self, addr: I, val: u8) {
        unsafe {
            *self.memory.get_unchecked_mut(addr.to_usize()) = val;
        }
    }
}

impl Nrom128MemoryMap {
    pub fn new() -> Self {
        Nrom128MemoryMap {
            memory: [0; 0x10000],
        }
    }

    #[inline]
    // NOTE: only used by outdated tests
    pub fn _get_mut(&mut self, addr: u16) -> &mut u8 {
        unsafe { self.memory.get_unchecked_mut(addr as usize) }
    }

    pub fn test_calc_addr(&self, mut addr: u16) {
        // if between 4000 and 4017 {
        //     do special apu/io stuff
        //     ...
        // }

        let is_lt_2000 = addr < 0x2000;
        // mask off high bits if address is < 0x2000 (i.e a mirror of 0-0x7ff)
        addr &= !(0b1100000000000 * is_lt_2000 as u16);

        // set to true if addr is between 2000 and 3fff (ppu registers)
        let is_ppu = is_lt_2000 ^ (addr < 0x4000);
        // TODO: apply this to addr instead and place ppu registers in 'memory'
        let ppu_register_index = addr & (0b111 | !is_ppu as u16 * 0xffff);

        // if this were mmc1, we would (unconditionally?)
        // perform shift register calculations here etc.

        println!("ppu register index = {:x}", ppu_register_index);
        println!("final addr: {:x}", addr);
    }
}
