// helper trait needed so that the read and write functions belonging to the
// 'MemoryMap' trait can be generic over u16s and u8s. these ints are small
// enough that unchecked indexing is safe (they can't hold values larger
// than the highest address in the address space represented by a 'MemoryMap').
// NOTE: though the above holds true for the cpu address space, u16::MAX is
// still out of range of the ppu address space. when ppu memory maps are
// implemented in the future, this needs to be taken into account
pub trait AddrInt {
    fn to_usize(self) -> usize;
}

impl AddrInt for u8 {
    #[inline]
    fn to_usize(self) -> usize {
        self as usize
    }
}

impl AddrInt for u16 {
    #[inline]
    fn to_usize(self) -> usize {
        self as usize
    }
}

// trait to represent operations on the cpu and ppu memory maps/address spaces. allows implementing
// custom memory read/write behavior for the various 'mappers' used by nes games/cartridges
pub trait MemoryMap {
    fn read_cpu<A: AddrInt>(&self, addr: A) -> u8;
    fn write_cpu<A: AddrInt>(&mut self, addr: A, val: u8);
    fn read_ppu<A: AddrInt>(&self, addr: A) -> u8;
    fn write_ppu<A: AddrInt>(&mut self, addr: A, val: u8);
}

// the cpu and ppu memory map for games that use the 'NROM' cartridge/mapper (ines mapper 0)
pub struct Nrom128MemoryMap {
    pub memory: [u8; 0x10000],
}

impl MemoryMap for Nrom128MemoryMap {
    #[inline]
    fn read_cpu<A: AddrInt>(&self, addr: A) -> u8 {
        // TODO: mirror 0x8000-0xbfff in 0xc000-0xffff
        unsafe { *self.memory.get_unchecked(addr.to_usize()) }
    }

    #[inline]
    fn write_cpu<A: AddrInt>(&mut self, addr: A, val: u8) {
        unsafe {
            *self.memory.get_unchecked_mut(addr.to_usize()) = val;
        }
    }

    #[inline]
    fn read_ppu<A: AddrInt>(&self, addr: A) -> u8 {
        // TODO: implement
        0
    }

    #[inline]
    fn write_ppu<A: AddrInt>(&mut self, addr: A, val: u8) {
        // TODO: implement
    }
}

impl Nrom128MemoryMap {
    pub fn new() -> Self {
        Nrom128MemoryMap {
            memory: [0; 0x10000],
            // 0x800 * (internal ram)
            // 0x8 * (ppu registers)
            // 0x1000 * (extra prg ram)
            // 0x4000 * prg rom
            // 0x4000 * chr rom/possibly ram
            //
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
        // address lines a13-a15 = 000 => internal ram
        // mask off high bits if address is < 0x2000 (i.e a mirror of 0-0x7ff)
        addr &= !(0b1100000000000 * is_lt_2000 as u16);

        // address lines a13-a15 = 001 => ppu registers
        // set to true if addr is between 2000 and 3fff (ppu registers)
        let is_ppu = is_lt_2000 ^ (addr < 0x4000);
        // TODO: apply this to addr instead and place ppu registers in 'memory'
        let ppu_register_index = addr & (0b111 | !is_ppu as u16 * 0xffff);

        // TODO: block writes to rom etc. (write to dummy location or something)

        // address lines a13-a15 = 011 => prg ram
        // addres lines a15 = 1 => prg rom

        // if this were mmc1, we would (unconditionally?)
        // perform shift register calculations here etc.

        println!("ppu register index = {:x}", ppu_register_index);
        println!("final addr: {:x}", addr);
    }
}
