// helper trait needed so that the read and write functions belonging to the
// 'MemoryMap' trait can be generic over u16s and u8s. these ints are small
// enough that unchecked indexing is safe (they can't hold values larger
// than the highest address in the address space represented by a 'MemoryMap').
// NOTE: though the above holds true for the cpu address space, u16::MAX is
// still out of range of the ppu address space. when ppu memory maps are
// implemented in the future, this needs to be taken into account
pub trait AddrInt {
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

// trait to represent operations on the cpu and ppu memory maps/
// address spaces. allows implementing custom memory read/write
// behavior for the various 'mappers' used by nes games/cartridges
pub trait MemoryMap {
    fn read_cpu<A: AddrInt>(&self, addr: A) -> u8;
    fn write_cpu<A: AddrInt>(&mut self, addr: A, val: u8);
    fn read_ppu<A: AddrInt>(&self, addr: A) -> u8;
    fn write_ppu<A: AddrInt>(&mut self, addr: A, val: u8);
    // TODO: default methods for loading into rom/ram/etc.
}

// the cpu and memory map for games that use the 'NROM128' cartridge/mapper (ines mapper 0)
// TODO: implement ppu side of things
pub struct Nrom128MemoryMap {
    memory: [u8; 0x5809],
    // the addresses passed to the read/write calls translate
    // to these ranges in the 'memory' array:
    // [0] = dummy write location (for attempts to write to rom)
    // [1..=8] = ppu registers
    // [9..=0x808] = internal ram
    // [0x809..=0x1808] = prg ram
    // [0x1809..=0x5808] = prg rom
    // TODO: 0x4000 * chr rom/possibly ram
}

impl Nrom128MemoryMap {
    pub fn new() -> Self {
        Nrom128MemoryMap {
            memory: [0; 0x5809],
        }
    }

    pub fn load_prg_rom(&mut self, rom: &[u8]) {
        assert!(rom.len() == 0x4000);
        self.memory[0x1809..=0x5808].copy_from_slice(rom);
    }

    // returns 'true' and masks off the high bits of 'addr' if
    // 'addr' points to internal ram. used in 'Nrom128MemoryMap's
    // 'read_cpu()' and 'write_cpu()' implementations
    #[inline]
    fn calc_internal_ram_addr(mut addr: u16) -> (u16, bool) {
        // address lines a13-a15 = 000 => internal ram
        let is_internal_ram = addr < 0x2000;
        if is_internal_ram {
            // mask off bit 11 and 12 for mirroring
            addr &= !0b1100000000000;
        }

        (addr, is_internal_ram)
    }

    // if 'addr' points to a ppu register, this function sets 'addr' equal to the
    // index into 'self.memory' corresponding to the correct register, minus one.
    // also returns whether 'addr' did point to a ppu register or not
    #[inline]
    fn calc_ppu_register_index(mut addr: u16, is_internal_ram: bool) -> (u16, bool) {
        // address lines a13-a15 = 001 => ppu registers
        let is_ppu_register = is_internal_ram ^ (addr < 0x4000);
        if is_ppu_register {
            // ignore all but lowest 3 bits
            addr &= 0b111;
        }

        (addr, is_ppu_register)
    }

    #[inline]
    fn calc_prg_ram_addr(mut addr: u16) -> (u16, bool) {
        // address lines a13-a15 = 011 => prg ram
        let is_prg_ram = (addr >> 13) == 3;
        if is_prg_ram {
            // mask off bit 12 for mirroring
            addr &= !0b1000000000000;
        }

        (addr, is_prg_ram)
    }

    #[inline]
    fn is_prg_rom(addr: u16) -> bool {
        // addres line a15 = 1 => prg rom
        (addr & 0x8000) != 0
    }

    #[inline]
    // calculates the index into 'memory' to read from based on the given address. used
    // by 'Nrom128MemoryMap's 'read_cpu()' implementation (and also in tests, which is
    // why this needs to be a separate function from 'Nrom128MemoryMap.read_cpu()')
    fn calc_cpu_read_addr(addr: u16) -> u16 {
        if addr < 0x6000 && addr >= 0x4000 {
            // TODO: special apu/io stuff in the 0x4000-0x4017 range. the
            // other addresses (up to 0x5fff) should probably just be ignored
            return 0;
        }

        // NOTE: the following function calls are all inlined by the compiler and
        // the branches are replaced by cmovcc instructions and bitops, so
        // performance should be fairly decent. i did try writing my own
        // completely branchless version of this function but it performed worse
        // than this, so i'm trusting the compiler on this one

        // set (or don't set) 'addr' equal to index into
        // 'self.memory' corresponding to internal ram
        let (addr, is_internal_ram) = Self::calc_internal_ram_addr(addr);
        // do the same for the ppu registers
        let (addr, is_ppu_register) = Self::calc_ppu_register_index(addr, is_internal_ram);
        // and prg ram
        let (mut addr, is_prg_ram) = Self::calc_prg_ram_addr(addr);
        // then determine whether address points to prg rom
        let is_prg_rom = Self::is_prg_rom(addr);

        if is_prg_rom {
            // mask off bit 14 for mirroring
            addr &= !0b100000000000000;
        }

        addr += 1;

        if is_ppu_register == false {
            // add offset of 8 if addr doesn't point to a ppu register
            addr += 8;
        }

        if is_prg_ram {
            // subtract offset to get to location of prg ram in memory
            addr -= 0x5800;
        }

        if is_prg_rom {
            // do the same for prg rom
            addr -= 0x6800;
        }

        addr
    }
}

impl MemoryMap for Nrom128MemoryMap {
    fn read_cpu<A: AddrInt>(&self, _addr: A) -> u8 {
        let mut addr = _addr.to_u16();
        addr = Self::calc_cpu_read_addr(addr);

        // TODO: use unchecked indexing
        unsafe { *self.memory.get(addr.to_usize()).unwrap() }
    }

    fn write_cpu<A: AddrInt>(&mut self, _addr: A, val: u8) {
        let addr = _addr.to_u16();

        if addr < 0x6000 && addr >= 0x4000 {
            // TODO: handle special apu/io stuff
            return;
        }

        let (addr, is_internal_ram) = Self::calc_internal_ram_addr(addr);
        let (addr, is_ppu_register) = Self::calc_ppu_register_index(addr, is_internal_ram);
        let (mut addr, is_prg_ram) = Self::calc_prg_ram_addr(addr);

        addr += 1;

        if is_ppu_register == false {
            addr += 8;
        }

        if is_prg_ram {
            addr -= 0x5800;
        }

        if Self::is_prg_rom(addr) {
            // redirect writes to rom to dummy index 0
            addr = 0;
        }

        // TODO: use unchecked indexing
        unsafe {
            *self.memory.get_mut(addr.to_usize()).unwrap() = val;
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

#[test]
fn test_calc_addr() {
    let mut memory = Nrom128MemoryMap::new();

    // internal ram reads
    assert_eq!(Nrom128MemoryMap::calc_cpu_read_addr(0xa0e), 0x217);
    assert_eq!(Nrom128MemoryMap::calc_cpu_read_addr(0x1000), 9);
    assert_eq!(Nrom128MemoryMap::calc_cpu_read_addr(0x18f0), 0xf9);

    // 'unmapped' area reads, should return 0
    assert_eq!(Nrom128MemoryMap::calc_cpu_read_addr(0x48f0), 0);
    assert_eq!(Nrom128MemoryMap::calc_cpu_read_addr(0x5000), 0);

    // ppu register writes
    memory.write_cpu(0x3fffu16, 0x0f);
    memory.write_cpu(0x2001u16, 0xbf);
    assert_eq!(memory.memory[8], 0x0f);
    assert_eq!(memory.memory[2], 0xbf);

    // prg ram writes
    memory.write_cpu(0x7fffu16, 0xfe);
    memory.write_cpu(0x6000u16, 0xce);
    assert_eq!(memory.memory[0x1808], 0xfe);
    assert_eq!(memory.memory[0x809], 0xce);

    // special io stuff, should just return 0
    assert_eq!(Nrom128MemoryMap::calc_cpu_read_addr(0x401f), 0);

    // prg rom reads
    assert_eq!(Nrom128MemoryMap::calc_cpu_read_addr(0x8000), 0x1809);
    assert_eq!(Nrom128MemoryMap::calc_cpu_read_addr(0xffff), 0x5808);

    // attempt to write to rom
    memory.write_cpu(0xcfffu16, 0xff);
    // assert that it ends up at index 0
    assert_eq!(memory.memory[0], 0xff);
}
