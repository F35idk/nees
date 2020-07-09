#[macro_use]
use super::super::log;
use super::{apu, ppu};
use super::{AddrInt, MemoryMap};

// the cpu memory map for games that use the 'NROM128' cartridge/mapper (ines mapper 0)
// TODO: implement ppu side of things
pub struct Nrom128MemoryMap {
    memory: [u8; 0x5808],
    // the addresses passed to the read/write calls translate
    // to these ranges in the 'memory' array:
    // [0..=0x7ff] = internal ram
    // [0x800..=0x47ff] = prg rom
    // [0x4800..=0x57ff] = prg ram
    // [0x5800..=0x5807] = ppu registers
    // TODO: 0x2000 * chr rom/possibly ram
}

impl Nrom128MemoryMap {
    pub fn new() -> Self {
        Nrom128MemoryMap {
            memory: [0; 0x5808],
        }
    }

    pub fn load_prg_rom(&mut self, rom: &[u8]) {
        assert!(rom.len() == 0x4000);
        self.memory[0x800..=0x47ff].copy_from_slice(rom);
    }
}

impl MemoryMap for Nrom128MemoryMap {
    fn read_cpu<A: AddrInt>(
        &self,
        ppu: &ppu::Ppu,
        apu: &apu::Apu,
        _addr: A, //
    ) -> u8 {
        let mut addr = _addr.to_u16();

        // address lines a13-a15 = 000 (0-0x1fff) => internal ram
        if (addr >> 13) == 0 {
            // mask off bit 11 and 12 for mirroring
            addr &= !0b1100000000000;
            // TODO: revert to unchecked indexing
            return unsafe { *self.memory.get(addr.to_usize()).unwrap() };
        }

        // address lines a13-a15 = 001 (0x2000-0x3fff) => ppu registers
        if (addr >> 13) == 1 {
            // set to 0x5800-0x5807
            addr &= 0b111;
            addr |= 0x5800;
            return unsafe { *self.memory.get(addr.to_usize()).unwrap() };
        }

        // address lines a13-a15 = 011 (0x6000-0x7fff) => prg ram
        if (addr >> 13) == 3 {
            // mask off bit 12 for mirroring
            addr &= !0b1000000000000;
            // subtract offset of 0x1800 to get index in range 0x4800-0x57ff
            addr -= 0x1800;
            return unsafe { *self.memory.get(addr.to_usize()).unwrap() };
        }

        // addres line a15 = 1 (0x8000-0xffff) => prg rom
        if (addr & 0x8000) != 0 {
            // mask off bit 14 for mirroring
            addr &= !0b100000000000000;
            // subtract offset
            addr -= 0x7800;
            return unsafe { *self.memory.get(addr.to_usize()).unwrap() };
        }

        // TODO: special apu/io stuff in the 0x4000-0x4017 range. the
        // other addresses (up to 0x5fff) should probably just be ignored

        return 0;
    }

    fn write_cpu<A: AddrInt>(
        &mut self,
        ppu: &mut ppu::Ppu,
        apu: &mut apu::Apu,
        _addr: A,
        val: u8, //
    ) {
        let addr = _addr.to_u16();

        logln!(
            "writing to address {:x}, corresponding to index {:x} in memory",
            _addr.to_usize(),
            addr
        );

        // NOTE: see 'calc_cpu_read_addr()' for comments explaining the address calculation
        if (addr >> 13) == 0 {
            unsafe {
                *self
                    .memory
                    .get_mut((addr & !0b1100000000000).to_usize())
                    .unwrap() = val;
            }
            return;
        }

        if (addr >> 13) == 1 {
            unsafe {
                *self
                    .memory
                    .get_mut(((addr & 0b111) | 0x5800).to_usize())
                    .unwrap() = val;
            }

            return;
        }

        if (addr >> 13) == 3 {
            unsafe {
                *self
                    .memory
                    .get_mut(((addr & !0b1000000000000) - 0x1800).to_usize())
                    .unwrap() = val;
            }
            return;
        }

        // TODO: apu/io stuff. note that when this is added, it'll also be
        // necessary to explicitly ignore attempts to write to rom

        return;
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
    fn calc_cpu_read_addr(addr: u16) -> u16 {
        if (addr >> 13) == 0 {
            return addr & !0b1100000000000;
        }

        if (addr >> 13) == 1 {
            return (addr & 0b111) | 0x5800;
        }

        if (addr >> 13) == 3 {
            return (addr & !0b1000000000000) - 0x1800;
        }

        if (addr & 0x8000) != 0 {
            return (addr & !0b100000000000000) - 0x7800;
        }

        return 0;
    }

    let mut memory = Nrom128MemoryMap::new();
    let mut ppu = ppu::Ppu {};
    let mut apu = apu::Apu {};

    // internal ram reads
    assert_eq!(calc_cpu_read_addr(0xa0e), 0x20e);
    assert_eq!(calc_cpu_read_addr(0x1000), 0);
    assert_eq!(calc_cpu_read_addr(0x18f0), 0xf0);

    // 'unmapped' area reads, should return 0
    assert_eq!(calc_cpu_read_addr(0x48f0), 0);
    assert_eq!(calc_cpu_read_addr(0x5000), 0);

    // ppu register writes
    memory.write_cpu(&mut ppu, &mut apu, 0x3fffu16, 0x0f);
    memory.write_cpu(&mut ppu, &mut apu, 0x2001u16, 0xbf);
    assert_eq!(memory.memory[0x5807], 0x0f);
    assert_eq!(memory.memory[0x5801], 0xbf);

    // prg ram writes
    memory.write_cpu(&mut ppu, &mut apu, 0x7fffu16, 0xfe);
    memory.write_cpu(&mut ppu, &mut apu, 0x6000u16, 0xce);
    assert_eq!(memory.memory[0x57ff], 0xfe);
    assert_eq!(memory.memory[0x4800], 0xce);

    // special io stuff, should just return 0
    assert_eq!(calc_cpu_read_addr(0x401f), 0);

    // prg rom reads
    assert_eq!(calc_cpu_read_addr(0x8000), 0x800);
    assert_eq!(calc_cpu_read_addr(0xffff), 0x47ff);

    // prg rom writes
    memory.write_cpu(&mut ppu, &mut apu, 0xcfffu16, 0xff);
    // should not take effect
    assert!(memory.read_cpu(&mut ppu, &mut apu, 0xcfffu16) != 0xff);
}
