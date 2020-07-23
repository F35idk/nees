#[macro_use]
use super::super::log;
use super::MemoryMap;
use super::{apu, ppu};

// the cpu and ppu memory maps for games that use the 'NROM-128' cartridge/mapper (ines mapper 0)
// TODO: implement ppu side of things
pub struct Nrom128MemoryMap {
    cpu_memory: [u8; 0x5800],
    // the addresses passed to the read/write calls translate
    // to these ranges in the 'cpu_memory' array:
    // TODO: make these separate fields instead
    // [0..=0x7ff] = internal ram
    // [0x800..=0x47ff] = prg rom
    // [0x4800..=0x57ff] = prg ram
    chr_ram: [u8; 0x2800],
    nametables: [u8; 0x800],
    palettes: [u8; 32],
    nametable_mirroring_mask: u16,
}

// 'NROM-256' (also ines mapper 0)
pub struct Nrom256MemoryMap {
    memory: [u8; 0x9800],
    // [0..=0x7ff] = internal ram
    // [0x800..=0x87ff] = prg rom
    // [0x8800..=0x97ff] = prg ram
    // TODO: 0x2000 * chr rom/possibly ram
}

impl Nrom128MemoryMap {
    pub fn new() -> Self {
        Nrom128MemoryMap {
            cpu_memory: [0; 0x5800],
            chr_ram: [0; 0x2800],
            nametables: [0; 0x800],
            palettes: [0; 32],
            nametable_mirroring_mask: 0,
        }
    }

    pub fn load_prg_rom(&mut self, rom: &[u8]) {
        assert!(rom.len() == 0x4000);
        self.cpu_memory[0x800..=0x47ff].copy_from_slice(rom);
    }
}

impl MemoryMap for Nrom128MemoryMap {
    fn read_cpu(&self, ptrs: &mut super::MemoryMapPtrs, mut addr: u16) -> u8 {
        // address lines a13-a15 = 000 (0-0x1fff) => internal ram
        if (addr >> 13) == 0 {
            // mask off bit 11 and 12 for mirroring
            addr &= !0b1100000000000;
            // TODO: revert to unchecked indexing
            return unsafe { *self.cpu_memory.get(addr as usize).unwrap() };
        }

        // address lines a13-a15 = 001 (0x2000-0x3fff) => ppu registers
        if (addr >> 13) == 1 {
            // ignore all but low 3 bits
            addr &= 0b111;
            return ptrs.ppu.read_register_by_index(addr as u8, self);
        }

        // address lines a13-a15 = 011 (0x6000-0x7fff) => prg ram
        if (addr >> 13) == 3 {
            // mask off bit 12 for mirroring
            addr &= !0b1000000000000;
            // subtract offset of 0x1800 to get index in range 0x4800-0x57ff
            addr -= 0x1800;
            return unsafe { *self.cpu_memory.get(addr as usize).unwrap() };
        }

        // addres line a15 = 1 (0x8000-0xffff) => prg rom
        if (addr & 0x8000) != 0 {
            // mask off bit 14 for mirroring
            addr &= !0b100000000000000;
            // subtract offset
            addr -= 0x7800;
            return unsafe { *self.cpu_memory.get(addr as usize).unwrap() };
        }

        // TODO: special apu/io stuff in the 0x4000-0x4017 range. the
        // other addresses (up to 0x5fff) should probably just be ignored

        0
    }

    fn write_cpu(&mut self, ptrs: &mut super::MemoryMapPtrs, mut addr: u16, val: u8) {
        // NOTE: see 'calc_cpu_read_addr()' for comments explaining the address calculation
        if (addr >> 13) == 0 {
            unsafe {
                *self
                    .cpu_memory
                    .get_mut((addr & !0b1100000000000) as usize)
                    .unwrap() = val;
            }
            return;
        }

        if (addr >> 13) == 1 {
            ptrs.ppu
                .write_register_by_index(addr as u8 & 0b111, val, self);
            return;
        }

        if (addr >> 13) == 3 {
            unsafe {
                *self
                    .cpu_memory
                    .get_mut(((addr & !0b1000000000000) - 0x1800) as usize)
                    .unwrap() = val;
            }
            return;
        }

        // TODO: apu/io stuff. note that when this is added, it'll also be
        // necessary to explicitly ignore attempts to write to rom

        return;
    }

    fn read_ppu(&self, mut addr: u16) -> u8 {
        // if address points to palette indices
        if addr >= 0x3f00 {
            // ignore all but lowest 5 bits (32 palettes)
            addr &= 0b11111;
            return unsafe { *self.palettes.get_unchecked(addr as usize) };
        }

        // if address is in the range 0x2000-0x3eff
        if addr >= 0x2000 {
            // mirror down to 0-0xfff
            addr &= !0x3000;
            // apply vertical or horizontal nametable mirroring
            addr &= self.nametable_mirroring_mask;
            return unsafe { *self.nametables.get_unchecked(addr as usize) };
        }

        unsafe { *self.chr_ram.get_unchecked(addr as usize) }
    }

    fn write_ppu(&mut self, addr: u16, val: u8) {
        // TODO: implement
    }
}

impl Nrom256MemoryMap {
    pub fn new() -> Self {
        Nrom256MemoryMap {
            memory: [0; 0x9800],
        }
    }

    pub fn load_prg_rom(&mut self, rom: &[u8]) {
        assert!(rom.len() == 0x8000);
        self.memory[0x800..=0x87ff].copy_from_slice(rom);
    }
}

impl MemoryMap for Nrom256MemoryMap {
    fn read_cpu(&self, ptrs: &mut super::MemoryMapPtrs, mut addr: u16) -> u8 {
        if (addr >> 13) == 0 {
            addr &= !0b1100000000000;
            return unsafe { *self.memory.get(addr as usize).unwrap() };
        }

        if (addr >> 13) == 1 {
            addr &= 0b111;
            // return ptrs.ppu.read_register_by_index(addr as u8, self);
            return 0;
        }

        if (addr >> 13) == 3 {
            addr &= !0b1000000000000;
            addr += 0x2800;
            return unsafe { *self.memory.get(addr as usize).unwrap() };
        }

        if (addr & 0x8000) != 0 {
            addr -= 0x7800;
            return unsafe { *self.memory.get(addr as usize).unwrap() };
        }

        0
    }

    fn write_cpu(&mut self, ptrs: &mut super::MemoryMapPtrs, addr: u16, val: u8) {
        if (addr >> 13) == 0 {
            unsafe {
                *self
                    .memory
                    .get_mut((addr & !0b1100000000000) as usize)
                    .unwrap() = val;
            }
            return;
        }

        if (addr >> 13) == 1 {
            unsafe {
                *self
                    .memory
                    .get_mut(((addr & 0b111) | 0x9800) as usize)
                    .unwrap() = val;
            }

            return;
        }

        if (addr >> 13) == 3 {
            // TODO: ppu registers
            return;
        }

        return;
    }

    fn read_ppu(&self, addr: u16) -> u8 {
        0
    }

    fn write_ppu(&mut self, addr: u16, val: u8) {}
}
#[test]
fn test_calc_addr_128() {
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

        0
    }

    let mut memory = Nrom128MemoryMap::new();
    let ref mut ppu = ppu::Ppu::default();
    let ref mut apu = apu::Apu {};
    let ref mut ptrs = super::MemoryMapPtrs { ppu, apu };

    // internal ram reads
    assert_eq!(calc_cpu_read_addr(0xa0e), 0x20e);
    assert_eq!(calc_cpu_read_addr(0x1000), 0);
    assert_eq!(calc_cpu_read_addr(0x18f0), 0xf0);

    // 'unmapped' area reads, should return 0
    assert_eq!(calc_cpu_read_addr(0x48f0), 0);
    assert_eq!(calc_cpu_read_addr(0x5000), 0);

    // prg ram writes
    memory.write_cpu(ptrs, 0x7fffu16, 0xfe);
    memory.write_cpu(ptrs, 0x6000u16, 0xce);
    assert_eq!(memory.cpu_memory[0x57ff], 0xfe);
    assert_eq!(memory.cpu_memory[0x4800], 0xce);

    // special io stuff, should just return 0
    assert_eq!(calc_cpu_read_addr(0x401f), 0);

    // prg rom reads
    assert_eq!(calc_cpu_read_addr(0x8000), 0x800);
    assert_eq!(calc_cpu_read_addr(0xffff), 0x47ff);

    // prg rom writes
    memory.write_cpu(ptrs, 0xcfffu16, 0xff);
    // should not take effect
    assert_ne!(memory.read_cpu(ptrs, 0xcfffu16), 0xff);
}

#[test]
fn test_calc_addr_256() {
    // TODO: ..
}

#[test]
fn test_ppu_calc_addr_128() {
    let mut memory = Nrom128MemoryMap::new();

    // set mirroring mask to vertical
    memory.nametable_mirroring_mask = 0x400;
    let _ = memory.read_ppu(0x2fffu16);

    // TODO: ..
}
