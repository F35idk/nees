use super::super::{apu, cpu, ppu, util, win, PixelRenderer};
use super::{CpuMemoryMap, PpuMemoryMap};

// the cpu and ppu memory maps for games that use the 'NROM-128' cartridge/mapper (ines mapper 0)
pub struct Nrom128CpuMemory<'a> {
    pub ppu: ppu::Ppu<'a>,
    pub apu: apu::Apu,
    // the addresses passed to the read/write calls translate
    // to these ranges in the 'memory' array:
    // TODO: make these separate fields instead
    // [0..=0x7ff] = internal ram
    // [0x800..=0x47ff] = prg rom
    // [0x4800..=0x57ff] = prg ram
    pub memory: [u8; 0x5800],
}

pub struct Nrom128PpuMemory {
    pub chr_ram: [u8; 0x2000],
    pub nametables: [u8; 0x800],
    pub palettes: [u8; 32],
    pub nametable_mirroring_mask: u16,
}

impl<'a> Nrom128CpuMemory<'a> {
    pub fn new(ppu: ppu::Ppu<'a>, apu: apu::Apu) -> Self {
        Self {
            ppu,
            apu,
            memory: [0; 0x5800],
        }
    }

    pub fn load_prg_rom(&mut self, rom: &[u8]) {
        assert_eq!(rom.len(), 0x4000);
        self.memory[0x800..=0x47ff].copy_from_slice(rom);
    }
}

impl Nrom128PpuMemory {
    pub fn new() -> Self {
        Self {
            chr_ram: [0; 0x2000],
            nametables: [0; 0x800],
            palettes: [0; 32],
            nametable_mirroring_mask: 0,
        }
    }

    pub fn load_chr_ram(&mut self, ram: &[u8]) {
        assert_eq!(ram.len(), 0x2000);
        self.chr_ram.copy_from_slice(ram);
    }
}

impl<'a> CpuMemoryMap for Nrom128CpuMemory<'a> {
    fn read(&mut self, mut addr: u16, cpu: &mut cpu::Cpu) -> u8 {
        // address lines a13-a15 = 000 (0-0x1fff) => internal ram
        if (addr >> 13) == 0 {
            // mask off bit 11 and 12 for mirroring
            addr &= !0b1100000000000;
            // TODO: revert to unchecked indexing
            return unsafe { *self.memory.get(addr as usize).unwrap() };
        }

        // address lines a13-a15 = 001 (0x2000-0x3fff) => ppu registers
        if (addr >> 13) == 1 {
            // catch the ppu up to the cpu before reading
            self.ppu.catch_up(cpu);

            // ignore all but low 3 bits
            addr &= 0b111;
            return self.ppu.read_register_by_index(addr as u8);
        }

        // address lines a13-a15 = 011 (0x6000-0x7fff) => prg ram
        if (addr >> 13) == 3 {
            // mask off bit 12 for mirroring
            addr &= !0b1000000000000;
            // subtract offset of 0x1800 to get index in range 0x4800-0x57ff
            addr -= 0x1800;
            return unsafe { *self.memory.get(addr as usize).unwrap() };
        }

        // addres line a15 = 1 (0x8000-0xffff) => prg rom
        if (addr & 0x8000) != 0 {
            // mask off bit 14 for mirroring
            addr &= !0b100000000000000;
            // subtract offset
            addr -= 0x7800;
            return unsafe { *self.memory.get(addr as usize).unwrap() };
        }

        // TODO: special apu/io stuff in the 0x4000-0x4017 range. the
        // other addresses (up to 0x5fff) should probably just be ignored

        0
    }

    fn write(&mut self, addr: u16, val: u8, cpu: &mut cpu::Cpu) {
        // NOTE: see 'calc_cpu_read_addr()' for comments explaining address calculation
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
            // catch the ppu up to the cpu before writing
            self.ppu.catch_up(cpu);
            self.ppu
                .write_register_by_index(addr as u8 & 0b111, val, cpu);

            return;
        }

        if (addr >> 13) == 3 {
            unsafe {
                *self
                    .memory
                    .get_mut(((addr & !0b1000000000000) - 0x1800) as usize)
                    .unwrap() = val;
            }
            return;
        }

        // ppu oamdma register
        if addr == 0x4014 {
            self.ppu.write_oamdma(val, cpu);

            return;
        }

        // TODO: apu/io stuff. note that when this is added, it may also be
        // necessary to explicitly ignore attempts to write to rom
    }
}

impl PpuMemoryMap for Nrom128PpuMemory {
    // NOTE: passing addresses higher than 0x3fff will read from palette ram
    fn read(&self, mut addr: u16) -> u8 {
        if cfg!(not(test)) {
            debug_assert!(addr <= 0x3fff);
        }

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

        // address is in the range 0-0x1fff (chr ram)
        unsafe { *self.chr_ram.get_unchecked(addr as usize) }
    }

    // NOTE: passing addresses higher than 0x3fff will write to palette ram
    fn write(&mut self, mut addr: u16, val: u8) {
        if cfg!(not(test)) {
            debug_assert!(addr <= 0x3fff);
        }

        if addr >= 0x3f00 {
            addr &= 0b11111;
            unsafe { *self.palettes.get_unchecked_mut(addr as usize) = val };
            return;
        }

        if addr >= 0x2000 {
            addr &= !0x3000;
            addr &= self.nametable_mirroring_mask;
            unsafe { *self.nametables.get_unchecked_mut(addr as usize) = val };
            return;
        }

        unsafe { *self.chr_ram.get_unchecked_mut(addr as usize) = val };
    }

    fn get_pattern_tables(&mut self) -> &mut [u8; 0x2000] {
        &mut self.chr_ram
    }
}

// 'NROM-256' (also ines mapper 0)
pub struct Nrom256CpuMemory {
    // [0..=0x7ff] = internal ram
    // [0x800..=0x87ff] = prg rom
    // [0x8800..=0x97ff] = prg ram
    // TODO: 0x2000 * chr rom/possibly ram
    memory: [u8; 0x9800],
}

pub struct Nrom256PpuMemory {}

impl Nrom256CpuMemory {
    pub fn new() -> Self {
        Nrom256CpuMemory {
            memory: [0; 0x9800],
        }
    }

    pub fn load_prg_rom(&mut self, rom: &[u8]) {
        assert!(rom.len() == 0x8000);
        self.memory[0x800..=0x87ff].copy_from_slice(rom);
    }
}

impl CpuMemoryMap for Nrom256CpuMemory {
    fn read(&mut self, mut addr: u16, cpu: &mut cpu::Cpu) -> u8 {
        if (addr >> 13) == 0 {
            addr &= !0b1100000000000;
            return unsafe { *self.memory.get(addr as usize).unwrap() };
        }

        if (addr >> 13) == 1 {
            addr &= 0b111;
            // return ctx.ppu.read_register_by_index(addr as u8, self);
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

    fn write(&mut self, addr: u16, val: u8, cpu: &mut cpu::Cpu) {
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
    }
}

impl PpuMemoryMap for Nrom256PpuMemory {
    fn read(&self, addr: u16) -> u8 {
        unimplemented!()
    }

    fn write(&mut self, addr: u16, val: u8) {
        unimplemented!()
    }

    fn get_pattern_tables(&mut self) -> &mut [u8; 0x2000] {
        unimplemented!()
    }
}

#[test]
fn test_calc_addr_128() {
    fn calc_cpu_read_addr(addr: u16) -> u16 {
        if (addr >> 13) == 0 {
            return addr & !0b1100000000000;
        }
        //
        if (addr >> 13) == 1 {
            return (addr & 0b111) | 0x5800;
        }
        //
        if (addr >> 13) == 3 {
            return (addr & !0b1000000000000) - 0x1800;
        }
        //
        if (addr & 0x8000) != 0 {
            return (addr & !0b100000000000000) - 0x7800;
        }
        //
        0
    }

    let mut win = win::XcbWindowWrapper::new("mynes", 1200, 600).unwrap();
    let renderer = PixelRenderer::new(&mut win.connection, win.win, 256, 240).unwrap();

    let ref mut ppu_memory = Nrom128PpuMemory::new();
    let ppu = ppu::Ppu::new(renderer, ppu_memory);
    let apu = apu::Apu {};
    let ref mut cpu = cpu::Cpu::default();
    let mut cpu_memory = Nrom128CpuMemory::new(ppu, apu);
    //
    // internal ram reads
    assert_eq!(calc_cpu_read_addr(0xa0e), 0x20e);
    assert_eq!(calc_cpu_read_addr(0x1000), 0);
    assert_eq!(calc_cpu_read_addr(0x18f0), 0xf0);
    //
    // 'unmapped' area reads, should return 0
    assert_eq!(calc_cpu_read_addr(0x48f0), 0);
    assert_eq!(calc_cpu_read_addr(0x5000), 0);
    //
    // prg ram writes
    cpu_memory.write(0x7fffu16, 0xfe, cpu);
    cpu_memory.write(0x6000u16, 0xce, cpu);
    assert_eq!(cpu_memory.memory[0x57ff], 0xfe);
    assert_eq!(cpu_memory.memory[0x4800], 0xce);
    //
    // special io stuff, should just return 0
    assert_eq!(calc_cpu_read_addr(0x401f), 0);
    //
    // prg rom reads
    assert_eq!(calc_cpu_read_addr(0x8000), 0x800);
    assert_eq!(calc_cpu_read_addr(0xffff), 0x47ff);
    //
    // prg rom writes
    cpu_memory.write(0xcfffu16, 0xff, cpu);
    // should not take effect
    assert_ne!(cpu_memory.read(0xcfffu16, cpu), 0xff);
}

#[test]
fn test_calc_addr_256() {
    // TODO: ..
}

#[test]
fn test_ppu_calc_addr_128() {
    let mut memory = Nrom128PpuMemory::new();

    // set mirroring mask to vertical
    memory.nametable_mirroring_mask = !0x800;
    // test nametable writes
    memory.write(0x2fff, 0xee);
    assert_eq!(memory.nametables[0x27ff - 0x2000], 0xee);
    memory.write(0x2bff, 0xcc);
    assert_eq!(memory.nametables[0x23ff - 0x2000], 0xcc);

    // palette ram reads/writes
    memory.write(0x3f20, 0x30);
    assert_eq!(memory.palettes[0], 0x30);
    // write out of bounds
    memory.write(0xffff, 0x20);
    // should end up in palette ram
    assert_eq!(memory.palettes[31], 0x20);
}
