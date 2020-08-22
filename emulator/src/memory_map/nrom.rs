use super::super::{apu, cpu, ppu, win, PixelRenderer};
use super::{CpuMemoryMap, PpuMemoryMap};

// the cpu memory map for games that use the 'NROM-128' cartridge/mapper (ines mapper 0)
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

// the ppu memory map for both 'NROM-128' and 'NROM-256'
pub struct NromPpuMemory {
    pub chr_ram: [u8; 0x2000],
    pub nametables: [u8; 0x800],
    pub palettes: [u8; 32],
    pub hor_mirroring: bool,
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

impl NromPpuMemory {
    pub fn new() -> Self {
        Self {
            chr_ram: [0; 0x2000],
            nametables: [0; 0x800],
            palettes: [0; 32],
            hor_mirroring: false,
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
            // FIXME: catch the ppu up to the cpu before reading?
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
            // FIXME: catch the ppu up to the cpu before writing?
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

impl PpuMemoryMap for NromPpuMemory {
    // NOTE: passing addresses higher than 0x3fff will read from palette ram
    fn read(&self, mut addr: u16) -> u8 {
        if cfg!(not(test)) {
            debug_assert!(addr <= 0x3fff);
        }

        // if address points to palette indices
        if addr >= 0x3f00 {
            // ignore all but lowest 5 bits (32 palettes)
            addr &= 0b11111;

            // ensure 0x10, 0x14, 0x18, 0x1c are mirrored down
            if matches!(addr, 0x10 | 0x14 | 0x18 | 0x1c) {
                addr &= 0xf;
            }

            return unsafe { *self.palettes.get_unchecked(addr as usize) };
        }

        // if address is in the range 0x2000-0x3eff
        if addr >= 0x2000 {
            // mirror down to 0-0xfff
            addr &= !0x3000;

            // apply horizontal nametable mirroring
            if self.hor_mirroring {
                let high_bits = addr & 0b110000000000;
                addr -= (high_bits.count_ones() as u16) << 10;
            }

            addr &= 0x7ff;

            // return unsafe { *self.nametables.get_unchecked(addr as usize) };
            return self.nametables[addr as usize];
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

            if matches!(addr, 0x10 | 0x14 | 0x18 | 0x1c) {
                addr &= 0xf;
            }

            unsafe { *self.palettes.get_unchecked_mut(addr as usize) = val };
            return;
        }

        if addr >= 0x2000 {
            addr &= !0x3000;

            if self.hor_mirroring {
                let high_bits = addr & 0b110000000000;
                addr -= (high_bits.count_ones() as u16) << 10;
            }

            addr &= 0x7ff;

            // unsafe { *self.nametables.get_unchecked_mut(addr as usize) = val };
            self.nametables[addr as usize] = val;
            return;
        }

        unsafe { *self.chr_ram.get_unchecked_mut(addr as usize) = val };
    }

    fn get_pattern_tables(&mut self) -> &mut [u8; 0x2000] {
        &mut self.chr_ram
    }
}

// 'NROM-256' (also ines mapper 0)
pub struct Nrom256CpuMemory<'a> {
    pub ppu: ppu::Ppu<'a>,
    pub apu: apu::Apu,
    // [0..=0x7ff] = internal ram
    // [0x800..=0x87ff] = prg rom
    // [0x8800..=0x97ff] = prg ram
    memory: [u8; 0x9800],
}

impl<'a> Nrom256CpuMemory<'a> {
    pub fn new(ppu: ppu::Ppu<'a>, apu: apu::Apu) -> Self {
        Self {
            ppu,
            apu,
            memory: [0; 0x9800],
        }
    }

    pub fn load_prg_rom(&mut self, rom: &[u8]) {
        assert!(rom.len() == 0x8000);
        self.memory[0x800..=0x87ff].copy_from_slice(rom);
    }
}

impl<'a> CpuMemoryMap for Nrom256CpuMemory<'a> {
    fn read(&mut self, mut addr: u16, cpu: &mut cpu::Cpu) -> u8 {
        if (addr >> 13) == 0 {
            addr &= !0b1100000000000;
            return unsafe { *self.memory.get(addr as usize).unwrap() };
        }

        if (addr >> 13) == 1 {
            // FIXME: catch up ppu to cpu?
            addr &= 0b111;
            return self.ppu.read_register_by_index(addr as u8);
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
            // FIXME: catch up ppu to cpu?
            self.ppu
                .write_register_by_index(addr as u8 & 0b111, val, cpu);

            return;
        }

        if (addr >> 13) == 3 {
            unsafe {
                *self
                    .memory
                    .get_mut(((addr & !0b1000000000000) + 0x2800) as usize)
                    .unwrap() = val;
            }
            return;
        }

        if addr == 0x4014 {
            self.ppu.write_oamdma(val, cpu);

            return;
        }
    }
}

mod test {
    use super::*;

    fn init_ppu_apu_cpu(ppu_memory: &mut dyn PpuMemoryMap) -> (ppu::Ppu, apu::Apu, cpu::Cpu) {
        let mut win = win::XcbWindowWrapper::new("test", 20, 20).unwrap();
        let renderer = PixelRenderer::new(&mut win.connection, win.win, 256, 240).unwrap();

        let ppu = ppu::Ppu::new(renderer, ppu_memory);
        let apu = apu::Apu {};
        let cpu = cpu::Cpu::default();

        (ppu, apu, cpu)
    }

    #[test]
    fn test_cpu_calc_addr_128() {
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

        let ref mut ppu_memory = NromPpuMemory::new();
        let (ppu, apu, mut cpu) = init_ppu_apu_cpu(ppu_memory);
        let mut cpu_memory = Nrom128CpuMemory::new(ppu, apu);

        // internal ram reads
        assert_eq!(calc_cpu_read_addr(0xa0e), 0x20e);
        assert_eq!(calc_cpu_read_addr(0x1000), 0);
        assert_eq!(calc_cpu_read_addr(0x18f0), 0xf0);

        // 'unmapped' area reads, should return 0
        assert_eq!(calc_cpu_read_addr(0x48f0), 0);
        assert_eq!(calc_cpu_read_addr(0x5000), 0);

        // prg ram writes
        cpu_memory.write(0x7fffu16, 0xfe, &mut cpu);
        cpu_memory.write(0x6000u16, 0xce, &mut cpu);
        assert_eq!(cpu_memory.memory[0x57ff], 0xfe);
        assert_eq!(cpu_memory.memory[0x4800], 0xce);

        // special io stuff, should just return 0
        assert_eq!(calc_cpu_read_addr(0x401f), 0);

        // prg rom reads
        assert_eq!(calc_cpu_read_addr(0x8000), 0x800);
        assert_eq!(calc_cpu_read_addr(0xffff), 0x47ff);

        // prg rom writes
        cpu_memory.write(0xcfffu16, 0xff, &mut cpu);
        // should not take effect
        assert_ne!(cpu_memory.read(0xcfffu16, &mut cpu), 0xff);
    }
    #[test]
    // TODO: when more mappers are added, make mapper-agnostic test functions
    fn test_cpu_calc_addr_256() {
        fn calc_cpu_read_addr(addr: u16) -> u16 {
            if (addr >> 13) == 0 {
                return addr & !0b1100000000000;
            }

            if (addr >> 13) == 1 {
                // ppu registers
                return addr & 0b111;
            }

            if (addr >> 13) == 3 {
                return (addr & !0b1000000000000) - 0x2800;
            }

            if (addr & 0x8000) != 0 {
                return addr - 0x7800;
            }

            0
        }

        let ref mut ppu_memory = NromPpuMemory::new();
        let (ppu, apu, mut cpu) = init_ppu_apu_cpu(ppu_memory);
        let mut cpu_memory = Nrom256CpuMemory::new(ppu, apu);

        // internal ram reads
        assert_eq!(calc_cpu_read_addr(0xa0e), 0x20e);
        assert_eq!(calc_cpu_read_addr(0x1000), 0);
        assert_eq!(calc_cpu_read_addr(0x18f0), 0xf0);

        // prg rom reads
        assert_eq!(calc_cpu_read_addr(0x8000), 0x800);
        assert_eq!(calc_cpu_read_addr(0xffff), 0x87ff);
        assert_eq!(calc_cpu_read_addr(0xc000), 0x4800);

        // 'unmapped' area reads, should return 0
        assert_eq!(calc_cpu_read_addr(0x48f0), 0);
        assert_eq!(calc_cpu_read_addr(0x5000), 0);

        // prg ram writes
        cpu_memory.write(0x7fff, 0xfe, &mut cpu);
        cpu_memory.write(0x6000, 0xce, &mut cpu);
        assert_eq!(cpu_memory.memory[0x97ff], 0xfe);
        assert_eq!(cpu_memory.memory[0x8800], 0xce);

        // prg rom writes
        cpu_memory.write(0xcfff, 0xff, &mut cpu);
        // should not take effect
        assert_ne!(cpu_memory.read(0xcfff, &mut cpu), 0xff);
    }

    #[test]
    fn test_ppu_calc_addr() {
        let mut memory = NromPpuMemory::new();

        // set mirroring = vertical
        memory.hor_mirroring = false;
        // test nametable writes
        memory.write(0x2fff, 0xee);
        assert_eq!(memory.nametables[0x7ff], 0xee);
        memory.write(0x2bff, 0xcc);
        assert_eq!(memory.nametables[0x3ff], 0xcc);
        memory.write(0x2800, 0x66);
        assert_eq!(memory.nametables[0], 0x66);
        memory.write(0x28ff, 0x99);
        assert_eq!(memory.nametables[0x0ff], 0x99);

        for i in memory.nametables.iter_mut() {
            *i = 0;
        }

        // set mirroring = horizontal
        memory.hor_mirroring = true;
        // test nametable writes
        memory.write(0x2fff, 0xee);
        assert_eq!(memory.nametables[0x7ff], 0xee);
        assert_eq!(memory.read(0x2bff), 0xee);
        memory.write(0x2bff, 0xdd);
        assert_eq!(memory.nametables[0x7ff], 0xdd);
        assert_eq!(memory.read(0x2fff), 0xdd);

        memory.write(0x2cf0, 0xaa);
        assert_eq!(memory.nametables[0x4f0], 0xaa);
        assert_eq!(memory.read(0x28f0), 0xaa);
        memory.write(0x28f0, 0x88);
        assert_eq!(memory.nametables[0x4f0], 0x88);
        assert_eq!(memory.read(0x2cf0), 0x88);

        for addr in 0x2800..=0x2bff {
            memory.write(addr as u16, 0xaa);
            assert_eq!(memory.nametables[addr - 0x2400], 0xaa);
            assert_eq!(memory.read(addr as u16), 0xaa);
            assert_eq!(memory.read(addr as u16 + 0x400), 0xaa);
            memory.write(addr as u16, 0);
        }

        for addr in 0x2c00..=0x2fff {
            memory.write(addr as u16, 0xaa);
            assert_eq!(memory.nametables[addr - 0x2800], 0xaa);
            assert_eq!(memory.read(addr as u16), 0xaa);
            assert_eq!(memory.read(addr as u16 - 0x400), 0xaa);
            memory.write(addr as u16, 0);
        }

        for addr in 0x2000..=0x23ff {
            memory.write(addr as u16, 0xaa);
            assert_eq!(memory.nametables[addr - 0x2000], 0xaa);
            assert_eq!(memory.read(addr as u16), 0xaa);
            assert_eq!(memory.read(addr as u16 + 0x400), 0xaa);
            memory.write(addr as u16, 0);
        }

        for addr in 0x2400..=0x27ff {
            memory.write(addr as u16, 0xaa);
            assert_eq!(memory.nametables[addr - 0x2400], 0xaa);
            assert_eq!(memory.read(addr as u16), 0xaa);
            assert_eq!(memory.read(addr as u16 - 0x400), 0xaa);
            memory.write(addr as u16, 0);
        }

        memory.write(0x2400, 0xcc);
        assert_eq!(memory.nametables[0], 0xcc);
        assert_eq!(memory.read(0x2000), 0xcc);
        memory.write(0x2000, 0x44);
        assert_eq!(memory.nametables[0], 0x44);
        assert_eq!(memory.read(0x2400), 0x44);

        memory.write(0x27ff, 0x11);
        assert_eq!(memory.nametables[0x3ff], 0x11);
        assert_eq!(memory.read(0x23ff), 0x11);
        memory.write(0x23ff, 0xff);
        assert_eq!(memory.nametables[0x3ff], 0xff);
        assert_eq!(memory.read(0x27ff), 0xff);

        // palette ram reads/writes
        memory.write(0x3f20, 0x30);
        assert_eq!(memory.palettes[0], 0x30);
        // write out of bounds
        memory.write(0xffff, 0x20);
        // should end up in palette ram
        assert_eq!(memory.palettes[31], 0x20);
    }
}
