use super::super::{apu, controller as ctrl, cpu, ppu, util, win, PixelRenderer};
use super::{CpuMemoryMap, CpuMemoryMapBase, PpuMemoryMap};

// the cpu memory map for games that use the 'NROM-128' cartridge/mapper (ines mapper 0)
pub struct NromCpuMemory {
    pub base: CpuMemoryMapBase,
    pub ppu_memory: NromPpuMemory,
    internal_ram: [u8; 0x800],
    prg_rom: Box<[u8]>,
    prg_ram: [u8; 0x1000],
}

// the ppu memory map for both 'NROM-128' and 'NROM-256'
pub struct NromPpuMemory {
    pub chr_ram: [u8; 0x2000],
    pub nametables: [u8; 0x800],
    pub palettes: [u8; 32],
    pub hor_mirroring: bool,
}

impl NromCpuMemory {
    // TODO: reduce unnecessary copying
    pub fn new(
        prg_rom: &[u8],
        ppu: ppu::Ppu,
        ppu_memory: NromPpuMemory,
        apu: apu::Apu,
        controller: ctrl::Controller,
        renderer: PixelRenderer,
    ) -> Self {
        // nrom-128 or nrom-256
        assert!(matches!(prg_rom.len(), 0x4000 | 0x8000));

        Self {
            internal_ram: [0; 0x800],
            prg_ram: [0; 0x1000],
            prg_rom: prg_rom.to_vec().into_boxed_slice(),
            ppu_memory,
            base: CpuMemoryMapBase::new(ppu, apu, controller, renderer),
        }
    }

    pub fn new_empty(
        prg_rom_size: u16,
        ppu: ppu::Ppu,
        ppu_memory: NromPpuMemory,
        apu: apu::Apu,
        controller: ctrl::Controller,
        renderer: PixelRenderer,
    ) -> Self {
        assert!(matches!(prg_rom_size, 0x4000 | 0x8000));

        Self {
            internal_ram: [0; 0x800],
            prg_ram: [0; 0x1000],
            prg_rom: vec![0; prg_rom_size as usize].into_boxed_slice(),
            ppu_memory,
            base: CpuMemoryMapBase::new(ppu, apu, controller, renderer),
        }
    }
}

impl NromPpuMemory {
    pub fn new(hor_mirroring: bool) -> Self {
        Self {
            chr_ram: [0; 0x2000],
            nametables: [0; 0x800],
            palettes: [0; 32],
            hor_mirroring,
        }
    }

    pub fn load_chr_ram(&mut self, ram: &[u8]) {
        assert_eq!(ram.len(), 0x2000);
        self.chr_ram.copy_from_slice(ram);
    }
}

impl CpuMemoryMap for NromCpuMemory {
    fn read(&mut self, mut addr: u16, cpu: &mut cpu::Cpu) -> u8 {
        // internal ram
        if super::is_0_to_1fff(addr) {
            // mask off bit 11 and 12 for mirroring
            addr &= !0b1100000000000;
            // TODO: revert to unchecked indexing
            return unsafe { *self.internal_ram.get(addr as usize).unwrap() };
        }

        // ppu registers
        if super::is_2000_to_3fff(addr) {
            // catch ppu up to cpu before reading
            let framebuffer = util::pixels_to_u32(&mut self.base.renderer);
            self.base
                .ppu
                .catch_up(cpu, &mut self.ppu_memory, framebuffer);
            // ignore all but low 3 bits
            addr &= 0b111;
            return self
                .base
                .ppu
                .read_register_by_index(addr as u8, &self.ppu_memory);
        }

        // prg ram
        if super::is_6000_to_7fff(addr) {
            // mask off bits 11-14 for mirroring
            addr &= !0b111_1000_0000_0000;
            return unsafe { *self.prg_ram.get(addr as usize).unwrap() };
        }

        // addres line a15 = 1 (0x8000-0xffff) => prg rom
        if (addr & 0x8000) != 0 {
            // calculate mirroring mask to apply (depends on whether prg rom.len()
            // is 0x8000 or 0x4000)
            // NOTE: 'prg_rom.len()' is always either 0x4000 or 0x8000 (nrom-128 or nrom-256)
            let mirroring_mask = (self.prg_rom.len() as u16) | 0b1000_0000_0000_0000;

            addr &= !mirroring_mask;
            return unsafe { *self.prg_rom.get(addr as usize).unwrap() };
        }

        if addr == 0x4016 {
            return self.base.controller.read();
        }

        // TODO: special apu/io stuff in the 0x4000-0x4017 range. the
        // other addresses (up to 0x5fff) should probably just be ignored

        0
    }

    fn write(&mut self, addr: u16, val: u8, cpu: &mut cpu::Cpu) {
        if super::is_0_to_1fff(addr) {
            unsafe {
                *self
                    .internal_ram
                    .get_mut((addr & !0b1100000000000) as usize)
                    .unwrap() = val;
            }
            return;
        }

        if super::is_2000_to_3fff(addr) {
            // catch ppu up to cpu before writing
            let framebuffer = util::pixels_to_u32(&mut self.base.renderer);
            self.base
                .ppu
                .catch_up(cpu, &mut self.ppu_memory, framebuffer);

            self.base.ppu.write_register_by_index(
                addr as u8 & 0b111,
                val,
                cpu,
                &mut self.ppu_memory,
            );

            return;
        }

        if super::is_6000_to_7fff(addr) {
            unsafe {
                *self
                    .prg_ram
                    .get_mut((addr & !0b111_1000_0000_0000) as usize)
                    .unwrap() = val;
            }
            return;
        }

        // ppu oamdma register
        if addr == 0x4014 {
            let framebuffer = util::pixels_to_u32(&mut self.base.renderer);
            self.base
                .ppu
                .catch_up(cpu, &mut self.ppu_memory, framebuffer);
            super::write_oamdma(self, val, cpu);
            return;
        }

        if addr == 0x4016 {
            self.base.controller.write(val);
        }

        // TODO: apu/io stuff. note that when this is added, it may also be
        // necessary to explicitly ignore attempts to write to rom
    }

    fn base(&mut self) -> (&mut CpuMemoryMapBase, &mut dyn PpuMemoryMap) {
        (&mut self.base, &mut self.ppu_memory)
    }
}

impl PpuMemoryMap for NromPpuMemory {
    // NOTE: passing addresses higher than 0x3fff will read from palette ram
    fn read(&self, addr: u16, _: i32) -> u8 {
        if cfg!(not(test)) {
            debug_assert!(addr <= 0x3fff);
        }

        // if address points to palette indices
        if addr >= 0x3f00 {
            let addr = super::calc_ppu_palette_addr(addr);
            return unsafe { *self.palettes.get_unchecked(addr as usize) };
        }

        // if address is in the range 0x2000-0x3eff
        if addr >= 0x2000 {
            let addr = super::calc_ppu_nametable_addr_with_mirroring(addr, self.hor_mirroring);
            return unsafe { *self.nametables.get_unchecked(addr as usize) };
        }

        // address is in the range 0-0x1fff (chr ram)
        unsafe { *self.chr_ram.get_unchecked(addr as usize) }
    }

    // NOTE: passing addresses higher than 0x3fff will write to palette ram
    fn write(&mut self, addr: u16, val: u8, _: i32) {
        if cfg!(not(test)) {
            debug_assert!(addr <= 0x3fff);
        }

        if addr >= 0x3f00 {
            let addr = super::calc_ppu_palette_addr(addr);
            unsafe { *self.palettes.get_unchecked_mut(addr as usize) = val };
            return;
        }

        if addr >= 0x2000 {
            let addr = super::calc_ppu_nametable_addr_with_mirroring(addr, self.hor_mirroring);
            unsafe { *self.nametables.get_mut(addr as usize).unwrap() = val };
            return;
        }

        unsafe { *self.chr_ram.get_unchecked_mut(addr as usize) = val };
    }

    fn read_palette_memory(&self, color_idx: u8) -> u8 {
        self.palettes[super::calc_ppu_palette_addr(color_idx as u16) as usize]
    }
}

mod test {
    use super::*;

    #[test]
    fn test_cpu_read_write() {
        let (mut cpu, mut cpu_memory) = util::init_nes();

        // nrom-128
        {
            // internal ram reads/writes
            cpu_memory.write(0x7ff, 0xaa, &mut cpu);
            assert_eq!(cpu_memory.read(0x7ff, &mut cpu), 0xaa);
            assert_eq!(cpu_memory.read(0xfff, &mut cpu), 0xaa);
            assert_eq!(cpu_memory.read(0x17ff, &mut cpu), 0xaa);
            assert_eq!(cpu_memory.read(0x1fff, &mut cpu), 0xaa);

            cpu_memory.write(0, 0xbb, &mut cpu);
            assert_eq!(cpu_memory.read(0, &mut cpu), 0xbb);
            assert_eq!(cpu_memory.read(0x800, &mut cpu), 0xbb);
            assert_eq!(cpu_memory.read(0x1000, &mut cpu), 0xbb);
            assert_eq!(cpu_memory.read(0x1800, &mut cpu), 0xbb);

            // 'unmapped' area reads, should return 0
            assert_eq!(cpu_memory.read(0x48f0, &mut cpu), 0);
            assert_eq!(cpu_memory.read(0x5000, &mut cpu), 0);

            // prg ram writes
            cpu_memory.write(0x7fffu16, 0xfe, &mut cpu);
            cpu_memory.write(0x6000u16, 0xce, &mut cpu);
            assert_eq!(cpu_memory.prg_ram[0x7ff], 0xfe);
            assert_eq!(cpu_memory.read(0x6fff, &mut cpu), 0xfe);
            assert_eq!(cpu_memory.prg_ram[0], 0xce);
            assert_eq!(cpu_memory.read(0x7000, &mut cpu), 0xce);

            // special io stuff, should just return 0
            assert_eq!(cpu_memory.read(0x401f, &mut cpu), 0);

            // prg rom reads (prg rom should be mirrored twice)
            cpu_memory.prg_rom[0x3fff] = 0xcc;
            assert_eq!(cpu_memory.read(0xbfff, &mut cpu), 0xcc);
            assert_eq!(cpu_memory.read(0xffff, &mut cpu), 0xcc);

            cpu_memory.prg_rom[0] = 0xdd;
            assert_eq!(cpu_memory.read(0x8000, &mut cpu), 0xdd);
            assert_eq!(cpu_memory.read(0xc000, &mut cpu), 0xdd);

            // prg rom writes
            cpu_memory.write(0xcfff, 0xff, &mut cpu);
            // should not take effect
            assert_ne!(cpu_memory.read(0xcfff, &mut cpu), 0xff);
        }

        // nrom-256
        {
            // set prg rom length = 0x8000 (nrom-256)
            cpu_memory.prg_rom = vec![0; 0x8000].into_boxed_slice();

            // prg rom reads (prg rom should not be mirrored)
            cpu_memory.prg_rom[0x3fff] = 0xcc;
            assert_eq!(cpu_memory.read(0xbfff, &mut cpu), 0xcc);
            assert_ne!(cpu_memory.read(0xffff, &mut cpu), 0xcc);

            cpu_memory.prg_rom[0] = 0xdd;
            assert_eq!(cpu_memory.read(0x8000, &mut cpu), 0xdd);
            assert_ne!(cpu_memory.read(0xc000, &mut cpu), 0xdd);

            cpu_memory.prg_rom[0x4000] = 0xee;
            assert_eq!(cpu_memory.read(0xc000, &mut cpu), 0xee);

            cpu_memory.prg_rom[0x7fff] = 0xff;
            assert_eq!(cpu_memory.read(0xffff, &mut cpu), 0xff);

            // prg rom writes
            cpu_memory.write(0xcfff, 0xff, &mut cpu);
            // should not take effect
            assert_ne!(cpu_memory.read(0xcfff, &mut cpu), 0xff);
        }
    }

    #[test]
    fn test_ppu_read_write() {
        let mut memory = NromPpuMemory::new(false);

        // test nametable writes (with mirroring = horizontal)
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
