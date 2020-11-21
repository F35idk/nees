use crate::{apu, controller as ctrl, cpu, parse, ppu, win};
#[macro_use]
use crate::util;
use super::{CpuAddressBus, CpuAddressBusBase, PpuAddressBus};
use crate::PixelRenderer;

// NOTE: ines header tells whether need care abt. nametable mirroring selection
// NOTE: currently only care about mapper 004
//

// notes about the current implementation
// NOTE: open bus behavior ignored
// NOTE: mmc6 compatibility ignored

pub struct Mmc3CpuAddressBus<'a> {
    pub base: CpuAddressBusBase<'a>,
    pub ppu_bus: Mmc3PpuAddressBus,
    internal_ram: [u8; 0x800],
    prg_ram: [u8; 0x2000],
    // up to 64 banks
    prg_banks: Box<[[u8; 0x2000]]>,
    // the bank register to update on the next write to
    // 'bank data'
    // NOTE: due to borrowck issues, the bank registers
    // r0-r7 are located in 'Mmc3PpuAddressBus', instead of
    // in this struct
    bank_register_to_update: u8,
    // misc bool flags
    bits: Mmc3CpuBits::BitField,
}

bitfield!(Mmc3CpuBits<u8>(
    // true means 0x8000-0x9fff is fixed to the second to last bank
    // and 0xc000-0xdfff is switchable, false means the opposite
    prg_banks_swapped: 0..0,
    prg_ram_enable: 1..1,
    prg_ram_protect: 2..2,
));

pub struct Mmc3PpuAddressBus {
    // bank registers r0-r7 (for both prg and chr ram). though it would've
    // made more sense to store this in 'Mmc3CpuAddressBus', putting it in
    // 'Mmc3PpuAddressBus' allows both the ppu and the cpu address buses
    // access to it while keeping the borrow checker happy
    r: [u8; 8],
    // up to 256 banks
    chr_banks: Box<[[u8; 0x400]]>,
    nametables: Box<[u8]>,
    palettes: [u8; 32],
    irq_counter: u8,
    irq_latch: u8,
    cycle_count_at_prev_a12_high: i32,
    bits: Mmc3PpuBits::BitField,
}

bitfield!(Mmc3PpuBits<u8>(
    hor_mirroring: 0..0,
    no_mirroring: 1..1,
    a12_invert: 2..2,
    prev_a12: 3..3,
    irq_reload: 4..4,
    irq_enable: 5..5,
    trigger_irq: 6..6,
));

impl Mmc3PpuAddressBus {
    fn clock_irq_counter(&mut self, a12: bool, cycle_count: i32, cpu: &mut cpu::Cpu) {
        // if current a12 is high and a12 was low on previous read/write (a12 has risen)
        if a12 {
            if !self.bits.prev_a12.is_true() {
                // ignore a12 rise if there was another a12 rise 6 or fewer cycles ago
                // FIXME:::: gt?? gte??
                if (cycle_count - self.cycle_count_at_prev_a12_high) as u32 > 6 {
                    if self.bits.irq_reload.is_true() || self.irq_counter == 0 {
                        // reload counter
                        self.irq_counter = self.irq_latch;
                        self.bits.irq_reload.set(0);
                    } else {
                        // decrement
                        self.irq_counter -= 1;
                    }

                    if self.irq_counter == 0 && self.bits.irq_enable.is_true() {
                        // if not already triggering cpu irq
                        if !self.bits.trigger_irq.is_true() {
                            self.bits.trigger_irq.set(1);
                            // increment 'irq' on cpu
                            cpu.irq += 1;
                        }
                    }
                }
            }

            self.cycle_count_at_prev_a12_high = cycle_count;
        }

        self.bits.prev_a12.set(a12 as u8);
    }
}

impl PpuAddressBus for Mmc3PpuAddressBus {
    fn read(&mut self, mut addr: u16, cycle_count: i32, cpu: &mut cpu::Cpu) -> u8 {
        assert!(addr <= 0x3fff);

        let a12 = (addr & 0b1_0000_0000_0000) != 0;
        self.clock_irq_counter(a12, cycle_count, cpu);

        // palette memory
        if addr >= 0x3f00 {
            let addr = super::calc_ppu_palette_addr(addr);
            return unsafe { *self.palettes.get_unchecked(addr as usize) };
        }

        // nametables (0x2000-0x3eff)
        if addr >= 0x2000 {
            // apply horizontal or vertical mirroring
            if !self.bits.no_mirroring.is_true() {
                addr = super::calc_ppu_nametable_addr_with_mirroring(
                    addr, //
                    self.bits.hor_mirroring.is_true(),
                );
            } else {
                addr &= !0x3000;
            }

            return unsafe { *self.nametables.get(addr as usize).unwrap() };
        }

        // pattern tables (0-0x1fff)

        let a12_invert = self.bits.a12_invert.is_true();
        let n_banks = self.chr_banks.len() as u16;
        assert!(n_banks.is_power_of_two());

        if a12 == a12_invert {
            // if this is reached, addr points to one of the 2kb chr banks

            // calculate bank register index (should point to either r1 or r0)
            let bank_register_idx = ((addr >> 11) & 1) as u8;

            let bank_idx = if addr & 0b0100_0000_0000 != 0 {
                // addr points to upper section of 2kb bank
                assert!(matches!(
                    addr,
                    (0x400..=0x7ff) | (0xc00..=0xfff) | (0x1400..=0x17ff) | (0x1c00..=0x1fff)
                ));

                // - set lowest to bit to make 'bank_idx' reflect this
                (self.r[bank_register_idx as usize] & ((n_banks - 1) as u8)) | 1
            } else {
                // addr points to lower section of 2kb bank
                assert!(matches!(
                    addr,
                    (0..=0x3ff) | (0x800..=0xbff) | (0x1000..=0x13ff) | (0x1800..=0x1bff)
                ));

                // - clear lowest bit
                // NOTE: the value in 'r[idx]' is %'d with the number of banks
                (self.r[bank_register_idx as usize] & ((n_banks - 1) as u8)) & !1
            };

            let bank = &self.chr_banks[bank_idx as usize];
            unsafe { *bank.get_unchecked(addr as usize & (bank.len() - 1)) }
        } else {
            // if this is reached, addr points to any of the 2kb chr banks (r2-r5)
            let bank_register_idx = if a12_invert {
                ((addr >> 10) + 2) as u8
            } else {
                ((addr >> 10) - 2) as u8
            };

            assert!(bank_register_idx >= 2);

            let bank_idx = self.r[bank_register_idx as usize] & ((n_banks - 1) as u8);
            let bank = &self.chr_banks[bank_idx as usize];
            unsafe { *bank.get_unchecked(addr as usize & (bank.len() - 1)) }
        }
    }

    fn write(&mut self, mut addr: u16, val: u8, cycle_count: i32, cpu: &mut cpu::Cpu) {
        let a12 = (addr & 0b1_0000_0000_0000) != 0;
        self.clock_irq_counter(a12, cycle_count, cpu);

        if addr >= 0x3f00 {
            let addr = super::calc_ppu_palette_addr(addr);
            unsafe { *self.palettes.get_unchecked_mut(addr as usize) = val };
            return;
        }

        if addr >= 0x2000 {
            if !self.bits.no_mirroring.is_true() {
                addr = super::calc_ppu_nametable_addr_with_mirroring(
                    addr, //
                    self.bits.hor_mirroring.is_true(),
                );
            }

            unsafe { *self.nametables.get_unchecked_mut((addr & !0x3000) as usize) = val };
        }
    }

    fn set_address(&mut self, addr: u16, cycle_count: i32, cpu: &mut cpu::Cpu) {
        let a12 = (addr & 0b1_0000_0000_0000) != 0;
        self.clock_irq_counter(a12, cycle_count, cpu);
    }

    fn read_palette_memory(&self, color_idx: u8) -> u8 {
        self.palettes[super::calc_ppu_palette_addr(color_idx as u16) as usize]
    }
}

impl<'a> Mmc3CpuAddressBus<'a> {
    pub fn new(
        prg_rom: &[u8],
        chr_rom: &[u8],
        mirroring: parse::MirroringType,
        ppu: ppu::Ppu,
        apu: apu::Apu,
        controller: ctrl::Controller,
        renderer: PixelRenderer<'a>,
    ) -> Self {
        // TODO: proper error handling if given invalid input
        assert!(prg_rom.len() <= 0x80000);
        assert!(prg_rom.len() >= 0x4000);
        assert!(prg_rom.len().is_power_of_two());

        assert!(chr_rom.len() <= 0x40000);
        assert!(chr_rom.len() >= 0x2000);
        assert!(chr_rom.len().is_power_of_two());

        let prg_banks = {
            let slice = prg_rom.to_vec().into_boxed_slice();
            let raw = Box::into_raw(slice) as *mut [u8; 0x2000];
            let n_banks = prg_rom.len() >> 13;

            unsafe {
                // ensure new slice has the proper length
                let slice = std::slice::from_raw_parts_mut(raw, n_banks);
                Box::from_raw(slice as *mut [[u8; 0x2000]])
            }
        };

        let chr_banks = {
            let slice = chr_rom.to_vec().into_boxed_slice();
            let raw = Box::into_raw(slice) as *mut [u8; 0x400];
            let n_banks = chr_rom.len() >> 10;

            unsafe {
                let slice = std::slice::from_raw_parts_mut(raw, n_banks);
                Box::from_raw(slice as *mut [[u8; 0x400]])
            }
        };

        let (n_nametables, hor_mirroring, no_mirroring) = match mirroring {
            parse::MirroringType::Hor => (2, true, false),
            parse::MirroringType::Vert => (2, false, false),
            parse::MirroringType::FourScreen => (4, false, true),
        };

        let nametables = vec![0u8; 0x400 * n_nametables].into_boxed_slice();

        let ppu_bus = Mmc3PpuAddressBus {
            chr_banks,
            nametables,
            palettes: [0; 32],
            r: [0; 8],
            irq_latch: 0,
            irq_counter: 0,
            cycle_count_at_prev_a12_high: 0,
            bits: Mmc3PpuBits::BitField::new(
                hor_mirroring as u8,
                no_mirroring as u8,
                0,
                0,
                0,
                0,
                0,
            ),
        };

        Self {
            base: CpuAddressBusBase::new(ppu, apu, controller, renderer),
            ppu_bus,
            internal_ram: [0; 0x800],
            prg_ram: [0; 0x2000],
            prg_banks,
            bank_register_to_update: 0,
            bits: Mmc3CpuBits::BitField::zeroed(),
        }
    }
}

impl<'a> CpuAddressBus<'a> for Mmc3CpuAddressBus<'a> {
    fn read(&mut self, mut addr: u16, cpu: &mut cpu::Cpu) -> u8 {
        // internal ram
        if super::is_0_to_1fff(addr) {
            addr &= !0b1_1000_0000_0000;
            return unsafe { *self.internal_ram.get(addr as usize).unwrap() };
        }

        // ppu registers
        if super::is_2000_to_3fff(addr) {
            self.base.ppu.catch_up(
                cpu,
                &mut self.ppu_bus,
                util::pixels_to_u32(&mut self.base.renderer),
            );

            addr &= 0b111;
            return self
                .base
                .ppu
                .read_register_by_index(addr as u8, &mut self.ppu_bus, cpu);
        }

        // prg ram
        if super::is_6000_to_7fff(addr) && self.bits.prg_ram_enable.is_true() {
            addr &= !0b110_0000_0000_0000;
            return unsafe { *self.prg_ram.get(addr as usize).unwrap() };
        }

        // FIXME: return open bus when ram is read from but is disabled

        // switchable or fixed bank
        if super::is_8000_to_9fff(addr) {
            let bank = if self.bits.prg_banks_swapped.is_true() {
                // the second to last bank
                &self.prg_banks[self.prg_banks.len() - 2]
            } else {
                let n_banks = self.prg_banks.len() as u8;
                // the bank pointed to by r6 (modulo the number of banks)
                &self.prg_banks[(self.ppu_bus.r[6] & (n_banks - 1)) as usize]
            };

            addr &= !0b1110_0000_0000_0000;
            return unsafe { *bank.get(addr as usize).unwrap() };
        }

        // switchable bank
        if super::is_a000_to_bfff(addr) {
            let n_banks = self.prg_banks.len() as u8;
            let bank = &self.prg_banks[(self.ppu_bus.r[7] & (n_banks - 1)) as usize];
            addr &= !0b1110_0000_0000_0000;
            return unsafe { *bank.get(addr as usize).unwrap() };
        }

        // fixed or switchable bank
        if super::is_c000_to_dfff(addr) {
            let bank = if self.bits.prg_banks_swapped.is_true() {
                let n_banks = self.prg_banks.len() as u8;
                &self.prg_banks[(self.ppu_bus.r[6] & (n_banks - 1)) as usize]
            } else {
                &self.prg_banks[self.prg_banks.len() - 2]
            };

            addr &= !0b1110_0000_0000_0000;
            return unsafe { *bank.get(addr as usize).unwrap() };
        }

        // fixed bank (last)
        if super::is_e000_to_ffff(addr) {
            let n_banks = self.prg_banks.len();
            let bank = &self.prg_banks[n_banks - 1];
            addr &= !0b1110_0000_0000_0000;
            return unsafe { *bank.get(addr as usize).unwrap() };
        }

        if addr == 0x4016 {
            return self.base.controller.read();
        }

        0
    }

    fn write(&mut self, mut addr: u16, val: u8, cpu: &mut cpu::Cpu) {
        if super::is_0_to_1fff(addr) {
            addr &= !0b1_1000_0000_0000;
            unsafe { *self.internal_ram.get_mut(addr as usize).unwrap() = val };
            return;
        }

        if super::is_2000_to_3fff(addr) {
            let framebuffer = util::pixels_to_u32(&mut self.base.renderer);
            self.base.ppu.catch_up(cpu, &mut self.ppu_bus, framebuffer);

            self.base
                .ppu
                .write_register_by_index(addr as u8 & 0b111, val, cpu, &mut self.ppu_bus);

            return;
        }

        if super::is_6000_to_7fff(addr)
            && self.bits.prg_ram_enable.is_true()
            && !self.bits.prg_ram_protect.is_true()
        {
            addr &= !0b110_0000_0000_0000;
            unsafe { *self.prg_ram.get_mut(addr as usize).unwrap() = val };
            return;
        }

        // bank select/data registers
        if super::is_8000_to_9fff(addr) {
            if addr & 1 == 0 {
                self.bank_register_to_update = val & 0b111;
                self.bits.prg_banks_swapped.set((val & 0b100_0000) >> 6);
                self.ppu_bus.bits.a12_invert.set((val & 0b1000_0000) >> 7);
            // TODO: mmc6 stuff
            } else {
                // NOTE: 'val' is not %'d with the number of banks, as
                // this is done when reading from the bank registers
                self.ppu_bus.r[self.bank_register_to_update as usize] = val;
            }

            return;
        }

        // mirroring/prg ram enable and protect
        if super::is_a000_to_bfff(addr) {
            if addr & 1 == 0 {
                self.ppu_bus.bits.hor_mirroring.set(val & 1);
            } else {
                self.bits.prg_ram_protect.set((val & 0b100_0000) >> 6);
                self.bits.prg_ram_enable.set((val & 0b1000_0000) >> 7);
                // TODO: mmc6 stuff
            }

            return;
        }

        // irq latch/reload
        if super::is_c000_to_dfff(addr) {
            if addr & 1 == 0 {
                self.ppu_bus.irq_latch = val;
            } else {
                self.ppu_bus.bits.irq_reload.set(1);
                self.ppu_bus.irq_counter = 0;
            }

            return;
        }

        // irq disable/enable
        if super::is_e000_to_ffff(addr) {
            self.ppu_bus.bits.irq_enable.set(addr as u8 & 1);
            if addr & 1 == 1 {}
            if addr & 1 == 0 {
                // stop triggering irq
                if self.ppu_bus.bits.trigger_irq.is_true() {
                    self.ppu_bus.bits.trigger_irq.set(0);
                    cpu.irq = cpu.irq.saturating_sub(1);
                }
            }

            return;
        }

        // oamdma
        if addr == 0x4014 {
            let framebuffer = util::pixels_to_u32(&mut self.base.renderer);
            self.base.ppu.catch_up(cpu, &mut self.ppu_bus, framebuffer);
            super::write_oamdma(self, val, cpu);
            return;
        }

        // standard controller 1
        if addr == 0x4016 {
            self.base.controller.write(val);
            return;
        }
    }

    fn base(&mut self) -> (&mut CpuAddressBusBase<'a>, &mut dyn PpuAddressBus) {
        (&mut self.base, &mut self.ppu_bus)
    }
}

mod test {
    use super::*;

    #[test]
    fn test_ppu_calc_addr() {
        let mut win = win::XcbWindowWrapper::new("test", 20, 20).unwrap();
        let renderer = PixelRenderer::new(&mut win.connection, win.win, 256, 240).unwrap();

        let ppu = ppu::Ppu::new();
        let apu = apu::Apu {};
        let controller = ctrl::Controller::default();

        let prg_rom = vec![0; 1024 * 128];
        let chr_rom = vec![0; 1024 * 128];

        let mut cpu = cpu::Cpu::default();
        let mut cpu_bus = Mmc3CpuAddressBus::new(
            &prg_rom,
            &chr_rom,
            parse::MirroringType::Hor,
            ppu,
            apu,
            controller,
            renderer,
        );

        assert_eq!(cpu_bus.ppu_bus.chr_banks.len(), 128);
        assert_eq!(cpu_bus.prg_banks.len(), 16);

        // enable prg ram
        cpu_bus.write(0xa001, 0x80, &mut cpu);
        assert!(cpu_bus.bits.prg_ram_enable.is_true());

        // disable prg ram
        cpu_bus.write(0xa001, 0, &mut cpu);
        cpu_bus.write(0x6000, 0xff, &mut cpu);
        assert_ne!(cpu_bus.read(0x6000, &mut cpu), 0xff);

        // r0 reads
        {
            cpu_bus.ppu_bus.chr_banks[21][0x3ff] = 0xaa;

            // select r0 as bank to update on next write
            cpu_bus.write(0x8000, 0, &mut cpu);
            // update r0 to point to bank 20 (the 1 in 21 is &'d away)
            cpu_bus.write(0x8001, 21, &mut cpu);

            // ppu_bus[0x7ff] = 0xaa
            assert_eq!(cpu_bus.ppu_bus.read(0x7ff, 0, &mut cpu), 0xaa);
            // ppu_bus[0x3ff] != 0xaa
            assert_ne!(cpu_bus.ppu_bus.read(0x3ff, 0, &mut cpu), 0xaa);

            // invert a12
            cpu_bus.write(0x8000, 0x80, &mut cpu);
            assert!(cpu_bus.ppu_bus.bits.a12_invert.is_true());

            // ppu_bus[0x17ff] = 0xaa
            assert_eq!(cpu_bus.ppu_bus.read(0x17ff, 0, &mut cpu), 0xaa);
            // ppu_bus[0x13ff] != 0xaa
            assert_ne!(cpu_bus.ppu_bus.read(0x13ff, 0, &mut cpu), 0xaa);
            // ppu_bus[0x7ff] != 0xaa
            assert_ne!(cpu_bus.ppu_bus.read(0x7ff, 0, &mut cpu), 0xaa);
        }

        // r2 reads
        {
            cpu_bus.ppu_bus.chr_banks[0x7f][0xff] = 0xbb;

            // select r2 as bank to update on next write and reset a12
            cpu_bus.write(0x8000, 2, &mut cpu);
            // update r2 to point to bank 0x7f (0xff should be %'d down to 0x7f)
            cpu_bus.write(0x8001, 0xff, &mut cpu);

            // ppu_bus[0x10ff] = 0xbb
            assert_eq!(cpu_bus.ppu_bus.read(0x10ff, 0, &mut cpu), 0xbb);
            assert_ne!(cpu_bus.ppu_bus.read(0x0ff, 0, &mut cpu), 0xbb);

            // invert a12
            cpu_bus.write(0x8000, 0x80, &mut cpu);

            // ppu_bus[0x0ff] = 0xbb
            assert_eq!(cpu_bus.ppu_bus.read(0x0ff, 0, &mut cpu), 0xbb);
        }

        // fixed prg bank (last bank) reads/writes
        {
            let n_banks = cpu_bus.prg_banks.len();
            cpu_bus.prg_banks[n_banks - 1][0x30] = 0xcc;

            // ppu_bus[0xe030] = 0cc
            assert_eq!(cpu_bus.read(0xe030, &mut cpu), 0xcc);

            // change prg rom bank mode
            cpu_bus.write(0x8000, 0x40, &mut cpu);
            // .. should not change anything
            assert_eq!(cpu_bus.read(0xe030, &mut cpu), 0xcc);

            // reset prg rom bank mode
            cpu_bus.write(0x8000, 0, &mut cpu);
        }

        // second to last bank reads
        {
            let n_banks = cpu_bus.prg_banks.len();
            cpu_bus.prg_banks[n_banks - 2][0x55] = 0xdd;

            // ppu_bus[0xc055] = 0xdd
            assert_eq!(cpu_bus.read(0xc055, &mut cpu), 0xdd);

            // change prg rom bank mode
            cpu_bus.write(0x8000, 0x40, &mut cpu);

            // ppu_bus[0x8055] = 0xdd
            assert_eq!(cpu_bus.read(0x8055, &mut cpu), 0xdd);

            // reset prg rom bank mode
            cpu_bus.write(0x8000, 0, &mut cpu);
        }

        // r6 reads
        {
            cpu_bus.prg_banks[8][0xff] = 0xee;

            // select r6 as bank to update on next write
            cpu_bus.write(0x8000, 6, &mut cpu);
            // update r6 to point to bank 8
            cpu_bus.write(0x8001, 8, &mut cpu);

            // ppu_bus[0x80ff] = 0xee
            assert_eq!(cpu_bus.read(0x80ff, &mut cpu), 0xee);

            // change prg rom bank mode
            cpu_bus.write(0x8000, 0x40, &mut cpu);

            // ppu_bus[0xc0ff] = 0xee
            assert_eq!(cpu_bus.read(0xc0ff, &mut cpu), 0xee);
        }

        // r3 reads
        {
            cpu_bus.ppu_bus.chr_banks[12][0] = 0xff;

            // select r3 as bank to update on next write and reset a12
            cpu_bus.write(0x8000, 3, &mut cpu);
            // update r3 to point to bank 12
            cpu_bus.write(0x8001, 12, &mut cpu);

            // ppu_bus[0x1400] = 0xff
            assert_eq!(cpu_bus.ppu_bus.read(0x1400, 0, &mut cpu), 0xff);

            // invert a12
            cpu_bus.write(0x8000, 0x80, &mut cpu);

            // ppu_bus[0x0ff] = 0xbb
            assert_eq!(cpu_bus.ppu_bus.read(0x400, 0, &mut cpu), 0xff);
        }

        // r1 reads
        {
            cpu_bus.ppu_bus.chr_banks[33][0x3ff] = 0x99;

            // select r1 as bank to update on next write and reset a12
            cpu_bus.write(0x8000, 1, &mut cpu);
            // update r1 to point to bank 32 (the 1 in 33 is &'d away)
            cpu_bus.write(0x8001, 33, &mut cpu);

            // ppu_bus[0xfff] = 0x99
            assert_eq!(cpu_bus.ppu_bus.read(0xfff, 0, &mut cpu), 0x99);
            // ppu_bus[0xbff] != 0x99
            assert_ne!(cpu_bus.ppu_bus.read(0xbff, 0, &mut cpu), 0x99);

            // invert a12
            cpu_bus.write(0x8000, 0x80, &mut cpu);

            // ppu_bus[0x1fff] = 0x99
            assert_eq!(cpu_bus.ppu_bus.read(0x1fff, 0, &mut cpu), 0x99);
            // ppu_bus[0x1bff] != 0x99
            assert_ne!(cpu_bus.ppu_bus.read(0x1bff, 0, &mut cpu), 0x99);
            // ppu_bus[0xbff] != 0x99
            assert_ne!(cpu_bus.ppu_bus.read(0xfff, 0, &mut cpu), 0x99);
        }
    }

    #[test]
    fn test_calc_chr_bank_register_idx() {
        fn calc_chr_bank_register_idx(addr: u16, a12_invert: bool) -> u8 {
            let a12 = (addr & 0b1_0000_0000_0000) != 0;

            if a12 == a12_invert {
                ((addr >> 11) & 1) as u8
            } else if a12_invert {
                ((addr >> 10) + 2) as u8
            } else {
                ((addr >> 10) - 2) as u8
            }
        }

        assert_eq!(calc_chr_bank_register_idx(0x1fff, true), 1);
        assert_eq!(calc_chr_bank_register_idx(0x1c00, true), 1);
        assert_eq!(calc_chr_bank_register_idx(0x1800, true), 1);
        assert_eq!(calc_chr_bank_register_idx(0x1bff, true), 1);

        assert_eq!(calc_chr_bank_register_idx(0x17ff, true), 0);
        assert_eq!(calc_chr_bank_register_idx(0x1400, true), 0);
        assert_eq!(calc_chr_bank_register_idx(0x13ff, true), 0);
        assert_eq!(calc_chr_bank_register_idx(0x1000, true), 0);

        assert_eq!(calc_chr_bank_register_idx(0x0fff, true), 5);
        assert_eq!(calc_chr_bank_register_idx(0x0c00, true), 5);

        assert_eq!(calc_chr_bank_register_idx(0x0bff, true), 4);
        assert_eq!(calc_chr_bank_register_idx(0x0800, true), 4);

        assert_eq!(calc_chr_bank_register_idx(0x07ff, true), 3);
        assert_eq!(calc_chr_bank_register_idx(0x0400, true), 3);

        assert_eq!(calc_chr_bank_register_idx(0x03ff, true), 2);
        assert_eq!(calc_chr_bank_register_idx(0x0000, true), 2);
    }
}

// prg windows:
//   swappable:
//     0x8000-0x9fff / 0xc000-0xdfff (depending on mode)
//     0xa000-0xbfff
//   fixed:
//     0xc000-0xdfff / 0x8000-0x9fff (depending on mode)
//     0xe000-0xffff

// chr windows (all swappable):
// (? = 1 | 0, a12 may be inverted)
//   0x?000-0x?7FF: 2 KB
//   0x?800-0x?FFF: 2 KB
//   0x?000-0x?3FF: 1 KB
//   0x?400-0x?7FF: 1 KB
//   0x?800-0x?BFF: 1 KB
//   0x?C00-0x?FFF: 1 KB
//
// TODO: overview/plan:
//
// * in 'ppu_bus', track a12 rises (and filter based on whether they
//   happened within 16 dots of each other). store this in 'a12_rise_count'.
//
// * then, when calling 'catch_up()', if 'irq_reload' is false, subtract
//   'a12_rise_count' from 'irq_counter'. if 'irq_reload' is true, set
//   'irq_counter' to zero 'irq_reload' to false.
//
// * if 'irq_reload' was false and 'irq_counter' is now zero, trigger irq.
