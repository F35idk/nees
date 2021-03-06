use crate::address_bus::PpuAddressBus;
use crate::cpu;

#[macro_use]
use derive_serialize::Serialize;

use std::cell::Cell;

mod bg_state;
mod palette;
mod sprite_state;
#[cfg(test)]
mod test;

#[derive(Serialize, Debug)]
pub struct Ppu {
    cycle_count: i32,
    current_scanline: i16,
    current_scanline_dot: u16,
    primary_oam: PrimaryOam,
    secondary_oam: SecondaryOam,
    bg_state: bg_state::BgDrawState,
    sprite_state: sprite_state::SpriteDrawState,
    // registers
    ppuctrl: u8,
    ppumask: u8,
    ppustatus: u8,
    // NOTE: on some ppu chips, there are bugs relating to
    // writing to oamaddr. not sure if these need to be
    // emulated, but it may be worth keeping in mind
    oamaddr: u8,
    ppudata_read_buffer: u8,
    // bitfield w/ misc ppu flags
    bits: PpuBits::BitField,
    // address of the current tile to be fetched and drawn. points
    // to a byte in one of the nametables in vram. referred to
    // as 'v' in the nesdev.com 'ppu scrolling' article
    current_vram_addr: VramAddrRegister,
    // temporary address, same as above but  doesn't get
    // incremented while drawing. this register is shared
    // by 'ppuscroll' and 'ppuaddr' (so writes to these
    // registers go into this). referred to as 't' in the
    // nesdev.com 'ppu scrolling' article
    temp_vram_addr: VramAddrRegister,
}

bitfield!(PpuBits<u8>(
    frame_done: 0..0,
    even_frame: 1..1,
    low_bits_toggle: 2..2,
    suppress_vblank_flag: 3..3,
    fine_x_scroll: 4..6,
));

#[derive(Copy, Clone)]
enum SpriteSize {
    S8x8 = 8,
    S8x16 = 16,
}

#[derive(Serialize, Debug)]
pub struct PrimaryOam {
    pub entries: [OamEntry; 64],
}

#[derive(Serialize, Debug)]
pub struct SecondaryOam {
    pub entries: [OamEntry; 8],
}

#[derive(Serialize, Copy, Clone, Default, Debug)]
#[repr(C)]
pub struct OamEntry {
    pub y: u8,
    pub tile_index: u8,
    pub attributes: u8,
    pub x: u8,
}

// convenience methods for the 'SecondaryOam' and 'PrimaryOam' structs
macro_rules! oam_impl {
    ($oam:ty, $n_entries:literal) => {
        impl $oam {
            pub fn as_bytes<'a>(&'a self) -> &'a [u8; $n_entries * 4] {
                unsafe { std::mem::transmute(self) }
            }

            pub fn as_bytes_mut<'a>(&'a mut self) -> &'a mut [u8; $n_entries * 4] {
                unsafe { std::mem::transmute(self) }
            }

            // NOTE: this function is only used by 'PrimaryOam'
            #[allow(dead_code)]
            pub fn get_byte(&self, index: u8) -> u8 {
                assert_eq!($n_entries, 64);
                // SAFETY: 'index' is a u8 and cannot be larger than
                // the size of oam, making unchecked indexing safe
                unsafe { *self.as_bytes().get_unchecked(index as usize) }
            }

            pub fn set_byte(&mut self, index: u8, val: u8) {
                assert!((index as u16) < $n_entries as u16 * 4);
                unsafe { *self.as_bytes_mut().get_unchecked_mut(index as usize) = val };
            }

            // gets the 'OamEntry' given by 'index' (a byte index into oam)
            // without doing bounds checking. for safety, this requires
            // that 'index' doesn't point past the last 4 bytes of oam.
            // for correctness, 'index' should also be a multiple of 4,
            // so as to fetch the right sprite data from oam.
            // NOTE: this function is only used by 'SecondaryOam'
            #[allow(dead_code)]
            pub unsafe fn get_sprite_unchecked(&self, index: u8) -> OamEntry {
                *(self.as_bytes().get_unchecked(index as usize) as *const _ as *const _)
            }
        }

        impl Default for $oam {
            fn default() -> Self {
                Self {
                    entries: [OamEntry::default(); $n_entries],
                }
            }
        }
    };
}

oam_impl!(PrimaryOam, 64);
oam_impl!(SecondaryOam, 8);

#[derive(Serialize, Copy, Clone, Debug)]
struct VramAddrRegister {
    inner: u16,
}

impl VramAddrRegister {
    fn get_addr(self) -> u16 {
        self.inner & 0x3fff
    }

    fn get_coarse_x(self) -> u8 {
        (self.inner & 0b11111) as u8
    }

    fn set_coarse_x(&mut self, coarse_x: u8) {
        self.inner = (self.inner & !0b11111) | coarse_x as u16;
    }

    fn get_coarse_y(self) -> u8 {
        ((self.inner >> 5) & 0b11111) as u8
    }

    fn set_coarse_y(&mut self, coarse_y: u8) {
        self.inner = (self.inner & !0b1111100000) | ((coarse_y as u16) << 5);
    }

    fn get_fine_y(self) -> u8 {
        ((self.inner & 0b111_00_00000_00000) >> 12) as u8
    }

    fn set_fine_y(&mut self, fine_y: u8) {
        self.inner = (self.inner & !0b111_00_00000_00000) | ((fine_y as u16) << 12);
    }

    fn get_nametable_select(self) -> u8 {
        ((self.inner & 0b11_00000_00000) >> 10) as u8
    }

    fn set_nametable_select(&mut self, select: u8) {
        self.inner = (self.inner & !0b11_00000_00000) | ((select as u16) << 10);
    }
}

impl Ppu {
    pub fn new() -> Self {
        Self {
            bg_state: bg_state::BgDrawState::default(),
            sprite_state: sprite_state::SpriteDrawState::default(),
            secondary_oam: SecondaryOam::default(),
            primary_oam: PrimaryOam::default(),
            ppuctrl: 0,
            ppumask: 0,
            ppustatus: 0,
            oamaddr: 0,
            ppudata_read_buffer: 0,
            current_vram_addr: VramAddrRegister { inner: 0 },
            temp_vram_addr: VramAddrRegister { inner: 0 },
            current_scanline: 240,
            current_scanline_dot: 0,
            bits: PpuBits::BitField::new(0, 1, 0, 0, 0),
            cycle_count: 0,
        }
    }

    #[cfg(test)]
    pub fn reset_state(&mut self) {
        self.secondary_oam = SecondaryOam::default();
        self.primary_oam = PrimaryOam::default();
        self.ppuctrl = 0;
        self.ppumask = 0;
        self.ppustatus = 0;
        self.oamaddr = 0;
        self.ppudata_read_buffer = 0;
        self.current_vram_addr = VramAddrRegister { inner: 0 };
        self.temp_vram_addr = VramAddrRegister { inner: 0 };
        self.current_scanline = 240;
        self.current_scanline_dot = 0;
        self.bits = PpuBits::BitField::new(0, 1, 0, 0, 0);
        self.cycle_count = 0;
    }

    pub fn sub_cycle_count(&mut self, sub: i32) {
        self.cycle_count -= sub;
    }

    // sets the low 5 bits of 'ppustatus' equal to the low 5 bits of 'val'
    pub fn set_ppustatus_low_bits(&mut self, val: u8) {
        self.ppustatus &= !0b11111;
        self.ppustatus |= val & 0b11111;
    }

    pub fn is_frame_done(&self) -> bool {
        self.bits.frame_done.is_true()
    }

    pub fn set_frame_done(&mut self, done: bool) {
        self.bits.frame_done.set(done as u8);
    }

    // NOTE: this is also used by 'write_oamdma()' in 'address_bus'
    pub fn write_to_oam_and_increment_addr(&mut self, val: u8) {
        self.primary_oam.set_byte(self.oamaddr, val);
        self.oamaddr = self.oamaddr.wrapping_add(1);
    }

    // used for reading the registers located in the cpu memory map at 0x2000-0x2007
    pub fn read_register_by_index(
        &mut self,
        index: u8,
        bus: &mut dyn PpuAddressBus,
        cpu: &mut cpu::Cpu,
    ) -> u8 {
        // NOTE: to help readability, this function and a few others on 'Ppu' are
        // split into smaller subfunctions. instead of factoring these subfunctions
        // out into the outer 'Ppu' impl block, i decided to keep them nested, so as
        // to not give the impression that they are needed anywhere else

        {
            return match index {
                // ppuctrl
                0 => 0,
                // ppumask
                1 => self.ppumask,
                // ppustatus
                2 => read_ppustatus(self),
                // oamaddr
                3 => 0,
                // oamdata
                4 => read_oamdata(self),
                // ppuscroll | ppuaddr
                5 | 6 => 0, // FIXME: should these reset the low bits toggle as well??
                // ppudata
                7 => read_ppudata(self, bus, cpu),
                _ => 0,
            };
        }

        fn read_ppustatus(ppu: &mut Ppu) -> u8 {
            // clear low bits toggle
            ppu.bits.low_bits_toggle.set(0);
            let status = ppu.ppustatus;
            // clear vblank flag
            ppu.set_vblank(false);

            if ppu.current_scanline == 241 && ppu.current_scanline_dot == 1 {
                // vblank flag should not have been set yet
                debug_assert!(status & 0b10000000 == 0);
                // if there is one cycle left before the vblank flag will be set
                // (when scanline = 241 and dot = 1, the flag will be set on the
                // next call to 'step()'), prevent the vblank flag from being set
                ppu.bits.suppress_vblank_flag.set(1);
            }

            status
        }

        fn read_oamdata(ppu: &mut Ppu) -> u8 {
            if let (0..=239, 1..=64) = (ppu.current_scanline, ppu.current_scanline_dot) {
                // if on dots/cycles 1-64 of a visible scanline, return 0xff
                return 0xff;
            }

            let mut byte = ppu.primary_oam.get_byte(ppu.oamaddr);
            if ppu.oamaddr % 4 == 2 {
                // if 'byte' is a sprite attribute byte, clear bits 2-4
                byte &= 0b11100011;
            }

            byte
        }

        fn read_ppudata(ppu: &mut Ppu, bus: &mut dyn PpuAddressBus, cpu: &mut cpu::Cpu) -> u8 {
            let val = if (ppu.current_vram_addr.inner >> 8) == 0b111111 {
                // read directly from vram if address is in range
                // 0x3f00-0x3fff (palette ram)
                let val = bus.read(ppu.current_vram_addr.get_addr(), ppu.cycle_count, cpu);
                // store value at mirrored address (down to 0x2f00-0x2fff)
                // in read buffer
                ppu.ppudata_read_buffer = bus.read(
                    ppu.current_vram_addr.get_addr() & !0x1000,
                    ppu.cycle_count,
                    cpu,
                );
                val
            } else {
                // read from read buffer if address is in range 0-0x3eff
                let val = ppu.ppudata_read_buffer;
                ppu.ppudata_read_buffer =
                    bus.read(ppu.current_vram_addr.get_addr(), ppu.cycle_count, cpu);
                val
            };

            if !ppu.is_currently_rendering() {
                // if not currently rendering, increment normally
                ppu.increment_vram_addr();
                // set address on address bus equal to 'current_vram_addr'
                bus.set_address(ppu.current_vram_addr.inner, ppu.cycle_count, cpu);
            } else {
                // if currently rendering, increment the bits of the address
                // corresponding to the y position and coarse x position (this
                // is afaik unintended behavior)
                ppu.increment_vram_addr_coarse_x();
                ppu.increment_vram_addr_y();
            }

            val
        }
    }

    // used for writing to the registers located in the cpu memory map at 0x2000-0x2007
    pub fn write_register_by_index(
        &mut self,
        index: u8,
        val: u8,
        cpu: &mut cpu::Cpu,
        bus: &mut dyn PpuAddressBus,
    ) {
        {
            match index {
                // ppuctrl
                0 => write_ppuctrl(self, val, cpu),
                // ppumask
                1 => write_ppumask(self, val, bus, cpu),
                // ppustatus, ignore attemps to write
                2 => return,
                // oamaddr
                3 => self.oamaddr = val,
                // oamdata
                4 => write_oamdata(self, val),
                // ppuscroll
                5 => write_ppuscroll(self, val),
                // ppuaddr
                6 => write_ppuaddr(self, val, bus, cpu),
                // ppudata
                7 => write_ppudata(self, val, bus, cpu),
                _ => (),
            }
        }

        fn write_ppuctrl(ppu: &mut Ppu, val: u8, cpu: &mut cpu::Cpu) {
            // set bits 10-11 of 'temp_vram_addr' equal to the low 2 bits of 'val'
            ppu.temp_vram_addr.set_nametable_select(val & 0b11);

            // true if nmi_enable bit went from 1 to 0 or 0 to 1
            let nmi_toggled = ((ppu.ppuctrl ^ val) >> 7) != 0;

            ppu.ppuctrl = val;
            ppu.set_ppustatus_low_bits(val);

            if nmi_toggled && ppu.is_vblank_nmi_enabled() && ppu.is_vblank() {
                cpu.bits.nmi.set(1);
                // HACK: delay nmi by one instruction
                cpu.bits.delay_nmi.set(1);
            }
        }

        fn write_ppumask(ppu: &mut Ppu, val: u8, bus: &mut dyn PpuAddressBus, cpu: &mut cpu::Cpu) {
            ppu.ppumask = val;

            if !ppu.is_currently_rendering() {
                bus.set_address(ppu.current_vram_addr.inner, ppu.cycle_count, cpu);
            }
        }

        fn write_oamdata(ppu: &mut Ppu, val: u8) {
            if ppu.is_currently_rendering() {
                // ignore attemps to write when rendering
                return;
            }

            ppu.write_to_oam_and_increment_addr(val);
            ppu.set_ppustatus_low_bits(val);
        }

        fn write_ppuscroll(ppu: &mut Ppu, val: u8) {
            // low bits toggle = 0 => x coordinate is being written
            if !ppu.bits.low_bits_toggle.is_true() {
                // write low 3 bits (fine x) to 'ppu.fine_x_scroll'
                ppu.bits.fine_x_scroll.set(val & 0b111);
                // write high 5 bits (coarse x) to low 5 bits
                // of temporary vram address register
                ppu.temp_vram_addr.set_coarse_x(val >> 3);
            }
            // low bits toggle = 1 => y coordinate is being written
            else {
                // write high 5 bits (coarse y) to
                // bits 5-10 of 'temp_vram_addr'
                ppu.temp_vram_addr.set_coarse_y(val >> 3);
                // write low 3 bits (fine y) to bits
                // 12-14 of 'temp_vram_addr'
                ppu.temp_vram_addr.set_fine_y(val & 0b111);
            }

            ppu.set_ppustatus_low_bits(val);
            ppu.toggle_low_bits_toggle();
        }

        fn write_ppuaddr(ppu: &mut Ppu, val: u8, bus: &mut dyn PpuAddressBus, cpu: &mut cpu::Cpu) {
            let mut temp_vram_addr_bytes = ppu.temp_vram_addr.inner.to_le_bytes();

            if !ppu.bits.low_bits_toggle.is_true() {
                // write low 6 bits into bits 8-13 of temporary
                // vram address register while clearing bit 14
                temp_vram_addr_bytes[1] = val & 0b0111111;
                // store back
                ppu.temp_vram_addr.inner = u16::from_le_bytes(temp_vram_addr_bytes);
            } else {
                // set all low bits of temporary vram register equal to 'val'
                temp_vram_addr_bytes[0] = val;
                ppu.temp_vram_addr.inner = u16::from_le_bytes(temp_vram_addr_bytes);

                // set 'current_vram_addr' equal to 'temp_vram_addr'
                ppu.current_vram_addr = ppu.temp_vram_addr;

                if !ppu.is_currently_rendering() {
                    // if not currently rendering, the address bus
                    // should be set to 'current_vram_addr'
                    bus.set_address(ppu.current_vram_addr.inner, ppu.cycle_count, cpu);
                }
            }

            ppu.set_ppustatus_low_bits(val);
            ppu.toggle_low_bits_toggle();
        }

        fn write_ppudata(ppu: &mut Ppu, val: u8, bus: &mut dyn PpuAddressBus, cpu: &mut cpu::Cpu) {
            bus.write(ppu.current_vram_addr.get_addr(), val, ppu.cycle_count, cpu);
            ppu.set_ppustatus_low_bits(val);

            // increment 'current_vram_addr' (same as when reading ppudata)
            if !ppu.is_currently_rendering() {
                ppu.increment_vram_addr();
                bus.set_address(ppu.current_vram_addr.inner, ppu.cycle_count, cpu);
            } else {
                ppu.increment_vram_addr_coarse_x();
                ppu.increment_vram_addr_y();
            }
        }
    }

    // catches the ppu up to the cpu (approximately)
    pub fn catch_up(
        &mut self,
        cpu: &mut cpu::Cpu,
        bus: &mut dyn PpuAddressBus,
        framebuffer: &[Cell<u32>; 256 * 240],
    ) {
        let target_cycles = cpu.cycle_count as i32 * 3;
        while self.cycle_count < target_cycles {
            self.step(cpu, bus, framebuffer);
        }
    }

    // steps the ppu for one tile worth of cycles or less (1-8 cycles).
    // only used internally by the ppu, in 'Ppu::catch_up()'
    fn step(
        &mut self,
        cpu: &mut cpu::Cpu,
        bus: &mut dyn PpuAddressBus,
        framebuffer: &[Cell<u32>; 256 * 240],
    ) {
        // NOTE: this function is split into multiple subfunctions
        {
            match self.current_scanline {
                // pre-render and visible scanlines
                -1..=239 => step_pre_render_or_visible_line(self, bus, framebuffer, cpu),
                // idle scanline
                240 => step_idle_line(self),
                // vblank 'scanlines'
                241..=260 => step_vblank_line(self, cpu),
                _ => (),
            };
        }

        fn step_pre_render_or_visible_line(
            ppu: &mut Ppu,
            bus: &mut dyn PpuAddressBus,
            framebuffer: &[Cell<u32>; 256 * 240],
            cpu: &mut cpu::Cpu,
        ) {
            match (ppu.current_scanline_dot, ppu.current_scanline) {
                (0, sl) => {
                    ppu.current_scanline_dot += 1;

                    // if rendering is enabled and we're on the first visible scanline,
                    // only increment cycle count if current frame is even-numbered (idle
                    // cycle is skipped on odd frames)
                    if sl == 0 && (ppu.is_background_enable() || ppu.is_sprites_enable()) {
                        ppu.cycle_count += ppu.bits.even_frame.get() as i32;
                    } else {
                        ppu.cycle_count += 1;
                    }

                    // reset 'sprites_found', 'eval_done' and 'current_sprite_idx' before
                    // use (in dots 65-256)
                    ppu.sprite_state.sprites_found = 0;
                    ppu.sprite_state.eval_done = false;
                    // TODO: obscure behavior where 'current_sprite_idx' is set equal to
                    // the current value of oamaddr (which may not be zero) at the start
                    // of sprite evaluation, meaning a litle later than this
                    ppu.sprite_state.current_sprite_idx = 0;
                }
                // NOTE: though we often match on contiguous ranges of dots (x..=y) while
                // stepping, the ppu is usually only advanced 8 cycles/dots at a time. this
                // means that most of the dots in the range are never actually hit.
                (1..=256, sl) => {
                    match sl {
                        // pre-render line
                        -1 => {
                            // OPTIMIZE: ideally, we would find some way of moving this
                            // block out into the outer match and have it fallthrough
                            // into here instead
                            if ppu.current_scanline_dot == 1 {
                                // clear vblank, sprite zero hit and sprite overflow flags
                                ppu.set_vblank(false);
                                ppu.set_sprite_zero_hit(false);
                                ppu.set_sprite_overflow(false);
                            }
                        }
                        // visible lines
                        _ => {
                            if ppu.current_scanline_dot >= 65
                                && (ppu.is_sprites_enable() || ppu.is_background_enable())
                            {
                                // evaluate sprite on next scanline
                                for _ in 0..4 {
                                    let sprite_overflow =
                                        ppu.sprite_state.eval_next_scanline_sprite(
                                            ppu.is_sprite_overflow(),
                                            ppu.get_sprite_size(),
                                            &ppu.primary_oam,
                                            &mut ppu.secondary_oam,
                                            ppu.current_scanline,
                                            ppu.current_scanline_dot,
                                        );

                                    if sprite_overflow {
                                        ppu.set_sprite_overflow(true);
                                    }
                                }
                            }

                            // draw a row of 8 pixels horizontally
                            let sprite_zero_hit = ppu.draw_8_pixels(framebuffer, bus);

                            if sprite_zero_hit {
                                ppu.set_sprite_zero_hit(true);
                            }
                        }
                    }

                    if ppu.is_background_enable() || ppu.is_sprites_enable() {
                        // if last pixel drawn was 256th (end of scanline)
                        if ppu.current_scanline_dot + 8 == 257 {
                            // increment fine y
                            // NOTE: may wish to increment x here as well (what the ppu does irl)
                            ppu.increment_vram_addr_y();
                        } else {
                            // shift previously drawn tile data leftwards
                            // in 'bg_state' shift registers
                            ppu.bg_state.shift_tile_data_by_8();
                            // fill rightmost 8 bits of 'bg_state' shift registers
                            // with tile data for the next 8 pixels to draw
                            ppu.bg_state.fetch_current_tile_data(
                                ppu.cycle_count,
                                ppu.get_background_pattern_table_addr(),
                                ppu.current_vram_addr,
                                bus,
                                cpu,
                            );
                            // increment 'current_vram_addr' by one tile horizontally
                            ppu.increment_vram_addr_coarse_x();
                        }
                    }

                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
                (257, _) => {
                    ppu.oamaddr = 0;
                    // set current sprite to zero so it can be re-used
                    // in 'fetch_next_scanline_sprite_data()'
                    ppu.sprite_state.current_sprite_idx = 0;
                    debug_assert!(ppu.sprite_state.sprites_found <= 8);

                    // fetch sprite data for the sprites found previously (during dots 65-256)
                    if ppu.is_sprites_enable() || ppu.is_background_enable() {
                        ppu.sprite_state.fetch_next_scanline_sprite_data(
                            &ppu.secondary_oam,
                            ppu.get_sprite_size(),
                            ppu.current_scanline,
                            ppu.current_scanline_dot,
                            ppu.get_8x8_sprite_pattern_table_addr(),
                            ppu.cycle_count,
                            bus,
                            cpu,
                        );
                    }

                    if ppu.is_sprites_enable() || ppu.is_background_enable() {
                        ppu.transfer_temp_horizontal_bits();
                    }

                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
                (258..=320, sl) => {
                    ppu.oamaddr = 0;

                    // continue fetching sprite data
                    if ppu.is_sprites_enable() || ppu.is_background_enable() {
                        ppu.sprite_state.fetch_next_scanline_sprite_data(
                            &ppu.secondary_oam,
                            ppu.get_sprite_size(),
                            ppu.current_scanline,
                            ppu.current_scanline_dot,
                            ppu.get_8x8_sprite_pattern_table_addr(),
                            ppu.cycle_count,
                            bus,
                            cpu,
                        );
                    }

                    if sl == -1
                        // NOTE: this should actually happen at dot 280
                        && ppu.current_scanline_dot == 281
                        && (ppu.is_sprites_enable() || ppu.is_background_enable())
                    {
                        ppu.transfer_temp_vert_bits();
                    }

                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
                (321..=327, _) => {
                    ppu.cycle_count += 7;
                    ppu.current_scanline_dot += 7;
                }
                (328..=336, _) => {
                    if ppu.is_background_enable() || ppu.is_sprites_enable() {
                        ppu.bg_state.shift_tile_data_by_8();
                        ppu.bg_state.fetch_current_tile_data(
                            ppu.cycle_count,
                            ppu.get_background_pattern_table_addr(),
                            ppu.current_vram_addr,
                            bus,
                            cpu,
                        );
                        ppu.increment_vram_addr_coarse_x();
                    }

                    match ppu.current_scanline_dot {
                        328 => {
                            ppu.cycle_count += 8;
                            ppu.current_scanline_dot += 8;
                        }
                        336 => {
                            ppu.cycle_count += 5;
                            ppu.current_scanline_dot = 0;
                            ppu.current_scanline += 1;

                            if ppu.current_scanline == 240 {
                                ppu.bits.frame_done.set(1);
                            }
                        }
                        _ => (),
                    }
                }
                _ => (),
            }
        }

        fn step_idle_line(ppu: &mut Ppu) {
            match ppu.current_scanline_dot {
                336 => {
                    ppu.cycle_count += 5;
                    ppu.current_scanline_dot = 0;
                    ppu.current_scanline = 241;
                }
                0 | 168 => {
                    ppu.cycle_count += 168;
                    ppu.current_scanline_dot += 168;
                }
                _ => (),
            }
        }

        fn step_vblank_line(ppu: &mut Ppu, cpu: &mut cpu::Cpu) {
            match ppu.current_scanline_dot {
                0 => {
                    ppu.cycle_count += 1;
                    ppu.current_scanline_dot += 1;
                }
                1 => {
                    // NOTE: setting of vblank flag may be suppressed by reads to ppustatus
                    if (ppu.current_scanline == 241) && !ppu.bits.suppress_vblank_flag.is_true() {
                        ppu.set_vblank(true);
                    }

                    ppu.cycle_count += 1;
                    ppu.current_scanline_dot += 1;
                }
                2 => {
                    ppu.cycle_count += 1;
                    ppu.current_scanline_dot += 1;
                }
                3 => {
                    // NOTE: we wait until dot 3 to assert nmi (even though the vblank flag
                    // is set on dot 1), in order to more accurately emulate nmi/vblank flag
                    // suppression. this way, any reads to ppustatus during dot 2 or 3 (before
                    // this chunk of code is executed) will prevent nmi from being asserted
                    if ppu.current_scanline == 241 {
                        if ppu.is_vblank_nmi_enabled() && ppu.is_vblank() {
                            cpu.bits.nmi.set(1);
                        }
                    }

                    ppu.cycle_count += 5;
                    ppu.current_scanline_dot += 5;
                }
                336 => {
                    ppu.cycle_count += 5;
                    ppu.current_scanline_dot = 0;

                    if ppu.current_scanline == 260 {
                        ppu.toggle_even_frame();
                        // reset scanline count
                        ppu.current_scanline = -1;
                        // ensure next frame's vblank flag will not be suppressed
                        ppu.bits.suppress_vblank_flag.set(0);
                    } else {
                        ppu.current_scanline += 1;
                    }
                }
                _ => {
                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
            }
        }
    }

    // draws 8 pixels, without making any state changes to 'self'.
    // returns whether sprite zero was hit
    fn draw_8_pixels(
        &self,
        framebuffer: &[Cell<u32>; 256 * 240],
        bus: &mut dyn PpuAddressBus,
    ) -> bool {
        // NOTE: this function is also split into subfunctions
        {
            if self.is_background_enable() || self.is_sprites_enable() {
                return draw_8_pixels_bg_and_sprites(self, framebuffer, bus);
            } else {
                draw_8_pixels_backdrop_color(self, framebuffer, bus);
                return false;
            }
        }

        fn draw_8_pixels_bg_and_sprites(
            ppu: &Ppu,
            framebuffer: &[Cell<u32>; 256 * 240],
            bus: &dyn PpuAddressBus,
        ) -> bool {
            let mut sprite_zero_hit = false;

            for i in 0..8 {
                let tile_offset = i + ppu.bits.fine_x_scroll.get();

                let bg_color_idx = match (
                    ppu.is_background_enable(),
                    ppu.current_scanline_dot + i as u16,
                    ppu.is_background_left_column_enable(),
                ) {
                    // set index to zero if bg is disabled
                    (false, _, _) => 0,
                    // or if on dots 1-8 and bg is disabled for this area
                    (_, 1..=8, false) => 0,
                    _ => {
                        let lo = ((ppu.bg_state.tile_bitplanes_lo.to_u16() >> (15 - tile_offset))
                            & 1) as u8;
                        let hi = (((ppu.bg_state.tile_bitplanes_hi.to_u16() >> (15 - tile_offset))
                            << 1)
                            & 2) as u8;
                        lo | hi
                    }
                };

                let bg_palette_idx = if tile_offset > 7 {
                    ppu.bg_state.tile_palette_indices & 0b11
                } else {
                    (ppu.bg_state.tile_palette_indices & 0b1100) >> 2
                };

                let mut sprite_zero = false;

                let pixel_color = match (
                    ppu.is_sprites_enable(),
                    ppu.current_scanline_dot + i as u16,
                    ppu.is_sprites_left_column_enable(),
                ) {
                    // draw bg color if sprites are disabled
                    (false, _, _) => calc_pixel_color(ppu, bg_palette_idx, bg_color_idx, bus),
                    // draw bg color if on dots 1-8 and sprite
                    // drawing is disabled for the first 8 pixels
                    (_, 1..=8, false) => calc_pixel_color(ppu, bg_palette_idx, bg_color_idx, bus),
                    // otherwise, search for an active, non-transparent sprite at the current dot
                    _ => match ppu
                        .sprite_state
                        .get_sprite_at_dot_info(ppu.current_scanline_dot + i as u16)
                    {
                        // draw bg color if no sprite was found
                        None => calc_pixel_color(ppu, bg_palette_idx, bg_color_idx, bus),
                        Some(info) => {
                            sprite_zero = info.is_sprite_zero;

                            if info.is_in_front || bg_color_idx == 0 {
                                calc_pixel_color(ppu, info.palette_index, info.color_index, bus)
                            } else {
                                calc_pixel_color(ppu, bg_palette_idx, bg_color_idx, bus)
                            }
                        }
                    },
                };

                if sprite_zero
                    && bg_color_idx != 0
                    && (ppu.current_scanline_dot + i as u16 - 1) != 0xff
                {
                    sprite_zero_hit = true;
                }

                let screen_x = (ppu.current_scanline_dot - 1) as usize + i as usize;
                let screen_y = ppu.current_scanline as usize;

                // OPTIMIZE: unchecked indexing
                framebuffer[screen_y * 256 + screen_x].set(pixel_color);
            }

            sprite_zero_hit
        }

        // draws 8 pixels of backdrop color (or if 'current_vram_addr'
        // >= 0x3f00, draws the color 'current_vram_addr' points to)
        fn draw_8_pixels_backdrop_color(
            ppu: &Ppu,
            framebuffer: &[Cell<u32>; 256 * 240],
            bus: &dyn PpuAddressBus,
        ) {
            for i in 0..8 {
                let pixel_color = {
                    let bg_color_idx = if ppu.current_vram_addr.get_addr() >= 0x3f00 {
                        logln!("background palette hack triggered");
                        (ppu.current_vram_addr.get_addr() & 0b11111) as u8
                    } else {
                        0
                    };

                    let bg_color_byte = bus.read_palette_memory(bg_color_idx);

                    palette::COLOR_LUT.get(
                        bg_color_byte,
                        ppu.is_greyscale_enabled(),
                        ppu.ppumask >> 5,
                    )
                };

                let screen_x = (ppu.current_scanline_dot - 1 + i as u16) as usize;
                let screen_y = ppu.current_scanline as usize;
                framebuffer[screen_y * 256 + screen_x].set(pixel_color);
            }
        }

        fn calc_pixel_color(
            ppu: &Ppu,
            palette_idx: u8,
            color_idx: u8,
            bus: &dyn PpuAddressBus,
        ) -> u32 {
            let final_idx = if color_idx == 0 {
                0
            } else {
                (((palette_idx << 2) as u16) | (color_idx as u16)) as u8
            };

            let color_byte = bus.read_palette_memory(final_idx);
            palette::COLOR_LUT.get(color_byte, ppu.is_greyscale_enabled(), ppu.ppumask >> 5)
        }
    }

    // increments 'current_vram_addr' by 1 or 32, depending on the increment mode bit in ppuctrl
    fn increment_vram_addr(&mut self) {
        let increment = if self.get_vram_addr_increment() {
            32
        } else {
            1
        };

        self.current_vram_addr.inner = (self.current_vram_addr.inner + increment) & 0x3fff;
    }

    // increments the fine y scroll bits in 'current_vram_addr',
    // potentially overflowing into the coarse y scroll bits
    fn increment_vram_addr_y(&mut self) {
        if self.current_vram_addr.get_fine_y() == 0b111 {
            // clear fine y bits if fine y bits = max
            self.current_vram_addr.set_fine_y(0);

            if self.current_vram_addr.get_coarse_y() == 29 {
                // if carry from fine y bits = 1 and coarse y bits
                // = 29 (there are 29 rows of tiles in a frame),
                // clear all coarse y bits and overflow into bit
                // 11 to move to next nametable vertically
                self.current_vram_addr.set_coarse_y(0);
                self.current_vram_addr.inner ^= 0b100000000000;
            } else if self.current_vram_addr.get_coarse_y() == 0b11111 {
                // if coarse y = maximum, wrap the value without overflowing into
                // bit 11 and switching nametables (unintended behavior afaik)
                self.current_vram_addr.set_coarse_y(0);
            } else {
                // increment coarse y bits of 'current_vram_addr'
                self.current_vram_addr.inner += 1 << 5;
            }
        } else {
            // increment fine y bits normally
            self.current_vram_addr.inner += 1 << 12;
        }
    }

    // increments the coarse x scroll/position bits in 'current_vram_addr'
    // (corresponds to moving one tile to the right in the current nametable)
    fn increment_vram_addr_coarse_x(&mut self) {
        if self.current_vram_addr.get_coarse_x() == 0b11111 {
            // if the coarse x component of 'current_vram_addr' is the highest
            // value it can be (31), clear all coarse x bits and overflow
            // into bit 10 (move to next nametable horizontally)
            self.current_vram_addr.set_coarse_x(0);
            self.current_vram_addr.inner ^= 0b10000000000;
        } else {
            // if not highest value, increment normally
            self.current_vram_addr.inner += 1;
        }
    }

    // transfers the coarse and fine y bits + high nametable select
    // bit from 'temp_vram_addr' to 'current_vram_addr'
    fn transfer_temp_vert_bits(&mut self) {
        let temp_high_nt_select = self.temp_vram_addr.inner & 0x800;
        self.current_vram_addr.inner &= !0x800;
        self.current_vram_addr.inner |= temp_high_nt_select;

        let temp_coarse_y = self.temp_vram_addr.get_coarse_y();
        self.current_vram_addr.set_coarse_y(temp_coarse_y);

        let temp_fine_y = self.temp_vram_addr.get_fine_y();
        self.current_vram_addr.set_fine_y(temp_fine_y);
    }

    // transfers the coarse x bits + the lowest nametable select bit from
    // 'temp_vram_addr' to 'current_vram_addr'
    fn transfer_temp_horizontal_bits(&mut self) {
        let temp_low_nt_select = self.temp_vram_addr.inner & 0x400;
        self.current_vram_addr.inner &= !0x400;
        self.current_vram_addr.inner |= temp_low_nt_select;

        let temp_coarse_x = self.temp_vram_addr.get_coarse_x();
        self.current_vram_addr.set_coarse_x(temp_coarse_x);
    }
}

// second impl block to separate private getter/setter/convenience functions from the rest
impl Ppu {
    fn toggle_even_frame(&mut self) {
        let prev = self.bits.even_frame.is_true();
        self.bits.even_frame.set(!prev as u8);
    }

    fn toggle_low_bits_toggle(&mut self) {
        let prev = self.bits.low_bits_toggle.is_true();
        self.bits.low_bits_toggle.set(!prev as u8);
    }

    #[cfg(test)]
    fn get_base_nametable_addr(&self) -> u16 {
        0x2000 | (((self.ppuctrl & 3) as u16) << 10)
    }

    // TODO: enum for clarity
    fn get_vram_addr_increment(&self) -> bool {
        (self.ppuctrl & 4) != 0
    }

    fn get_8x8_sprite_pattern_table_addr(&self) -> u16 {
        ((self.ppuctrl & 0b1000) as u16) << 9
    }

    fn get_background_pattern_table_addr(&self) -> u16 {
        ((self.ppuctrl & 0b10000) as u16) << 8
    }

    fn get_sprite_size(&self) -> SpriteSize {
        if self.ppuctrl & 0b100000 != 0 {
            SpriteSize::S8x16
        } else {
            SpriteSize::S8x8
        }
    }

    fn is_vblank_nmi_enabled(&self) -> bool {
        (self.ppuctrl >> 7) != 0
    }

    fn is_greyscale_enabled(&self) -> bool {
        (self.ppumask & 1) != 0
    }

    // whether background will be displayed in the leftmost 8 pixel columns
    fn is_background_left_column_enable(&self) -> bool {
        (self.ppumask & 2) != 0
    }

    // whether sprites will be displayed in the leftmost 8 pixel columns
    fn is_sprites_left_column_enable(&self) -> bool {
        (self.ppumask & 4) != 0
    }

    // whether background will be displayed
    fn is_background_enable(&self) -> bool {
        (self.ppumask & 8) != 0
    }

    // whether sprites will be displayed
    fn is_sprites_enable(&self) -> bool {
        (self.ppumask & 0b10000) != 0
    }

    fn is_sprite_overflow(&self) -> bool {
        (self.ppustatus & 0b100000) != 0
    }

    fn set_sprite_overflow(&mut self, overflow: bool) {
        self.ppustatus = (self.ppustatus & !0b100000) | ((overflow as u8) << 5);
    }

    fn set_sprite_zero_hit(&mut self, hit: bool) {
        self.ppustatus = (self.ppustatus & !0b1000000) | ((hit as u8) << 6);
    }

    fn is_vblank(&self) -> bool {
        (self.ppustatus >> 7) != 0
    }

    fn set_vblank(&mut self, vblank: bool) {
        self.ppustatus = (self.ppustatus & !0b10000000) | ((vblank as u8) << 7);
    }

    fn is_currently_rendering(&self) -> bool {
        self.current_scanline < 241 && (self.is_sprites_enable() || self.is_background_enable())
    }
}
