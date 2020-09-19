#[macro_use]
use super::{cpu, util};
use super::memory_map::PpuMemoryMap;
use super::pixel_renderer::PixelRenderer;

mod draw;
mod oam;
mod palette;
pub mod test;

// TODO: unpub fields
pub struct Ppu<'a> {
    // object attribute memory
    pub oam: oam::Oam,
    pub cycle_count: u32,
    pub renderer: PixelRenderer,
    // TODO: investigate how much code bloat generics would cause
    // - would it be worth using it over a trait object?
    pub memory: &'a mut dyn PpuMemoryMap,

    // registers
    pub ppuctrl: u8,
    pub ppumask: u8,
    pub ppustatus: u8,
    // NOTE: on some ppu chips, there are bugs relating to
    // writing to oamaddr. not sure if these need to be
    // emulated, but it may be worth keeping in mind
    pub oamaddr: u8,
    pub ppudata_read_buffer: u8,

    // internal registers
    // used to toggle whether reads and writes to 'ppuscroll'
    // and 'ppuaddr' access the high or low bits of the register.
    // referred to as 'w' in the nesdev.com 'ppu scrolling' article
    // OPTIMIZE: pack these together (bitfields, somehow?)
    pub low_bits_toggle: bool,
    pub even_frame: bool,
    // address of the current tile to be fetched and drawn. points
    // to a byte in one of the nametables in vram. referred to
    // as 'v' in the nesdev.com 'ppu scrolling' article
    pub current_vram_addr: VramAddrRegister,
    // temporary address, same as above but  doesn't get
    // incremented while drawing. this register is shared
    // by 'ppuscroll' and 'ppuaddr' (so writes to these
    // registers go into this). referred to as 't' in the
    // nesdev.com 'ppu scrolling' article
    pub temp_vram_addr: VramAddrRegister,
    // low 3 bits consist of fine x scroll value
    pub fine_x_scroll: u8,

    // counter variables used for rendering
    pub current_scanline: i16,
    pub current_scanline_dot: u16,
}

// flags returned by 'Ppu.catch_up()', 'Ppu.step()' and 'Ppu.step_scanline()'
pub enum PpuState {
    MidFrame,
    FrameDone,
}

#[derive(Copy, Clone)]
pub struct VramAddrRegister {
    pub inner: u16,
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

impl<'a> Ppu<'a> {
    pub fn new(renderer: PixelRenderer, memory: &'a mut dyn PpuMemoryMap) -> Self {
        Self {
            oam: oam::Oam::default(),
            cycle_count: 0,
            ppuctrl: 0,
            ppumask: 0,
            ppustatus: 0,
            oamaddr: 0,
            ppudata_read_buffer: 0,
            low_bits_toggle: false,
            current_vram_addr: VramAddrRegister { inner: 0 },
            temp_vram_addr: VramAddrRegister { inner: 0 },
            fine_x_scroll: 0,
            current_scanline: -1,
            current_scanline_dot: 0,
            even_frame: true,
            renderer,
            memory,
        }
    }

    // used for reading the registers located in the cpu memory map at 0x2000-0x2007
    pub fn read_register_by_index(&mut self, index: u8) -> u8 {
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
                7 => read_ppudata(self),
                _ => 0,
            };
        }

        fn read_ppustatus(ppu: &mut Ppu) -> u8 {
            // clear low bits toggle
            ppu.low_bits_toggle = false;
            let status = ppu.ppustatus;
            // clear vblank flag
            ppu.set_vblank(false);
            status
        }

        // TODO: special values when rendering
        fn read_oamdata(ppu: &mut Ppu) -> u8 {
            if let (0..=239, 1..=64) = (ppu.current_scanline, ppu.current_scanline_dot) {
                // if on dots/cycles 1-64 of a visible scanline, return 0xff
                return 0xff;
            }

            let mut byte = ppu.oam.primary.get_byte(ppu.oamaddr);
            if ppu.oamaddr % 4 == 2 {
                // if 'byte' is a sprite attribute byte, clear bits 2-4
                byte &= 0b11100011;
            }

            byte
        }

        fn read_ppudata(ppu: &mut Ppu) -> u8 {
            let val = if (ppu.current_vram_addr.inner >> 8) == 0b111111 {
                // read directly from vram if address is in range
                // 0x3f00-0x3fff (palette ram)
                let val = ppu.memory.read(ppu.current_vram_addr.get_addr());
                // store value at mirrored address (down to 0x2f00-0x2fff)
                // in read buffer
                ppu.ppudata_read_buffer = ppu
                    .memory
                    .read(ppu.current_vram_addr.get_addr() & !0b01000000000000);
                val
            } else {
                // read from read buffer if address is in range 0-0x3eff
                let val = ppu.ppudata_read_buffer;
                ppu.ppudata_read_buffer = ppu.memory.read(ppu.current_vram_addr.get_addr());
                val
            };

            if !ppu.is_currently_rendering() {
                // if not currently rendering, increment normally
                ppu.increment_vram_addr();
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
    pub fn write_register_by_index(&mut self, index: u8, val: u8, cpu: &mut cpu::Cpu) {
        {
            match index {
                // ppuctrl
                0 => write_ppuctrl(self, val, cpu),
                // ppumask
                1 => self.ppumask = val,
                // ppustatus, ignore attemps to write
                2 => return,
                // oamaddr
                3 => self.oamaddr = val,
                // oamdata
                4 => write_oamdata(self, val),
                // ppuscroll
                5 => write_ppuscroll(self, val),
                // ppuaddr
                6 => write_ppuaddr(self, val),
                // ppudata
                7 => write_ppudata(self, val),
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
                cpu.nmi = true;
            }
        }

        fn write_oamdata(ppu: &mut Ppu, val: u8) {
            if ppu.is_currently_rendering() {
                // ignore attemps to write when rendering
                return;
            }

            ppu.oam.primary.set_byte(ppu.oamaddr, val);
            ppu.oamaddr = ppu.oamaddr.wrapping_add(1);
            ppu.set_ppustatus_low_bits(val);
        }

        fn write_ppuscroll(ppu: &mut Ppu, val: u8) {
            // low bits toggle = 0 => x coordinate is being written
            if !ppu.low_bits_toggle {
                // write low 3 bits (fine x) to 'ppu.fine_x_scroll'
                ppu.set_fine_x_scroll(val & 0b111);
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
            // reset low bits toggle
            ppu.low_bits_toggle = !ppu.low_bits_toggle;
        }

        fn write_ppuaddr(ppu: &mut Ppu, val: u8) {
            let mut temp_vram_addr_bytes = ppu.temp_vram_addr.inner.to_le_bytes();

            if !ppu.low_bits_toggle {
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
            }

            ppu.set_ppustatus_low_bits(val);
            ppu.low_bits_toggle = !ppu.low_bits_toggle;
        }

        fn write_ppudata(ppu: &mut Ppu, val: u8) {
            ppu.memory.write(ppu.current_vram_addr.get_addr(), val);

            ppu.set_ppustatus_low_bits(val);

            // increment 'current_vram_addr' (same as when reading ppudata)
            if !ppu.is_currently_rendering() {
                ppu.increment_vram_addr();
            } else {
                ppu.increment_vram_addr_coarse_x();
                ppu.increment_vram_addr_y();
            }
        }
    }

    // catches the ppu up to the cpu (approximately)
    pub fn catch_up(&mut self, cpu: &mut cpu::Cpu) -> PpuState {
        let target_cycles = cpu.cycle_count as u32 * 3;
        let cycles_to_catch_up = target_cycles.saturating_sub(self.cycle_count);

        // if the ppu is behind the cpu by more than 2 scanlines worth of cycles,
        // start by using a separate scanline algorithm to catch up
        if cycles_to_catch_up > 341 * 2 {
            // line up with the next scanline
            while self.current_scanline_dot != 0 {
                self.step(cpu);
            }

            // step using scanline algorithm
            let n_scanlines = (target_cycles - self.cycle_count) / 341;
            for _ in 0..n_scanlines {
                self.step_scanline(cpu);
            }
        }

        // step normally
        while self.cycle_count < target_cycles {
            if let PpuState::FrameDone = self.step(cpu) {
                return PpuState::FrameDone;
            }
        }

        PpuState::MidFrame
    }

    // steps the ppu for one tile worth of cycles (1-8 cycles).
    // only used internally by the ppu, in 'Ppu.catch_up()'
    fn step(&mut self, cpu: &mut cpu::Cpu) -> PpuState {
        // NOTE: this function is split into multiple subfunctions
        {
            return match self.current_scanline {
                // pre-render scanline
                -1 => step_pre_render_line(self),
                // visible scanlines
                0..=239 => step_visible_line(self),
                // idle scanline
                240 => step_idle_line(self),
                // vblank 'scanlines'
                241..=260 => step_vblank_line(self, cpu),
                _ => PpuState::MidFrame,
            };
        }

        fn step_pre_render_line(ppu: &mut Ppu) -> PpuState {
            match ppu.current_scanline_dot {
                // idle cycle
                0 => {
                    ppu.current_scanline_dot += 1;
                    // if rendering, don't increment cycles on odd frames (idle cycle is skipped)
                    if ppu.is_sprites_enable() || ppu.is_background_enable() {
                        ppu.cycle_count += ppu.even_frame as u32;
                    }
                }
                1 => {
                    // clear vblank and sprite zero hit flags
                    ppu.set_vblank(false);
                    ppu.set_sprite_zero_hit(false);

                    ppu.cycle_count += 7;
                    ppu.current_scanline_dot += 7;
                }
                2..=255 => {
                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
                256 => {
                    if ppu.is_sprites_enable() || ppu.is_background_enable() {
                        ppu.transfer_temp_horizontal_bits();
                    }

                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
                257..=279 => {
                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
                280 => {
                    if ppu.is_sprites_enable() || ppu.is_background_enable() {
                        ppu.transfer_temp_vert_bits();
                    }

                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
                281..=335 => {
                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
                336 => {
                    ppu.cycle_count += 5;
                    ppu.current_scanline_dot = 0;
                    ppu.current_scanline = 0;
                }
                _ => (),
            }

            PpuState::MidFrame
        }

        fn step_visible_line(ppu: &mut Ppu) -> PpuState {
            match ppu.current_scanline_dot {
                0 => {
                    ppu.current_scanline_dot += 1;
                    ppu.cycle_count += 1;
                    // reset 'sprites_found' and 'current_sprite' before use (in dots 65-256)
                    ppu.oam.sprites_found = 0;
                    ppu.oam.current_sprite = 0;
                }
                1..=256 => {
                    if ppu.current_scanline_dot >= 65
                        && (ppu.is_sprites_enable() || ppu.is_background_enable())
                    {
                        // evaluate sprite on next scanline
                        for _ in 0..3 {
                            ppu.oam.eval_next_scanline_sprite(
                                ppu.current_scanline,
                                ppu.current_scanline_dot,
                            );
                        }
                    }

                    // draw a tile's worth of background and sprite pixels horizontally (or less
                    // than a tile's worth, if the current tile straddles the screen boundary)
                    let pixels_drawn = draw::draw_tile_row(
                        ppu,
                        ppu.is_background_enable(),
                        ppu.is_sprites_enable(),
                    );

                    ppu.cycle_count += pixels_drawn as u32;
                    ppu.current_scanline_dot += pixels_drawn as u16;

                    if ppu.is_background_enable() || ppu.is_sprites_enable() {
                        // if last pixel drawn was 256th (end of scanline)
                        if ppu.current_scanline_dot == 257 {
                            // increment fine y
                            ppu.increment_vram_addr_y();
                        } else {
                            ppu.increment_vram_addr_coarse_x();
                        }
                    }
                }
                257 => {
                    // set current sprite to zero so it can be re-used
                    // in 'fetch_next_scanline_sprite_data()'
                    ppu.oam.current_sprite = 0;
                    debug_assert!(ppu.oam.sprites_found <= 8);

                    // fetch sprite data for the sprites found previosuly (during dots 65-256)
                    if ppu.is_sprites_enable() {
                        ppu.oam.fetch_next_scanline_sprite_data(
                            ppu.current_scanline,
                            ppu.current_scanline_dot,
                            ppu.get_8x8_sprite_pattern_table_addr(),
                            ppu.memory,
                        );
                    }

                    if ppu.is_sprites_enable() || ppu.is_background_enable() {
                        ppu.transfer_temp_horizontal_bits();
                    }

                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
                258..=320 => {
                    // continue fetching sprite data
                    if ppu.is_sprites_enable() {
                        ppu.oam.fetch_next_scanline_sprite_data(
                            ppu.current_scanline,
                            ppu.current_scanline_dot,
                            ppu.get_8x8_sprite_pattern_table_addr(),
                            ppu.memory,
                        );
                    }

                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
                321..=336 => {
                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
                337 => {
                    ppu.cycle_count += 4;
                    ppu.current_scanline_dot = 0;
                    ppu.current_scanline += 1;
                }
                _ => (),
            }

            PpuState::MidFrame
        }

        fn step_idle_line(ppu: &mut Ppu) -> PpuState {
            match ppu.current_scanline_dot {
                256 => {
                    if ppu.is_sprites_enable() || ppu.is_background_enable() {
                        ppu.transfer_temp_horizontal_bits();
                    }

                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
                336 => {
                    ppu.cycle_count += 5;
                    ppu.current_scanline_dot = 0;
                    ppu.current_scanline += 1;
                }
                _ => {
                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
            }

            PpuState::MidFrame
        }

        fn step_vblank_line(ppu: &mut Ppu, cpu: &mut cpu::Cpu) -> PpuState {
            match ppu.current_scanline_dot {
                0 => {
                    ppu.cycle_count += 1;
                    ppu.current_scanline_dot += 1;
                }
                1 => {
                    if ppu.current_scanline == 241 {
                        ppu.set_vblank(true);
                        if ppu.is_vblank_nmi_enabled() {
                            cpu.nmi = true;
                        }
                    }

                    ppu.cycle_count += 7;
                    ppu.current_scanline_dot += 7;
                }
                256 => {
                    if ppu.is_sprites_enable() || ppu.is_background_enable() {
                        ppu.transfer_temp_horizontal_bits();
                    }

                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
                336 => {
                    ppu.cycle_count += 5;
                    ppu.current_scanline_dot = 0;

                    if ppu.current_scanline == 260 {
                        // reset scanline count
                        ppu.current_scanline = -1;

                        // toggle 'even_frame' if rendering is enabled
                        if ppu.is_sprites_enable() || ppu.is_background_enable() {
                            ppu.even_frame = !ppu.even_frame;
                        }

                        return PpuState::FrameDone;
                    } else {
                        ppu.current_scanline += 1;
                    }
                }
                _ => {
                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
            }

            PpuState::MidFrame
        }
    }

    // steps the ppu for a scanline worth of cycles. only
    // used internally by the ppu, in 'Ppu.catch_up()'
    fn step_scanline(&mut self, cpu: &mut cpu::Cpu) -> PpuState {
        // NOTE: this function is split into multiple subfunctions
        {
            return match self.current_scanline {
                // pre-render scanline
                -1 => step_pre_render_line_full(self),
                // visible scanlines
                0..=239 => step_visible_line_full(self),
                // idle scanline
                240 => step_idle_line_full(self),
                // vblank 'scanlines'
                241 => step_first_vblank_line_full(self, cpu),
                242..=259 => step_vblank_line_full(self),
                260 => step_last_vblank_line_full(self),
                _ => PpuState::MidFrame,
            };
        }

        fn step_pre_render_line_full(ppu: &mut Ppu) -> PpuState {
            ppu.set_vblank(false);
            ppu.set_sprite_zero_hit(false);

            if ppu.is_sprites_enable() || ppu.is_background_enable() {
                ppu.transfer_temp_horizontal_bits();
                ppu.transfer_temp_vert_bits();
            }

            ppu.current_scanline = 0;
            ppu.cycle_count += 340 + ppu.even_frame as u32;

            PpuState::MidFrame
        }

        fn step_visible_line_full(ppu: &mut Ppu) -> PpuState {
            ppu.current_scanline_dot = 1;
            ppu.oam.sprites_found = 0;
            ppu.oam.current_sprite = 0;

            // FIXME: 72 iterations?
            if ppu.is_sprites_enable() || ppu.is_background_enable() {
                for _ in 0..72 {
                    ppu.oam.eval_next_scanline_sprite(ppu.current_scanline, 65);
                }
            }

            let tiles_to_draw = if ppu.fine_x_scroll == 0 { 0..32 } else { 0..33 };

            // if rendering is enabled
            if ppu.is_background_enable() || ppu.is_sprites_enable() {
                for _ in tiles_to_draw {
                    // OPTIMIZE: make separate drawing
                    // algorithm for drawing entire scanlines
                    let pixels_drawn = draw::draw_tile_row(
                        ppu,
                        ppu.is_background_enable(),
                        ppu.is_sprites_enable(),
                    );

                    ppu.current_scanline_dot += pixels_drawn as u16;
                    ppu.increment_vram_addr_coarse_x();
                }

                // increment fine y
                ppu.increment_vram_addr_y();
            } else {
                for _ in tiles_to_draw {
                    let pixels_drawn = draw::draw_tile_row(
                        ppu,
                        ppu.is_background_enable(),
                        ppu.is_sprites_enable(),
                    );

                    ppu.current_scanline_dot += pixels_drawn as u16;
                }
            }

            assert_eq!(ppu.current_scanline_dot, 257);

            if ppu.is_sprites_enable() || ppu.is_background_enable() {
                ppu.transfer_temp_horizontal_bits();
            }

            ppu.oam.current_sprite = 0;

            if ppu.is_sprites_enable() {
                for _ in 0..8 {
                    ppu.oam.fetch_next_scanline_sprite_data(
                        ppu.current_scanline,
                        ppu.current_scanline_dot,
                        ppu.get_8x8_sprite_pattern_table_addr(),
                        ppu.memory,
                    );
                }
            }

            ppu.current_scanline += 1;
            ppu.cycle_count += 341;
            ppu.current_scanline_dot = 0;

            PpuState::MidFrame
        }

        fn step_idle_line_full(ppu: &mut Ppu) -> PpuState {
            if ppu.is_sprites_enable() || ppu.is_background_enable() {
                ppu.transfer_temp_horizontal_bits();
            }

            ppu.current_scanline = 241;
            ppu.cycle_count += 341;

            PpuState::MidFrame
        }

        fn step_first_vblank_line_full(ppu: &mut Ppu, cpu: &mut cpu::Cpu) -> PpuState {
            ppu.set_vblank(true);
            if ppu.is_vblank_nmi_enabled() {
                cpu.nmi = true;
            }

            if ppu.is_sprites_enable() || ppu.is_background_enable() {
                ppu.transfer_temp_horizontal_bits();
            }

            ppu.current_scanline = 242;
            ppu.cycle_count += 341;

            PpuState::MidFrame
        }

        fn step_vblank_line_full(ppu: &mut Ppu) -> PpuState {
            if ppu.is_sprites_enable() || ppu.is_background_enable() {
                ppu.transfer_temp_horizontal_bits();
            }

            ppu.current_scanline += 1;
            ppu.cycle_count += 341;

            PpuState::MidFrame
        }

        fn step_last_vblank_line_full(ppu: &mut Ppu) -> PpuState {
            if ppu.is_sprites_enable() || ppu.is_background_enable() {
                ppu.transfer_temp_horizontal_bits();
                ppu.even_frame = !ppu.even_frame;
            }

            ppu.current_scanline = -1;
            ppu.cycle_count += 341;

            PpuState::FrameDone
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

    // sets the low 5 bits of 'ppustatus' equal to the low 5 bits of 'val'
    fn set_ppustatus_low_bits(&mut self, val: u8) {
        self.ppustatus &= !0b11111;
        self.ppustatus |= val & 0b11111;
    }

    // NOTE: this expects the fine x value to be in the low 3 bits of 'fine_x'
    fn set_fine_x_scroll(&mut self, fine_x: u8) {
        self.fine_x_scroll = (self.fine_x_scroll & !0b111) | fine_x;
    }

    fn get_base_nametable_addr(&self) -> u16 {
        // NOTE: may not need to actually multiply out etc.
        0x2000 | (((self.ppuctrl & 3) as u16) << 10)
    }

    // TODO: enum for clarity
    fn get_vram_addr_increment(&self) -> bool {
        (self.ppuctrl & 4) != 0
    }

    fn get_8x8_sprite_pattern_table_addr(&self) -> u16 {
        // NOTE: may not need to actually multiply out etc.
        ((self.ppuctrl & 0b1000) as u16) << 9
    }

    fn get_background_pattern_table_addr(&self) -> u16 {
        ((self.ppuctrl & 0b10000) as u16) << 8
    }

    // TODO: enum or whatever
    fn get_sprite_size(&self) -> bool {
        (self.ppuctrl & 0b100000) != 0
    }

    // TODO: enum or whatever
    fn get_master_slave_mode(&self) -> bool {
        (self.ppuctrl & 0b1000000) != 0
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

    fn is_sprite_zero_hit(&self) -> bool {
        (self.ppustatus & 0b1000000) != 0
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
