#[macro_use]
use super::{cpu, util};
use super::memory_map::PpuMemoryMap;
use super::pixel_renderer::PixelRenderer;

mod palette;
pub mod test;

pub struct Ppu<'a> {
    // object attribute memory
    oam: Oam,
    pub cycle_count: u32,
    pub renderer: PixelRenderer,
    // TODO: investigate how much code bloat generics would cause
    // - would it be worth using it over a trait object?
    pub memory: &'a mut dyn PpuMemoryMap,

    // registers
    // TODO: remove pub
    pub ppuctrl: u8,
    pub ppumask: u8,
    // NOTE: first 5 bits of this register contain the least significant
    // bits of any value previously written into a ppu register.
    // shouldn't need to emulate this, but may be worth noting
    pub ppustatus: u8,
    // NOTE: on some ppu chips, there are bugs relating to
    // writing to oamaddr. not sure if these need to be
    // emulated either, but it may be worth keeping in mind
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
    current_vram_addr: VramAddrRegister,
    // temporary address, same as above but  doesn't get
    // incremented while drawing. this register is shared
    // by 'ppuscroll' and 'ppuaddr' (so writes to these
    // registers go into this). referred to as 't' in the
    // nesdev.com 'ppu scrolling' article
    temp_vram_addr: VramAddrRegister,
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

#[derive(Copy, Clone, Default)]
#[repr(C)]
struct OamEntry {
    y_coord: u8,
    x_coord: u8,
    tile_num: u8,
    attribute: u8,
}

struct Oam {
    entries: [OamEntry; 64],
}

impl Oam {
    fn as_bytes<'a>(&'a self) -> &'a [u8; 64 * 4] {
        unsafe { std::mem::transmute(self) }
    }

    pub fn as_bytes_mut<'a>(&'a mut self) -> &'a mut [u8; 64 * 4] {
        unsafe { std::mem::transmute(self) }
    }

    fn get_byte(&self, index: u8) -> u8 {
        unsafe { *self.as_bytes().get_unchecked(index as usize) }
    }

    fn set_byte(&mut self, index: u8, val: u8) {
        unsafe { *self.as_bytes_mut().get_unchecked_mut(index as usize) = val };
    }

    #[inline]
    unsafe fn get_sprite_unchecked(&mut self, index: u8) -> OamEntry {
        *(self.as_bytes_mut().get_unchecked_mut(index as usize) as *mut _ as *mut _)
    }
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
            oam: Oam {
                entries: [OamEntry::default(); 64],
            },
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

        fn read_oamdata(ppu: &mut Ppu) -> u8 {
            // TODO: special values when rendering
            ppu.oam.get_byte(ppu.oamaddr)
        }

        fn read_ppudata(ppu: &mut Ppu) -> u8 {
            let val = if (ppu.current_vram_addr.inner >> 8) == 0b111111 {
                // read directly from vram if address is in range
                // 0x3f00-0x3ff (palette ram)
                let val = ppu.memory.read(ppu.current_vram_addr.get_addr());
                // store value at mirrored address (down to 0x2f00-0x2fff)
                // in read buffer
                ppu.ppudata_read_buffer = ppu
                    .memory
                    .read(ppu.current_vram_addr.get_addr() & !0b01000000000000);
                val
            } else {
                // read from read buffer if address is in range 0-0x0x3eff
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
            ppu.ppuctrl = val;
            ppu.set_ppustatus_low_bits(val);

            if ppu.is_vblank_nmi_enabled() && ppu.is_vblank() {
                cpu.nmi = true;
            }
        }

        fn write_oamdata(ppu: &mut Ppu, val: u8) {
            if ppu.is_currently_rendering() {
                // ignore attemps to write when rendering
                return;
            }

            ppu.oam.set_byte(ppu.oamaddr, val);
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

    // used for writing to 'oamdma' (0x4014 in the cpu memory map)
    pub fn write_oamdma(&mut self, val: u8, cpu: &mut cpu::Cpu) {
        self.set_ppustatus_low_bits(val);
        // if 'val' is $XX, start address should be $XX00
        let start_addr = (val as u16) << 8;

        for (i, addr) in ((start_addr)..=(start_addr + 0xff)).enumerate() {
            let byte = self.memory.read(addr);
            self.oam.set_byte(self.oamaddr, byte);
            self.oamaddr = self.oamaddr.wrapping_add(1);

            cpu.cycle_count += 2;

            // catch the ppu up to the cpu on every 4th iteration
            if i % 4 == 0 {
                self.catch_up(cpu);
            }
        }

        // set nmi = false in case a call to 'catch_up()' set it to true
        // TODO: less hacky solution (pass 'do_nmi' param to 'catch_up()' or something)
        cpu.nmi = false;

        // in total, dma should take 513 cpu cycles (or 514 if on an odd cpu cycle)
        cpu.cycle_count += 1 + (cpu.cycle_count % 2);
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

            if ppu.is_sprites_enable() || ppu.is_background_enable() {
                ppu.transfer_temp_x_and_nt_select();
                ppu.transfer_temp_y();
            }

            ppu.current_scanline = 0;
            ppu.cycle_count += 340 + ppu.even_frame as u32;

            PpuState::MidFrame
        }

        fn step_visible_line_full(ppu: &mut Ppu) -> PpuState {
            ppu.current_scanline_dot = 1;

            // if background rendering is disabled
            if !ppu.is_background_enable() {
                for _ in 0..32 {
                    ppu.draw_tile_row_backdrop();
                }
            } else {
                for _ in 0..32 {
                    //  OPTIMIZE: make separate drawing
                    // algorithm for drawing entire scanlines
                    let pixels_drawn = ppu.draw_tile_row();
                    ppu.current_scanline_dot += pixels_drawn as u16;
                    ppu.increment_vram_addr_coarse_x();
                }

                debug_assert_eq!(ppu.current_scanline_dot, 257);

                // increment fine y
                ppu.increment_vram_addr_y();
            }

            if ppu.is_sprites_enable() || ppu.is_background_enable() {
                ppu.transfer_temp_x_and_nt_select();
            }

            ppu.current_scanline += 1;
            ppu.cycle_count += 341;
            ppu.current_scanline_dot = 0;

            PpuState::MidFrame
        }

        fn step_idle_line_full(ppu: &mut Ppu) -> PpuState {
            if ppu.is_sprites_enable() || ppu.is_background_enable() {
                ppu.transfer_temp_x_and_nt_select();
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
                ppu.transfer_temp_x_and_nt_select();
            }

            ppu.current_scanline = 242;
            ppu.cycle_count += 341;

            PpuState::MidFrame
        }

        fn step_vblank_line_full(ppu: &mut Ppu) -> PpuState {
            if ppu.is_sprites_enable() || ppu.is_background_enable() {
                ppu.transfer_temp_x_and_nt_select();
            }

            ppu.current_scanline += 1;
            ppu.cycle_count += 341;

            PpuState::MidFrame
        }

        fn step_last_vblank_line_full(ppu: &mut Ppu) -> PpuState {
            if ppu.is_sprites_enable() || ppu.is_background_enable() {
                ppu.transfer_temp_x_and_nt_select();
                ppu.even_frame = !ppu.even_frame;
            }

            ppu.current_scanline = -1;
            ppu.cycle_count += 341;

            PpuState::FrameDone
        }
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
                    // don't increment cycles on odd frames (idle cycle is skipped)
                    ppu.cycle_count += ppu.even_frame as u32;
                }
                1 => {
                    // clear vblank flag
                    ppu.set_vblank(false);

                    ppu.cycle_count += 7;
                    ppu.current_scanline_dot += 7;
                }
                2..=255 => {
                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
                256 => {
                    if ppu.is_sprites_enable() || ppu.is_background_enable() {
                        ppu.transfer_temp_x_and_nt_select();
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
                        ppu.transfer_temp_y();
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
                }
                1..=256 => {
                    // if background rendering is disabled
                    if !ppu.is_background_enable() {
                        // NOTE: if the background is disabled mid-scanline,
                        // there will be weird artifacts
                        ppu.draw_tile_row_backdrop();
                        ppu.cycle_count += 8;
                    } else {
                        // draw one row of a tile (or less, if the current
                        // tile straddles the screen boundary).
                        let pixels_drawn = ppu.draw_tile_row();
                        ppu.cycle_count += pixels_drawn as u32;
                        ppu.current_scanline_dot += pixels_drawn as u16;

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
                    if ppu.is_sprites_enable() || ppu.is_background_enable() {
                        ppu.transfer_temp_x_and_nt_select();
                    }

                    ppu.cycle_count += 8;
                    ppu.current_scanline_dot += 8;
                }
                258..=336 => {
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
                        ppu.transfer_temp_x_and_nt_select();
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
                        ppu.transfer_temp_x_and_nt_select();
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

    // draws a horizontal line of pixels starting at 'current_scanline_dot'
    // - 1 and stopping at the end of the current tile in the background
    // nametable (or the end of the screen, if the current tile happens to
    // poke outside of it). returns how many pixels were drawn
    fn draw_tile_row(&mut self) -> u8 {
        // NOTE: as with some other functions on 'Ppu', 'draw_tile_row()'
        // is split into smaller subfunctions to help readability

        {
            return draw_tile_row_main(self);
        }

        fn draw_tile_row_main(ppu: &mut Ppu) -> u8 {
            let current_tile = get_current_tile(ppu);
            let palette_index = get_tile_palette_index(ppu);

            // get the high and low bitplanes for the current row of the current tile
            let fine_y = ppu.current_vram_addr.get_fine_y();
            let bitplane_low = unsafe { *current_tile.get_unchecked(0 + fine_y as usize) };
            let bitplane_high = unsafe { *current_tile.get_unchecked(8 + fine_y as usize) };

            // calculate the index of the current tile in the current scanline
            let horizontal_tile_count =
                ((ppu.current_scanline_dot - 1 + ppu.fine_x_scroll as u16) >> 3) as u8 & 0x3f;

            let pixels_range = if horizontal_tile_count == 32 {
                // if on tile 32 (meaning 'fine_x_scroll' is non-zero and
                // this is the last tile in the scanline), start drawing
                // at offset 0 from current tile and stop at end of screen
                let screen_x = (ppu.current_scanline_dot - 1) as u8;
                std::ops::Range {
                    start: 0,
                    end: 8 - (screen_x % 8),
                }
            } else if horizontal_tile_count == 0 {
                // if on first tile, start drawing pixel at 'fine_x_scroll'
                std::ops::Range {
                    start: ppu.fine_x_scroll,
                    end: 8,
                }
            } else {
                // if on any other tile, draw all pixels in it
                std::ops::Range { start: 0, end: 8 }
            };

            log!("tile: {}, ", horizontal_tile_count);

            pixels_range
                .into_iter()
                .enumerate()
                .map(|(i, tile_offset)| {
                    let color_index_low = (bitplane_low >> (7 - tile_offset)) & 1;
                    let color_index_high = ((bitplane_high >> (7 - tile_offset)) << 1) & 2;
                    let color_index = color_index_low | color_index_high;

                    let color = get_pixel_color(ppu, palette_index, color_index);

                    let screen_x = (ppu.current_scanline_dot - 1) as usize + i;
                    let screen_y = ppu.current_scanline as usize;
                    let framebuffer = util::pixels_to_u32(&mut ppu.renderer);
                    // TODO: OPTIMIZE: unchecked indexing
                    framebuffer[screen_y * 256 + screen_x] = color;

                    log!(
                        "(x: {}, tile_offset: {}), ",
                        ppu.current_scanline_dot - 1 + i as u16,
                        tile_offset
                    );
                })
                // return amt of pixels drawn
                .count() as u8
        }

        fn get_current_tile(ppu: &mut Ppu) -> [u8; 16] {
            // get tile index from nametable using 'current_vram_addr' + 0x2000
            let addr = ppu.current_vram_addr.get_addr() | 0x2000;
            let tile_index = ppu.memory.read(addr);
            let background_table_addr = ppu.get_background_pattern_table_addr() as usize;
            let background_table_ptr = ppu.memory.get_pattern_tables();

            unsafe {
                // get tile from pattern table using the tile index
                // SAFETY: 'current_tile_index' * 16 cannot be
                // larger than 0x1000 (the size of a pattern table)
                *((background_table_ptr
                    .get_unchecked_mut(background_table_addr + tile_index as usize * 16))
                    as *mut _ as *mut [u8; 16])
            }
        }

        fn get_tile_palette_index(ppu: &mut Ppu) -> u8 {
            let coarse_y = ppu.current_vram_addr.get_coarse_y();
            let coarse_x = ppu.current_vram_addr.get_coarse_x();

            // calculate the address of the current tile's 'attribute' in the attribute table
            let attribute_addr = 0x23c0
                | (ppu.current_vram_addr.get_nametable_select() as u16) << 10
                | (coarse_y << 1) as u16 & 0b111000
                | (coarse_x >> 2) as u16;

            // get the 'attribute' byte from the attribute table
            let attribute = ppu.memory.read(attribute_addr);
            // calculate how much to shift 'attribute' by to get the current tile's palette index
            let shift_amt = ((coarse_y << 1) & 0b100) | (coarse_x & 0b10);

            (attribute >> shift_amt) & 0b11
        }

        fn get_pixel_color(ppu: &mut Ppu, palette_index: u8, color_index: u8) -> u32 {
            let mut addr = 0x3f00 | ((palette_index as u16) << 2);
            addr |= color_index as u16;

            if ((ppu.current_scanline_dot - 1) < 8 && !ppu.is_background_left_column_enable())
                || color_index == 0
            {
                // set 'addr' to point to universal backdrop color
                addr = 0x3f00;
            }

            let final_color_index = ppu.memory.read(addr);
            ppu.get_color_from_index(final_color_index)
        }
    }

    // draws 8 pixels of backdrop color (or if 'current_vram_addr'
    // >= 0x3f00, draws the color 'current_vram_addr' points to)
    fn draw_tile_row_backdrop(&mut self) {
        for _ in 0..8 {
            let color_index_addr = if self.current_vram_addr.get_addr() >= 0x3f00 {
                logln!("background palette hack triggered");
                self.current_vram_addr.get_addr()
            } else {
                0x3f00
            };

            let color_index = self.memory.read(color_index_addr);
            let color = self.get_color_from_index(color_index);

            let screen_x = (self.current_scanline_dot - 1) as usize;
            let screen_y = self.current_scanline as usize;
            let framebuffer = util::pixels_to_u32(&mut self.renderer);
            framebuffer[screen_y * 256 + screen_x] = color;

            self.current_scanline_dot = self.current_scanline_dot.wrapping_add(1);
        }

        if self.current_scanline_dot == 0 {
            self.current_scanline += 1;
        }
    }

    fn get_color_from_index(&self, mut index: u8) -> u32 {
        if self.is_greyscale_enabled() {
            index &= 0x30;
        } else {
            index &= 0x3f;
        }

        let emphasis = self.ppumask >> 5;

        unsafe {
            u32::from_le_bytes([
                palette::COLORS
                    .get_unchecked(emphasis as usize)
                    .get_unchecked(index as usize)[0],
                palette::COLORS
                    .get_unchecked(emphasis as usize)
                    .get_unchecked(index as usize)[1],
                palette::COLORS
                    .get_unchecked(emphasis as usize)
                    .get_unchecked(index as usize)[2],
                1,
            ])
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

    // transfers the coarse and fine y bits from 'temp_vram_addr' to 'current_vram_addr'
    fn transfer_temp_y(&mut self) {
        let temp_coarse_y = self.temp_vram_addr.get_coarse_y();
        self.current_vram_addr.set_coarse_y(temp_coarse_y);

        let temp_fine_y = self.temp_vram_addr.get_fine_y();
        self.current_vram_addr.set_fine_y(temp_fine_y);
    }

    // transfers the coarse x and nametable select bits from 'temp_vram_addr'
    // to 'current_vram_addr'
    fn transfer_temp_x_and_nt_select(&mut self) {
        let temp_nametable = self.temp_vram_addr.get_nametable_select();
        self.current_vram_addr.set_nametable_select(temp_nametable);

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

    fn is_red_emphasized(&self) -> bool {
        (self.ppumask & 0b100000) != 0
    }

    fn is_green_emphasized(&self) -> bool {
        (self.ppumask & 0b1000000) != 0
    }

    fn is_blue_emphasized(&self) -> bool {
        (self.ppumask >> 7) != 0
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
