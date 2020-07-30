#[macro_use]
use super::{memory_map as mmap, util};
use super::pixel_renderer::PixelRenderer;
use mmap::MemoryMap;

pub mod test;

#[rustfmt::skip]
// 'Wavebeam' color palette
static COLORS: [[u8; 3]; 64] = [
    [0x6b, 0x6b, 0x6b], [0x00, 0x1b, 0x87], [0x21, 0x00, 0x9a], [0x40, 0x00, 0x8c],
    [0x60, 0x00, 0x67], [0x64, 0x00, 0x1e], [0x59, 0x08, 0x00], [0x46, 0x16, 0x00],
    [0x26, 0x36, 0x00], [0x00, 0x45, 0x00], [0x00, 0x47, 0x08], [0x00, 0x42, 0x1d],
    [0x00, 0x36, 0x59], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00],
    [0xb4, 0xb4, 0xb4], [0x15, 0x55, 0xce], [0x43, 0x37, 0xea], [0x71, 0x24, 0xda],
    [0x9c, 0x1a, 0xb6], [0xaa, 0x11, 0x64], [0xa8, 0x2e, 0x00], [0x87, 0x4b, 0x00],
    [0x66, 0x6b, 0x00], [0x21, 0x83, 0x00], [0x00, 0x8a, 0x00], [0x00, 0x81, 0x44],
    [0x00, 0x76, 0x91], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00],
    [0xff, 0xff, 0xff], [0x63, 0xaf, 0xff], [0x82, 0x96, 0xff], [0xc0, 0x7d, 0xfe],
    [0xe9, 0x77, 0xff], [0xf5, 0x72, 0xcd], [0xf4, 0x88, 0x6b], [0xdd, 0xa0, 0x29],
    [0xbd, 0xbd, 0x0a], [0x89, 0xd2, 0x0e], [0x5c, 0xde, 0x3e], [0x4b, 0xd8, 0x86],
    [0x4d, 0xcf, 0xd2], [0x50, 0x50, 0x50], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00],
    [0xff, 0xff, 0xff], [0xbe, 0xe1, 0xff], [0xd4, 0xd4, 0xff], [0xe3, 0xca, 0xff],
    [0xf0, 0xc9, 0xff], [0xff, 0xc6, 0xe3], [0xff, 0xce, 0xc9], [0xf4, 0xdc, 0xaf],
    [0xeb, 0xe5, 0xa1], [0xd2, 0xef, 0xa2], [0xbe, 0xf4, 0xb5], [0xb8, 0xf1, 0xd0],
    [0xb8, 0xed, 0xf1], [0xbd, 0xbd, 0xbd], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00],
];

fn get_color_from_index(mut index: u8) -> u32 {
    index &= 0x3f;
    unsafe {
        u32::from_le_bytes([
            COLORS.get_unchecked(index as usize)[0],
            COLORS.get_unchecked(index as usize)[1],
            COLORS.get_unchecked(index as usize)[2],
            1,
        ])
    }
}

pub struct Ppu {
    // object attribute memory
    oam: Oam,

    // registers
    // TODO: remove pub
    pub ppuctrl: u8,
    ppumask: u8,
    // NOTE: first 5 bits of this register contain the least significant
    // significant bits of any value previously written into a ppu
    // register. shouldn't need to emulate this, but may be worth noting
    ppustatus: u8,
    // NOTE: on some ppu chips, there are bugs relating to
    // writing to oamaddr. not sure if these need to be
    // emulated either, but it may be worth keeping in mind
    oamaddr: u8,
    ppudata_read_buffer: u8,

    // internal registers
    // used to toggle whether reads and writes to 'ppuscroll'
    // and 'ppuaddr' access the high or low bits of the register.
    // referred to as 'w' in the nesdev.com 'ppu scrolling' article
    // OPTIMIZE: pack these together (bitfields, somehow?)
    low_bits_toggle: bool,
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
    current_scanline: u8,
    current_screen_x: u8,
    horizontal_tile_count: u8,
}

union Oam {
    entries: [OamEntry; 64],
    bytes: [u8; 64 * 4],
}

#[derive(Copy, Clone, Default)]
#[repr(C)]
struct OamEntry {
    y_coord: u8,
    x_coord: u8,
    tile_num: u8,
    attribute: u8,
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
        self.inner = (self.inner & !0b111_00_00000_00000) | (fine_y as u16) << 12;
    }

    fn get_nametable_select(self) -> u8 {
        ((self.inner & 0b11_00000_00000) >> 10) as u8
    }

    fn set_nametable_select(&mut self, select: u8) {
        self.inner = (self.inner & !0b11_00000_00000) | ((select as u16) << 10);
    }
}

// TODO: make lookup table of colors in srgb format,
// where the index corresponds to the value or whatever

impl Default for Ppu {
    fn default() -> Self {
        Ppu {
            oam: Oam {
                entries: [OamEntry::default(); 64],
            },
            ppuctrl: 0,
            ppumask: 0,
            ppustatus: 0,
            oamaddr: 0,
            ppudata_read_buffer: 0,
            low_bits_toggle: false,
            current_vram_addr: VramAddrRegister { inner: 0 },
            temp_vram_addr: VramAddrRegister { inner: 0 },
            fine_x_scroll: 0,
            current_scanline: 0,
            current_screen_x: 0,
            horizontal_tile_count: 0,
        }
    }
}

impl Ppu {
    // used for reading the registers located in the cpu memory map at 0x2000-0x2007
    pub fn read_register_by_index(&mut self, index: u8, memory: &mmap::Nrom128MemoryMap) -> u8 {
        match index {
            // ppuctrl
            0 => 0,
            // ppumask
            1 => self.ppumask,
            // ppustatus
            2 => {
                // clear low bits toggle
                self.low_bits_toggle = false;
                self.ppustatus
            }
            // oamaddr
            3 => 0,
            // oamdata
            // TODO: special values when rendering
            4 => unsafe { *self.oam.bytes.get_unchecked(self.oamaddr as usize) },
            // ppuscroll | ppuaddr
            5 | 6 => 0, // FIXME: should these reset the low bits toggle as well??
            // ppudata
            7 => {
                let val = if (self.current_vram_addr.inner >> 8) == 0b111111 {
                    // read directly from vram if address is in range
                    // 0x3f00-0x3ff (palette ram)
                    let val = memory.read_ppu(self.current_vram_addr.get_addr());
                    // store value at mirrored address (down to 0x2f00-0x2fff)
                    // in read buffer
                    self.ppudata_read_buffer =
                        memory.read_ppu(self.current_vram_addr.get_addr() & !0b01000000000000);
                    val
                } else {
                    // read from read buffer if address is in range 0-0x0x3eff
                    let val = self.ppudata_read_buffer;
                    self.ppudata_read_buffer = memory.read_ppu(self.current_vram_addr.get_addr());
                    val
                };

                if !self.is_rendering() {
                    // if not currently rendering, increment normally
                    self.increment_vram_addr();
                } else {
                    // if currently rendering, increment the bits of the address
                    // corresponding to the y position and coarse x position (this
                    // is afaik unintended behavior)
                    self.increment_vram_addr_coarse_x();
                    self.increment_vram_addr_y();
                }

                val
            }
            _ => 0,
        }
    }

    pub fn write_register_by_index(
        &mut self,
        index: u8,
        val: u8,
        memory: &mut mmap::Nrom128MemoryMap,
    ) {
        match index {
            // ppuctrl
            0 => self.write_ppuctrl(val),
            // ppumask
            1 => self.ppumask = val,
            // ppustatus, ignore attemps to write
            2 => return,
            // oamaddr
            3 => self.oamaddr = val,
            // oamdata
            4 => self.write_oamdata(val),
            // ppuscroll
            5 => self.write_ppuscroll(val),
            // ppuaddr
            6 => self.write_ppuaddr(val),
            // ppudata
            7 => self.write_ppudata(val, memory),
            _ => (),
        }
    }

    fn write_ppuctrl(&mut self, val: u8) {
        // set bits 10-11 of 'temp_vram_addr' equal to the low 2 bits of 'val'
        self.temp_vram_addr.set_nametable_select(val & 0b11);

        self.ppuctrl = val;
    }

    fn write_oamdata(&mut self, val: u8) {
        if self.is_rendering() {
            // ignore attemps to write when rendering
            return;
        }

        unsafe { *self.oam.bytes.get_unchecked_mut(self.oamaddr as usize) = val };
        self.oamaddr = self.oamaddr.wrapping_add(1);
    }

    fn write_ppuscroll(&mut self, val: u8) {
        // low bits toggle = 0 => x coordinate is being written
        if !self.low_bits_toggle {
            // write low 3 bits (fine x) to 'self.fine_x_scroll'
            self.set_fine_x_scroll(val & 0b111);
            // write high 5 bits (coarse x) to low 5 bits
            // of temporary vram address register
            self.temp_vram_addr.set_coarse_x(val >> 3);
        }
        // low bits toggle = 1 => y coordinate is being written
        else {
            // write high 5 bits (coarse y) to
            // bits 5-10 of 'temp_vram_addr'
            self.temp_vram_addr.set_coarse_y(val >> 3);
            // write low 3 bits (fine y) to bits
            // 12-14 of 'temp_vram_addr'
            self.temp_vram_addr.set_fine_y(val & 0b111);
        }

        // reset low bits toggle
        self.low_bits_toggle = !self.low_bits_toggle;
    }

    fn write_ppuaddr(&mut self, val: u8) {
        let mut temp_vram_addr_bytes = self.temp_vram_addr.inner.to_le_bytes();

        if !self.low_bits_toggle {
            // write low 6 bits into bits 8-13 of temporary
            // vram address register while clearing bit 14
            temp_vram_addr_bytes[1] = val & 0b0111111;
            // store back
            self.temp_vram_addr.inner = u16::from_le_bytes(temp_vram_addr_bytes);
        } else {
            // set all low bits of temporary vram register equal to 'val'
            temp_vram_addr_bytes[0] = val;
            self.temp_vram_addr.inner = u16::from_le_bytes(temp_vram_addr_bytes);

            // set 'current_vram_addr' equal to 'temp_vram_addr'
            self.current_vram_addr = self.temp_vram_addr;
        }

        self.low_bits_toggle = !self.low_bits_toggle;
    }

    fn write_ppudata(&mut self, val: u8, memory: &mut mmap::Nrom128MemoryMap) {
        memory.write_ppu(self.current_vram_addr.get_addr(), val);

        // increment 'current_vram_addr' (same as when reading ppudata)
        if !self.is_rendering() {
            self.increment_vram_addr();
        } else {
            self.increment_vram_addr_coarse_x();
            self.increment_vram_addr_y();
        }
    }

    pub fn write_oamdma(
        &mut self,
        val: u8,
        cpu_cycle_count: &mut u64,
        memory: &mut mmap::Nrom128MemoryMap,
    ) {
        // if 'val' is $XX, start address should be $XX00
        let start_addr = (val as u16) << 8;

        for addr in (start_addr)..=(start_addr + 0xff) {
            let byte = memory.read_ppu(addr);
            unsafe { *self.oam.bytes.get_unchecked_mut(self.oamaddr as usize) = byte };
            self.oamaddr = self.oamaddr.wrapping_add(1);
        }

        // increment cycle count by 513 (or 514 if on an odd cpu cycle)
        *cpu_cycle_count += 513 + (*cpu_cycle_count % 2);
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
    // (corresponds to moving one tile to the left in the current nametable)
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

    // draws a horizontal line of pixels starting at 'current_screen_x' and
    // stopping at the end of the current tile in the background nametable
    // (or the end of the screen, if the current tile happens to poke outside
    // of it). returns whether drawing is finished (entire screen is drawn)
    // TODO: set vblank when drawing is finished
    pub fn draw_tile_row(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        renderer: &mut PixelRenderer,
    ) -> bool {
        // if background rendering is disabled
        if !self.is_background_enable() {
            // draw 8 pixels of backdrop color (or if 'current_vram_addr'
            // >= 0x3f00, draw the color 'current_vram_addr' points to)
            for _ in 0..8 {
                let color_index_addr = if self.current_vram_addr.get_addr() >= 0x3f00 {
                    logln!("background palette hack triggered");
                    self.current_vram_addr.get_addr()
                } else {
                    0x3f00
                };

                let color_index = memory.read_ppu(color_index_addr);
                let color = get_color_from_index(color_index);

                let pixels = util::pixels_to_u32(renderer);
                let screen_x = self.current_screen_x as usize;
                let screen_y = self.current_scanline as usize;
                pixels[screen_y * 256 + screen_x] = color;

                self.current_screen_x = self.current_screen_x.wrapping_add(1);
            }

            let mut is_finished = false;
            // if at end of scanline (and 'current_screen_x' has wrapped around to zero)
            if self.current_screen_x == 0 {
                if self.current_scanline == 239 {
                    is_finished = true;
                    self.current_scanline = 0;
                } else {
                    self.current_scanline += 1;
                }

                self.horizontal_tile_count = 0;
            } else {
                self.horizontal_tile_count += 1;
            }

            return is_finished;
        }

        // if at start of scanline
        if self.horizontal_tile_count == 0 {
            // if on first scanline
            if self.current_scanline == 0 {
                // copy coarse y bits from 'temp_vram_addr' into 'current_vram_addr'
                let temp_coarse_y = self.temp_vram_addr.get_coarse_y();
                self.current_vram_addr.set_coarse_y(temp_coarse_y);

                // copy fine y bits from 'temp_vram_addr' into 'current_vram_addr'
                let temp_fine_y = self.temp_vram_addr.get_fine_y();
                self.current_vram_addr.set_fine_y(temp_fine_y);
            }

            // copy nametable select bits from 'temp_vram_addr' into 'current_vram_addr'
            let temp_nametable = self.temp_vram_addr.get_nametable_select();
            self.current_vram_addr.set_nametable_select(temp_nametable);

            // copy coarse x scroll/position bits from 'temp_vram_addr' into 'current_vram_addr'
            let temp_coarse_x = self.temp_vram_addr.get_coarse_x();
            self.current_vram_addr.set_coarse_x(temp_coarse_x);
        }

        let current_tile = {
            // get tile index from nametable using 'current_vram_addr' + 0x2000
            let addr = (self.current_vram_addr.inner & 0xfff) | 0x2000;
            let tile_index = memory.read_ppu(addr);
            let background_table_ptr = memory.get_pattern_tables();

            unsafe {
                // get tile from pattern table using the tile index
                // SAFETY: 'current_tile_index' * 16 cannot be
                // larger than 0x1000 (the size of a nametable)
                *((background_table_ptr.get_unchecked_mut(
                    self.get_background_pattern_table_addr() as usize + tile_index as usize * 16,
                )) as *mut _ as *mut [u8; 16])
            }
        };

        let palette_index = {
            // calculate the address of the current tile's 'attribute' in the attribute table
            let attribute_addr = 0x23c0
                | (self.current_vram_addr.get_nametable_select() as u16) << 10
                | (self.current_vram_addr.get_coarse_y() << 1) as u16 & 0b111000
                | (self.current_vram_addr.get_coarse_x() >> 2) as u16;

            // get the 'attribute' byte from the attribute table
            let attribute = memory.read_ppu(attribute_addr);
            // calculate how much to shift 'attribute' by to get the current tile's palette index
            let shift_amt = (self.horizontal_tile_count & 2) | ((self.current_scanline >> 2) & 4);

            (attribute >> shift_amt) & 0b11
        };

        // get the high and low bitplanes for the current row of the current tile
        let fine_y = self.current_vram_addr.get_fine_y();
        let bitplane_low = unsafe { *current_tile.get_unchecked(0 + fine_y as usize) };
        let bitplane_high = unsafe { *current_tile.get_unchecked(8 + fine_y as usize) };

        let pixels_range = if self.horizontal_tile_count == 32 {
            // if on tile 32 (meaning 'fine_x_scroll' is non-zero and
            // this is the last tile in the scanline), start drawing
            // at offset 0 from current tile and stop at end of screen
            0..(8 - (self.current_screen_x % 8))
        } else if self.horizontal_tile_count == 0 {
            // if on first tile, start drawing pixel at 'fine_x_scroll'
            self.fine_x_scroll..8
        } else {
            // if on any other tile, draw all pixels in it
            0..8
        };

        log!("tile: {}, ", self.horizontal_tile_count);

        for i in pixels_range {
            let color_index_low = (bitplane_low >> (7 - i)) & 1;
            let color_index_high = ((bitplane_high >> (7 - i)) << 1) & 2;
            let color_index = color_index_low | color_index_high;

            log!("(x: {}, i: {}), ", self.current_screen_x, i);

            let pixels = util::pixels_to_u32(renderer);
            let color = {
                let mut addr = 0x3f00 | ((palette_index as u16) << 2);
                addr += color_index as u16;

                if (self.current_screen_x < 8 && !self.is_background_left_column_enable())
                    || color_index == 0
                {
                    // set 'addr' to point to universal backdrop color
                    addr = 0x3f00;
                }

                let final_color_index = memory.read_ppu(addr);
                get_color_from_index(final_color_index)
            };

            // TODO: OPTIMIZE: unchecked indexing
            pixels[self.current_scanline as usize * 256 + self.current_screen_x as usize] = color;
            self.current_screen_x = self.current_screen_x.wrapping_add(1);
        }
        logln!("");

        let mut is_finished = false;

        if self.current_screen_x == 0 {
            // if on last scanline
            if self.current_scanline == 239 {
                self.current_scanline = 0;
                is_finished = true; // TODO: set vblank = true, etc.
            } else {
                self.current_scanline += 1;
                // increment fine y
                self.increment_vram_addr_y();
            }

            self.horizontal_tile_count = 0;
        } else {
            self.horizontal_tile_count += 1;
            self.increment_vram_addr_coarse_x();
        }

        is_finished
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
        self.ppustatus |= (overflow as u8) << 5;
    }

    fn is_sprite_zero_hit(&self) -> bool {
        (self.ppustatus & 0b1000000) != 0
    }

    fn set_sprite_zero_hit(&mut self, hit: bool) {
        self.ppustatus |= (hit as u8) << 6;
    }

    fn is_vblank(&self) -> bool {
        (self.ppustatus >> 7) != 0
    }

    fn set_vblank(&mut self, vblank: bool) {
        self.ppustatus |= (vblank as u8) << 7;
    }

    fn is_rendering(&self) -> bool {
        !self.is_vblank() && (self.is_sprites_enable() || self.is_background_enable())
    }
}
