#[macro_use]
use super::memory_map as mmap;
use super::pixel_renderer::PixelRenderer;
use mmap::MemoryMap;

mod test;

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
    // NOTE: instead of the fine y scroll value being stored in
    // the upper 3 bits of this, (as is the case on the actual
    // ppu), they are stored in 'fine_xy_scroll'
    // TODO: store fine y scroll in this
    pub current_vram_addr: u16,
    // temporary address, same as above but minus 0x2000 and
    // doesn't get incremented while drawing. this register is
    // shared by 'ppuscroll' and 'ppuaddr' (so writes to these
    // registers go into this). referred to as 't' in the
    // nesdev.com 'ppu scrolling' article
    pub temp_vram_addr: u16,
    // low 3 bits consist of fine x scroll value, high 3 bits
    // consist of fine y scroll
    pub fine_xy_scroll: u8,

    // counter variables used for rendering
    scanline_count: u8,
    // holds the x-position on the screen (not the x-position in
    // the current nametable) of the current tile to be drawn.
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
            current_vram_addr: 0,
            temp_vram_addr: 0,
            fine_xy_scroll: 0,
            scanline_count: 0,
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
                let val = if (self.current_vram_addr >> 8) == 0b111111 {
                    // read directly from vram if address is in range
                    // 0x3f00-0x3ff (palette ram)
                    let val = memory.read_ppu(self.current_vram_addr);
                    // store value at mirrored address (down to 0x2f00-0x2fff)
                    // in read buffer
                    self.ppudata_read_buffer =
                        memory.read_ppu(self.current_vram_addr & !0b01000000000000);
                    val
                } else {
                    // read from read buffer if address is in range 0-0x0x3eff
                    let val = self.ppudata_read_buffer;
                    self.ppudata_read_buffer = memory.read_ppu(self.current_vram_addr);
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
        // set bits 10-11 of 'temp_vram_addr'
        // equal to the low 2 bits of 'val'
        self.temp_vram_addr &= !0b110000000000;
        self.temp_vram_addr |= ((val & 0b11) as u16) << 10;

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
            // write low 3 bits (fine x) to 'self.fine_xy_scroll'
            self.set_fine_x_scroll(val & 0b111);
            // write high 5 bits (coarse x) to low 5 bits
            // of temporary vram address register
            self.temp_vram_addr &= !0b11111;
            self.temp_vram_addr |= (val >> 3) as u16;
        }
        // low bits toggle = 1 => y coordinate is being written
        else {
            // write high 5 bits (coarse y) to
            // bits 5-10 of 'temp_vram_addr'
            self.temp_vram_addr &= !0b1111100000;
            self.temp_vram_addr |= (val as u16 & !0b111) << 2;
            // write low 3 bits (fine y) to bits
            // 12-14 of 'temp_vram_addr'
            self.temp_vram_addr &= !0b111000000000000;
            self.temp_vram_addr |= (val as u16 & 0b111) << 12;
        }

        // reset low bits toggle
        self.low_bits_toggle = !self.low_bits_toggle;
    }

    fn write_ppuaddr(&mut self, val: u8) {
        let mut temp_vram_addr_bytes = self.temp_vram_addr.to_le_bytes();

        if !self.low_bits_toggle {
            // write low 6 bits into bits 8-13 of temporary vram address register
            temp_vram_addr_bytes[1] = val & 0b111111;
            // clear bit 14
            temp_vram_addr_bytes[1] &= !0b01000000;

            // store back
            self.temp_vram_addr = u16::from_le_bytes(temp_vram_addr_bytes);
        } else {
            // set all low bits of temporary vram register equal to 'val'
            temp_vram_addr_bytes[0] = val;
            self.temp_vram_addr = u16::from_le_bytes(temp_vram_addr_bytes);

            // write bits 12-13 into fine y scroll bits of 'self.fine_xy_scroll'
            let fine_y = (self.temp_vram_addr & 0b011_00_00000_00000) >> 7;
            self.fine_xy_scroll = (self.fine_xy_scroll & !0b01100000) | (fine_y as u8);

            // set 'current_vram_addr' equal to 'temp_vram_addr'
            self.current_vram_addr = self.temp_vram_addr;
        }

        self.low_bits_toggle = !self.low_bits_toggle;
    }

    fn write_ppudata(&mut self, val: u8, memory: &mut mmap::Nrom128MemoryMap) {
        memory.write_ppu(self.current_vram_addr, val);

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
        self.current_vram_addr = (self.current_vram_addr + increment) & 0x3fff;
    }

    // increments the fine y scroll bits in 'fine_xy_scroll', potentially
    // overflowing into the coarse y scroll bits in 'current_vram_addr'
    fn increment_vram_addr_y(&mut self) {
        // increment fine y bits
        let (res, carry) = self.fine_xy_scroll.overflowing_add(1 << 5);

        if carry {
            self.fine_xy_scroll &= !0b11100000;

            if (self.current_vram_addr & 0b1111100000) == 29 << 5 {
                // if carry from fine y bits = 1 and coarse y bits
                // = 29 (there are 29 rows of tiles in a frame),
                // clear all coarse y bits and overflow into bit
                // 11 to move to next nametable vertically
                self.current_vram_addr &= !0b1111100000;
                self.current_vram_addr ^= 0b100000000000;
            } else if (self.current_vram_addr & 0b1111100000) == 0b1111100000 {
                // if coarse y = maximum, wrap the value without overflowing into
                // bit 11 and switching nametables (unintended behavior afaik)
                self.current_vram_addr &= !0b1111100000;
            } else {
                // increment coarse y bits of 'current_vram_addr'
                self.current_vram_addr += 1 << 5;
            }
        } else {
            self.fine_xy_scroll = res;
        }
    }

    // increments the coarse y scroll/position bits in 'current_vram_addr'
    // (corresponds to moving one tile to the left in the current nametable)
    fn increment_vram_addr_coarse_x(&mut self) {
        if (self.current_vram_addr & 0b11111) == 0b11111 {
            // if the coarse x component of 'current_vram_addr' is the highest
            // value it can be (31), clear all coarse x bits and overflow
            // into bit 10 (move to next nametable horizontally)
            self.current_vram_addr &= !0b11111;
            self.current_vram_addr ^= 0b10000000000;
        } else {
            // if not highest value, increment normally
            self.current_vram_addr += 1;
        }
    }

    pub fn draw_tile_row(
        &mut self,
        memory: *mut mmap::Nrom128MemoryMap,
        renderer: &mut PixelRenderer,
    ) {
        let fine_y_pos = self.fine_xy_scroll >> 5;
        let mut fine_x_pos = 0;

        // if at start of scanline
        if self.horizontal_tile_count == 0 {
            // if on first scanline
            if self.scanline_count == 0 {
                // copy coarse y bits from 'temp_vram_addr' into 'current_vram_addr'
                self.current_vram_addr &= !0b1111100000;
                self.current_vram_addr |= self.temp_vram_addr & 0b1111100000;

                // copy high 3 bits from 'temp_vram_addr' (corresponding to fine
                // y scroll/position) into high 3 bits of 'fine_xy_scroll'
                self.set_fine_y_scroll(self.temp_vram_addr.to_le_bytes()[1] & 0b11100000);
            }

            // set bits 10-11 in 'current_vram_addr' equal to the nametable select bits in ppuctrl
            self.current_vram_addr &= !0b110000000000;
            self.current_vram_addr |= (self.ppuctrl as u16 & 0b11) << 10;

            // set 'fine_x_pos' equal to the fine x value in 'fine_xy_scroll'
            fine_x_pos = self.fine_xy_scroll & 0b111;

            // copy coarse x scroll/position bits from 'temp_vram_addr' into 'current_vram_addr'
            self.current_vram_addr &= !0b11111;
            self.current_vram_addr |= self.temp_vram_addr & 0b11111;
        }

        log!("fine x, y: ({}, {}), ", fine_x_pos, fine_y_pos);

        // get a raw pointer to the background pattern table. FIXME: explain why
        // FIXME: don't need the raw pointers here, should be able to remove this
        let background_table_ptr = unsafe {
            (((*memory).get_pattern_tables_raw() as usize)
                + self.get_background_pattern_table_addr() as usize)
                as *mut [u8; 0x1000]
        };

        let current_tile = unsafe {
            // get tile index from nametable using 'current_vram_addr' + 0x2000
            let fetch_addr = (self.current_vram_addr & 0xfff) | 0x2000;
            let tile_index = (*memory).read_ppu(fetch_addr);
            logln!(
                "tile_index: {:x} at {:x}",
                tile_index,
                (self.current_vram_addr & 0xfff) | 0x2000
            );

            // get tile from pattern table using the tile index
            // SAFETY: 'current_tile_index' cannot be larger than 255
            *((background_table_ptr as usize + tile_index as usize * 16) as *mut [u8; 16])
        };

        let tile_attribute = { /* TODO: calculate attribute address etc. */ };

        // get the high and low bitplanes for the current row of the current tile
        // TODO: unchecked indexing
        let mut bitplane_low = current_tile[0 + fine_y_pos as usize];
        let mut bitplane_high = current_tile[8 + fine_y_pos as usize];

        let palette = [
            [0, 0, 0, 0],       // black
            [0xff, 0, 0, 0xff], // red
            [0, 0xff, 0, 0xff], // green
            [0, 0, 0xff, 0xff], // blue
        ];

        // start at rightmost pixel in tile and move leftwards
        // until the pixel at 'fine_x_pos' is reached
        for i in 0..8 - fine_x_pos {
            let current_palette_index = (bitplane_low & 1) | ((bitplane_high << 1) & 0b11);
            bitplane_low >>= 1;
            bitplane_high >>= 1;

            let screen_x = ((self.horizontal_tile_count << 3) | (7 - i)) as usize;
            let screen_y = self.scanline_count as usize;

            let pixels = super::pixels_to_u32(renderer);
            pixels[screen_y * 256 + screen_x] =
                u32::from_le_bytes(palette[current_palette_index as usize]);
        }

        // if at end of scanline
        if self.horizontal_tile_count == 31 {
            // if on last scanline
            if self.scanline_count == 239 {
                self.scanline_count = 0;
            }

            self.horizontal_tile_count = 0;
            self.scanline_count += 1;

            // increment fine y
            self.increment_vram_addr_y();
        } else {
            self.horizontal_tile_count += 1;
            self.increment_vram_addr_coarse_x();
        }
    }

    fn comments() {
        // start fetching from current addr

        //
        // TODO: ..
        // for each entire screen rendered:
        //     re-calculate nametable fetch addr ('v') (from base
        //     nametable addr and 'temp_vram_addr' register)
        // for each pixel in a scanline:
        //     increment fetch addr x
        // for every second row of a tile in a scanline:
        //     calculate attribute address
        //     compute pixel color from palette
        // for each scanline:
        //     update horizontal bits of nametable fetch addr from 'temp_vram_addr'
        //     increment fetch addr y
        //
        // fetch from addr
        // do other stuff that i haven't gotten to yet
        // NOTE: buggy behavior with oam when oamaddr is > 8
        // display contents
    }

    // NOTE: this expects the fine x value to be in the low 3 bits of 'fine_x'
    fn set_fine_x_scroll(&mut self, fine_x: u8) {
        self.fine_xy_scroll = (self.fine_xy_scroll & !0b111) | fine_x;
    }

    // NOTE: this expects the fine y value to be in the high 3 bits of 'fine_y'
    fn set_fine_y_scroll(&mut self, fine_y: u8) {
        self.fine_xy_scroll = (self.fine_xy_scroll & !0b11100000) | fine_y;
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
