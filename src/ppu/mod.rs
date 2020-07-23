use super::memory_map as mmap;
use mmap::MemoryMap;

mod test;

pub struct Ppu {
    // object attribute memory
    oam: Oam,
    // registers
    ppuctrl: u8,
    ppumask: u8,
    // NOTE: first 5 bits of this register contain the least
    // significant bits of any value previously written into
    // a ppu register. shouldn't need to emulate this, but
    ppustatus: u8,
    // NOTE: on some ppu chips, there are bugs relating to
    // writing to oamaddr. not sure if these need to be
    // emulated either, but it may be worth keeping in mind
    oamaddr: u8,
    ppudata: u8,
    ppudata_read_buffer: u8,
    // internal registers
    // used to toggle whether reads and writes to 'ppuscroll'
    // and 'ppuaddr' access the high or low bits of the register.
    // referred to as 'w' in the nesdev.com 'ppu scrolling' article
    // OPTIMIZE: pack these together (bitfields, somehow?)
    high_bits_toggle: bool,
    // address of the current tile to be fetched and drawn. points
    // to a byte in one of the nametables in vram. referred to
    // as 'v' in the nesdev.com 'ppu scrolling' article
    // NOTE: instead of the fine y scroll value being stored in
    // the upper 3 bits of this, (as is the case on the actual
    // ppu), they are stored in 'fine_xy_scroll'
    current_vram_addr: u16,
    // temporary address, same as above but minus 0x2000 and
    // doesn't get incremented while drawing. this register is
    // shared by 'ppuscroll' and 'ppuaddr' (so writes to these
    // registers go into this). referred to as 't' in the
    // nesdev.com 'ppu scrolling' article
    temp_vram_addr: u16,
    // low 3 bits consist of fine x scroll value, high 3 bits
    // consist of fine y scroll
    fine_xy_scroll: u8,
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

// TODO: make associatvie array of colors in srgb format,
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
            ppudata: 0,
            ppudata_read_buffer: 0,
            high_bits_toggle: false,
            current_vram_addr: 0,
            temp_vram_addr: 0,
            fine_xy_scroll: 0,
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
                // clear high bits toggle
                self.high_bits_toggle = false;
                self.ppustatus
            }
            // oamaddr
            3 => 0,
            // oamdata
            // TODO: special values when rendering
            4 => unsafe { *self.oam.bytes.get_unchecked(self.oamaddr as usize) },
            // ppuscroll | ppuaddr
            5 | 6 => 0, // FIXME: should these reset the high bits toggle as well??
            // ppudata
            7 => {
                let val = if (self.current_vram_addr >> 8) == 0b111111 {
                    // read directly from vram if address is in range
                    // 0x3f00..=0x3ff (palette ram)
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
            0 => {
                // set bits 11-12 of 'temp_vram_addr' equal to
                // the low 2 bits of the value to be written
                self.temp_vram_addr &= !0b110000000000;
                self.temp_vram_addr |= ((val & 0b11) as u16) << 10;

                self.ppuctrl = val;
            }
            // ppumask
            1 => self.ppumask = val,
            // ppustatus, ignore attemps to write
            2 => return,
            // oamaddr
            3 => self.oamaddr = val,
            // oamdata
            4 => {
                if self.is_rendering() {
                    // ignore attemps to write when rendering
                    return;
                }

                unsafe { *self.oam.bytes.get_unchecked_mut(self.oamaddr as usize) = val };
                self.oamaddr = self.oamaddr.wrapping_add(1);
            }
            // ppuscroll
            5 => {
                // high bits toggle = 0 => x coordinate is being written
                if !self.high_bits_toggle {
                    // write low 3 bits to fine scroll register
                    self.set_fine_x_scroll(val & 0b111);
                    // write high 5 bits to temporary vram address register
                    self.temp_vram_addr = (val >> 3) as u16 & 0b11111;
                }
                // high bits toggle = 1 => y coordinate is being written
                else {
                    // write low 3 bits to high 3 bits of fine scroll register
                    self.set_fine_y_scroll(val << 5);
                    self.temp_vram_addr |= (val as u16 & !0b111) << 2;
                }

                // reset high bits toggle
                self.high_bits_toggle = !self.high_bits_toggle;
            }
            // ppuaddr
            6 => {
                let mut temp_vram_addr_bytes = self.temp_vram_addr.to_le_bytes();

                if !self.high_bits_toggle {
                    // write low 6 bits into high bits of 'temp_vram_addr'
                    temp_vram_addr_bytes[1] = val & 0b111111;
                    self.temp_vram_addr = u16::from_le_bytes(temp_vram_addr_bytes);
                } else {
                    temp_vram_addr_bytes[0] = val;
                    self.temp_vram_addr = u16::from_le_bytes(temp_vram_addr_bytes);
                    // set 'current_vram_addr' equal to 'temp_vram_addr' immediately
                    self.current_vram_addr = self.temp_vram_addr;
                }

                self.high_bits_toggle = !self.high_bits_toggle;
            }
            // ppudata
            7 => {
                self.ppudata = val;
                memory.write_ppu(self.current_vram_addr, val);

                // increment 'current_vram_addr' (same as when reading ppudata)
                if !self.is_rendering() {
                    self.increment_vram_addr();
                } else {
                    self.increment_vram_addr_coarse_x();
                    self.increment_vram_addr_y();
                }
            }
            _ => (),
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
                self.current_vram_addr += 1;
            }
        } else {
            self.fine_xy_scroll = res;
        }
    }

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

    pub fn render(&mut self) {
        // TODO: ..
        // calculate nametable fetch addr ('v') (base nametable addr and 'temp_vram_addr' register)
        // increment fetch addr x for each tile
        // increment fetch addr y for each scanline
        // fetch from addr
        // do other stuff that i haven't gotten to yet
        // NOTE: buggy behavior with oam when oamaddr is > 8
        // display contents
    }

    // expects the fine x value to be in the low 3 bits of 'fine_x'
    fn set_fine_x_scroll(&mut self, fine_x: u8) {
        self.fine_xy_scroll = (self.fine_xy_scroll & !0b111) | fine_x;
    }

    // expects the fine y value to be in the high 3 bits of 'fine_y'
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
