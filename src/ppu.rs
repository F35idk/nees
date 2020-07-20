use super::memory_map as mmap;
use mmap::MemoryMap;

pub struct Ppu {
    oam: [OamEntry; 64],
    regs: Registers,
}

#[derive(Copy, Clone, Default)]
struct OamEntry {
    y_coord: u8,
    x_coord: u8,
    tile_num: u8,
    attribute: u8,
}

// TODO: make associatvie array of colors in srgb format,
// where the index corresponds to the value or whatever

#[derive(Default)]
// the repr(C) attribute makes it possible to safely index into some
// of the registers as if they were in an array (this is done by
// 'Ppu::read_register_by_index()' and 'Ppu::write_register_by_index()')
#[repr(C)]
struct Registers {
    ppuctrl: u8,
    ppumask: u8,
    // NOTE: first 5 bits of this register contain the least
    // significant bits of any value previously written into
    // a ppu register. shouldn't need to emulate this, but
    ppustatus: u8,
    // NOTE: on some ppu chips, there are bugs relating to
    // writing to 'oamaddr'. not sure if these need to be
    // emulated either, but it may be worth keeping in mind
    oamaddr: u8,
    oamdata: u8,
    ppudata: u8,
    oamdma: u8,
    // used to toggle whether reads and writes to 'ppuscroll'
    // and 'ppuaddr' access the high or low bits of the register.
    // referred to as 'w' in the nesdev.com 'ppu scrolling' article
    // OPTIMIZE: pack these together (bitfields, somehow?)
    high_bits_toggle: bool,
    // address of the current tile to be fetched and drawn. points
    // to a byte in one of the nametables in vram. referred to
    // as 'v' in the nesdev.com 'ppu scrolling' article
    current_vram_addr: u16,
    // temporary address, same as above except isn't incremented
    // while drawing. this register is shared by 'ppuscroll' and
    // 'ppuaddr' (so writes to these registers go into this).
    // referred to as 't' in the nesdev.com 'ppu scrolling' article
    temp_vram_addr: u16,
    // low 3 bits consist of fine x scroll value, high 3 bits
    // consist of fine y scroll
    fine_xy_scroll: u8,
}

impl Default for Ppu {
    fn default() -> Self {
        Ppu {
            oam: [OamEntry::default(); 64],
            regs: Registers::default(),
        }
    }
}

impl Ppu {
    // used for reading the registers located in the cpu memory map at 0x2000-0x2007
    // TODO: unsafe version with no bounds check
    pub fn read_register_by_index(&mut self, index: u8) -> u8 {
        match index {
            // ppustatus
            2 => {
                // clear high bits toggle
                self.regs.high_bits_toggle = false;
                self.regs.ppustatus
            }
            1 | 3 | 4 => unsafe {
                // use spooky (but safe) pointer arithmetic to get the correct register
                let start_ptr = &mut self.regs.ppuctrl as *mut u8;
                *((start_ptr as usize + index as usize) as *const u8)
            },
            // FIXME:NOTE:TODO: what to read from ppudata???
            7 => self.regs.ppudata,
            // FIXME: should ppuscroll and ppuaddr reads reset the high bits toggle as well??
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
                self.regs.temp_vram_addr &= !0b110000000000;
                self.regs.temp_vram_addr |= ((val & 0b11) as u16) << 10;

                self.regs.ppuctrl = val;
            }
            // ppustatus, ignore attemps to write
            2 => (),
            // ppumask | oamaddr
            1 | 3 => unsafe {
                let start_ptr = &mut self.regs.ppuctrl as *mut u8;
                let dest_ptr = (start_ptr as usize + index as usize) as *mut u8;
                *dest_ptr = val;
            },
            // oamdata
            4 => {
                // TODO: if not vblank, ignore attempts to write to oamdata
            }
            // ppuscroll
            5 => {
                if !self.regs.high_bits_toggle {
                    // write low 3 bits to fine scroll register
                    self.regs.fine_xy_scroll |= val & 0b111;
                    // write high 5 bits to temporary vram address register
                    self.regs.temp_vram_addr = (val >> 3) as u16 & 0b11111;
                } else {
                    self.regs.fine_xy_scroll |= val << 5;
                    self.regs.temp_vram_addr |= (val as u16 & !0b111) << 2;
                }

                // reset high bits toggle
                self.regs.high_bits_toggle = !self.regs.high_bits_toggle;
            }
            // ppuaddr
            6 => {
                let mut temp_vram_addr_bytes = self.regs.temp_vram_addr.to_le_bytes();

                if !self.regs.high_bits_toggle {
                    // write low 6 bits into high bits of 'temp_vram_addr'
                    temp_vram_addr_bytes[1] = val & 0b111111;
                    self.regs.temp_vram_addr = u16::from_le_bytes(temp_vram_addr_bytes);
                } else {
                    temp_vram_addr_bytes[0] = val;
                    self.regs.temp_vram_addr = u16::from_le_bytes(temp_vram_addr_bytes);
                    // set 'current_vram_addr' equal to 'temp_vram_addr' immediately
                    self.regs.current_vram_addr = self.regs.temp_vram_addr;
                }

                self.regs.high_bits_toggle = !self.regs.high_bits_toggle;
            }
            // ppudata
            7 => {
                self.regs.ppudata = val;
                memory.write_ppu(self.regs.current_vram_addr, val);

                if self.is_vblank() || (!self.is_sprites_enable() && !self.is_background_enable()) {
                    // if not currently rendering, increment normally

                    let increment = if self.get_vram_addr_increment() {
                        32
                    } else {
                        1
                    };
                    self.regs.current_vram_addr = self
                        .regs
                        .current_vram_addr //
                        .wrapping_add(increment);
                } else {
                    // if currently rendering, increment the bits of the address corresponding to
                    // the y position and coarse x position (this is afaik unintended behavior)

                    // increment coarse x
                    if (self.regs.current_vram_addr & 0b11111) == 0b11111 {
                        self.regs.current_vram_addr &= !0b11111;
                        // overflow into bit 10 (move to next nametable horizontally)
                        self.regs.current_vram_addr ^= 0b10000000000;
                    } else {
                        self.regs.current_vram_addr = self.regs.current_vram_addr.wrapping_add(1);
                    }

                    // FIXME:FIXME:FIXME:FIXME: need to skip attribute tables!!!!!
                    // increment y
                    let (res, carry) = self.regs.fine_xy_scroll.overflowing_add(1 << 5);
                    self.regs.fine_xy_scroll = res;
                    // add carry from fine y bits to coarse y bits
                    self.regs.current_vram_addr = self
                        .regs
                        .current_vram_addr
                        .wrapping_add((carry as u16) << 5);
                }
            }
            _ => (),
        };
    }

    fn increment_vram_addr() {
        // NOTE: this has to be able to be done in steps
    }

    pub fn render(&mut self) {
        // TODO: ..
        // calculate nametable fetch addr ('v') (from base nametable
        // addr and low 5 bits of x and y passed to ppuscrol)
        // increment fetch addr x for each tile
        // increment fetch addr y for each scanline
        // fetch from addr
        // do other stuff that i haven't gotten to yet
        // display contents
    }

    fn get_base_nametable_addr(&self) -> u16 {
        // NOTE: may not need to actually multiply out etc.
        0x2000 | (((self.regs.ppuctrl & 3) as u16) << 10)
    }

    // TODO: enum for clarity
    fn get_vram_addr_increment(&self) -> bool {
        (self.regs.ppuctrl & 4) != 0
    }

    fn get_8x8_sprite_pattern_table_addr(&self) -> u16 {
        // NOTE: may not need to actually multiply out etc.
        ((self.regs.ppuctrl & 0b1000) as u16) << 9
    }

    fn get_background_pattern_table_addr(&self) -> u16 {
        ((self.regs.ppuctrl & 0b10000) as u16) << 8
    }

    // TODO: enum or whatever
    fn get_sprite_size(&self) -> bool {
        (self.regs.ppuctrl & 0b100000) != 0
    }

    // TODO: enum or whatever
    fn get_master_slave_mode(&self) -> bool {
        (self.regs.ppuctrl & 0b1000000) != 0
    }

    fn is_vblank_nmi_enabled(&self) -> bool {
        (self.regs.ppuctrl >> 7) != 0
    }

    fn is_greyscale_enabled(&self) -> bool {
        (self.regs.ppumask & 1) != 0
    }

    // whether background will be displayed in the leftmost 8 pixel columns
    fn is_background_left_column_enable(&self) -> bool {
        (self.regs.ppumask & 2) != 0
    }

    // whether sprites will be displayed in the leftmost 8 pixel columns
    fn is_sprites_left_column_enable(&self) -> bool {
        (self.regs.ppumask & 4) != 0
    }

    // whether background will be displayed
    fn is_background_enable(&self) -> bool {
        (self.regs.ppumask & 8) != 0
    }

    // whether sprites will be displayed
    fn is_sprites_enable(&self) -> bool {
        (self.regs.ppumask & 0b10000) != 0
    }

    fn is_red_emphasized(&self) -> bool {
        (self.regs.ppumask & 0b100000) != 0
    }

    fn is_green_emphasized(&self) -> bool {
        (self.regs.ppumask & 0b1000000) != 0
    }

    fn is_blue_emphasized(&self) -> bool {
        (self.regs.ppumask >> 7) != 0
    }

    fn is_sprite_overflow(&self) -> bool {
        (self.regs.ppustatus & 0b100000) != 0
    }

    fn set_sprite_overflow(&mut self, overflow: bool) {
        self.regs.ppustatus |= (overflow as u8) << 5;
    }

    fn is_sprite_zero_hit(&self) -> bool {
        (self.regs.ppustatus & 0b1000000) != 0
    }

    fn set_sprite_zero_hit(&mut self, hit: bool) {
        self.regs.ppustatus |= (hit as u8) << 6;
    }

    fn is_vblank(&self) -> bool {
        (self.regs.ppustatus >> 7) != 0
    }

    fn set_vblank(&mut self, vblank: bool) {
        self.regs.ppustatus |= (vblank as u8) << 7;
    }
}

#[test]
fn test_regs() {
    let mut ppu = Ppu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();

    ppu.regs.ppuctrl = 0b00000011;
    assert_eq!(ppu.get_base_nametable_addr(), 0x2c00);

    ppu.regs.ppudata = 0xff;
    assert_eq!(ppu.read_register_by_index(7), 0xff);

    ppu.regs.ppuctrl = 0b00001000;
    assert_eq!(ppu.get_8x8_sprite_pattern_table_addr(), 0x1000);

    ppu.regs.ppuctrl = 0b00010000;
    assert_eq!(ppu.get_background_pattern_table_addr(), 0x1000);

    ppu.regs.ppumask = 0b00000100;
    assert!(ppu.is_sprites_left_column_enable());

    ppu.regs.ppumask = 0b00000010;
    assert!(ppu.is_background_left_column_enable());

    ppu.regs.ppumask = 0b01000000;
    assert!(ppu.is_green_emphasized());

    ppu.regs.ppumask = 0b00010000;
    assert!(ppu.is_sprites_enable());

    ppu.regs.ppumask = 0b00001000;
    assert!(ppu.is_background_enable());

    ppu.regs.ppustatus = 0b00100000;
    assert!(ppu.is_sprite_overflow());

    ppu.regs.ppustatus = 0b11011111;
    assert!(ppu.is_sprite_overflow() == false);
    ppu.set_sprite_overflow(true);
    assert_eq!(ppu.regs.ppustatus, 0xff);

    let x_coord = 0b00110011;
    let y_coord = 0b10001101;
    ppu.write_register_by_index(5, x_coord, memory);
    ppu.write_register_by_index(5, y_coord, memory);

    let ppuctrl = 0b00000011;
    ppu.write_register_by_index(0, ppuctrl, memory);

    assert_eq!(ppu.regs.temp_vram_addr, 0b11_10001_00110);
    assert_eq!(ppu.regs.fine_xy_scroll & 0b111, x_coord & 0b111);
    assert_eq!(ppu.regs.fine_xy_scroll >> 5, y_coord & 0b111);

    let addr_hi = 0x21;
    let addr_low = 0x0f;
    ppu.write_register_by_index(6, addr_hi, memory);
    ppu.write_register_by_index(6, addr_low, memory);

    assert_eq!(ppu.regs.current_vram_addr, 0x210f);

    let addr_hi = 0x4f;
    let addr_low = 0xe8;
    ppu.write_register_by_index(6, addr_hi, memory);
    ppu.write_register_by_index(6, addr_low, memory);

    // address is mirrored down
    assert_eq!(ppu.regs.current_vram_addr, 0x4fe8 % 0x4000);
}
