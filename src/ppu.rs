pub struct Ppu {
    oam: [OamEntry; 64],
    registers: Registers,
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
// the repr(C) attribute makes it possible to safely index into
// the registers as if they were in an array (this is done by
// 'Ppu::read_register_by_index()' and 'Ppu::write_register_by_index()')
#[repr(C)]
struct Registers {
    ppuctrl: u8,
    ppumask: u8,
    // NOTE: first 5 bits of 'ppustatus' contains the least
    // significant bits of any value previously written into
    // a ppu register. shouldn't need to emulate this, but
    ppustatus: u8,
    // NOTE: on some ppu chips, there are bugs relating to
    // writing to 'oamaddr'. not sure if these need to be
    // emulated either, but it may be worth keeping in mind
    oamaddr: u8,
    oamdata: u8,
    ppuscroll: [u8; 2],
    ppuaddr: [u8; 2],
    ppudata: u8,
    oamdma: u8,
    // used to toggle whether reads and writes to 'ppuscroll'
    // and 'ppuaddr' access the high or low byte of the register.
    // this is set when reading/writing TODO: implement and
    // cleared when reading/writing TODO: implement
    high_byte_toggle: bool,
    // TODO: internal registers
    // v: u16,
    // t: u16,
    // x: u8,
}

impl Default for Ppu {
    fn default() -> Self {
        Ppu {
            oam: [OamEntry::default(); 64],
            registers: Registers::default(),
        }
    }
}

impl Ppu {
    // used for reading the registers located in the cpu memory map at 0x2000-0x2007
    // TODO: unsafe version with no bounds check
    pub fn read_register_by_index(&mut self, index: u8) -> u8 {
        match index {
            i @ 0..=4 => unsafe {
                if i == 2 {
                    // reset high byte toggle if reading ppustatus
                    self.registers.high_byte_toggle = false;
                }
                // use spooky (but safe) pointer arithmetic to get the correct register
                let start_ptr = &mut self.registers.ppuctrl as *mut u8;
                *((start_ptr as usize + index as usize) as *const u8)
            },
            5 => {
                // FIXME: should ppuscroll reads reset the high byte toggle as well??
                // use 'high_byte_toggle' to determine whether the high or low byte is read
                self.registers.ppuscroll[self.registers.high_byte_toggle as usize]
            }
            6 => {
                // FIXME: should ppuaddr reads reset the high byte toggle as well??
                self.registers.ppuaddr[self.registers.high_byte_toggle as usize]
            }
            // TODO: what to read from ppudata???
            7 => self.registers.ppudata,

            _ => 0,
        }
    }

    pub fn write_register_by_index(&mut self, index: u8, val: u8) {
        match index {
            1 | 4 => unsafe {
                let start_ptr = &mut self.registers.ppuctrl as *mut u8;
                let dest_ptr = (start_ptr as usize + index as usize) as *mut u8;
                *dest_ptr = val;
            },
            // ignore attempts to write to ppustatus
            2 => (),
            3 => unsafe {
                // TODO: if not vblank, ignore attempts to write to oamdata
            },
            5 => {
                self.registers.high_byte_toggle = !self.registers.high_byte_toggle;
                self.registers.ppuscroll[self.registers.high_byte_toggle as usize] = val;
            }
            6 => {
                self.registers.high_byte_toggle = !self.registers.high_byte_toggle;
                self.registers.ppuaddr[self.registers.high_byte_toggle as usize] = val;
            }
            7 => {
                // TODO: write to stuff
                self.registers.ppudata = val;
            }
            _ => (),
        };
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
        0x2000 | (((self.registers.ppuctrl & 3) as u16) << 10)
    }

    // TODO: enum or whatever
    fn get_vram_increment_mode(&self) -> bool {
        (self.registers.ppuctrl & 4) != 0
    }

    fn get_8x8_sprite_pattern_table_addr(&self) -> u16 {
        // NOTE: may not need to actually multiply out etc.
        ((self.registers.ppuctrl & 0b1000) as u16) << 9
    }

    fn get_background_pattern_table_addr(&self) -> u16 {
        ((self.registers.ppuctrl & 0b10000) as u16) << 8
    }

    // TODO: enum or whatever
    fn get_sprite_size(&self) -> bool {
        (self.registers.ppuctrl & 0b100000) != 0
    }

    // TODO: enum or whatever
    fn get_master_slave_mode(&self) -> bool {
        (self.registers.ppuctrl & 0b1000000) != 0
    }

    fn is_vblank_nmi_enabled(&self) -> bool {
        (self.registers.ppuctrl >> 7) != 0
    }

    fn is_greyscale_enabled(&self) -> bool {
        (self.registers.ppumask & 1) != 0
    }

    // whether background will be displayed in the leftmost 8 pixel columns
    fn is_background_left_column_enable(&self) -> bool {
        (self.registers.ppumask & 2) != 0
    }

    // whether sprites will be displayed in the leftmost 8 pixel columns
    fn is_sprites_left_column_enable(&self) -> bool {
        (self.registers.ppumask & 4) != 0
    }

    // whether background will be displayed
    fn is_background_enable(&self) -> bool {
        (self.registers.ppumask & 8) != 0
    }

    // whether sprites will be displayed
    fn is_sprites_enable(&self) -> bool {
        (self.registers.ppumask & 0b10000) != 0
    }

    fn is_red_emphasized(&self) -> bool {
        (self.registers.ppumask & 0b100000) != 0
    }

    fn is_green_emphasized(&self) -> bool {
        (self.registers.ppumask & 0b1000000) != 0
    }

    fn is_blue_emphasized(&self) -> bool {
        (self.registers.ppumask >> 7) != 0
    }

    fn is_sprite_overflow(&self) -> bool {
        (self.registers.ppustatus & 0b100000) != 0
    }

    fn set_sprite_overflow(&mut self, overflow: bool) {
        self.registers.ppustatus |= (overflow as u8) << 5;
    }

    fn is_sprite_zero_hit(&self) -> bool {
        (self.registers.ppustatus & 0b1000000) != 0
    }

    fn set_sprite_zero_hit(&mut self, hit: bool) {
        self.registers.ppustatus |= (hit as u8) << 6;
    }

    fn is_vblank(&self) -> bool {
        (self.registers.ppustatus >> 7) != 0
    }

    fn set_vblank(&mut self, vblank: bool) {
        self.registers.ppustatus |= (vblank as u8) << 7;
    }
}

#[test]
fn test_registers() {
    let mut ppu = Ppu::default();
    ppu.registers.ppuctrl = 0b00000011;
    assert_eq!(ppu.get_base_nametable_addr(), 0x2c00);

    ppu.registers.ppudata = 0xff;
    assert_eq!(ppu.read_register_by_index(7), 0xff);

    ppu.registers.ppuaddr = [0xee, 0xff];
    ppu.registers.high_byte_toggle = true;
    assert_eq!(ppu.read_register_by_index(6), 0xff);
    ppu.registers.high_byte_toggle = false;
    assert_eq!(ppu.read_register_by_index(6), 0xee);

    ppu.registers.ppuctrl = 0b00001000;
    assert_eq!(ppu.get_8x8_sprite_pattern_table_addr(), 0x1000);

    ppu.registers.ppuctrl = 0b00010000;
    assert_eq!(ppu.get_background_pattern_table_addr(), 0x1000);

    ppu.registers.ppumask = 0b00000100;
    assert!(ppu.is_sprites_left_column_enable());

    ppu.registers.ppumask = 0b00000010;
    assert!(ppu.is_background_left_column_enable());

    ppu.registers.ppumask = 0b01000000;
    assert!(ppu.is_green_emphasized());

    ppu.registers.ppumask = 0b00010000;
    assert!(ppu.is_sprites_enable());

    ppu.registers.ppumask = 0b00001000;
    assert!(ppu.is_background_enable());

    ppu.registers.ppustatus = 0b00100000;
    assert!(ppu.is_sprite_overflow());

    ppu.registers.ppustatus = 0b11011111;
    assert!(ppu.is_sprite_overflow() == false);
    ppu.set_sprite_overflow(true);
    assert_eq!(ppu.registers.ppustatus, 0xff);
}
