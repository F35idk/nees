use super::apu;
use super::cpu;
use super::memory_map as mmap;
use mmap::MemoryMap;

pub struct Ppu {
    // object attribute memory
    oam: [OamEntry; 64],
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
    oamdata: u8,
    ppudata: u8,
    oamdma: u8,
    // internal registers
    // used to toggle whether reads and writes to 'ppuscroll'
    // and 'ppuaddr' access the high or low bits of the register.
    // referred to as 'w' in the nesdev.com 'ppu scrolling' article
    // OPTIMIZE: pack these together (bitfields, somehow?)
    high_bits_toggle: bool,
    // address of the current tile to be fetched and drawn. points
    // to a byte in one of the nametables in vram. referred to
    // as 'v' in the nesdev.com 'ppu scrolling' article
    current_vram_addr: u16,
    // temporary address, same as above but minus 0x2000 and
    // doesn't get incremented while drawing. this register is
    // shared by 'ppuscroll' and 'ppuaddr' (so writes to these
    // registers go into this). referred to as 't' in the
    // nesdev.com 'ppu scrolling' article
    temp_vram_addr: u16,
    // low 3 bits consist of fine x scroll value, high 3 bits
    // consist of fine y scroll
    // FIXME: use high 4 and low 4 for carry bits as well??
    fine_xy_scroll: u8,
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

impl Default for Ppu {
    fn default() -> Self {
        Ppu {
            oam: [OamEntry::default(); 64],
            ppuctrl: 0,
            ppumask: 0,
            ppustatus: 0,
            oamaddr: 0,
            oamdata: 0,
            ppudata: 0,
            oamdma: 0,
            high_bits_toggle: false,
            current_vram_addr: 0,
            temp_vram_addr: 0,
            fine_xy_scroll: 0,
        }
    }
}

impl Ppu {
    // used for reading the registers located in the cpu memory map at 0x2000-0x2007
    pub fn read_register_by_index(&mut self, index: u8) -> u8 {
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
            3 => self.oamaddr,
            // oamdata
            4 => self.oamdata, // FIXME:NOTE:TODO: what to read from ppudata???
            // ppuscroll / ppuaddr
            5 | 6 => 0,
            // ppudata
            7 => self.ppudata,
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
                self.temp_vram_addr &= !0b110000000000;
                self.temp_vram_addr |= ((val & 0b11) as u16) << 10;

                self.ppuctrl = val;
            }
            // ppumask
            1 => self.ppumask = val,
            // ppustatus, ignore attemps to write
            2 => (),
            // oamaddr
            3 => self.oamaddr = val,
            // oamdata
            4 => {
                // TODO: if not vblank, ignore attempts to write to oamdata
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

                if self.is_vblank() || (!self.is_sprites_enable() && !self.is_background_enable()) {
                    // if not currently rendering, increment normally
                    let increment = if self.get_vram_addr_increment() {
                        32
                    } else {
                        1
                    };
                    self.current_vram_addr = self
                        .current_vram_addr //
                        .wrapping_add(increment);
                } else {
                    // if currently rendering, increment the bits of the address
                    // corresponding to the y position and coarse x position (this
                    // is afaik unintended behavior)

                    // increment coarse x
                    if (self.current_vram_addr & 0b11111) == 0b11111 {
                        // if coarse x component of 'current_vram_addr' is the highest
                        // possible value it can be (31), clear all coarse x bits and
                        // overflow into bit 10 (move to next nametable horizontally)
                        self.current_vram_addr &= !0b11111;
                        self.current_vram_addr ^= 0b10000000000;
                    } else {
                        // if not highest value, increment normally
                        self.current_vram_addr += 1;
                    }

                    // increment fine y bits
                    let (res, carry) = self.fine_xy_scroll.overflowing_add(1 << 5);
                    self.fine_xy_scroll = res;

                    if carry {
                        if (self.current_vram_addr & 0b1111100000) == 29 << 5 {
                            // if carry from fine y bits = 1 and coarse y bits
                            // = 29 (there are 29 rows of tiles in a frame),
                            // clear all coarse y bits and overflow into bit
                            // 11 to move to next nametable vertically
                            self.current_vram_addr &= !0b1111100000;
                            self.current_vram_addr ^= 0b100000000000;
                        } else if (self.current_vram_addr & 0b1111100000) == 0b1111100000 {
                            // if coarse y = maximum, wrap the value without
                            // overflowing into bit 11 and switching nametables
                            self.current_vram_addr &= !0b111110000;
                        } else {
                            self.current_vram_addr += 1;
                        }
                    }
                }
            }
            _ => (),
        }
    }

    fn increment_vram_addr() {
        // NOTE: this has to be able to be done in steps
    }

    pub fn render(&mut self) {
        // TODO: ..
        // calculate nametable fetch addr ('v') (base nametable addr and 'temp_vram_addr register')
        // increment fetch addr x for each tile
        // increment fetch addr y for each scanline
        // fetch from addr
        // do other stuff that i haven't gotten to yet
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
}

#[test]
fn test_registers() {
    let mut ppu = Ppu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();

    ppu.ppuctrl = 0b00000011;
    assert_eq!(ppu.get_base_nametable_addr(), 0x2c00);

    ppu.ppudata = 0xff;
    assert_eq!(ppu.read_register_by_index(7), 0xff);

    ppu.ppuctrl = 0b00001000;
    assert_eq!(ppu.get_8x8_sprite_pattern_table_addr(), 0x1000);

    ppu.ppuctrl = 0b00010000;
    assert_eq!(ppu.get_background_pattern_table_addr(), 0x1000);

    ppu.ppumask = 0b00000100;
    assert!(ppu.is_sprites_left_column_enable());

    ppu.ppumask = 0b00000010;
    assert!(ppu.is_background_left_column_enable());

    ppu.ppumask = 0b01000000;
    assert!(ppu.is_green_emphasized());

    ppu.ppumask = 0b00010000;
    assert!(ppu.is_sprites_enable());

    ppu.ppumask = 0b00001000;
    assert!(ppu.is_background_enable());

    ppu.ppustatus = 0b00100000;
    assert!(ppu.is_sprite_overflow());

    ppu.ppustatus = 0b11011111;
    assert!(ppu.is_sprite_overflow() == false);
    ppu.set_sprite_overflow(true);
    assert_eq!(ppu.ppustatus, 0xff);

    let x_coord = 0b00110011;
    let y_coord = 0b10001101;
    ppu.write_register_by_index(5, x_coord, memory);
    ppu.write_register_by_index(5, y_coord, memory);

    let ppuctrl = 0b00000011;
    ppu.write_register_by_index(0, ppuctrl, memory);

    assert_eq!(ppu.temp_vram_addr, 0b11_10001_00110);
    assert_eq!(ppu.fine_xy_scroll & 0b111, x_coord & 0b111);
    assert_eq!(ppu.fine_xy_scroll >> 5, y_coord & 0b111);

    let addr_hi = 0x21;
    let addr_low = 0x0f;
    ppu.write_register_by_index(6, addr_hi, memory);
    ppu.write_register_by_index(6, addr_low, memory);

    assert_eq!(ppu.current_vram_addr, 0x210f);

    let addr_hi = 0x4f;
    let addr_low = 0xe8;
    ppu.write_register_by_index(6, addr_hi, memory);
    ppu.write_register_by_index(6, addr_low, memory);

    // address is mirrored down
    assert_eq!(ppu.current_vram_addr, 0x4fe8 % 0x4000);
}

#[test]
fn test_write_2000() {
    let mut cpu = cpu::Cpu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();
    let ref mut ppu = Ppu::default();
    let ref mut apu = apu::Apu {};
    let ref mut ptrs = mmap::MemoryMapPtrs { ppu, apu };

    // LDA #ff
    memory.write_cpu(ptrs, 0u16, 0xa9);
    memory.write_cpu(ptrs, 1u16, 0xff);
    // STA $2000
    memory.write_cpu(ptrs, 2u16, 0x8d);
    memory.write_cpu(ptrs, 3u16, 00);
    memory.write_cpu(ptrs, 4u16, 0x20);

    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(memory, ptrs);
    }

    assert_eq!(ppu.temp_vram_addr, 0b11_00000_00000)
}

#[test]
fn test_read_2002() {
    let mut cpu = cpu::Cpu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();
    let ref mut ppu = Ppu::default();
    let ref mut apu = apu::Apu {};
    let ref mut ptrs = mmap::MemoryMapPtrs { ppu, apu };

    // LDA $2002
    memory.write_cpu(ptrs, 0u16, 0xad);
    memory.write_cpu(ptrs, 1u16, 02);
    memory.write_cpu(ptrs, 2u16, 0x20);

    ptrs.ppu.high_bits_toggle = true;
    cpu.pc = 0;
    cpu.exec_instruction(memory, ptrs);

    assert_eq!(ppu.high_bits_toggle, false);
}

#[test]
fn test_write_2005() {
    let mut cpu = cpu::Cpu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();
    let ref mut ppu = Ppu::default();
    let ref mut apu = apu::Apu {};
    let ref mut ptrs = mmap::MemoryMapPtrs { ppu, apu };

    // LDA #7d (0b01111_101)
    memory.write_cpu(ptrs, 0u16, 0xa9);
    memory.write_cpu(ptrs, 1u16, 0x7d);
    // STA $2005
    memory.write_cpu(ptrs, 2u16, 0x8d);
    memory.write_cpu(ptrs, 3u16, 05);
    memory.write_cpu(ptrs, 4u16, 0x20);

    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(memory, ptrs);
    }

    assert_eq!(ptrs.ppu.fine_xy_scroll, 0b101);
    assert_eq!(ptrs.ppu.high_bits_toggle, true);
    assert_eq!(ptrs.ppu.temp_vram_addr, 0b00_00000_01111);

    // LDA #5e (0b01011_110)
    memory.write_cpu(ptrs, 5u16, 0xa9);
    memory.write_cpu(ptrs, 6u16, 0x5e);
    // STA $2005
    memory.write_cpu(ptrs, 7u16, 0x8d);
    memory.write_cpu(ptrs, 8u16, 05);
    memory.write_cpu(ptrs, 9u16, 0x20);

    for _ in 0..2 {
        cpu.exec_instruction(memory, ptrs);
    }

    assert_eq!(ptrs.ppu.fine_xy_scroll >> 5, 0b110);
    assert_eq!(ptrs.ppu.high_bits_toggle, false);
    assert_eq!(ptrs.ppu.temp_vram_addr, 0b00_01011_01111);
}

#[test]
fn test_write_2006() {
    let mut cpu = cpu::Cpu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();
    let ref mut ppu = Ppu::default();
    let ref mut apu = apu::Apu {};
    let ref mut ptrs = mmap::MemoryMapPtrs { ppu, apu };

    // LDA #3d (0b00111101)
    memory.write_cpu(ptrs, 0u16, 0xa9);
    memory.write_cpu(ptrs, 1u16, 0x3d);
    // STA $2006
    memory.write_cpu(ptrs, 2u16, 0x8d);
    memory.write_cpu(ptrs, 3u16, 06);
    memory.write_cpu(ptrs, 4u16, 0x20);

    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(memory, ptrs);
    }

    assert_eq!(ptrs.ppu.high_bits_toggle, true);
    assert_eq!(ptrs.ppu.temp_vram_addr, 0b111101_00000000);

    // LDA #f0 (0b11110000)
    memory.write_cpu(ptrs, 0u16, 0xa9);
    memory.write_cpu(ptrs, 1u16, 0xf0);
    // STA $2006
    memory.write_cpu(ptrs, 2u16, 0x8d);
    memory.write_cpu(ptrs, 3u16, 06);
    memory.write_cpu(ptrs, 4u16, 0x20);

    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(memory, ptrs);
    }

    assert_eq!(ptrs.ppu.high_bits_toggle, false);
    assert_eq!(ptrs.ppu.temp_vram_addr, 0b111101_11110000);
    assert_eq!(ptrs.ppu.temp_vram_addr, ptrs.ppu.current_vram_addr);
}
