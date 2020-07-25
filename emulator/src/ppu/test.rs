use super::super::apu;
use super::super::cpu;
use super::super::memory_map as mmap;
use super::super::PtrsWrapper;
use mmap::MemoryMap;

#[test]
fn test_registers() {
    let mut ppu = super::Ppu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();

    ppu.ppuctrl = 0b00000011;
    assert_eq!(ppu.get_base_nametable_addr(), 0x2c00);

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

    let x_coord = 0b00110_011;
    let y_coord = 0b10001_101;
    ppu.write_register_by_index(5, x_coord, memory);
    ppu.write_register_by_index(5, y_coord, memory);

    let ppuctrl = 0b00000011;
    ppu.write_register_by_index(0, ppuctrl, memory);

    assert_eq!(ppu.temp_vram_addr, 0b101_11_10001_00110);
    assert_eq!(ppu.fine_xy_scroll & 0b111, x_coord & 0b111);
    // bits 12-14 of 'temp_vram_addr' should be set to temporary fine y scroll
    assert_eq!((ppu.temp_vram_addr >> 12) as u8, y_coord & 0b111);

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
fn test_write_2007() {
    // ppu.current_vram_addr = 0;
    // // write 0xff to ppudata
    // ppu.write_register_by_index(7, 0xff, memory);
    // ppu.current_vram_addr = 0;
    // // read from ppudata
    // assert_eq!(ppu.read_register_by_index(7, memory), 0xff);
}

#[test]
fn test_write_2000() {
    let mut cpu = cpu::Cpu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();
    let ref mut ppu = super::Ppu::default();
    let ref mut apu = apu::Apu {};
    let ref mut cpu_cycles = 0;
    let ref mut ptrs = PtrsWrapper {
        ppu,
        apu,
        cpu_cycles,
    };

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
    let ref mut ppu = super::Ppu::default();
    let ref mut apu = apu::Apu {};
    let ref mut cpu_cycles = 0;
    let ref mut ptrs = PtrsWrapper {
        ppu,
        apu,
        cpu_cycles,
    };

    // LDA $2002
    memory.write_cpu(ptrs, 0u16, 0xad);
    memory.write_cpu(ptrs, 1u16, 02);
    memory.write_cpu(ptrs, 2u16, 0x20);

    ptrs.ppu.low_bits_toggle = true;
    cpu.pc = 0;
    cpu.exec_instruction(memory, ptrs);

    assert_eq!(ppu.low_bits_toggle, false);
}

#[test]
fn test_write_2005() {
    let mut cpu = cpu::Cpu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();
    let ref mut ppu = super::Ppu::default();
    let ref mut apu = apu::Apu {};
    let ref mut cpu_cycles = 0;
    let ref mut ptrs = PtrsWrapper {
        ppu,
        apu,
        cpu_cycles,
    };

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
    assert_eq!(ptrs.ppu.low_bits_toggle, true);
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

    assert_eq!(ptrs.ppu.temp_vram_addr >> 12, 0b110);
    assert_eq!(ptrs.ppu.low_bits_toggle, false);
    assert_eq!(ptrs.ppu.temp_vram_addr, 0b110_00_01011_01111);
}

#[test]
fn test_write_2006() {
    let mut cpu = cpu::Cpu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();
    let ref mut ppu = super::Ppu::default();
    let ref mut apu = apu::Apu {};
    let ref mut cpu_cycles = 0;
    let ref mut ptrs = PtrsWrapper {
        ppu,
        apu,
        cpu_cycles,
    };

    // LDA #3d (0b00111101)
    memory.write_cpu(ptrs, 0u16, 0xa9);
    memory.write_cpu(ptrs, 1u16, 0x3d);
    // STA $2006
    memory.write_cpu(ptrs, 2u16, 0x8d);
    memory.write_cpu(ptrs, 3u16, 06);
    memory.write_cpu(ptrs, 4u16, 0x20);

    // set bit 14 of 'temp_vram_addr'
    ptrs.ppu.temp_vram_addr |= 0b0100000000000000;

    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(memory, ptrs);
    }

    assert_eq!(ptrs.ppu.low_bits_toggle, true);
    // bit 14 should be cleared and bits 8-13 should be
    // equal to bits 0-6 of the value written to 0x2006
    assert_eq!(ptrs.ppu.temp_vram_addr, 0b000_00_111101_00000000);

    // LDA #f0 (0b11110000)
    memory.write_cpu(ptrs, 5u16, 0xa9);
    memory.write_cpu(ptrs, 6u16, 0xf0);
    // STA $2006
    memory.write_cpu(ptrs, 7u16, 0x8d);
    memory.write_cpu(ptrs, 8u16, 06);
    memory.write_cpu(ptrs, 9u16, 0x20);

    for _ in 0..2 {
        cpu.exec_instruction(memory, ptrs);
    }

    assert_eq!(ptrs.ppu.low_bits_toggle, false);
    assert_eq!(ptrs.ppu.temp_vram_addr, 0b000_00_111101_11110000);
    assert_eq!(ptrs.ppu.temp_vram_addr, ptrs.ppu.current_vram_addr);
}

#[test]
fn test_write_2003_read_2004() {
    let mut cpu = cpu::Cpu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();
    let ref mut ppu = super::Ppu::default();
    let ref mut apu = apu::Apu {};
    let ref mut cpu_cycles = 0;
    let ref mut ptrs = PtrsWrapper {
        ppu,
        apu,
        cpu_cycles,
    };

    // LDA #ff
    memory.write_cpu(ptrs, 0u16, 0xa9);
    memory.write_cpu(ptrs, 1u16, 0xff);
    // STA $2003 (write 0xff to oamaddr)
    memory.write_cpu(ptrs, 2u16, 0x8d);
    memory.write_cpu(ptrs, 3u16, 03);
    memory.write_cpu(ptrs, 4u16, 0x20);

    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(memory, ptrs);
    }

    // set oam[0xff] = 0xee
    unsafe {
        ptrs.ppu.oam.bytes[0xff] = 0xee;
    }

    // LDA $2004 (read from oamdata, i.e read the byte at oam[oamaddr])
    memory.write_cpu(ptrs, 5u16, 0xad);
    memory.write_cpu(ptrs, 6u16, 04);
    memory.write_cpu(ptrs, 7u16, 0x20);

    cpu.exec_instruction(memory, ptrs);

    assert_eq!(cpu.a, 0xee);
}

#[test]
fn test_increment_vram_addr() {
    let ref mut ppu = super::Ppu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();

    ppu.current_vram_addr = 0;
    // set increment mode to 32
    ppu.write_register_by_index(0, 0b00000100, memory);
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr, 32);

    ppu.current_vram_addr = 0;
    // set increment mode to 1
    ppu.write_register_by_index(0, 0b00000000, memory);
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr, 1);

    // check wrapping behavior ((3fff + 1) % 4000 = 0)
    ppu.current_vram_addr = 0x3fff;
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr, 0);

    // check wrapping behavior ((3fe0 + 0x20) % 4000 = 0)
    ppu.current_vram_addr = 0x3fe0;
    ppu.write_register_by_index(0, 0b00000100, memory);
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr, 0);
}

#[test]
fn test_increment_vram_addr_xy() {
    let ref mut ppu = super::Ppu::default();

    ppu.current_vram_addr = 0b01_01010_11111;
    ppu.increment_vram_addr_coarse_x();
    // increment should overflow into bit 10
    assert_eq!(ppu.current_vram_addr, 0b00_01010_00000);

    // set coarse y = 31
    ppu.current_vram_addr = 0b10_11111_01010;
    // fine y = 7
    ppu.fine_xy_scroll = 0b111_00_000;
    ppu.increment_vram_addr_y();
    // increment should not overflow into bit 11
    assert_eq!(ppu.current_vram_addr, 0b10_00000_01010);
    assert_eq!(ppu.fine_xy_scroll >> 5, 0b000);

    // set coarse y = 29
    ppu.current_vram_addr = 0b10_11101_01010;
    // fine y = 7
    ppu.fine_xy_scroll = 0b111_00_000;
    ppu.increment_vram_addr_y();
    // increment should overflow into bit 11
    assert_eq!(ppu.current_vram_addr, 0b00_00000_01010);
    assert_eq!(ppu.fine_xy_scroll >> 5, 0b000);

    // set coarse y = 29
    ppu.current_vram_addr = 0b10_11111_01010;
    // fine y = 7
    ppu.fine_xy_scroll = 0b110_00_000;
    ppu.increment_vram_addr_y();
    // increment should not change coarse y
    assert_eq!(ppu.current_vram_addr, 0b10_11111_01010);
    assert_eq!(ppu.fine_xy_scroll >> 5, 0b111);
}

#[test]
fn test_oamdma() {
    let mut ppu = super::Ppu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();
    let ref mut cpu_cycle_count = 0;

    ppu.write_register_by_index(3, 0xee, memory);
    ppu.write_oamdma(0x20, cpu_cycle_count, memory);

    assert_eq!(*cpu_cycle_count, 513);
    assert_eq!(ppu.oamaddr, 0xee);
}

fn todo_ppu_memory_map_calc() {
    // 0b10000000000000 => PA13 is high => 0x2000
    // 0b01000000000000 => PA12 is high => 0x1000
    // 0b11000000000000 => PA13 & PA12 are high => 0x3000
    let mut read_addr = 0x3f00;

    if (read_addr >> 13) == 1 {
        read_addr &= !0b01000000000000;
    }

    println!("read_addr: {:x}", read_addr);
}

#[test]
fn test_render() {
    let mut cpu = cpu::Cpu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();
    let ref mut ppu = super::Ppu::default();
    let ref mut apu = apu::Apu {};
    let ref mut cpu_cycles = 0;
    let ref mut ptrs = PtrsWrapper {
        ppu,
        apu,
        cpu_cycles,
    };

    // set background pattern table addr = 0x1000
    ppu.ppuctrl = 0b10000;
    // set 'current_vram_addr' = first byte of second nametable
    ppu.current_vram_addr = 0x2400;
    // set nametable mirroring mask = vertical mirroring
    memory.nametable_mirroring_mask = !0x800;
    // write 255 to first byte of second nametable (tile index)
    memory.write_ppu(0x2400, 0xff);

    // write high and low bitplanes to the 255th tile of the pattern table at 0x1000
    let tile_lo_bits = 0b00010011;
    let tile_hi_bits = 0b00010101;
    memory.write_ppu(16 * 255 + 0x1000, tile_lo_bits);
    memory.write_ppu(16 * 255 + 0x1000 + 7, tile_hi_bits);
    // set fine x pos = 3 to only draw pixels 3-7 in the tile
    ppu.fine_xy_scroll = 0b011;

    ppu.draw_scanline_tile(memory);
    println!("");
    ppu.draw_scanline_tile(memory);

    assert!(false);
}
