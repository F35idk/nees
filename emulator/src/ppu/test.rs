use super::super::PixelRenderer;
use super::super::{apu, cpu, memory_map as mmap, parse, win};
use mmap::{CpuMemoryMap, PpuMemoryMap};

fn init_nes() -> (cpu::Cpu, mmap::Nrom128CpuMemory<'static>) {
    let mut win = win::XcbWindowWrapper::new("test", 20, 20).unwrap();
    let renderer = PixelRenderer::new(&mut win.connection, win.win, 256, 240).unwrap();

    let ppu_memory = Box::leak(Box::new(mmap::NromPpuMemory::new()));
    let ppu = super::Ppu::new(renderer, ppu_memory);
    let apu = apu::Apu {};
    let cpu = cpu::Cpu::default();
    let cpu_memory = mmap::Nrom128CpuMemory::new(ppu, apu);

    (cpu, cpu_memory)
}

#[test]
fn test_registers() {
    let (ref mut cpu, mmap::Nrom128CpuMemory { ref mut ppu, .. }) = init_nes();

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
    ppu.write_register_by_index(5, x_coord, cpu);
    ppu.write_register_by_index(5, y_coord, cpu);

    let ppuctrl = 0b00000011;
    ppu.write_register_by_index(0, ppuctrl, cpu);

    assert_eq!(ppu.temp_vram_addr.inner, 0b101_11_10001_00110);
    assert_eq!(ppu.fine_x_scroll & 0b111, x_coord & 0b111);
    // bits 12-14 of 'temp_vram_addr' should be set to temporary fine y scroll
    assert_eq!((ppu.temp_vram_addr.inner >> 12) as u8, y_coord & 0b111);

    let addr_hi = 0x21;
    let addr_low = 0x0f;
    ppu.write_register_by_index(6, addr_hi, cpu);
    ppu.write_register_by_index(6, addr_low, cpu);

    assert_eq!(ppu.current_vram_addr.get_addr(), 0x210f);

    assert_eq!(
        ppu.current_vram_addr.inner,
        0x210f | (ppu.temp_vram_addr.inner & !0x3fff)
    );

    let addr_hi = 0x4f;
    let addr_low = 0xe8;
    ppu.write_register_by_index(6, addr_hi, cpu);
    ppu.write_register_by_index(6, addr_low, cpu);

    // address is mirrored down
    assert_eq!(ppu.current_vram_addr.inner, 0x4fe8 % 0x4000);
}

#[test]
fn test_write_2007() {
    let (ref mut cpu, ref mut cpu_memory) = init_nes();

    // LDA #ee
    cpu_memory.write(0u16, 0xa9, cpu);
    cpu_memory.write(1u16, 0xee, cpu);
    // STA $2007
    cpu_memory.write(2u16, 0x8d, cpu);
    cpu_memory.write(3u16, 07, cpu);
    cpu_memory.write(4u16, 0x20, cpu);

    // disable renderering
    cpu_memory.ppu.ppumask = 0;
    // set scanline = visible
    cpu_memory.ppu.current_scanline = 222;

    // set increment mode = 32
    cpu_memory.ppu.ppuctrl = 0b100;

    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(cpu_memory);
    }

    assert_eq!(cpu_memory.ppu.current_vram_addr.inner, 32);
    cpu_memory.ppu.current_vram_addr.inner = 0;

    // first read should yield nothing
    assert_eq!(cpu_memory.ppu.read_register_by_index(7), 0);
    // second read should get contents at addr
    assert_eq!(cpu_memory.ppu.read_register_by_index(7), 0xee);
}

#[test]
fn test_write_2000() {
    let (ref mut cpu, ref mut cpu_memory) = init_nes();

    // LDA #ff
    cpu_memory.write(0u16, 0xa9, cpu);
    cpu_memory.write(1u16, 0xff, cpu);
    // STA $2000
    cpu_memory.write(2u16, 0x8d, cpu);
    cpu_memory.write(3u16, 00, cpu);
    cpu_memory.write(4u16, 0x20, cpu);

    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(cpu_memory);
    }

    assert_eq!(cpu_memory.ppu.temp_vram_addr.inner, 0b11_00000_00000)
}

#[test]
fn test_read_2002() {
    let (ref mut cpu, ref mut cpu_memory) = init_nes();

    // LDA $2002
    cpu_memory.write(0u16, 0xad, cpu);
    cpu_memory.write(1u16, 02, cpu);
    cpu_memory.write(2u16, 0x20, cpu);

    cpu_memory.ppu.low_bits_toggle = true;
    cpu.pc = 0;
    cpu.exec_instruction(cpu_memory);

    assert_eq!(cpu_memory.ppu.low_bits_toggle, false);
}

#[test]
fn test_write_2005() {
    let (ref mut cpu, ref mut cpu_memory) = init_nes();

    // LDA #7d (0b01111_101)
    cpu_memory.write(0u16, 0xa9, cpu);
    cpu_memory.write(1u16, 0x7d, cpu);
    // STA $2005
    cpu_memory.write(2u16, 0x8d, cpu);
    cpu_memory.write(3u16, 05, cpu);
    cpu_memory.write(4u16, 0x20, cpu);

    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(cpu_memory);
    }

    assert_eq!(cpu_memory.ppu.fine_x_scroll, 0b101);
    assert_eq!(cpu_memory.ppu.low_bits_toggle, true);
    assert_eq!(cpu_memory.ppu.temp_vram_addr.inner, 0b00_00000_01111);

    // LDA #5e (0b01011_110)
    cpu_memory.write(5u16, 0xa9, cpu);
    cpu_memory.write(6u16, 0x5e, cpu);
    // STA $2005
    cpu_memory.write(7u16, 0x8d, cpu);
    cpu_memory.write(8u16, 05, cpu);
    cpu_memory.write(9u16, 0x20, cpu);

    for _ in 0..2 {
        cpu.exec_instruction(cpu_memory);
    }

    assert_eq!(cpu_memory.ppu.temp_vram_addr.inner >> 12, 0b110);
    assert_eq!(cpu_memory.ppu.low_bits_toggle, false);
    assert_eq!(cpu_memory.ppu.temp_vram_addr.inner, 0b110_00_01011_01111);
}

#[test]
fn test_write_2006() {
    let (ref mut cpu, ref mut cpu_memory) = init_nes();

    // LDA #ed (0b11101101)
    cpu_memory.write(0u16, 0xa9, cpu);
    cpu_memory.write(1u16, 0xed, cpu);
    // STA $2006
    cpu_memory.write(2u16, 0x8d, cpu);
    cpu_memory.write(3u16, 06, cpu);
    cpu_memory.write(4u16, 0x20, cpu);
    // set bit 14 of 'temp_vram_addr'
    cpu_memory.ppu.temp_vram_addr.inner |= 0b0100000000000000;
    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(cpu_memory);
    }

    assert_eq!(cpu_memory.ppu.low_bits_toggle, true);
    // bit 14 should be cleared and bits 8-13 should be
    // equal to bits 0-6 of the value written to 0x2006
    assert_eq!(cpu_memory.ppu.temp_vram_addr.inner, 0b010_11_01000_00000);

    // LDA #f0 (0b11110000)
    cpu_memory.write(5u16, 0xa9, cpu);
    cpu_memory.write(6u16, 0xf0, cpu);
    // STA $2006
    cpu_memory.write(7u16, 0x8d, cpu);
    cpu_memory.write(8u16, 06, cpu);
    cpu_memory.write(9u16, 0x20, cpu);

    for _ in 0..2 {
        cpu.exec_instruction(cpu_memory);
    }

    assert_eq!(cpu_memory.ppu.low_bits_toggle, false);
    // low 8 bits should all be set equal to the value written
    assert_eq!(cpu_memory.ppu.temp_vram_addr.inner, 0b010_11_01111_10000);
    // 'temp_vram_addr' should've been transferred to 'current_vram_addr'
    assert_eq!(
        cpu_memory.ppu.temp_vram_addr.inner,
        cpu_memory.ppu.current_vram_addr.inner
    );
}

#[test]
fn test_write_2003_read_2004() {
    let (ref mut cpu, ref mut cpu_memory) = init_nes();

    // LDA #ff
    cpu_memory.write(0u16, 0xa9, cpu);
    cpu_memory.write(1u16, 0xff, cpu);
    // STA $2003 (write 0xff to oamaddr)
    cpu_memory.write(2u16, 0x8d, cpu);
    cpu_memory.write(3u16, 03, cpu);
    cpu_memory.write(4u16, 0x20, cpu);

    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(cpu_memory);
    }

    // set oam[0xff] = 0xee
    cpu_memory.ppu.oam.primary.set_byte(0xff, 0xee);

    // LDA $2004 (read from oamdata, i.e read the byte at oam[oamaddr])
    cpu_memory.write(5u16, 0xad, cpu);
    cpu_memory.write(6u16, 04, cpu);
    cpu_memory.write(7u16, 0x20, cpu);

    cpu.exec_instruction(cpu_memory);

    assert_eq!(cpu.a, 0xee);
}

#[test]
fn test_increment_vram_addr() {
    let (ref mut cpu, mmap::Nrom128CpuMemory { ref mut ppu, .. }) = init_nes();

    ppu.current_vram_addr.inner = 0;
    // set increment mode to 32
    ppu.write_register_by_index(0, 0b00000100, cpu);
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr.inner, 32);

    ppu.current_vram_addr.inner = 0;
    // set increment mode to 1
    ppu.write_register_by_index(0, 0b00000000, cpu);
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr.inner, 1);

    // check wrapping behavior ((3fff + 1) % 4000 = 0)
    ppu.current_vram_addr.inner = 0x3fff;
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr.inner, 0);

    // check wrapping behavior ((3fe0 + 0x20) % 4000 = 0)
    ppu.current_vram_addr.inner = 0x3fe0;
    ppu.write_register_by_index(0, 0b00000100, cpu);
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr.inner, 0);

    ppu.current_vram_addr.inner = 0x200a;
    ppu.write_register_by_index(0, 0b00000000, cpu);
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr.inner, 0x200b);
}

#[test]
fn test_increment_vram_addr_xy() {
    let mmap::Nrom128CpuMemory { ref mut ppu, .. } = init_nes().1;

    ppu.current_vram_addr.inner = 0b01_01010_11111;
    ppu.increment_vram_addr_coarse_x();
    // increment should overflow into bit 10
    assert_eq!(ppu.current_vram_addr.inner, 0b00_01010_00000);

    // set coarse y = 31, fine y = 7
    ppu.current_vram_addr.inner = 0b111_10_11111_01010;
    ppu.increment_vram_addr_y();
    // increment should not overflow into bit 11
    assert_eq!(ppu.current_vram_addr.inner, 0b000_10_00000_01010);

    // set coarse y = 29, fine y = 7
    ppu.current_vram_addr.inner = 0b111_10_11101_01010;
    ppu.increment_vram_addr_y();
    // increment should overflow into bit 11
    assert_eq!(ppu.current_vram_addr.inner, 0b000_00_00000_01010);

    // set coarse y = 29, fine y = 6
    ppu.current_vram_addr.inner = 0b110_10_11111_01010;
    ppu.increment_vram_addr_y();
    // increment should not change coarse y
    assert_eq!(ppu.current_vram_addr.inner, 0b111_10_11111_01010);
}

// draws the pattern table at address 0x1000 of 'rom'
pub fn test_draw(rom: &[u8]) {
    assert!(parse::is_valid(&rom));

    let prg_size = 0x4000 * (parse::get_prg_size(&rom) as usize);
    let chr_size = 0x2000 * (parse::get_chr_size(&rom) as usize);

    let mut ppu_memory = mmap::NromPpuMemory::new();

    // load 'rom' into chr ram
    ppu_memory.load_chr_ram(&rom[0x10 + prg_size..=prg_size + chr_size + 0xf]);

    {
        // set universal background color = black
        ppu_memory.palettes[0] = 0x0f;
        // set palette 2 = [red, green, blue]
        ppu_memory.palettes[8 + 1] = 0x06;
        ppu_memory.palettes[8 + 2] = 0x19;
        ppu_memory.palettes[8 + 3] = 0x02;
        for (i, byte) in ppu_memory.nametables.iter_mut().enumerate() {
            // set nametable bytes to point to tiles 0-255 in the attribute table
            *byte = i as u8;
        }
        for attr in ppu_memory.nametables[0x3c0..0x400].iter_mut() {
            // ensure all tiles will use palette 2
            *attr = 0b10101010;
        }
        for attr in ppu_memory.nametables[0x400 + 0x3c0..].iter_mut() {
            *attr = 0b10101010;
        }
    }

    // set nametable mirroring = vertical
    ppu_memory.hor_mirroring = false;

    let mut win = win::XcbWindowWrapper::new("test", 1200, 600).unwrap();
    let renderer = PixelRenderer::new(&mut win.connection, win.win, 256, 240).unwrap();

    let mut ppu = super::Ppu::new(renderer, &mut ppu_memory);
    let ref mut cpu = cpu::Cpu::new_nestest();

    // set background pattern table addr = 0x1000 and base nametable addr = 0x2400
    ppu.write_register_by_index(0, 0b100001, cpu);
    // set coarse x scroll = 0 (offset by 0 in the nametable)
    ppu.temp_vram_addr.inner |= 0b00000;
    // set fine x scroll = 0
    ppu.fine_x_scroll = 0b000;
    // ensure ppu will show background and sprites
    ppu.ppumask = 0b00011110;
    // set color emphasis = red
    ppu.ppumask |= 0b00100000;

    win.map_and_flush();

    loop {
        let start_of_frame = std::time::Instant::now();

        // step through pre-render scanline, all visible scanlines and idle scanline (scanline 240)
        for _ in -1..=240 {
            // each scanline is 341 cycles long, except for the pre-render
            // scanline on odd frames, which is 340 cycles long
            let cycles_in_scanline = 341 - (!ppu.even_frame && ppu.current_scanline == -1) as u64;
            ppu.cycle_count = 0;
            while ppu.cycle_count < cycles_in_scanline as u32 {
                ppu.step(cpu);
            }
            assert_eq!(ppu.cycle_count, cycles_in_scanline as u32);
        }

        // increment fine x
        {
            if ppu.fine_x_scroll == 0b111 {
                if ppu.temp_vram_addr.get_coarse_x() == 0b11111 {
                    ppu.temp_vram_addr.set_coarse_x(0);
                    ppu.temp_vram_addr.inner ^= 0b10000000000;
                } else {
                    ppu.temp_vram_addr.inner += 1;
                }
                ppu.fine_x_scroll = 0;
            } else {
                ppu.fine_x_scroll += 1;
            }
        }

        // step through vblank scanlines
        for _ in 0..20 {
            let cycles_in_scanline = 341 - (!ppu.even_frame && ppu.current_scanline == -1) as u32;
            ppu.cycle_count = 0;
            while ppu.cycle_count < cycles_in_scanline as u32 {
                ppu.step(cpu);
            }
            assert_eq!(ppu.cycle_count, cycles_in_scanline as u32);
        }

        let frame_index = ppu.renderer.render_frame();
        let elapsed = start_of_frame.elapsed();

        let time_left = match std::time::Duration::from_nanos(16_666_677).checked_sub(elapsed) {
            Some(t) => t,
            None => {
                println!("frame took {} μs to render!", elapsed.as_micros());
                std::time::Duration::default()
            }
        };

        std::thread::sleep(time_left);
        ppu.renderer.present(frame_index);
    }
}
