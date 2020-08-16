use super::super::PixelRenderer;
use super::super::{apu, context, cpu, memory_map as mmap, parse, util, win};
use mmap::MemoryMap;
use std::ops::Sub;

fn init_nes() -> super::super::Nes {
    let cpu = cpu::Cpu::default();
    let memory = mmap::Nrom128MemoryMap::new();
    let ppu = super::Ppu::default();
    let apu = apu::Apu {};
    let mut win = win::XcbWindowWrapper::new("test", 20, 20).unwrap();
    let renderer = PixelRenderer::new(&mut win.connection, win.win, 256, 240).unwrap();

    super::super::Nes {
        cpu,
        ppu,
        apu,
        memory,
        renderer,
    }
}

#[test]
fn test_registers() {
    let mut ppu = super::Ppu::default();
    let ref mut cpu = cpu::Cpu::default();
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
    ppu.write_register_by_index(5, x_coord, cpu, memory);
    ppu.write_register_by_index(5, y_coord, cpu, memory);

    let ppuctrl = 0b00000011;
    ppu.write_register_by_index(0, ppuctrl, cpu, memory);

    assert_eq!(ppu.temp_vram_addr.inner, 0b101_11_10001_00110);
    assert_eq!(ppu.fine_x_scroll & 0b111, x_coord & 0b111);
    // bits 12-14 of 'temp_vram_addr' should be set to temporary fine y scroll
    assert_eq!((ppu.temp_vram_addr.inner >> 12) as u8, y_coord & 0b111);

    let addr_hi = 0x21;
    let addr_low = 0x0f;
    ppu.write_register_by_index(6, addr_hi, cpu, memory);
    ppu.write_register_by_index(6, addr_low, cpu, memory);

    assert_eq!(ppu.current_vram_addr.get_addr(), 0x210f);

    assert_eq!(
        ppu.current_vram_addr.inner,
        0x210f | (ppu.temp_vram_addr.inner & !0x3fff)
    );

    let addr_hi = 0x4f;
    let addr_low = 0xe8;
    ppu.write_register_by_index(6, addr_hi, cpu, memory);
    ppu.write_register_by_index(6, addr_low, cpu, memory);

    // address is mirrored down
    assert_eq!(ppu.current_vram_addr.inner, 0x4fe8 % 0x4000);
}

#[test]
fn test_write_2007() {
    // ppu.current_vram_addr.inner = 0;
    // // write 0xff to ppudata
    // ppu.write_register_by_index(7, 0xff, memory);
    // ppu.current_vram_addr.inner = 0;
    // // read from ppudata
    // assert_eq!(ppu.read_register_by_index(7, memory), 0xff);
}

#[test]
fn test_write_2000() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);

    // LDA #ff
    ctx.memory.write_cpu(0u16, 0xa9, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(1u16, 0xff, &mut ctx.memory_ctx);
    // STA $2000
    ctx.memory.write_cpu(2u16, 0x8d, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(3u16, 00, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(4u16, 0x20, &mut ctx.memory_ctx);

    ctx.cpu.pc = 0;
    for _ in 0..2 {
        ctx.cpu.exec_instruction(&mut ctx.memory, &mut ctx.cpu_ctx);
    }

    assert_eq!(ctx.ppu.temp_vram_addr.inner, 0b11_00000_00000)
}

#[test]
fn test_read_2002() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);

    // LDA $2002
    ctx.memory.write_cpu(0u16, 0xad, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(1u16, 02, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(2u16, 0x20, &mut ctx.memory_ctx);
    //
    ctx.ppu.low_bits_toggle = true;
    ctx.cpu.pc = 0;
    ctx.cpu.exec_instruction(&mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(ctx.ppu.low_bits_toggle, false);
}

#[test]
fn test_write_2005() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);

    // LDA #7d (0b01111_101)
    ctx.memory.write_cpu(0u16, 0xa9, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(1u16, 0x7d, &mut ctx.memory_ctx);
    // STA $2005
    ctx.memory.write_cpu(2u16, 0x8d, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(3u16, 05, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(4u16, 0x20, &mut ctx.memory_ctx);
    //
    ctx.cpu.pc = 0;
    for _ in 0..2 {
        ctx.cpu.exec_instruction(&mut ctx.memory, &mut ctx.cpu_ctx);
    }
    //
    assert_eq!(ctx.ppu.fine_x_scroll, 0b101);
    assert_eq!(ctx.ppu.low_bits_toggle, true);
    assert_eq!(ctx.ppu.temp_vram_addr.inner, 0b00_00000_01111);
    //
    // LDA #5e (0b01011_110)
    ctx.memory.write_cpu(5u16, 0xa9, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(6u16, 0x5e, &mut ctx.memory_ctx);
    // STA $2005
    ctx.memory.write_cpu(7u16, 0x8d, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(8u16, 05, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(9u16, 0x20, &mut ctx.memory_ctx);
    //
    for _ in 0..2 {
        ctx.cpu.exec_instruction(&mut ctx.memory, &mut ctx.cpu_ctx);
    }
    //
    assert_eq!(ctx.ppu.temp_vram_addr.inner >> 12, 0b110);
    assert_eq!(ctx.ppu.low_bits_toggle, false);
    assert_eq!(ctx.ppu.temp_vram_addr.inner, 0b110_00_01011_01111);
}

#[test]
fn test_write_2006() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);

    // LDA #ed (0b11101101)
    ctx.memory.write_cpu(0u16, 0xa9, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(1u16, 0xed, &mut ctx.memory_ctx);
    // STA $2006
    ctx.memory.write_cpu(2u16, 0x8d, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(3u16, 06, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(4u16, 0x20, &mut ctx.memory_ctx);
    // set bit 14 of 'temp_vram_addr'
    ctx.ppu.temp_vram_addr.inner |= 0b0100000000000000;
    ctx.cpu.pc = 0;
    for _ in 0..2 {
        ctx.cpu.exec_instruction(&mut ctx.memory, &mut ctx.cpu_ctx);
    }
    //
    assert_eq!(ctx.ppu.low_bits_toggle, true);
    // bit 14 should be cleared and bits 8-13 should be
    // equal to bits 0-6 of the value written to 0x2006
    assert_eq!(ctx.ppu.temp_vram_addr.inner, 0b010_11_01000_00000);
    //
    // LDA #f0 (0b11110000)
    ctx.memory.write_cpu(5u16, 0xa9, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(6u16, 0xf0, &mut ctx.memory_ctx);
    // STA $2006
    ctx.memory.write_cpu(7u16, 0x8d, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(8u16, 06, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(9u16, 0x20, &mut ctx.memory_ctx);
    //
    for _ in 0..2 {
        ctx.cpu.exec_instruction(&mut ctx.memory, &mut ctx.cpu_ctx);
    }
    //
    assert_eq!(ctx.ppu.low_bits_toggle, false);
    // low 8 bits should all be set equal to the value written
    assert_eq!(ctx.ppu.temp_vram_addr.inner, 0b010_11_01111_10000);
    // 'temp_vram_addr' should've been transferred to 'current_vram_addr'
    assert_eq!(
        ctx.ppu.temp_vram_addr.inner,
        ctx.ppu.current_vram_addr.inner
    );
}

#[test]
fn test_write_2003_read_2004() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);

    // LDA #ff
    ctx.memory.write_cpu(0u16, 0xa9, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(1u16, 0xff, &mut ctx.memory_ctx);
    // STA $2003 (write 0xff to oamaddr)
    ctx.memory.write_cpu(2u16, 0x8d, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(3u16, 03, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(4u16, 0x20, &mut ctx.memory_ctx);
    //
    ctx.cpu.pc = 0;
    for _ in 0..2 {
        ctx.cpu.exec_instruction(&mut ctx.memory, &mut ctx.cpu_ctx);
    }
    //
    // set oam[0xff] = 0xee
    unsafe {
        ctx.ppu.oam.bytes[0xff] = 0xee;
    }
    //
    // LDA $2004 (read from oamdata, i.e read the byte at oam[oamaddr])
    ctx.memory.write_cpu(5u16, 0xad, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(6u16, 04, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(7u16, 0x20, &mut ctx.memory_ctx);
    //
    ctx.cpu.exec_instruction(&mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(ctx.cpu.a, 0xee);
}

#[test]
fn test_increment_vram_addr() {
    let ref mut ppu = super::Ppu::default();
    let ref mut cpu = cpu::Cpu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();
    //
    ppu.current_vram_addr.inner = 0;
    // set increment mode to 32
    ppu.write_register_by_index(0, 0b00000100, cpu, memory);
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr.inner, 32);
    //
    ppu.current_vram_addr.inner = 0;
    // set increment mode to 1
    ppu.write_register_by_index(0, 0b00000000, cpu, memory);
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr.inner, 1);
    //
    // check wrapping behavior ((3fff + 1) % 4000 = 0)
    ppu.current_vram_addr.inner = 0x3fff;
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr.inner, 0);
    //
    // check wrapping behavior ((3fe0 + 0x20) % 4000 = 0)
    ppu.current_vram_addr.inner = 0x3fe0;
    ppu.write_register_by_index(0, 0b00000100, cpu, memory);
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr.inner, 0);
}

#[test]
fn test_increment_vram_addr_xy() {
    let ref mut ppu = super::Ppu::default();
    //
    ppu.current_vram_addr.inner = 0b01_01010_11111;
    ppu.increment_vram_addr_coarse_x();
    // increment should overflow into bit 10
    assert_eq!(ppu.current_vram_addr.inner, 0b00_01010_00000);
    //
    // set coarse y = 31, fine y = 7
    ppu.current_vram_addr.inner = 0b111_10_11111_01010;
    ppu.increment_vram_addr_y();
    // increment should not overflow into bit 11
    assert_eq!(ppu.current_vram_addr.inner, 0b000_10_00000_01010);
    //
    // set coarse y = 29, fine y = 7
    ppu.current_vram_addr.inner = 0b111_10_11101_01010;
    ppu.increment_vram_addr_y();
    // increment should overflow into bit 11
    assert_eq!(ppu.current_vram_addr.inner, 0b000_00_00000_01010);
    //
    // set coarse y = 29, fine y = 6
    ppu.current_vram_addr.inner = 0b110_10_11111_01010;
    ppu.increment_vram_addr_y();
    // increment should not change coarse y
    assert_eq!(ppu.current_vram_addr.inner, 0b111_10_11111_01010);
}

#[test]
fn test_oamdma() {
    let mut ppu = super::Ppu::default();
    let ref mut cpu = cpu::Cpu::default();
    let ref mut memory = mmap::Nrom128MemoryMap::new();
    let ref mut framebuffer = [0u32; 240 * 256];
    //
    ppu.write_register_by_index(3, 0xee, cpu, memory);
    ppu.write_oamdma(0x20, cpu, memory, framebuffer);
    //
    assert_eq!(cpu.cycle_count, 513);
    assert_eq!(ppu.oamaddr, 0xee);
}

#[test]
fn test_misc() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);

    ctx.ppu.write_ppuaddr(0x24);
    ctx.ppu.write_ppuaddr(0x00);
    //
    assert_eq!(ctx.ppu.current_vram_addr.inner, 0x2400);
}

// draws the pattern table at address 0x1000 of 'rom'
pub fn test_draw(rom: &[u8]) {
    assert!(parse::is_valid(&rom));

    let mut win = win::XcbWindowWrapper::new("mynes", 1200, 600).unwrap();
    let renderer = PixelRenderer::new(&mut win.connection, win.win, 256, 240).unwrap();
    let memory = mmap::Nrom128MemoryMap::new();
    let ppu = super::Ppu::default();
    let apu = apu::Apu {};
    let cpu = cpu::Cpu::default();

    let mut nes = super::super::Nes {
        ppu,
        cpu,
        memory,
        renderer,
        apu,
    };

    let ctx = context::NesContext::new(&mut nes);
    let ref mut ppu = ctx.ppu;
    let ref mut cpu = ctx.cpu;

    let prg_size = 0x4000 * (parse::get_prg_size(&rom) as usize);
    let chr_size = 0x2000 * (parse::get_chr_size(&rom) as usize);
    ctx.memory
        .load_chr_ram(&rom[0x10 + prg_size..=prg_size + chr_size + 0xf]);

    {
        // set universal background color = black
        ctx.memory.palettes[0] = 0x0f;
        // set palette 2 = [red, green, blue]
        ctx.memory.palettes[8 + 1] = 0x06;
        ctx.memory.palettes[8 + 2] = 0x19;
        ctx.memory.palettes[8 + 3] = 0x02;

        for (i, byte) in ctx.memory.nametables.iter_mut().enumerate() {
            // set nametable bytes to point to tiles 0-255 in the attribute table
            *byte = i as u8;
        }

        for attr in ctx.memory.nametables[0x3c0..0x400].iter_mut() {
            // ensure all tiles will use palette 2
            *attr = 0b10101010;
        }

        for attr in ctx.memory.nametables[0x400 + 0x3c0..].iter_mut() {
            *attr = 0b10101010;
        }
    }

    // set nametable mirroring mask = vertical mirroring
    ctx.memory.nametable_mirroring_mask = !0x800;
    // set background pattern table addr = 0x1000 and base nametable addr = 0x2400
    ppu.write_register_by_index(0, 0b100001, cpu, &mut ctx.memory);
    // set coarse x scroll = 0 (offset by 0 in the nametable)
    ppu.temp_vram_addr.inner |= 0b00000;
    // set fine x scroll = 0
    ppu.fine_x_scroll = 0b000;
    // ensure ppu will show background and sprites
    ppu.ppumask = 0b11110;

    win.map_and_flush();

    loop {
        let start_of_frame = std::time::Instant::now();

        // step through pre-render scanline, all visible scanlines and idle scanline (scanline 240)
        for _ in -1..=240 {
            // each scanline is 341 cycles long, except for the pre-render
            // scanline on odd frames, which is 340 cycles long
            let cycles_in_scanline = 341 - (!ppu.even_frame && ppu.current_scanline == -1) as u64;
            let framebuffer = util::pixels_to_u32(&mut ctx.renderer);
            ppu.cycle_count = 0;

            ppu.step(cycles_in_scanline, cpu, &mut ctx.memory, framebuffer);
            assert_eq!(ppu.cycle_count, cycles_in_scanline as u64);
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
            let cycles_in_scanline = 341 - (!ppu.even_frame && ppu.current_scanline == -1) as u64;
            let framebuffer = util::pixels_to_u32(&mut ctx.renderer);
            ppu.cycle_count = 0;

            ppu.step(cycles_in_scanline, cpu, &mut ctx.memory, framebuffer);
            assert_eq!(ppu.cycle_count, cycles_in_scanline as u64);
        }

        let frame_index = ctx.renderer.render_frame();

        let elapsed = start_of_frame.elapsed();
        let time_left = match std::time::Duration::from_nanos(16_666_677).checked_sub(elapsed) {
            Some(t) => t,
            None => {
                println!("frame took {} μs to render!", elapsed.as_micros());
                std::time::Duration::default()
            }
        };
        std::thread::sleep(time_left);

        ctx.renderer.present(frame_index);
    }
}
