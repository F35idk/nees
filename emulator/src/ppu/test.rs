use crate::Nes;
use crate::{address_bus as bus, cpu};
use bus::{CpuAddressBus, PpuAddressBus};

#[cfg(test)]
fn test_registers(cpu: &mut cpu::Cpu, ppu: &mut super::Ppu, ppu_bus: &mut dyn PpuAddressBus) {
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
    ppu.write_register_by_index(5, x_coord, cpu, ppu_bus);
    ppu.write_register_by_index(5, y_coord, cpu, ppu_bus);

    let ppuctrl = 0b00000011;
    ppu.write_register_by_index(0, ppuctrl, cpu, ppu_bus);

    assert_eq!(ppu.temp_vram_addr.inner, 0b101_11_10001_00110);
    assert_eq!(ppu.bits.fine_x_scroll.get(), x_coord & 0b111);
    // bits 12-14 of 'temp_vram_addr' should be set to temporary fine y scroll
    assert_eq!((ppu.temp_vram_addr.inner >> 12) as u8, y_coord & 0b111);

    let addr_hi = 0x21;
    let addr_low = 0x0f;
    ppu.write_register_by_index(6, addr_hi, cpu, ppu_bus);
    ppu.write_register_by_index(6, addr_low, cpu, ppu_bus);

    assert_eq!(ppu.current_vram_addr.get_addr(), 0x210f);

    assert_eq!(
        ppu.current_vram_addr.inner,
        0x210f | (ppu.temp_vram_addr.inner & !0x3fff)
    );

    let addr_hi = 0x4f;
    let addr_low = 0xe8;
    ppu.write_register_by_index(6, addr_hi, cpu, ppu_bus);
    ppu.write_register_by_index(6, addr_low, cpu, ppu_bus);

    // address is mirrored down
    assert_eq!(ppu.current_vram_addr.inner, 0x4fe8 % 0x4000);
}

#[cfg(test)]
fn test_write_2007(cpu: &mut cpu::Cpu, cpu_bus: &mut dyn CpuAddressBus) {
    // LDA #ee
    cpu_bus.write(0u16, 0xa9, cpu);
    cpu_bus.write(1u16, 0xee, cpu);
    // STA $2007
    cpu_bus.write(2u16, 0x8d, cpu);
    cpu_bus.write(3u16, 07, cpu);
    cpu_bus.write(4u16, 0x20, cpu);

    // disable renderering
    cpu_bus.base().0.ppu.ppumask = 0;
    // set scanline = visible
    cpu_bus.base().0.ppu.current_scanline = 222;

    // set increment mode = 32
    cpu_bus.base().0.ppu.ppuctrl = 0b100;

    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(cpu_bus);
    }

    assert_eq!(cpu_bus.base().0.ppu.current_vram_addr.inner, 32);
    cpu_bus.base().0.ppu.current_vram_addr.inner = 0;

    {
        let (bus::CpuAddressBusBase { ppu, .. }, ref mut ppu_bus) = cpu_bus.base();

        // first read should yield nothing
        assert_eq!(ppu.read_register_by_index(7, *ppu_bus, cpu), 0);

        // second read should get contents at addr
        assert_eq!(ppu.read_register_by_index(7, *ppu_bus, cpu), 0xee);
    }
}

#[cfg(test)]
fn test_write_2000(cpu: &mut cpu::Cpu, cpu_bus: &mut dyn CpuAddressBus) {
    // LDA #ff
    cpu_bus.write(0u16, 0xa9, cpu);
    cpu_bus.write(1u16, 0xff, cpu);
    // STA $2000
    cpu_bus.write(2u16, 0x8d, cpu);
    cpu_bus.write(3u16, 00, cpu);
    cpu_bus.write(4u16, 0x20, cpu);

    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(cpu_bus);
    }

    assert_eq!(cpu_bus.base().0.ppu.temp_vram_addr.inner, 0b11_00000_00000)
}

#[cfg(test)]
fn test_read_2002(cpu: &mut cpu::Cpu, cpu_bus: &mut dyn CpuAddressBus) {
    // LDA $2002
    cpu_bus.write(0u16, 0xad, cpu);
    cpu_bus.write(1u16, 02, cpu);
    cpu_bus.write(2u16, 0x20, cpu);

    cpu_bus.base().0.ppu.bits.low_bits_toggle.set(1);
    cpu.pc = 0;
    cpu.exec_instruction(cpu_bus);

    assert_eq!(cpu_bus.base().0.ppu.bits.low_bits_toggle.is_true(), false);
}

#[cfg(test)]
fn test_write_2005(cpu: &mut cpu::Cpu, cpu_bus: &mut dyn CpuAddressBus) {
    // LDA #7d (0b01111_101)
    cpu_bus.write(0u16, 0xa9, cpu);
    cpu_bus.write(1u16, 0x7d, cpu);
    // STA $2005
    cpu_bus.write(2u16, 0x8d, cpu);
    cpu_bus.write(3u16, 05, cpu);
    cpu_bus.write(4u16, 0x20, cpu);

    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(cpu_bus);
    }

    assert_eq!(cpu_bus.base().0.ppu.bits.fine_x_scroll.get(), 0b101);
    assert_eq!(cpu_bus.base().0.ppu.bits.low_bits_toggle.is_true(), true);
    assert_eq!(cpu_bus.base().0.ppu.temp_vram_addr.inner, 0b00_00000_01111);

    // LDA #5e (0b01011_110)
    cpu_bus.write(5u16, 0xa9, cpu);
    cpu_bus.write(6u16, 0x5e, cpu);
    // STA $2005
    cpu_bus.write(7u16, 0x8d, cpu);
    cpu_bus.write(8u16, 05, cpu);
    cpu_bus.write(9u16, 0x20, cpu);

    for _ in 0..2 {
        cpu.exec_instruction(cpu_bus);
    }

    assert_eq!(cpu_bus.base().0.ppu.temp_vram_addr.inner >> 12, 0b110);
    assert_eq!(cpu_bus.base().0.ppu.bits.low_bits_toggle.is_true(), false);
    assert_eq!(
        cpu_bus.base().0.ppu.temp_vram_addr.inner,
        0b110_00_01011_01111
    );
}

#[cfg(test)]
fn test_write_2006(cpu: &mut cpu::Cpu, cpu_bus: &mut dyn CpuAddressBus) {
    // LDA #ed (0b11101101)
    cpu_bus.write(0u16, 0xa9, cpu);
    cpu_bus.write(1u16, 0xed, cpu);
    // STA $2006
    cpu_bus.write(2u16, 0x8d, cpu);
    cpu_bus.write(3u16, 06, cpu);
    cpu_bus.write(4u16, 0x20, cpu);
    // set bit 14 of 'temp_vram_addr'
    cpu_bus.base().0.ppu.temp_vram_addr.inner |= 0b0100000000000000;
    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(cpu_bus);
    }

    assert_eq!(cpu_bus.base().0.ppu.bits.low_bits_toggle.is_true(), true);
    // bit 14 should be cleared and bits 8-13 should be
    // equal to bits 0-6 of the value written to 0x2006
    assert_eq!(
        cpu_bus.base().0.ppu.temp_vram_addr.inner,
        0b010_11_01000_00000
    );

    // LDA #f0 (0b11110000)
    cpu_bus.write(5u16, 0xa9, cpu);
    cpu_bus.write(6u16, 0xf0, cpu);
    // STA $2006
    cpu_bus.write(7u16, 0x8d, cpu);
    cpu_bus.write(8u16, 06, cpu);
    cpu_bus.write(9u16, 0x20, cpu);

    for _ in 0..2 {
        cpu.exec_instruction(cpu_bus);
    }

    {
        let ref ppu = cpu_bus.base().0.ppu;

        assert_eq!(ppu.bits.low_bits_toggle.is_true(), false);
        // low 8 bits should all be set equal to the value written
        assert_eq!(ppu.temp_vram_addr.inner, 0b010_11_01111_10000);
        // 'temp_vram_addr' should've been transferred to 'current_vram_addr'
        assert_eq!(ppu.temp_vram_addr.inner, ppu.current_vram_addr.inner);
    }
}

#[cfg(test)]
fn test_write_2003_read_2004(cpu: &mut cpu::Cpu, cpu_bus: &mut dyn CpuAddressBus) {
    // LDA #ff
    cpu_bus.write(0u16, 0xa9, cpu);
    cpu_bus.write(1u16, 0xff, cpu);
    // STA $2003 (write 0xff to oamaddr)
    cpu_bus.write(2u16, 0x8d, cpu);
    cpu_bus.write(3u16, 03, cpu);
    cpu_bus.write(4u16, 0x20, cpu);

    cpu.pc = 0;
    for _ in 0..2 {
        cpu.exec_instruction(cpu_bus);
    }

    // set oam[0xff] = 0xee
    cpu_bus.base().0.ppu.primary_oam.set_byte(0xff, 0xee);

    // LDA $2004 (read from oamdata, i.e read the byte at oam[oamaddr])
    cpu_bus.write(5u16, 0xad, cpu);
    cpu_bus.write(6u16, 04, cpu);
    cpu_bus.write(7u16, 0x20, cpu);

    cpu.exec_instruction(cpu_bus);

    assert_eq!(cpu.a, 0xee);
}

#[cfg(test)]
fn test_increment_vram_addr(
    cpu: &mut cpu::Cpu,
    ppu: &mut super::Ppu,
    ppu_bus: &mut dyn PpuAddressBus,
) {
    ppu.current_vram_addr.inner = 0;
    // set increment mode to 32
    ppu.write_register_by_index(0, 0b00000100, cpu, ppu_bus);
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr.inner, 32);

    ppu.current_vram_addr.inner = 0;
    // set increment mode to 1
    ppu.write_register_by_index(0, 0b00000000, cpu, ppu_bus);
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr.inner, 1);

    // check wrapping behavior ((3fff + 1) % 4000 = 0)
    ppu.current_vram_addr.inner = 0x3fff;
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr.inner, 0);

    // check wrapping behavior ((3fe0 + 0x20) % 4000 = 0)
    ppu.current_vram_addr.inner = 0x3fe0;
    ppu.write_register_by_index(0, 0b00000100, cpu, ppu_bus);
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr.inner, 0);

    ppu.current_vram_addr.inner = 0x200a;
    ppu.write_register_by_index(0, 0b00000000, cpu, ppu_bus);
    ppu.increment_vram_addr();
    assert_eq!(ppu.current_vram_addr.inner, 0x200b);
}

#[cfg(test)]
fn test_increment_vram_addr_xy(ppu: &mut super::Ppu) {
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

#[cfg(test)]
fn test_temp_to_current_vram_transfer(ppu: &mut super::Ppu) {
    ppu.temp_vram_addr.inner = 0b01_00000_10101;
    ppu.current_vram_addr.inner = 0b10_10000_00000;
    ppu.transfer_temp_horizontal_bits();

    assert_eq!(ppu.current_vram_addr.inner, 0b11_10000_10101);

    ppu.temp_vram_addr.inner = 0b10_10101_00000;
    ppu.current_vram_addr.inner = 0b01_00000_10000;
    ppu.transfer_temp_vert_bits();

    assert_eq!(ppu.current_vram_addr.inner, 0b11_10101_10000);
}

#[test]
fn test_all() {
    let mut framebuffer = [0u32; 256 * 240];
    let mut nes = Nes::new_test(&mut framebuffer);

    {
        let (bus::CpuAddressBusBase { ref mut ppu, .. }, ref mut ppu_bus) = nes.bus.base();
        test_registers(&mut nes.cpu, ppu, *ppu_bus);
        nes.reset_state();
    }

    test_write_2007(&mut nes.cpu, nes.bus);
    nes.reset_state();

    test_write_2000(&mut nes.cpu, nes.bus);
    nes.reset_state();

    test_read_2002(&mut nes.cpu, nes.bus);
    nes.reset_state();

    test_write_2005(&mut nes.cpu, nes.bus);
    nes.reset_state();

    test_write_2006(&mut nes.cpu, nes.bus);
    nes.reset_state();

    test_write_2003_read_2004(&mut nes.cpu, nes.bus);
    nes.reset_state();

    {
        let (bus::CpuAddressBusBase { ppu, .. }, ref mut ppu_bus) = nes.bus.base();
        test_increment_vram_addr(&mut nes.cpu, ppu, *ppu_bus);
        nes.reset_state();
    }

    test_increment_vram_addr_xy(&mut nes.bus.base().0.ppu);
    nes.reset_state();

    test_temp_to_current_vram_transfer(&mut nes.bus.base().0.ppu);
}
