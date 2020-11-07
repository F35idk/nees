use super::super::{memory_map as mem, util};
use super::Cpu;
use mem::{CpuMemoryMap, PpuMemoryMap};

fn test_adc(cpu: &mut Cpu, memory: &mut mem::NromCpuMemory) {
    cpu.p = 0x6e;
    cpu.adc(0x69);

    assert_eq!(cpu.a, 0x69);
    assert_eq!(cpu.p, 0x2c);

    cpu.a = 1;
    cpu.p = 0x6d;
    cpu.adc(0x69);

    assert_eq!(cpu.a, 0x6b);
    assert_eq!(cpu.p, 0x2c);

    cpu.a = 0x7f;
    cpu.p = 0x25;
    cpu.adc(0x80);

    assert_eq!(cpu.a, 0);
    assert_eq!(cpu.p, 0x27);

    memory.write(0x80u16, 00, cpu);
    memory.write(0x81u16, 02, cpu);
    memory.write(0x200u16, 0x69u8, cpu);
    cpu.a = 0;
    cpu.p = 0x66;
    // ADC ($80, X) (indexed indirect)
    let cyc = cpu.debug_exec_opcode([0x61, 0x80, 0x00], memory);

    assert_eq!(cyc, 6);
    assert_eq!(cpu.a, 0x69);
    assert_eq!(cpu.p, 0x24);

    memory.write(0x80u16, 0, cpu);
    memory.write(0x81u16, 0, cpu);
    memory.write(0x200u16, 0, cpu);
    memory.write(0x78u16, 0x69, cpu);
    cpu.a = 0;
    cpu.p = 0x66;
    // ADC $78 (zero page)
    let cyc = cpu.debug_exec_opcode([0x65, 0x78, 0x00], memory);

    assert_eq!(cyc, 3);
    assert_eq!(cpu.a, 0x69);
    assert_eq!(cpu.p, 0x24);

    memory.write(0x78u16, 0, cpu);
    memory.write(0x678u16, 0x69, cpu);
    cpu.a = 0;
    cpu.p = 0x66;
    let cyc = cpu.debug_exec_opcode([0x6d, 0x78, 0x06], memory);

    assert_eq!(cyc, 4);
    assert_eq!(cpu.a, 0x69);
    assert_eq!(cpu.p, 0x24);
}

fn test_and(cpu: &mut Cpu, memory: &mut mem::NromCpuMemory) {
    cpu.a = 0x55;
    cpu.p = 0;
    let _ = cpu.and(0xaa);

    // assert_eq!(cyc, 2);
    assert_eq!(cpu.a, 0);
    assert_eq!(cpu.p, 2); // zero-flag should be set

    memory.write(0x80u16, 0, cpu);
    memory.write(0x81u16, 02, cpu);
    memory.write(0x200u16, 0xaa, cpu);
    cpu.a = 0x55;
    cpu.p = 0;
    // AND ($80, X) @ 80 = 0x200 = 0xaa
    let cyc = cpu.debug_exec_opcode([0x21, 0x80, 00], memory);

    assert_eq!(cyc, 6);
    assert_eq!(cpu.a, 0);
    assert_eq!(cpu.p, 2); // zero-flag should be set
}

fn test_asl(cpu: &mut Cpu, memory: &mut mem::NromCpuMemory) {
    cpu.a = 0x80;
    cpu.p = 0xe5;
    // ASL A
    let cyc = cpu.debug_exec_opcode([0x0a, 00, 00], memory);

    assert_eq!(cyc, 2);
    assert_eq!(cpu.a, 0);
    assert_eq!(cpu.p, 0x67);

    cpu.a = 0;
    cpu.p = 0xe5;
    memory.write(0x78u16, 0x80, cpu);
    // ASL $78
    let cyc = cpu.debug_exec_opcode([0x06, 0x78, 00], memory);

    assert_eq!(cyc, 5);
    assert_eq!(memory.read(0x78u16, cpu), 0);
    assert_eq!(cpu.p, 0x67);

    cpu.p = 0xa5;
    memory.write(0x78u16, 0, cpu);
    memory.write(0x678u16, 0x55, cpu);
    // ASL $0678
    let cyc = cpu.debug_exec_opcode([0x0e, 0x78, 0x06], memory);

    assert_eq!(cyc, 6);
    assert_eq!(memory.read(0x678u16, cpu), 0xaa);
    assert_eq!(cpu.p, 0xa4);
}

fn test_branch_instrs(cpu: &mut Cpu, memory: &mut mem::NromCpuMemory) {
    cpu.p = 0;
    cpu.pc = 0x100;
    let cyc = cpu.debug_exec_opcode([0x90, 0x80, 00], memory);

    assert_eq!(cpu.pc, 0x100 - 0x80 + 2);
    assert_eq!(cyc, 4);

    cpu.pc = 0x100;
    cpu.p = 0;
    let cyc = cpu.debug_exec_opcode([0x90, 0x7f, 00], memory);

    assert_eq!(cpu.pc, 0x100 + 0x7f + 2);
    assert_eq!(cyc, 3);

    cpu.pc = 0x100;
    cpu.p = 0b01000000;
    let cyc = cpu.debug_exec_opcode([0x50, 0xff, 00], memory);

    assert_eq!(cpu.pc, 0x100 + 2);
    assert_eq!(cyc, 2);

    cpu.pc = 0x100;
    cpu.p = 0b01000000;
    let cyc = cpu.debug_exec_opcode([0x70, 0xff, 00], memory);

    assert_eq!(cpu.pc, 0x100 - 1 + 2);
    assert_eq!(cyc, 3);
}

fn test_bit(cpu: &mut Cpu, memory: &mut mem::NromCpuMemory) {
    cpu.p = 0xa4;
    cpu.a = 0xff;
    cpu.pc = 0x40;
    memory.write(1u16, 0xff, cpu);
    // BIT $01
    let cyc = cpu.debug_exec_opcode([0x24, 0x01, 00], memory);

    assert_eq!(cyc, 3);
    assert_eq!(cpu.p, 0xe4);
}

fn test_cmp(cpu: &mut Cpu) {
    cpu.a = 0x40;
    cpu.p = 0x25;
    let _ = cpu.compare_register_val(cpu.a, 0x41);

    // assert_eq!(cyc, 2);
    assert_eq!(cpu.p, 0xa4);
}

fn test_dec_inc(cpu: &mut Cpu, memory: &mut mem::NromCpuMemory) {
    memory.write(0x78u16, 0x80, cpu);
    cpu.p = 0xa4;
    // DEC $78
    let cyc = cpu.debug_exec_opcode([0xc6, 0x78, 00], memory);

    assert_eq!(cyc, 5);
    assert_eq!(cpu.pc, 2);
    assert_eq!(cpu.p, 0x24);
}

fn test_eor(cpu: &mut Cpu, memory: &mut mem::NromCpuMemory) {
    cpu.p = 0x6c;
    cpu.a = 0x5f;
    // EOR #$AA
    let cyc = cpu.debug_exec_opcode([0x49, 0xaa, 00], memory);

    assert_eq!(cyc, 2);
    assert_eq!(cpu.pc, 2);
    assert_eq!(cpu.p, 0xec);

    memory.write(0x80u16, 00, cpu);
    memory.write(0x81u16, 02, cpu);
    memory.write(0x200u16, 0xaa, cpu);
    cpu.p = 0x64;
    cpu.a = 0x5f;
    cpu.x = 0;
    // EOR ($80, X) (indexed indirect)
    let cyc = cpu.debug_exec_opcode([0x41, 0x80, 00], memory);

    assert_eq!(cyc, 6);
    assert_eq!(cpu.pc, 4);
    assert_eq!(cpu.p, 0xe4);
}

fn test_jmp(cpu: &mut Cpu, memory: &mut mem::NromCpuMemory) {
    // JMP $c5f5
    cpu.debug_exec_opcode([0x4c, 0xf5, 0xc5], memory);

    assert_eq!(cpu.pc, 0xc5f5);

    cpu.pc = 0;
    memory.write(0x2ffu16, 0x00, cpu);
    memory.write(0x200u16, 0x03, cpu);
    // JMP ($02ff) (@2ff = 0, @200 = 03)
    cpu.debug_exec_opcode([0x6c, 0xff, 0x02], memory);

    assert_eq!(cpu.pc, 0x300);
}

fn test_jsr(cpu: &mut Cpu, memory: &mut mem::NromCpuMemory) {
    cpu.pc = 0x300;
    cpu.sp = 0xff;

    let cyc = cpu.debug_exec_opcode([0x20, 00, 00], memory);

    assert_eq!(cyc, 6);
    assert_eq!(
        memory.read(cpu.sp.wrapping_add(1) as u16 + 0x100, cpu),
        0x02
    );
    assert_eq!(
        memory.read(cpu.sp.wrapping_add(2) as u16 + 0x100, cpu),
        0x03
    );

    cpu.pc = 0x300;
    cpu.sp = 0x00;

    // sp = 0x00, so the jsr will cause it to underflow through zero
    cpu.debug_exec_opcode([0x20, 00, 00], memory);

    assert_eq!(
        memory.read(cpu.sp.wrapping_add(1) as u16 + 0x100, cpu),
        0x02
    );
    assert_eq!(
        memory.read(cpu.sp.wrapping_add(2) as u16 + 0x100, cpu),
        0x03
    );

    cpu.pc = 0x65fd;
    cpu.sp = 0xfd;
    cpu.debug_exec_opcode([0x20, 0xe5, 0xf7], memory);

    assert_eq!(cpu.sp, 0xfb);
    assert_eq!(cpu.pc, 0xf7e5);
    assert_eq!(
        memory.read(cpu.sp.wrapping_add(1) as u16 + 0x100, cpu),
        0xfd + 2
    );
    assert_eq!(
        memory.read(cpu.sp.wrapping_add(2) as u16 + 0x100, cpu),
        0x65
    );
}

fn test_jsr_2(cpu: &mut Cpu, memory: &mut mem::NromCpuMemory) {
    cpu.pc = 0x1620;
    cpu.sp = 0xfb;
    cpu.x = 0x33;
    cpu.y = 0xba;

    let cyc = cpu.debug_exec_opcode([0x20, 0xe5, 0xf7], memory);

    assert_eq!(cpu.pc, 0xf7e5);
    assert_eq!(cyc, 6);
    assert_eq!(
        memory.read(cpu.sp.wrapping_add(1) as u16 + 0x100, cpu),
        0x20 + 2
    );
    assert_eq!(
        memory.read(cpu.sp.wrapping_add(2) as u16 + 0x100, cpu),
        0x16
    );
}

fn test_ld(cpu: &mut Cpu, memory: &mut mem::NromCpuMemory) {
    memory.write(0x89u16, 0x00, cpu);
    memory.write(0x8au16, 0x03, cpu);
    memory.write(0x300u16, 0x89, cpu);
    cpu.y = 0;
    cpu.p = 0x27;
    // LDA ($89), Y (indirect indexed)
    let cyc = cpu.debug_exec_opcode([0xb1, 0x89, 00], memory);

    assert_eq!(cyc, 5);
    assert_eq!(cpu.pc, 2);
    assert_eq!(cpu.p, 0xa5);

    memory.write(0x633u16, 0xaa, cpu);
    cpu.y = 0;
    cpu.p = 0x67;
    // LDY ($0633, X) (indexed indirect)
    let cyc = cpu.debug_exec_opcode([0xbc, 0x33, 06], memory);

    assert_eq!(cyc, 4);
    assert_eq!(cpu.y, 0xaa);
    assert_eq!(cpu.p, 0xe5);
}

fn test_lsr() {
    // TODO: ..
}

fn test_ora() {
    // TODO: ..
}

fn test_push_pull(cpu: &mut Cpu, memory: &mut mem::NromCpuMemory) {
    cpu.a = 0xff;
    cpu.sp = 0xfb;
    // PHA (push accumulator)
    let cyc_1 = cpu.debug_exec_opcode([0x48, 00, 00], memory);
    // PLP (pop top of stack into status flags)
    let cyc_2 = cpu.debug_exec_opcode([0x28, 00, 00], memory);

    assert_eq!(cyc_1, 3);
    assert_eq!(cyc_2, 4);
    assert_eq!(cpu.p, 0xef);
    assert_eq!(cpu.sp, 0xfb);

    cpu.a = 0;
    cpu.p = 0x6f;
    cpu.sp = 0xfb;
    // PHP (push status flags)
    let cyc_1 = cpu.debug_exec_opcode([0x08, 00, 00], memory);
    // PLA (pop top of stack into accumulator)
    let cyc_2 = cpu.debug_exec_opcode([0x68, 00, 00], memory);

    assert_eq!(cyc_1, 3);
    assert_eq!(cyc_2, 4);
    assert_eq!(cpu.a, 0x7f);
    assert_eq!(cpu.p, 0x6d);
    assert_eq!(cpu.sp, 0xfb);
}

fn test_rol_ror(cpu: &mut Cpu, memory: &mut mem::NromCpuMemory) {
    cpu.a = 0x55;
    cpu.p = 0x24;
    let cyc = cpu.debug_exec_opcode([0x2a, 00, 00], memory);

    assert_eq!(cyc, 2);
    assert_eq!(cpu.a, 0xaa);
    assert_eq!(cpu.p, 0xa4);

    cpu.x = 0x55;
    cpu.p = 0x24;
    memory.write(0x655u16, 0x55, cpu);
    // ROL $0600, X (absolute indexed)
    let cyc = cpu.debug_exec_opcode([0x3e, 00, 06], memory);

    assert_eq!(cyc, 7);
    assert_eq!(cpu.p, 0xa4);
    assert_eq!(memory.read(0x655u16, cpu), 0xaa);

    cpu.x = 0x55;
    cpu.p = 0x65;
    memory.write(0x55u16, 1, cpu);
    // ROR $00, X (zero page indexed)
    let cyc = cpu.debug_exec_opcode([0x76, 00, 00], memory);

    assert_eq!(cyc, 6);
    assert_eq!(cpu.p, 0xe5);
    assert_eq!(memory.read(0x55u16, cpu), 0x80);
}

fn test_rti() {
    // TODO: ..
}

fn test_rts(cpu: &mut Cpu, memory: &mut mem::NromCpuMemory) {
    cpu.pc = 0x0401;
    cpu.sp = 0xf0;
    // JSR $182e
    let cyc_1 = cpu.debug_exec_opcode([0x20, 0x2e, 0x18], memory);
    assert_eq!(cyc_1, 6);
    assert_eq!(cpu.pc, 0x182e);
    assert_eq!(
        memory.read(0x100 + cpu.sp.wrapping_add(1) as u16, cpu),
        01 + 2
    );
    assert_eq!(memory.read(0x100 + cpu.sp.wrapping_add(2) as u16, cpu), 04);

    // RTS
    let cyc_2 = cpu.debug_exec_opcode([0x60, 00, 00], memory);
    assert_eq!(cyc_2, 6);
    assert_eq!(cpu.pc, 0x0401 + 3);
    assert_eq!(cpu.sp, 0xf0);
}

fn test_sbc(cpu: &mut Cpu) {
    cpu.a = 0x40;
    cpu.p = 0x65;
    cpu.sbc(0x40);

    assert_eq!(cpu.a, 0);
    assert_eq!(cpu.p, 0x27);

    cpu.a = 0x40;
    cpu.p = 0x25;
    cpu.sbc(0x3f);

    assert_eq!(cpu.a, 1);
    assert_eq!(cpu.p, 0x25);

    cpu.a = 0x40;
    cpu.p = 0xe5;
    cpu.sbc(0x41);

    assert_eq!(cpu.a, 0xff);
    assert_eq!(cpu.p, 0xa4);

    cpu.a = 0x81;
    cpu.p = 0xe5;
    cpu.sbc(0x7f);

    assert_eq!(cpu.a, 2);
    assert_eq!(cpu.p, 0x65);
}

fn test_sta_stx_sty() {
    // TODO: ..
}

fn test_brk() {
    // FIXME: ..
    // TODO: ..
}

#[test]
fn test_all() {
    let (ref mut cpu, ref mut memory) = util::init_nes();

    test_adc(cpu, memory);
    util::reset_nes_state(cpu, memory);

    test_and(cpu, memory);
    util::reset_nes_state(cpu, memory);

    test_asl(cpu, memory);
    util::reset_nes_state(cpu, memory);

    test_branch_instrs(cpu, memory);
    util::reset_nes_state(cpu, memory);

    test_bit(cpu, memory);
    util::reset_nes_state(cpu, memory);

    test_cmp(cpu);
    util::reset_nes_state(cpu, memory);

    test_dec_inc(cpu, memory);
    util::reset_nes_state(cpu, memory);

    test_eor(cpu, memory);
    util::reset_nes_state(cpu, memory);

    test_jmp(cpu, memory);
    util::reset_nes_state(cpu, memory);

    test_jsr(cpu, memory);
    util::reset_nes_state(cpu, memory);

    test_jsr_2(cpu, memory);
    util::reset_nes_state(cpu, memory);

    test_ld(cpu, memory);
    util::reset_nes_state(cpu, memory);

    test_push_pull(cpu, memory);
    util::reset_nes_state(cpu, memory);

    test_rol_ror(cpu, memory);
    util::reset_nes_state(cpu, memory);

    test_rts(cpu, memory);
    util::reset_nes_state(cpu, memory);

    test_sbc(cpu);
}
