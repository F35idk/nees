use super::super::PixelRenderer;
use super::super::{apu, context, memory_map as mmap, ppu, util, win};
use super::Cpu;
use mmap::MemoryMap;

fn init_nes() -> super::super::Nes {
    let cpu = Cpu::default();
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
fn test_adc() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);

    ctx.cpu.p = 0x6e;
    ctx.cpu.adc(0x69);
    //
    assert_eq!(ctx.cpu.a, 0x69);
    assert_eq!(ctx.cpu.p, 0x2c);
    //
    ctx.cpu.a = 1;
    ctx.cpu.p = 0x6d;
    ctx.cpu.adc(0x69);
    //
    assert_eq!(ctx.cpu.a, 0x6b);
    assert_eq!(ctx.cpu.p, 0x2c);
    //
    ctx.cpu.a = 0x7f;
    ctx.cpu.p = 0x25;
    ctx.cpu.adc(0x80);
    //
    assert_eq!(ctx.cpu.a, 0);
    assert_eq!(ctx.cpu.p, 0x27);
    //
    ctx.memory.write_cpu(0x80u16, 00, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(0x81u16, 02, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(0x200u16, 0x69u8, &mut ctx.memory_ctx);
    ctx.cpu.a = 0;
    ctx.cpu.p = 0x66;
    // ADC ($80, X) (indexed indirect)
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x61, 0x80, 0x00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc, 6);
    assert_eq!(ctx.cpu.a, 0x69);
    assert_eq!(ctx.cpu.p, 0x24);
    //
    ctx.memory.write_cpu(0x80u16, 0, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(0x81u16, 0, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(0x200u16, 0, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(0x78u16, 0x69, &mut ctx.memory_ctx);
    ctx.cpu.a = 0;
    ctx.cpu.p = 0x66;
    // ADC $78 (zero page)
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x65, 0x78, 0x00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc, 3);
    assert_eq!(ctx.cpu.a, 0x69);
    assert_eq!(ctx.cpu.p, 0x24);
    //
    ctx.memory.write_cpu(0x78u16, 0, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(0x678u16, 0x69, &mut ctx.memory_ctx);
    ctx.cpu.a = 0;
    ctx.cpu.p = 0x66;
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x6d, 0x78, 0x06], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc, 4);
    assert_eq!(ctx.cpu.a, 0x69);
    assert_eq!(ctx.cpu.p, 0x24);
}

#[test]
fn test_and() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);

    ctx.cpu.a = 0x55;
    ctx.cpu.p = 0;
    let _ = ctx.cpu.and(0xaa);

    // assert_eq!(cyc, 2);
    assert_eq!(ctx.cpu.a, 0);
    assert_eq!(ctx.cpu.p, 2); // zero-flag should be set

    ctx.memory.write_cpu(0x80u16, 0, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(0x81u16, 02, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(0x200u16, 0xaa, &mut ctx.memory_ctx);
    ctx.cpu.a = 0x55;
    ctx.cpu.p = 0;
    // AND ($80, X) @ 80 = 0x200 = 0xaa
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x21, 0x80, 00], &mut ctx.memory, &mut ctx.cpu_ctx);

    assert_eq!(cyc, 6);
    assert_eq!(ctx.cpu.a, 0);
    assert_eq!(ctx.cpu.p, 2); // zero-flag should be set
}

#[test]
fn test_asl() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);
    //
    ctx.cpu.a = 0x80;
    ctx.cpu.p = 0xe5;
    // ASL A
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x0a, 00, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc, 2);
    assert_eq!(ctx.cpu.a, 0);
    assert_eq!(ctx.cpu.p, 0x67);
    //
    ctx.cpu.a = 0;
    ctx.cpu.p = 0xe5;
    ctx.memory.write_cpu(0x78u16, 0x80, &mut ctx.memory_ctx);
    // ASL $78
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x06, 0x78, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc, 5);
    assert_eq!(ctx.memory.read_cpu(0x78u16, &mut ctx.memory_ctx), 0);
    assert_eq!(ctx.cpu.p, 0x67);
    //
    ctx.cpu.p = 0xa5;
    ctx.memory.write_cpu(0x78u16, 0, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(0x678u16, 0x55, &mut ctx.memory_ctx);
    // ASL $0678
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x0e, 0x78, 0x06], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc, 6);
    assert_eq!(ctx.memory.read_cpu(0x678u16, &mut ctx.memory_ctx), 0xaa);
    assert_eq!(ctx.cpu.p, 0xa4);
}

#[test]
fn test_branch_instrs() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);
    //
    ctx.cpu.p = 0;
    ctx.cpu.pc = 0x100;
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x90, 0x80, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(ctx.cpu.pc, 0x100 - 0x80 + 2);
    assert_eq!(cyc, 4);
    //
    ctx.cpu.pc = 0x100;
    ctx.cpu.p = 0;
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x90, 0x7f, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(ctx.cpu.pc, 0x100 + 0x7f + 2);
    assert_eq!(cyc, 3);
    //
    ctx.cpu.pc = 0x100;
    ctx.cpu.p = 0b01000000;
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x50, 0xff, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(ctx.cpu.pc, 0x100 + 2);
    assert_eq!(cyc, 2);
    //
    ctx.cpu.pc = 0x100;
    ctx.cpu.p = 0b01000000;
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x70, 0xff, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(ctx.cpu.pc, 0x100 - 1 + 2);
    assert_eq!(cyc, 3);
}
//
#[test]
fn test_bit() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);
    //
    ctx.cpu.p = 0xa4;
    ctx.cpu.a = 0xff;
    ctx.cpu.pc = 0x40;
    ctx.memory.write_cpu(1u16, 0xff, &mut ctx.memory_ctx);
    // BIT $01
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x24, 0x01, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc, 3);
    assert_eq!(ctx.cpu.p, 0xe4);
}
//
#[test]
fn test_cmp() {
    let ref mut cpu = Cpu::default();
    cpu.a = 0x40;
    cpu.p = 0x25;
    let _ = cpu.compare_register_val(cpu.a, 0x41);
    //
    // assert_eq!(cyc, 2);
    assert_eq!(cpu.p, 0xa4);
}

#[test]
fn test_dec_inc() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);

    ctx.memory.write_cpu(0x78u16, 0x80, &mut ctx.memory_ctx);
    ctx.cpu.p = 0xa4;
    // DEC $78
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0xc6, 0x78, 00], &mut ctx.memory, &mut ctx.cpu_ctx);

    assert_eq!(cyc, 5);
    assert_eq!(ctx.cpu.pc, 2);
    assert_eq!(ctx.cpu.p, 0x24);
}
//
#[test]
fn test_eor() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);
    //
    ctx.cpu.p = 0x6c;
    ctx.cpu.a = 0x5f;
    // EOR #$AA
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x49, 0xaa, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc, 2);
    assert_eq!(ctx.cpu.pc, 2);
    assert_eq!(ctx.cpu.p, 0xec);
    //
    ctx.memory.write_cpu(0x80u16, 00, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(0x81u16, 02, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(0x200u16, 0xaa, &mut ctx.memory_ctx);
    ctx.cpu.p = 0x64;
    ctx.cpu.a = 0x5f;
    ctx.cpu.x = 0;
    // EOR ($80, X) (indexed indirect)
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x41, 0x80, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc, 6);
    assert_eq!(ctx.cpu.pc, 4);
    assert_eq!(ctx.cpu.p, 0xe4);
}
//
#[test]
fn test_jmp() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);
    //
    // JMP $c5f5
    ctx.cpu
        .debug_exec_opcode([0x4c, 0xf5, 0xc5], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(ctx.cpu.pc, 0xc5f5);
    //
    ctx.cpu.pc = 0;
    ctx.memory.write_cpu(0x2ffu16, 0x00, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(0x200u16, 0x03, &mut ctx.memory_ctx);
    // JMP ($02ff) (@2ff = 0, @200 = 03)
    ctx.cpu
        .debug_exec_opcode([0x6c, 0xff, 0x02], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(ctx.cpu.pc, 0x300);
}
//
#[test]
fn test_jsr() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);
    //
    ctx.cpu.pc = 0x300;
    ctx.cpu.sp = 0xff;
    //
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x20, 00, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc, 6);
    assert_eq!(
        ctx.memory.read_cpu(
            ctx.cpu.sp.wrapping_add(1) as u16 + 0x100,
            &mut ctx.memory_ctx
        ),
        0x02
    );
    assert_eq!(
        ctx.memory.read_cpu(
            ctx.cpu.sp.wrapping_add(2) as u16 + 0x100,
            &mut ctx.memory_ctx
        ),
        0x03
    );
    //
    ctx.cpu.pc = 0x300;
    ctx.cpu.sp = 0x00;
    //
    // sp = 0x00, so the jsr will cause it to underflow through zero
    ctx.cpu
        .debug_exec_opcode([0x20, 00, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(
        ctx.memory.read_cpu(
            ctx.cpu.sp.wrapping_add(1) as u16 + 0x100,
            &mut ctx.memory_ctx
        ),
        0x02
    );
    assert_eq!(
        ctx.memory.read_cpu(
            ctx.cpu.sp.wrapping_add(2) as u16 + 0x100,
            &mut ctx.memory_ctx
        ),
        0x03
    );
    //
    ctx.cpu.pc = 0x65fd;
    ctx.cpu.sp = 0xfd;
    ctx.cpu
        .debug_exec_opcode([0x20, 0xe5, 0xf7], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(ctx.cpu.sp, 0xfb);
    assert_eq!(ctx.cpu.pc, 0xf7e5);
    assert_eq!(
        ctx.memory.read_cpu(
            ctx.cpu.sp.wrapping_add(1) as u16 + 0x100,
            &mut ctx.memory_ctx
        ),
        0xfd + 2
    );
    assert_eq!(
        ctx.memory.read_cpu(
            ctx.cpu.sp.wrapping_add(2) as u16 + 0x100,
            &mut ctx.memory_ctx
        ),
        0x65
    );
}
//
#[test]
fn test_jsr_2() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);
    //
    ctx.cpu.pc = 0x1620;
    ctx.cpu.sp = 0xfb;
    ctx.cpu.x = 0x33;
    ctx.cpu.y = 0xba;
    //
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x20, 0xe5, 0xf7], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(ctx.cpu.pc, 0xf7e5);
    assert_eq!(cyc, 6);
    assert_eq!(
        ctx.memory.read_cpu(
            ctx.cpu.sp.wrapping_add(1) as u16 + 0x100,
            &mut ctx.memory_ctx
        ),
        0x20 + 2
    );
    assert_eq!(
        ctx.memory.read_cpu(
            ctx.cpu.sp.wrapping_add(2) as u16 + 0x100,
            &mut ctx.memory_ctx
        ),
        0x16
    );
}
//
#[test]
fn test_ld() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);
    //
    ctx.memory.write_cpu(0x89u16, 0x00, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(0x8au16, 0x03, &mut ctx.memory_ctx);
    ctx.memory.write_cpu(0x300u16, 0x89, &mut ctx.memory_ctx);
    ctx.cpu.y = 0;
    ctx.cpu.p = 0x27;
    // LDA ($89), Y (indirect indexed)
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0xb1, 0x89, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc, 5);
    assert_eq!(ctx.cpu.pc, 2);
    assert_eq!(ctx.cpu.p, 0xa5);
    //
    ctx.memory.write_cpu(0x633u16, 0xaa, &mut ctx.memory_ctx);
    ctx.cpu.y = 0;
    ctx.cpu.p = 0x67;
    // LDY ($0633, X) (indexed indirect)
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0xbc, 0x33, 06], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc, 4);
    assert_eq!(ctx.cpu.y, 0xaa);
    assert_eq!(ctx.cpu.p, 0xe5);
}
//
fn test_lsr() {
    // TODO: ..
}
//
fn test_ora() {
    // TODO: ..
}
//
#[test]
fn test_push_pull() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);
    //
    ctx.cpu.a = 0xff;
    ctx.cpu.sp = 0xfb;
    // PHA (push accumulator)
    let cyc_1 = ctx
        .cpu
        .debug_exec_opcode([0x48, 00, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    // PLP (pop top of stack into status flags)
    let cyc_2 = ctx
        .cpu
        .debug_exec_opcode([0x28, 00, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc_1, 3);
    assert_eq!(cyc_2, 4);
    assert_eq!(ctx.cpu.p, 0xef);
    assert_eq!(ctx.cpu.sp, 0xfb);
    //
    ctx.cpu.a = 0;
    ctx.cpu.p = 0x6f;
    ctx.cpu.sp = 0xfb;
    // PHP (push status flags)
    let cyc_1 = ctx
        .cpu
        .debug_exec_opcode([0x08, 00, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    // PLA (pop top of stack into accumulator)
    let cyc_2 = ctx
        .cpu
        .debug_exec_opcode([0x68, 00, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc_1, 3);
    assert_eq!(cyc_2, 4);
    assert_eq!(ctx.cpu.a, 0x7f);
    assert_eq!(ctx.cpu.p, 0x6d);
    assert_eq!(ctx.cpu.sp, 0xfb);
}
//
#[test]
fn test_rol_ror() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);
    //
    ctx.cpu.a = 0x55;
    ctx.cpu.p = 0x24;
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x2a, 00, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc, 2);
    assert_eq!(ctx.cpu.a, 0xaa);
    assert_eq!(ctx.cpu.p, 0xa4);
    //
    ctx.cpu.x = 0x55;
    ctx.cpu.p = 0x24;
    ctx.memory.write_cpu(0x655u16, 0x55, &mut ctx.memory_ctx);
    // ROL $0600, X (absolute indexed)
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x3e, 00, 06], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc, 7);
    assert_eq!(ctx.cpu.p, 0xa4);
    assert_eq!(ctx.memory.read_cpu(0x655u16, &mut ctx.memory_ctx), 0xaa);
    //
    ctx.cpu.x = 0x55;
    ctx.cpu.p = 0x65;
    ctx.memory.write_cpu(0x55u16, 1, &mut ctx.memory_ctx);
    // ROR $00, X (zero page indexed)
    let cyc = ctx
        .cpu
        .debug_exec_opcode([0x76, 00, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    //
    assert_eq!(cyc, 6);
    assert_eq!(ctx.cpu.p, 0xe5);
    assert_eq!(ctx.memory.read_cpu(0x55u16, &mut ctx.memory_ctx), 0x80);
}
//
fn test_rti() {
    // TODO: ..
}
//
#[test]
fn test_rts() {
    let mut nes = init_nes();
    let ctx = context::NesContext::new(&mut nes);

    ctx.cpu.pc = 0x0401;
    ctx.cpu.sp = 0xf0;
    // JSR $182e
    let cyc_1 = ctx
        .cpu
        .debug_exec_opcode([0x20, 0x2e, 0x18], &mut ctx.memory, &mut ctx.cpu_ctx);
    assert_eq!(cyc_1, 6);
    assert_eq!(ctx.cpu.pc, 0x182e);
    assert_eq!(
        ctx.memory.read_cpu(
            0x100 + ctx.cpu.sp.wrapping_add(1) as u16,
            &mut ctx.memory_ctx
        ),
        01 + 2
    );
    assert_eq!(
        ctx.memory.read_cpu(
            0x100 + ctx.cpu.sp.wrapping_add(2) as u16,
            &mut ctx.memory_ctx
        ),
        04
    );

    // RTS
    let cyc_2 = ctx
        .cpu
        .debug_exec_opcode([0x60, 00, 00], &mut ctx.memory, &mut ctx.cpu_ctx);
    assert_eq!(cyc_2, 6);
    assert_eq!(ctx.cpu.pc, 0x0401 + 3);
    assert_eq!(ctx.cpu.sp, 0xf0);
}
//
#[test]
fn test_sbc() {
    let ref mut cpu = Cpu::default();
    //
    cpu.a = 0x40;
    cpu.p = 0x65;
    cpu.sbc(0x40);
    //
    assert_eq!(cpu.a, 0);
    assert_eq!(cpu.p, 0x27);
    //
    cpu.a = 0x40;
    cpu.p = 0x25;
    cpu.sbc(0x3f);
    //
    assert_eq!(cpu.a, 1);
    assert_eq!(cpu.p, 0x25);
    //
    cpu.a = 0x40;
    cpu.p = 0xe5;
    cpu.sbc(0x41);
    //
    assert_eq!(cpu.a, 0xff);
    assert_eq!(cpu.p, 0xa4);
    //
    cpu.a = 0x81;
    cpu.p = 0xe5;
    cpu.sbc(0x7f);
    //
    assert_eq!(cpu.a, 2);
    assert_eq!(cpu.p, 0x65);
}
//
fn test_sta_stx_sty() {
    // TODO: ..
}
//
fn test_brk() {
    // FIXME: ..
    // TODO: ..
}
