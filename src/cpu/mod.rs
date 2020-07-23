use super::memory_map as mmap;
use super::memory_map::MemoryMap;

mod addressing;
mod test;

#[derive(Default)]
pub struct Cpu {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub p: u8,
    pub sp: u8,
    pub pc: u16,
    pub cycle_count: u32,
}

impl Cpu {
    pub fn new_nestest() -> Self {
        Self {
            pc: 0xc000,
            p: 0x24,
            sp: 0xfd,
            cycle_count: 7, // for whatever reason
            ..Default::default()
        }
    }

    pub fn exec_instruction(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        match memory.read_cpu(ptrs, self.pc) {
            // ADC
            0x69 => self.adc_imm(memory, ptrs),
            0x65 => self.adc_zero_page(memory, ptrs),
            0x75 => self.adc_zero_page_indexed(memory, ptrs),
            0x6d => self.adc_abs(memory, ptrs),
            0x7d => self.adc_abs_indexed(self.x, memory, ptrs),
            0x79 => self.adc_abs_indexed(self.y, memory, ptrs),
            0x61 => self.adc_indexed_indirect(memory, ptrs),
            0x71 => self.adc_indirect_indexed(memory, ptrs),
            // AND
            0x29 => self.and_imm(memory, ptrs),
            0x25 => self.and_zero_page(memory, ptrs),
            0x35 => self.and_zero_page_indexed(memory, ptrs),
            0x2d => self.and_abs(memory, ptrs),
            0x3d => self.and_abs_indexed(self.x, memory, ptrs),
            0x39 => self.and_abs_indexed(self.y, memory, ptrs),
            0x21 => self.and_indexed_indirect(memory, ptrs),
            0x31 => self.and_indirect_indexed(memory, ptrs),
            // ASL
            0x0a => self.asl_a(),
            0x06 => self.asl_zero_page(memory, ptrs),
            0x16 => self.asl_zero_page_indexed(memory, ptrs),
            0x0e => self.asl_abs(memory, ptrs),
            0x1e => self.asl_abs_indexed(memory, ptrs),
            // BCC
            0x90 => self.branch_if((self.p & 1) == 0, memory, ptrs),
            // BCS
            0xb0 => self.branch_if((self.p & 1) != 0, memory, ptrs),
            // BEQ
            0xf0 => self.branch_if((self.p & 2) != 0, memory, ptrs),
            // BMI
            0x30 => self.branch_if((self.p as i8) < 0, memory, ptrs),
            // BNE
            0xd0 => self.branch_if((self.p & 2) == 0, memory, ptrs),
            // BPL
            0x10 => self.branch_if((self.p as i8) > 0, memory, ptrs),
            // BVC
            0x50 => self.branch_if((self.p & 0b01000000) == 0, memory, ptrs),
            // BVS
            0x70 => self.branch_if((self.p & 0b01000000) != 0, memory, ptrs),
            // BIT
            0x24 => self.bit_zero_page(memory, ptrs),
            0x2c => self.bit_abs(memory, ptrs),
            // BRK
            0x00 => self.brk(memory, ptrs),
            // CLC
            0x18 => self.clc(),
            // CLD
            0xd8 => self.cld(),
            // CLI
            0x58 => self.cli(),
            // CLV
            0xb8 => self.clv(),
            // CMP
            0xc9 => self.compare_register_imm(self.a, memory, ptrs),
            0xc5 => self.compare_register_zero_page(self.a, memory, ptrs),
            0xd5 => self.compare_register_zero_page_indexed(self.a, self.x, memory, ptrs),
            0xcd => self.compare_register_abs(self.a, memory, ptrs),
            0xdd => self.compare_register_abs_indexed(self.a, self.x, memory, ptrs),
            0xd9 => self.compare_register_abs_indexed(self.a, self.y, memory, ptrs),
            0xc1 => self.compare_register_indexed_indirect(self.a, memory, ptrs),
            0xd1 => self.compare_register_indirect_indexed(self.a, memory, ptrs),
            // CPX
            0xe0 => self.compare_register_imm(self.x, memory, ptrs),
            0xe4 => self.compare_register_zero_page(self.x, memory, ptrs),
            0xec => self.compare_register_abs(self.x, memory, ptrs),
            // CPY
            0xc0 => self.compare_register_imm(self.y, memory, ptrs),
            0xc4 => self.compare_register_zero_page(self.y, memory, ptrs),
            0xcc => self.compare_register_abs(self.y, memory, ptrs),
            // DEC
            0xc6 => self.dec_zero_page(memory, ptrs),
            0xd6 => self.dec_zero_page_indexed(memory, ptrs),
            0xce => self.dec_abs(memory, ptrs),
            0xde => self.dec_abs_indexed(memory, ptrs),
            // DEX
            0xca => self.dex(),
            // DEY
            0x88 => self.dey(),
            // EOR
            0x49 => self.eor_imm(memory, ptrs),
            0x45 => self.eor_zero_page(memory, ptrs),
            0x55 => self.eor_zero_page_indexed(memory, ptrs),
            0x4d => self.eor_abs(memory, ptrs),
            0x5d => self.eor_abs_indexed(self.x, memory, ptrs),
            0x59 => self.eor_abs_indexed(self.y, memory, ptrs),
            0x41 => self.eor_indexed_indirect(memory, ptrs),
            0x51 => self.eor_indirect_indexed(memory, ptrs),
            // INC
            0xe6 => self.inc_zero_page(memory, ptrs),
            0xf6 => self.inc_zero_page_indexed(memory, ptrs),
            0xee => self.inc_abs(memory, ptrs),
            0xfe => self.inc_abs_indexed(memory, ptrs),
            // INX
            0xe8 => self.inx(),
            // INY
            0xc8 => self.iny(),
            // JMP
            0x4c => self.jmp_abs(memory, ptrs),
            0x6c => self.jmp_abs_indirect(memory, ptrs),
            // JSR
            0x20 => self.jsr(memory, ptrs),
            // LDA
            0xa9 => self.lda_imm(memory, ptrs),
            0xa5 => self.lda_zero_page(memory, ptrs),
            0xb5 => self.lda_zero_page_indexed(memory, ptrs),
            0xad => self.lda_abs(memory, ptrs),
            0xbd => self.lda_abs_indexed(self.x, memory, ptrs),
            0xb9 => self.lda_abs_indexed(self.y, memory, ptrs),
            0xa1 => self.lda_indexed_indirect(memory, ptrs),
            0xb1 => self.lda_indirect_indexed(memory, ptrs),
            // LDX
            0xa2 => self.ldx_imm(memory, ptrs),
            0xa6 => self.ldx_zero_page(memory, ptrs),
            0xb6 => self.ldx_zero_page_indexed(memory, ptrs),
            0xae => self.ldx_abs(memory, ptrs),
            0xbe => self.ldx_abs_indexed(self.y, memory, ptrs),
            // LDY
            0xa0 => self.ldy_imm(memory, ptrs),
            0xa4 => self.ldy_zero_page(memory, ptrs),
            0xb4 => self.ldy_zero_page_indexed(memory, ptrs),
            0xac => self.ldy_abs(memory, ptrs),
            0xbc => self.ldy_abs_indexed(self.x, memory, ptrs),
            // LSR
            0x4a => self.lsr_a(),
            0x46 => self.lsr_zero_page(memory, ptrs),
            0x56 => self.lsr_zero_page_indexed(memory, ptrs),
            0x4e => self.lsr_abs(memory, ptrs),
            0x5e => self.lsr_abs_indexed(memory, ptrs),
            // NOP
            0xea => self.nop(),
            // NOP (undocumented)
            0x1a | 0x3a | 0x5a | 0x7a | 0xda | 0xfa => self.nop(),
            // SKB/NOP (undocumented)
            0x80 | 0x82 | 0x89 | 0xc2 | 0xe2 => self.nop_imm(),
            // NOP/IGN (undocumented)
            0x04 | 0x44 | 0x64 => self.nop_zero_page(),
            0x14 | 0x34 | 0x54 | 0x74 | 0xd4 | 0xf4 => self.nop_zero_page_indexed(memory, ptrs),
            0x0c => self.nop_abs(memory, ptrs),
            0x1c | 0x3c | 0x5c | 0x7c | 0xdc | 0xfc => self.nop_abs_indexed(self.x, memory, ptrs),
            // ORA
            0x09 => self.ora_imm(memory, ptrs),
            0x05 => self.ora_zero_page(memory, ptrs),
            0x15 => self.ora_zero_page_indexed(memory, ptrs),
            0x0d => self.ora_abs(memory, ptrs),
            0x1d => self.ora_abs_indexed(self.x, memory, ptrs),
            0x19 => self.ora_abs_indexed(self.y, memory, ptrs),
            0x01 => self.ora_indexed_indirect(memory, ptrs),
            0x11 => self.ora_indirect_indexed(memory, ptrs),
            // PHA
            0x48 => self.pha(memory, ptrs),
            // PHP
            0x08 => self.php(memory, ptrs),
            // PLA
            0x68 => self.pla(memory, ptrs),
            // PLP
            0x28 => self.plp(memory, ptrs),
            // ROL
            0x2a => self.rol_a(),
            0x26 => self.rol_zero_page(memory, ptrs),
            0x36 => self.rol_zero_page_indexed(memory, ptrs),
            0x2e => self.rol_abs(memory, ptrs),
            0x3e => self.rol_abs_indexed(memory, ptrs),
            // ROR
            0x6a => self.ror_a(),
            0x66 => self.ror_zero_page(memory, ptrs),
            0x76 => self.ror_zero_page_indexed(memory, ptrs),
            0x6e => self.ror_abs(memory, ptrs),
            0x7e => self.ror_abs_indexed(memory, ptrs),
            // RTI
            0x40 => self.rti(memory, ptrs),
            // RTS
            0x60 => self.rts(memory, ptrs),
            // SBC
            0xe9 => self.sbc_imm(memory, ptrs),
            0xe5 => self.sbc_zero_page(memory, ptrs),
            0xf5 => self.sbc_zero_page_indexed(memory, ptrs),
            0xed => self.sbc_abs(memory, ptrs),
            0xfd => self.sbc_abs_indexed(self.x, memory, ptrs),
            0xf9 => self.sbc_abs_indexed(self.y, memory, ptrs),
            0xe1 => self.sbc_indexed_indirect(memory, ptrs),
            0xf1 => self.sbc_indirect_indexed(memory, ptrs),
            // SEC
            0x38 => self.sec(),
            // SED
            0xf8 => self.sed(),
            // SEI
            0x78 => self.sei(),
            // STA
            0x85 => self.sta_zero_page(memory, ptrs),
            0x95 => self.sta_zero_page_indexed(memory, ptrs),
            0x8d => self.sta_abs(memory, ptrs),
            0x9d => self.sta_abs_indexed(self.x, memory, ptrs),
            0x99 => self.sta_abs_indexed(self.y, memory, ptrs),
            0x81 => self.sta_indexed_indirect(memory, ptrs),
            0x91 => self.sta_indirect_indexed(memory, ptrs),
            // STX
            0x86 => self.stx_zero_page(memory, ptrs),
            0x96 => self.stx_zero_page_indexed(memory, ptrs),
            0x8e => self.stx_abs(memory, ptrs),
            // STY
            0x84 => self.sty_zero_page(memory, ptrs),
            0x94 => self.sty_zero_page_indexed(memory, ptrs),
            0x8c => self.sty_abs(memory, ptrs),
            // TAX
            0xaa => self.tax(),
            // TAY
            0xa8 => self.tay(),
            // TSX
            0xba => self.tsx(),
            // TXA
            0x8a => self.txa(),
            // TXS
            0x9a => self.txs(),
            // TYA
            0x98 => self.tya(),
            _ => panic!("TODO: handle invalid opcode"),
        }
    }

    // returns the byte at pc+1
    pub fn fetch_operand_byte(
        &self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) -> u8 {
        memory.read_cpu(ptrs, self.pc + 1)
    }

    // returns the two bytes at [pc+1, pc+2] as a u16
    pub fn fetch_operand_u16(
        &self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) -> u16 {
        u16::from_le_bytes([
            memory.read_cpu(ptrs, self.pc + 1),
            memory.read_cpu(ptrs, self.pc + 2),
        ])
    }

    // returns the two bytes at [pc+1, pc+2]
    pub fn fetch_operand_bytes(
        &self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) -> [u8; 2] {
        [
            memory.read_cpu(ptrs, self.pc + 1),
            memory.read_cpu(ptrs, self.pc + 2),
        ]
    }

    #[inline]
    // sets the carry flag based on the 'carry' bool
    fn set_c_from_bool(&mut self, carry: bool) {
        self.p = (self.p & !1) | carry as u8;
    }

    #[inline]
    // ors 'bit' directly with the carry flag
    fn set_c_from_bit(&mut self, bit: u8) {
        self.p = (self.p & !1) | bit;
    }

    #[inline]
    fn set_v_from_bool(&mut self, overflow: bool) {
        self.p = (self.p & !0b01000000) | ((overflow as u8) << 6);
    }

    #[inline]
    fn set_v_from_bit(&mut self, bit: u8) {
        self.p = (self.p & !0b01000000) | bit;
    }

    #[inline]
    fn set_z_from_val(&mut self, val: u8) {
        self.p = (self.p & !2) | (((val == 0) as u8) << 1);
    }

    #[inline]
    fn set_z_from_bool(&mut self, zero: bool) {
        self.p = (self.p & !2) | ((zero as u8) << 1);
    }

    #[inline]
    fn set_n_from_val(&mut self, val: u8) {
        self.p = (self.p & !0x80) | val & 0x80;
    }

    #[inline]
    fn set_n_from_bool(&mut self, negative: bool) {
        self.p = (self.p & !0x80) | (negative as u8) << 7;
    }

    #[inline]
    fn set_i_from_bit(&mut self, bit: u8) {
        self.p = (self.p & !4) | bit;
    }

    fn adc(&mut self, val: u8) {
        // add 'val' to the accumulator first
        let (res_1, carry_1) = val.overflowing_add(self.a);
        let (_, overflow_1) = (val as i8).overflowing_add(self.a as i8);

        // then add the carry from the status flag to the accumulator
        let (res_2, carry_2) = res_1.overflowing_add(self.p & 1);
        let (_, overflow_2) = (res_1 as i8).overflowing_add((self.p & 1) as i8);

        self.a = res_2;
        self.set_c_from_bool(carry_1 | carry_2);
        self.set_v_from_bool(overflow_1 | overflow_2);
        self.set_z_from_val(res_2);
        self.set_n_from_val(res_2);
    }

    fn adc_imm(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_imm(self, memory, ptrs);
        self.adc(val);
    }

    fn adc_zero_page(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_zero_page(self, memory, ptrs);
        self.adc(val);
    }

    fn adc_zero_page_indexed(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_zero_page_indexed(self, self.x, memory, ptrs);
        self.adc(val);
    }

    fn adc_abs(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_abs(self, memory, ptrs);
        self.adc(val);
    }

    fn adc_abs_indexed(
        &mut self,
        index: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_abs_indexed(self, index, memory, ptrs);
        self.adc(val);
    }

    fn adc_indexed_indirect(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_indexed_indirect(self, memory, ptrs);
        self.adc(val);
    }

    fn adc_indirect_indexed(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_indirect_indexed(self, memory, ptrs);
        self.adc(val);
    }

    fn and(&mut self, val: u8) {
        self.a &= val;
        self.set_n_from_val(self.a);
        self.set_z_from_val(self.a);
    }

    fn and_imm(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_imm(self, memory, ptrs);
        self.and(val);
    }

    fn and_zero_page(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_zero_page(self, memory, ptrs);
        self.and(val);
    }

    fn and_zero_page_indexed(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_zero_page_indexed(self, self.x, memory, ptrs);
        self.and(val);
    }

    fn and_abs(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_abs(self, memory, ptrs);
        self.and(val);
    }

    fn and_abs_indexed(
        &mut self,
        index: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_abs_indexed(self, index, memory, ptrs);
        self.and(val);
    }

    fn and_indexed_indirect(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_indexed_indirect(self, memory, ptrs);
        self.and(val);
    }

    fn and_indirect_indexed(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_indirect_indexed(self, memory, ptrs);
        self.and(val);
    }

    fn asl(&mut self, val: u8) -> u8 {
        self.set_c_from_bool((val >> 7) != 0);
        let res = val << 1;

        self.set_z_from_val(res);
        self.set_n_from_val(res);

        res
    }

    fn asl_a(&mut self) {
        self.a = self.asl(self.a);
        self.cycle_count += 2;
        self.pc += 1;
    }

    fn asl_zero_page(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_zero_page(self, memory, ptrs, Self::asl);
    }

    fn asl_zero_page_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_zero_page_indexed(self, self.x, memory, ptrs, Self::asl);
    }

    fn asl_abs(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        addressing::read_write_abs(self, memory, ptrs, Self::asl);
    }

    fn asl_abs_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_abs_indexed(self, self.x, memory, ptrs, Self::asl);
    }

    fn branch_if(
        &mut self,
        condition: bool,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let offset = self.fetch_operand_byte(memory, ptrs);
        self.pc += 2;

        if condition {
            // sign extend 'offset' into an i16
            let offset_sign_ext = (offset as i8) as i16;
            // get the carry from adding the low bytes
            let (_, carry) = (self.pc as u8).overflowing_add(offset as u8);
            // perform the full addition
            self.pc = (self.pc as i16 + offset_sign_ext) as u16;
            // xor sign of 'offset' with 'carry' to determine whether a page boundary was crossed
            let boundary_crossed = ((offset as i8) < 0) ^ carry;

            self.cycle_count += 3 + boundary_crossed as u32
        } else {
            self.cycle_count += 2;
        }
    }

    fn bit(&mut self, val: u8) {
        self.set_v_from_bit(val & 0b01000000);
        self.set_n_from_val(val);

        let res = val & self.a;
        self.set_z_from_val(res);
    }

    fn bit_zero_page(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_zero_page(self, memory, ptrs);
        self.bit(val);
    }

    fn bit_abs(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_abs(self, memory, ptrs);
        self.bit(val);
    }

    fn brk(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        // NOTE: pc + 2 is pushed, despite brk being a one byte instruction
        let pc_bytes = (self.pc + 2).to_le_bytes();

        // push high bits of pc + 2
        memory.write_cpu(ptrs, self.sp as u16 + 0x100, pc_bytes[1]);
        self.sp = self.sp.wrapping_sub(1);
        // push low bits of pc + 2
        memory.write_cpu(ptrs, self.sp as u16 + 0x100, pc_bytes[0]);
        self.sp = self.sp.wrapping_sub(1);

        // push status flags (with the 'b-flag' set)
        memory.write_cpu(ptrs, self.sp as u16 + 0x100, self.p | 0b10000);
        self.sp = self.sp.wrapping_sub(1);

        // set interrupt disable flag
        self.set_i_from_bit(4);

        // set pc to address in brk/irq vector
        let brk_vector = u16::from_le_bytes([
            memory.read_cpu(ptrs, 0xfffeu16),
            memory.read_cpu(ptrs, 0xffffu16),
        ]);
        self.pc = brk_vector;

        self.cycle_count += 7;
    }

    fn clc(&mut self) {
        self.pc += 1;
        self.set_c_from_bool(false);
        self.cycle_count += 2;
    }

    fn cld(&mut self) {
        self.pc += 1;
        self.p = self.p & !8;
        self.cycle_count += 2;
    }

    fn cli(&mut self) {
        self.pc += 1;
        self.set_i_from_bit(0);
        self.cycle_count += 2;
    }

    fn clv(&mut self) {
        self.pc += 1;
        self.set_v_from_bit(0);
        self.cycle_count += 2;
    }

    // used for cmp, cpx, cpy instructions
    fn compare_register_val(&mut self, register: u8, val: u8) {
        let (sub, underflow) = register.overflowing_sub(val);
        self.set_c_from_bool((sub == 0) | !underflow);
        self.set_z_from_val(sub);
        self.set_n_from_val(sub);
    }

    fn compare_register_imm(
        &mut self,
        register: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_imm(self, memory, ptrs);
        self.compare_register_val(register, val);
    }

    fn compare_register_zero_page(
        &mut self,
        register: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_zero_page(self, memory, ptrs);
        self.compare_register_val(register, val);
    }

    fn compare_register_zero_page_indexed(
        &mut self,
        register: u8,
        index: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_zero_page_indexed(self, index, memory, ptrs);
        self.compare_register_val(register, val);
    }

    fn compare_register_abs(
        &mut self,
        register: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_abs(self, memory, ptrs);
        self.compare_register_val(register, val);
    }

    fn compare_register_abs_indexed(
        &mut self,
        register: u8,
        index: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_abs_indexed(self, index, memory, ptrs);
        self.compare_register_val(register, val);
    }

    fn compare_register_indexed_indirect(
        &mut self,
        register: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_indexed_indirect(self, memory, ptrs);
        self.compare_register_val(register, val);
    }

    fn compare_register_indirect_indexed(
        &mut self,
        register: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_indirect_indexed(self, memory, ptrs);
        self.compare_register_val(register, val);
    }

    fn decrement_val(&mut self, val: u8) -> u8 {
        let res = val.wrapping_sub(1);
        self.set_z_from_val(res);
        self.set_n_from_val(res);

        res
    }

    fn dec_zero_page(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_zero_page(self, memory, ptrs, Self::decrement_val);
    }

    fn dec_zero_page_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_zero_page_indexed(self, self.x, memory, ptrs, Self::decrement_val);
    }

    fn dec_abs(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        addressing::read_write_abs(self, memory, ptrs, Self::decrement_val);
    }

    fn dec_abs_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_abs_indexed(self, self.x, memory, ptrs, Self::decrement_val);
    }

    fn dex(&mut self) {
        self.x = self.decrement_val(self.x);
        self.pc += 1;
        self.cycle_count += 2;
    }

    fn dey(&mut self) {
        self.y = self.decrement_val(self.y);
        self.pc += 1;
        self.cycle_count += 2;
    }

    fn eor(&mut self, val: u8) {
        self.a ^= val;
        self.set_z_from_val(self.a);
        self.set_n_from_val(self.a);
    }

    fn eor_imm(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_imm(self, memory, ptrs);
        self.eor(val);
    }

    fn eor_zero_page(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_zero_page(self, memory, ptrs);
        self.eor(val);
    }

    fn eor_zero_page_indexed(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_zero_page_indexed(self, self.x, memory, ptrs);
        self.eor(val);
    }

    fn eor_abs(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_abs(self, memory, ptrs);
        self.eor(val);
    }

    fn eor_abs_indexed(
        &mut self,
        index: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_abs_indexed(self, index, memory, ptrs);
        self.eor(val);
    }

    fn eor_indexed_indirect(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_indexed_indirect(self, memory, ptrs);
        self.eor(val);
    }

    fn eor_indirect_indexed(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_indirect_indexed(self, memory, ptrs);
        self.eor(val);
    }

    fn increment_val(&mut self, val: u8) -> u8 {
        let res = val.wrapping_add(1);
        self.set_z_from_val(res);
        self.set_n_from_val(res);

        res
    }

    fn inc_zero_page(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_zero_page(self, memory, ptrs, Self::increment_val);
    }

    fn inc_zero_page_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_zero_page_indexed(self, self.x, memory, ptrs, Self::increment_val);
    }

    fn inc_abs(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        addressing::read_write_abs(self, memory, ptrs, Self::increment_val);
    }

    fn inc_abs_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_abs_indexed(self, self.x, memory, ptrs, Self::increment_val);
    }

    fn inx(&mut self) {
        self.x = self.increment_val(self.x);
        self.pc += 1;
        self.cycle_count += 2;
    }

    fn iny(&mut self) {
        self.y = self.increment_val(self.y);
        self.pc += 1;
        self.cycle_count += 2;
    }

    fn jmp_abs(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        self.pc = self.fetch_operand_u16(memory, ptrs);
        self.cycle_count += 3;
    }

    fn jmp_abs_indirect(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let mut addr_bytes = self.fetch_operand_bytes(memory, ptrs);

        let final_addr_lo = memory.read_cpu(ptrs, u16::from_le_bytes(addr_bytes));
        // add 1 to low bits without carry to get address of high bits of final address
        addr_bytes[0] = addr_bytes[0].wrapping_add(1);
        let final_addr_hi = memory.read_cpu(ptrs, u16::from_le_bytes(addr_bytes));

        self.pc = u16::from_le_bytes([final_addr_lo, final_addr_hi]);
        self.cycle_count += 5;
    }

    fn jsr(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let addr = self.fetch_operand_u16(memory, ptrs);

        // get return address (next instruction - 1)
        let ret_addr = (self.pc + 2).to_le_bytes();
        // push low bits of address to sp - 1
        memory.write_cpu(ptrs, self.sp.wrapping_sub(1) as u16 + 0x100, ret_addr[0]);
        // push high bits of address to sp
        memory.write_cpu(ptrs, self.sp as u16 + 0x100, ret_addr[1]);

        self.sp = self.sp.wrapping_sub(2);
        self.pc = addr;
        self.cycle_count += 6;
    }

    fn lda(&mut self, val: u8) {
        self.set_z_from_val(val);
        self.set_n_from_val(val);
        self.a = val;
    }

    fn lda_imm(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_imm(self, memory, ptrs);
        self.lda(val);
    }

    fn lda_zero_page(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_zero_page(self, memory, ptrs);
        self.lda(val);
    }

    fn lda_zero_page_indexed(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_zero_page_indexed(self, self.x, memory, ptrs);
        self.lda(val);
    }

    fn lda_abs(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_abs(self, memory, ptrs);
        self.lda(val);
    }

    fn lda_abs_indexed(
        &mut self,
        index: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_abs_indexed(self, index, memory, ptrs);
        self.lda(val);
    }

    fn lda_indexed_indirect(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_indexed_indirect(self, memory, ptrs);
        self.lda(val);
    }

    fn lda_indirect_indexed(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_indirect_indexed(self, memory, ptrs);
        self.lda(val);
    }

    fn ldx(&mut self, val: u8) {
        self.set_z_from_val(val);
        self.set_n_from_val(val);
        self.x = val;
    }

    fn ldx_imm(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_imm(self, memory, ptrs);
        self.ldx(val);
    }

    fn ldx_zero_page(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_zero_page(self, memory, ptrs);
        self.ldx(val);
    }

    fn ldx_zero_page_indexed(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_zero_page_indexed(self, self.y, memory, ptrs);
        self.ldx(val);
    }

    fn ldx_abs(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_abs(self, memory, ptrs);
        self.ldx(val);
    }

    fn ldx_abs_indexed(
        &mut self,
        index: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_abs_indexed(self, index, memory, ptrs);
        self.ldx(val);
    }

    fn ldy(&mut self, val: u8) {
        self.set_z_from_val(val);
        self.set_n_from_val(val);
        self.y = val;
    }

    fn ldy_imm(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_imm(self, memory, ptrs);
        self.ldy(val);
    }

    fn ldy_zero_page(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_zero_page(self, memory, ptrs);
        self.ldy(val);
    }

    fn ldy_zero_page_indexed(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_zero_page_indexed(self, self.x, memory, ptrs);
        self.ldy(val);
    }

    fn ldy_abs(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_abs(self, memory, ptrs);
        self.ldy(val);
    }

    fn ldy_abs_indexed(
        &mut self,
        index: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_abs_indexed(self, index, memory, ptrs);
        self.ldy(val);
    }

    fn lsr(&mut self, val: u8) -> u8 {
        let res = val >> 1;
        self.set_c_from_bit(val & 1);
        self.set_z_from_val(res);
        self.set_n_from_val(res);

        res
    }

    fn lsr_a(&mut self) {
        self.a = self.lsr(self.a);
        self.pc += 1;
        self.cycle_count += 2;
    }

    fn lsr_zero_page(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_zero_page(self, memory, ptrs, Self::lsr);
    }

    fn lsr_zero_page_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_zero_page_indexed(self, self.x, memory, ptrs, Self::lsr);
    }

    fn lsr_abs(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        addressing::read_write_abs(self, memory, ptrs, Self::lsr);
    }

    fn lsr_abs_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_abs_indexed(self, self.x, memory, ptrs, Self::lsr);
    }

    fn nop(&mut self) {
        self.pc += 1;
        self.cycle_count += 2;
    }

    fn nop_imm(&mut self) {
        self.pc += 2;
        self.cycle_count += 2;
    }

    fn nop_zero_page(&mut self) {
        // NOTE: no need to call 'read_cpu()' here, as the nop won't have any side
        // effects when the address of its operand is restricted to the zero page
        self.pc += 2;
        self.cycle_count += 3;
    }

    fn nop_zero_page_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let _ = addressing::read_zero_page_indexed(self, self.x, memory, ptrs);
    }

    fn nop_abs(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        // read from address and ignore result (the redundant read
        // must be performed, as it may have side effects)
        let _ = addressing::read_abs(self, memory, ptrs);
    }

    fn nop_abs_indexed(
        &mut self,
        index: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let _ = addressing::read_abs_indexed(self, index, memory, ptrs);
    }

    fn ora(&mut self, val: u8) {
        self.a |= val;
        self.set_z_from_val(self.a);
        self.set_n_from_val(self.a);
    }

    fn ora_imm(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_imm(self, memory, ptrs);
        self.ora(val);
    }

    fn ora_zero_page(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_zero_page(self, memory, ptrs);
        self.ora(val);
    }

    fn ora_zero_page_indexed(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_zero_page_indexed(self, self.x, memory, ptrs);
        self.ora(val);
    }

    fn ora_abs(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_abs(self, memory, ptrs);
        self.ora(val);
    }

    fn ora_abs_indexed(
        &mut self,
        index: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_abs_indexed(self, index, memory, ptrs);
        self.ora(val);
    }

    fn ora_indexed_indirect(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_indexed_indirect(self, memory, ptrs);
        self.ora(val);
    }

    fn ora_indirect_indexed(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_indirect_indexed(self, memory, ptrs);
        self.ora(val);
    }

    // used for pha, php instructions
    fn push_val(
        &mut self,
        val: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        self.pc += 1;
        self.cycle_count += 3;

        memory.write_cpu(ptrs, self.sp as u16 + 0x100, val);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn pha(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        self.push_val(self.a, memory, ptrs)
    }

    fn php(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        // NOTE: the 'b-flag' bit is set when pushing
        // FIXME: may need to set bit 5 when pushing as well? it should be set by default, but
        self.push_val(self.p | 0b10000, memory, ptrs);
    }

    // used for pla, plp instructions
    fn pull_val(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) -> u8 {
        self.pc += 1;
        self.cycle_count += 4;

        self.sp = self.sp.wrapping_add(1);
        memory.read_cpu(ptrs, self.sp as u16 + 0x100)
    }

    fn pla(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        self.a = self.pull_val(memory, ptrs);
        self.set_z_from_val(self.a);
        self.set_n_from_val(self.a);
    }

    fn plp(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        self.p = (self.pull_val(memory, ptrs) & !0b10000) | 0b100000;
    }

    fn rol(&mut self, val: u8) -> u8 {
        // shift 'val' left and or carry into bit 0
        let res = (val << 1) | (self.p & 1);
        // set carry to bit 7 of 'val'
        self.set_c_from_bit(val >> 7);
        self.set_z_from_val(res);
        self.set_n_from_val(res);

        res
    }

    fn rol_a(&mut self) {
        self.a = self.rol(self.a);
        self.pc += 1;
        self.cycle_count += 2;
    }

    fn rol_zero_page(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_zero_page(self, memory, ptrs, Self::rol);
    }

    fn rol_zero_page_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_zero_page_indexed(self, self.x, memory, ptrs, Self::rol);
    }

    fn rol_abs(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        addressing::read_write_abs(self, memory, ptrs, Self::rol);
    }

    fn rol_abs_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_abs_indexed(self, self.x, memory, ptrs, Self::rol);
    }

    fn ror(&mut self, val: u8) -> u8 {
        // shift 'val' right and or carry into bit 7
        let res = (val >> 1) | (self.p << 7);
        // set carry to bit 0 of 'val'
        self.set_c_from_bit(val & 1);
        self.set_z_from_val(res);
        self.set_n_from_val(res);

        res
    }

    fn ror_a(&mut self) {
        self.a = self.ror(self.a);
        self.pc += 1;
        self.cycle_count += 2;
    }

    fn ror_zero_page(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_zero_page(self, memory, ptrs, Self::ror);
    }

    fn ror_zero_page_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_zero_page_indexed(self, self.x, memory, ptrs, Self::ror);
    }

    fn ror_abs(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        addressing::read_write_abs(self, memory, ptrs, Self::ror);
    }

    fn ror_abs_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::read_write_abs_indexed(self, self.x, memory, ptrs, Self::ror);
    }

    fn rti(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        // pull into status flags
        self.sp = self.sp.wrapping_add(1);
        // NOTE: bit 4 is cleared and bit 5 is set when pulling into status register
        self.p = (memory.read_cpu(ptrs, self.sp as u16 + 0x100) & !0b10000) | 0b100000;

        self.sp = self.sp.wrapping_add(1);
        // get program counter low bits
        let pc_low = memory.read_cpu(ptrs, self.sp as u16 + 0x100);
        self.sp = self.sp.wrapping_add(1);
        // get program counter high bits
        let pc_hi = memory.read_cpu(ptrs, self.sp as u16 + 0x100);

        self.pc = u16::from_le_bytes([pc_low, pc_hi]);
        self.cycle_count += 6;
    }

    fn rts(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        // pull into program counter
        self.sp = self.sp.wrapping_add(1);
        let pc_low = memory.read_cpu(ptrs, self.sp as u16 + 0x100);
        self.sp = self.sp.wrapping_add(1);
        let pc_hi = memory.read_cpu(ptrs, self.sp as u16 + 0x100);
        // add 1 since pushed value is expected to be pc - 1 (from the jsr instruction)
        // FIXME: should discard carry from low 8 bits when adding?
        self.pc = u16::from_le_bytes([pc_low, pc_hi]).wrapping_add(1);
        self.cycle_count += 6;
    }

    fn sbc(&mut self, val: u8) {
        let (res_1, borrow_1) = self.a.overflowing_sub(val);
        let (_, overflow_1) = (self.a as i8).overflowing_sub(val as i8);

        // subtract the not of the carry from the result
        let (res_2, borrow_2) = res_1.overflowing_sub((self.p & 1) ^ 1);
        let (_, overflow_2) = (res_1 as i8).overflowing_sub(((self.p & 1) ^ 1) as i8);

        self.a = res_2;
        self.set_c_from_bool(!(borrow_1 | borrow_2));
        self.set_v_from_bool(overflow_1 | overflow_2);
        self.set_z_from_val(res_2);
        self.set_n_from_val(res_2);
    }

    fn sbc_imm(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_imm(self, memory, ptrs);
        self.sbc(val);
    }

    fn sbc_zero_page(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_zero_page(self, memory, ptrs);
        self.sbc(val);
    }

    fn sbc_zero_page_indexed(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_zero_page_indexed(self, self.x, memory, ptrs);
        self.sbc(val);
    }

    fn sbc_abs(&mut self, memory: &mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        let val = addressing::read_abs(self, memory, ptrs);
        self.sbc(val);
    }

    fn sbc_abs_indexed(
        &mut self,
        index: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_abs_indexed(self, index, memory, ptrs);
        self.sbc(val);
    }

    fn sbc_indexed_indirect(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_indexed_indirect(self, memory, ptrs);
        self.sbc(val);
    }

    fn sbc_indirect_indexed(
        &mut self,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let val = addressing::read_indirect_indexed(self, memory, ptrs);
        self.sbc(val);
    }

    fn sec(&mut self) {
        self.pc += 1;
        self.set_c_from_bit(1);
        self.cycle_count += 2;
    }

    fn sed(&mut self) {
        self.pc += 1;
        self.p |= 8;
        self.cycle_count += 2;
    }

    fn sei(&mut self) {
        self.pc += 1;
        self.set_i_from_bit(4);
        self.cycle_count += 2;
    }

    fn sta_zero_page(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::write_zero_page(self, self.a, memory, ptrs);
    }

    fn sta_zero_page_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::write_zero_page_indexed(self, self.a, self.x, memory, ptrs);
    }

    fn sta_abs(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        addressing::write_abs(self, self.a, memory, ptrs);
    }

    fn sta_abs_indexed(
        &mut self,
        index: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::write_abs_indexed(self, self.a, index, memory, ptrs);
    }

    fn sta_indexed_indirect(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::write_indexed_indirect(self, self.a, memory, ptrs);
    }

    fn sta_indirect_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::write_indirect_indexed(self, self.a, memory, ptrs);
    }

    fn stx_zero_page(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::write_zero_page(self, self.x, memory, ptrs);
    }

    fn stx_zero_page_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::write_zero_page_indexed(self, self.x, self.y, memory, ptrs);
    }

    fn stx_abs(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        addressing::write_abs(self, self.x, memory, ptrs);
    }

    fn sty_zero_page(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::write_zero_page(self, self.y, memory, ptrs);
    }

    fn sty_zero_page_indexed(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        addressing::write_zero_page_indexed(self, self.y, self.x, memory, ptrs);
    }

    fn sty_abs(&mut self, memory: &mut mmap::Nrom128MemoryMap, ptrs: &mut mmap::MemoryMapPtrs) {
        addressing::write_abs(self, self.y, memory, ptrs);
    }

    fn tax(&mut self) {
        self.pc += 1;
        self.cycle_count += 2;
        self.x = self.a;
        self.set_z_from_val(self.x);
        self.set_n_from_val(self.x);
    }

    fn tay(&mut self) {
        self.pc += 1;
        self.cycle_count += 2;
        self.y = self.a;
        self.set_z_from_val(self.y);
        self.set_n_from_val(self.y);
    }

    fn tsx(&mut self) {
        self.pc += 1;
        self.cycle_count += 2;
        self.x = self.sp;
        self.set_z_from_val(self.x);
        self.set_n_from_val(self.x);
    }

    fn txa(&mut self) {
        self.pc += 1;
        self.cycle_count += 2;
        self.a = self.x;
        self.set_z_from_val(self.a);
        self.set_n_from_val(self.a);
    }

    fn txs(&mut self) {
        self.pc += 1;
        self.cycle_count += 2;
        self.sp = self.x;
    }

    fn tya(&mut self) {
        self.pc += 1;
        self.cycle_count += 2;
        self.a = self.y;
        self.set_z_from_val(self.a);
        self.set_n_from_val(self.a);
    }

    fn debug_exec_opcode(
        &mut self,
        opc: [u8; 3],
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) -> u8 {
        memory.write_cpu(ptrs, self.pc, opc[0]);
        memory.write_cpu(ptrs, self.pc + 1, opc[1]);
        memory.write_cpu(ptrs, self.pc + 2, opc[2]);

        let prev_cycles = self.cycle_count;
        self.exec_instruction(memory, ptrs);
        (self.cycle_count - prev_cycles) as u8
    }

    pub fn log_register_values(&self) {
        log!("A:{:0>2X} ", self.a);
        log!("X:{:0>2X} ", self.x);
        log!("Y:{:0>2X} ", self.y);
        log!("P:{:0>2X} ", self.p);
        log!("SP:{:0>2X} ", self.sp);
        log!("CYC:{}\n", self.cycle_count)
    }
}
