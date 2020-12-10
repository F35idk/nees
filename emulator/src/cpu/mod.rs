use crate::address_bus::CpuAddressBus;

#[macro_use]
use derive_serialize::Serialize;

mod addressing;
mod test;

#[derive(Serialize)]
pub struct Cpu {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub p: u8,
    pub sp: u8,
    pub pc: u16,
    pub cycle_count: i16,
    pub irq: u8,
    pub bits: CpuBits::BitField,
}

bitfield!(CpuBits<u8>(
    nmi: 0..0,
    delay_nmi: 1..1,
    delay_irq: 2..2
));

impl Default for Cpu {
    fn default() -> Self {
        Self {
            a: 0,
            x: 0,
            y: 0,
            p: 0,
            sp: 0,
            pc: 0,
            cycle_count: 0,
            irq: 0,
            bits: CpuBits::BitField::zeroed(),
        }
    }
}

impl Cpu {
    pub fn exec_instruction(&mut self, bus: &mut dyn CpuAddressBus) {
        if self.bits.nmi.is_true() && !self.bits.delay_nmi.is_true() {
            self.nmi(bus);
            self.bits.nmi.set(0);
            return;
        }

        if self.is_irq_pending() && !self.bits.delay_irq.is_true() {
            self.irq(bus);
            return;
        }

        // HACK: in the absence of a properly emulated, cycle-steppable
        // cpu, we use 'delay' flags to delay irqs and nmis that have been
        // asserted after the irq/nmi polling phase of an instruction
        self.bits.delay_nmi.set(0);
        self.bits.delay_irq.set(0);

        // TODO: implement more subtle interrupt (mis)behavior (i.e interrupt
        // hijacking, irq/nmi delay on 3-cycle branch instructions, etc.)

        let opcode = bus.read(self.pc, self);

        match opcode {
            // ADC
            0x69 => self.adc_imm(bus),
            0x65 => self.adc_zero_page(bus),
            0x75 => self.adc_zero_page_indexed(bus),
            0x6d => self.adc_abs(bus),
            0x7d => self.adc_abs_indexed(self.x, bus),
            0x79 => self.adc_abs_indexed(self.y, bus),
            0x61 => self.adc_indexed_indirect(bus),
            0x71 => self.adc_indirect_indexed(bus),
            // AND
            0x29 => self.and_imm(bus),
            0x25 => self.and_zero_page(bus),
            0x35 => self.and_zero_page_indexed(bus),
            0x2d => self.and_abs(bus),
            0x3d => self.and_abs_indexed(self.x, bus),
            0x39 => self.and_abs_indexed(self.y, bus),
            0x21 => self.and_indexed_indirect(bus),
            0x31 => self.and_indirect_indexed(bus),
            // ASL
            0x0a => self.asl_a(),
            0x06 => self.asl_zero_page(bus),
            0x16 => self.asl_zero_page_indexed(bus),
            0x0e => self.asl_abs(bus),
            0x1e => self.asl_abs_indexed(bus),
            // BCC
            0x90 => self.branch_if((self.p & 1) == 0, bus),
            // BCS
            0xb0 => self.branch_if((self.p & 1) != 0, bus),
            // BEQ
            0xf0 => self.branch_if((self.p & 2) != 0, bus),
            // BMI
            0x30 => self.branch_if((self.p as i8) < 0, bus),
            // BNE
            0xd0 => self.branch_if((self.p & 2) == 0, bus),
            // BPL
            0x10 => self.branch_if((self.p as i8) > 0, bus),
            // BVC
            0x50 => self.branch_if((self.p & 0b01000000) == 0, bus),
            // BVS
            0x70 => self.branch_if((self.p & 0b01000000) != 0, bus),
            // BIT
            0x24 => self.bit_zero_page(bus),
            0x2c => self.bit_abs(bus),
            // BRK
            0x00 => self.brk(bus),
            // CLC
            0x18 => self.clc(),
            // CLD
            0xd8 => self.cld(),
            // CLI
            0x58 => self.cli(),
            // CLV
            0xb8 => self.clv(),
            // CMP
            0xc9 => self.compare_register_imm(self.a, bus),
            0xc5 => self.compare_register_zero_page(self.a, bus),
            0xd5 => self.compare_register_zero_page_indexed(self.a, self.x, bus),
            0xcd => self.compare_register_abs(self.a, bus),
            0xdd => self.compare_register_abs_indexed(self.a, self.x, bus),
            0xd9 => self.compare_register_abs_indexed(self.a, self.y, bus),
            0xc1 => self.compare_register_indexed_indirect(self.a, bus),
            0xd1 => self.compare_register_indirect_indexed(self.a, bus),
            // CPX
            0xe0 => self.compare_register_imm(self.x, bus),
            0xe4 => self.compare_register_zero_page(self.x, bus),
            0xec => self.compare_register_abs(self.x, bus),
            // CPY
            0xc0 => self.compare_register_imm(self.y, bus),
            0xc4 => self.compare_register_zero_page(self.y, bus),
            0xcc => self.compare_register_abs(self.y, bus),
            // DEC
            0xc6 => self.dec_zero_page(bus),
            0xd6 => self.dec_zero_page_indexed(bus),
            0xce => self.dec_abs(bus),
            0xde => self.dec_abs_indexed(bus),
            // DEX
            0xca => self.dex(),
            // DEY
            0x88 => self.dey(),
            // EOR
            0x49 => self.eor_imm(bus),
            0x45 => self.eor_zero_page(bus),
            0x55 => self.eor_zero_page_indexed(bus),
            0x4d => self.eor_abs(bus),
            0x5d => self.eor_abs_indexed(self.x, bus),
            0x59 => self.eor_abs_indexed(self.y, bus),
            0x41 => self.eor_indexed_indirect(bus),
            0x51 => self.eor_indirect_indexed(bus),
            // INC
            0xe6 => self.inc_zero_page(bus),
            0xf6 => self.inc_zero_page_indexed(bus),
            0xee => self.inc_abs(bus),
            0xfe => self.inc_abs_indexed(bus),
            // INX
            0xe8 => self.inx(),
            // INY
            0xc8 => self.iny(),
            // JMP
            0x4c => self.jmp_abs(bus),
            0x6c => self.jmp_abs_indirect(bus),
            // JSR
            0x20 => self.jsr(bus),
            // LDA
            0xa9 => self.lda_imm(bus),
            0xa5 => self.lda_zero_page(bus),
            0xb5 => self.lda_zero_page_indexed(bus),
            0xad => self.lda_abs(bus),
            0xbd => self.lda_abs_indexed(self.x, bus),
            0xb9 => self.lda_abs_indexed(self.y, bus),
            0xa1 => self.lda_indexed_indirect(bus),
            0xb1 => self.lda_indirect_indexed(bus),
            // LDX
            0xa2 => self.ldx_imm(bus),
            0xa6 => self.ldx_zero_page(bus),
            0xb6 => self.ldx_zero_page_indexed(bus),
            0xae => self.ldx_abs(bus),
            0xbe => self.ldx_abs_indexed(self.y, bus),
            // LDY
            0xa0 => self.ldy_imm(bus),
            0xa4 => self.ldy_zero_page(bus),
            0xb4 => self.ldy_zero_page_indexed(bus),
            0xac => self.ldy_abs(bus),
            0xbc => self.ldy_abs_indexed(self.x, bus),
            // LSR
            0x4a => self.lsr_a(),
            0x46 => self.lsr_zero_page(bus),
            0x56 => self.lsr_zero_page_indexed(bus),
            0x4e => self.lsr_abs(bus),
            0x5e => self.lsr_abs_indexed(bus),
            // NOP
            0xea => self.nop(),
            // NOP (undocumented)
            0x1a | 0x3a | 0x5a | 0x7a | 0xda | 0xfa => self.nop(),
            // SKB/NOP (undocumented)
            0x80 | 0x82 | 0x89 | 0xc2 | 0xe2 => self.nop_imm(bus),
            // NOP/IGN (undocumented)
            0x04 | 0x44 | 0x64 => self.nop_zero_page(bus),
            0x14 | 0x34 | 0x54 | 0x74 | 0xd4 | 0xf4 => self.nop_zero_page_indexed(bus),
            0x0c => self.nop_abs(bus),
            0x1c | 0x3c | 0x5c | 0x7c | 0xdc | 0xfc => self.nop_abs_indexed(self.x, bus),
            // ORA
            0x09 => self.ora_imm(bus),
            0x05 => self.ora_zero_page(bus),
            0x15 => self.ora_zero_page_indexed(bus),
            0x0d => self.ora_abs(bus),
            0x1d => self.ora_abs_indexed(self.x, bus),
            0x19 => self.ora_abs_indexed(self.y, bus),
            0x01 => self.ora_indexed_indirect(bus),
            0x11 => self.ora_indirect_indexed(bus),
            // PHA
            0x48 => self.pha(bus),
            // PHP
            0x08 => self.php(bus),
            // PLA
            0x68 => self.pla(bus),
            // PLP
            0x28 => self.plp(bus),
            // ROL
            0x2a => self.rol_a(),
            0x26 => self.rol_zero_page(bus),
            0x36 => self.rol_zero_page_indexed(bus),
            0x2e => self.rol_abs(bus),
            0x3e => self.rol_abs_indexed(bus),
            // ROR
            0x6a => self.ror_a(),
            0x66 => self.ror_zero_page(bus),
            0x76 => self.ror_zero_page_indexed(bus),
            0x6e => self.ror_abs(bus),
            0x7e => self.ror_abs_indexed(bus),
            // RTI
            0x40 => self.rti(bus),
            // RTS
            0x60 => self.rts(bus),
            // SBC
            0xe9 | 0xeb => self.sbc_imm(bus),
            0xe5 => self.sbc_zero_page(bus),
            0xf5 => self.sbc_zero_page_indexed(bus),
            0xed => self.sbc_abs(bus),
            0xfd => self.sbc_abs_indexed(self.x, bus),
            0xf9 => self.sbc_abs_indexed(self.y, bus),
            0xe1 => self.sbc_indexed_indirect(bus),
            0xf1 => self.sbc_indirect_indexed(bus),
            // SEC
            0x38 => self.sec(),
            // SED
            0xf8 => self.sed(),
            // SEI
            0x78 => self.sei(),
            // STA
            0x85 => self.sta_zero_page(bus),
            0x95 => self.sta_zero_page_indexed(bus),
            0x8d => self.sta_abs(bus),
            0x9d => self.sta_abs_indexed(self.x, bus),
            0x99 => self.sta_abs_indexed(self.y, bus),
            0x81 => self.sta_indexed_indirect(bus),
            0x91 => self.sta_indirect_indexed(bus),
            // STX
            0x86 => self.stx_zero_page(bus),
            0x96 => self.stx_zero_page_indexed(bus),
            0x8e => self.stx_abs(bus),
            // STY
            0x84 => self.sty_zero_page(bus),
            0x94 => self.sty_zero_page_indexed(bus),
            0x8c => self.sty_abs(bus),
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
            o => panic!("TODO: handle invalid opcode: 0x{:>01x}", o),
        }
    }

    pub fn fetch_operand_byte(&mut self, bus: &mut dyn CpuAddressBus) -> u8 {
        self.pc += 1;
        self.cycle_count += 1;
        bus.read(self.pc, self)
    }

    pub fn fetch_operand_bytes(&mut self, bus: &mut dyn CpuAddressBus) -> [u8; 2] {
        let low_byte = self.fetch_operand_byte(bus);
        let high_byte = self.fetch_operand_byte(bus);
        [low_byte, high_byte]
    }

    pub fn fetch_operand_u16(&mut self, bus: &mut dyn CpuAddressBus) -> u16 {
        u16::from_le_bytes(self.fetch_operand_bytes(bus))
    }

    fn is_irq_pending(&self) -> bool {
        self.irq > 0 && !self.is_i_set()
    }

    // sets the carry flag based on the 'carry' bool
    fn set_c_from_bool(&mut self, carry: bool) {
        self.p = (self.p & !1) | carry as u8;
    }

    // ors 'bit' directly with the carry flag
    fn set_c_from_bit(&mut self, bit: u8) {
        self.p = (self.p & !1) | bit;
    }

    fn set_v_from_bool(&mut self, overflow: bool) {
        self.p = (self.p & !0b01000000) | ((overflow as u8) << 6);
    }

    fn set_v_from_bit(&mut self, bit: u8) {
        self.p = (self.p & !0b01000000) | bit;
    }

    fn set_z_from_val(&mut self, val: u8) {
        self.p = (self.p & !2) | (((val == 0) as u8) << 1);
    }

    fn set_z_from_bool(&mut self, zero: bool) {
        self.p = (self.p & !2) | ((zero as u8) << 1);
    }

    fn set_n_from_val(&mut self, val: u8) {
        self.p = (self.p & !0x80) | val & 0x80;
    }

    fn set_n_from_bool(&mut self, negative: bool) {
        self.p = (self.p & !0x80) | (negative as u8) << 7;
    }

    fn set_i_from_bit(&mut self, bit: u8) {
        self.p = (self.p & !4) | bit;
    }

    fn is_i_set(&self) -> bool {
        (self.p & 4) != 0
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
        self.set_v_from_bool(overflow_1 ^ overflow_2);
        self.set_z_from_val(res_2);
        self.set_n_from_val(res_2);
    }

    fn adc_imm(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_imm(self, bus);
        self.adc(val);
    }

    fn adc_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page(self, bus);
        self.adc(val);
    }

    fn adc_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page_indexed(self, self.x, bus);
        self.adc(val);
    }

    fn adc_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs(self, bus);
        self.adc(val);
    }

    fn adc_abs_indexed(&mut self, index: u8, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs_indexed(self, index, bus);
        self.adc(val);
    }

    fn adc_indexed_indirect(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_indexed_indirect(self, bus);
        self.adc(val);
    }

    fn adc_indirect_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_indirect_indexed(self, bus);
        self.adc(val);
    }

    fn and(&mut self, val: u8) {
        self.a &= val;
        self.set_n_from_val(self.a);
        self.set_z_from_val(self.a);
    }

    fn and_imm(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_imm(self, bus);
        self.and(val);
    }

    fn and_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page(self, bus);
        self.and(val);
    }

    fn and_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page_indexed(self, self.x, bus);
        self.and(val);
    }

    fn and_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs(self, bus);
        self.and(val);
    }

    fn and_abs_indexed(&mut self, index: u8, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs_indexed(self, index, bus);
        self.and(val);
    }

    fn and_indexed_indirect(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_indexed_indirect(self, bus);
        self.and(val);
    }

    fn and_indirect_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_indirect_indexed(self, bus);
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

    fn asl_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_zero_page(self, bus, Self::asl);
    }

    fn asl_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_zero_page_indexed(self, self.x, bus, Self::asl);
    }

    fn asl_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_abs(self, bus, Self::asl);
    }

    fn asl_abs_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_abs_indexed(self, self.x, bus, Self::asl);
    }

    fn branch_if(&mut self, condition: bool, bus: &mut dyn CpuAddressBus) {
        let offset = self.fetch_operand_byte(bus);
        self.cycle_count += 1;
        self.pc += 1;

        if condition {
            // dummy opcode fetch attempt (this fetch corresponds to what would've
            // been the next instruction's opcode fetch if the branch wasn't taken)
            let _ = bus.read(self.pc, self);

            let (new_pc_low, carry) = (self.pc as u8).overflowing_add(offset as u8);
            self.pc = u16::from_le_bytes([new_pc_low, self.pc.to_le_bytes()[1]]);
            self.cycle_count += 1;

            let offset_sign = (offset as i8) < 0;
            // xor sign of 'offset' with 'carry' to determine
            // whether a page boundary will be crossed
            let page_crossed = offset_sign ^ carry;

            if page_crossed {
                let _ = bus.read(self.pc + 1, self);
                self.cycle_count += 1;
            }

            // fix high byte of pc
            self.pc = u16::from_le_bytes([
                self.pc.to_le_bytes()[0],
                self.pc.to_le_bytes()[1]
                    .wrapping_add(carry as u8)
                    .wrapping_add((offset_sign as u8) * 0xff),
            ]);
        }
    }

    fn bit(&mut self, val: u8) {
        self.set_v_from_bit(val & 0b01000000);
        self.set_n_from_val(val);

        let res = val & self.a;
        self.set_z_from_val(res);
    }

    fn bit_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page(self, bus);
        self.bit(val);
    }

    fn bit_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs(self, bus);
        self.bit(val);
    }

    fn interrupt(&mut self, vector_addr: u16, status_flags: u8, bus: &mut dyn CpuAddressBus) {
        self.cycle_count += 2;
        let pc_bytes = self.pc.to_le_bytes();

        // push high bits of pc
        bus.write(self.sp as u16 + 0x100, pc_bytes[1], self);
        self.sp = self.sp.wrapping_sub(1);
        self.cycle_count += 1;

        // push low bits of pc
        bus.write(self.sp as u16 + 0x100, pc_bytes[0], self);
        self.sp = self.sp.wrapping_sub(1);
        self.cycle_count += 1;

        // push status flags
        bus.write(self.sp as u16 + 0x100, status_flags, self);
        self.sp = self.sp.wrapping_sub(1);
        self.cycle_count += 1;

        // set interrupt disable flag
        self.set_i_from_bit(4);

        let vector = {
            let vector_lo = bus.read(vector_addr, self);
            self.cycle_count += 1;

            let vector_hi = bus.read(vector_addr + 1, self);
            self.cycle_count += 1;

            u16::from_le_bytes([vector_lo, vector_hi])
        };

        // set pc to interrupt vector address
        self.pc = vector;
    }

    fn brk(&mut self, bus: &mut dyn CpuAddressBus) {
        // NOTE: pc + 2 is pushed to the stack, despite brk being a one byte instruction
        self.pc += 2;
        self.interrupt(0xfffe, self.p | 0b10000, bus);
    }

    fn irq(&mut self, bus: &mut dyn CpuAddressBus) {
        self.interrupt(0xfffe, self.p | 0b10000, bus);
    }

    fn nmi(&mut self, bus: &mut dyn CpuAddressBus) {
        self.interrupt(0xfffa, self.p, bus);
    }

    fn reset(&mut self, bus: &mut dyn CpuAddressBus) {
        // TODO: ..
    }

    fn clc(&mut self) {
        self.pc += 1;
        self.set_c_from_bool(false);
        self.cycle_count += 2;
    }

    fn cld(&mut self) {
        self.pc += 1;
        self.p &= !8;
        self.cycle_count += 2;
    }

    fn cli(&mut self) {
        self.pc += 1;
        self.set_i_from_bit(0);

        // delay potential irq (known 6502 quirk that also affects sei and plp)
        if self.is_irq_pending() {
            self.bits.delay_irq.set(1);
        }

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

    fn compare_register_imm(&mut self, register: u8, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_imm(self, bus);
        self.compare_register_val(register, val);
    }

    fn compare_register_zero_page(&mut self, register: u8, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page(self, bus);
        self.compare_register_val(register, val);
    }

    fn compare_register_zero_page_indexed(
        &mut self,
        register: u8,
        index: u8,
        bus: &mut dyn CpuAddressBus,
    ) {
        let val = addressing::read_zero_page_indexed(self, index, bus);
        self.compare_register_val(register, val);
    }

    fn compare_register_abs(&mut self, register: u8, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs(self, bus);
        self.compare_register_val(register, val);
    }

    fn compare_register_abs_indexed(
        &mut self,
        register: u8,
        index: u8,
        bus: &mut dyn CpuAddressBus,
    ) {
        let val = addressing::read_abs_indexed(self, index, bus);
        self.compare_register_val(register, val);
    }

    fn compare_register_indexed_indirect(&mut self, register: u8, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_indexed_indirect(self, bus);
        self.compare_register_val(register, val);
    }

    fn compare_register_indirect_indexed(&mut self, register: u8, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_indirect_indexed(self, bus);
        self.compare_register_val(register, val);
    }

    fn decrement_val(&mut self, val: u8) -> u8 {
        let res = val.wrapping_sub(1);
        self.set_z_from_val(res);
        self.set_n_from_val(res);

        res
    }

    fn dec_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_zero_page(self, bus, Self::decrement_val);
    }

    fn dec_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_zero_page_indexed(self, self.x, bus, Self::decrement_val);
    }

    fn dec_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_abs(self, bus, Self::decrement_val);
    }

    fn dec_abs_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_abs_indexed(self, self.x, bus, Self::decrement_val);
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

    fn eor_imm(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_imm(self, bus);
        self.eor(val);
    }

    fn eor_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page(self, bus);
        self.eor(val);
    }

    fn eor_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page_indexed(self, self.x, bus);
        self.eor(val);
    }

    fn eor_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs(self, bus);
        self.eor(val);
    }

    fn eor_abs_indexed(&mut self, index: u8, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs_indexed(self, index, bus);
        self.eor(val);
    }

    fn eor_indexed_indirect(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_indexed_indirect(self, bus);
        self.eor(val);
    }

    fn eor_indirect_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_indirect_indexed(self, bus);
        self.eor(val);
    }

    fn increment_val(&mut self, val: u8) -> u8 {
        let res = val.wrapping_add(1);
        self.set_z_from_val(res);
        self.set_n_from_val(res);

        res
    }

    fn inc_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_zero_page(self, bus, Self::increment_val);
    }

    fn inc_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_zero_page_indexed(self, self.x, bus, Self::increment_val);
    }

    fn inc_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_abs(self, bus, Self::increment_val);
    }

    fn inc_abs_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_abs_indexed(self, self.x, bus, Self::increment_val);
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

    fn jmp_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        self.pc = self.fetch_operand_u16(bus);
        self.cycle_count += 1;
    }

    fn jmp_abs_indirect(&mut self, bus: &mut dyn CpuAddressBus) {
        let mut addr_bytes = self.fetch_operand_bytes(bus);
        self.cycle_count += 1;

        let final_addr_lo = bus.read(u16::from_le_bytes(addr_bytes), self);
        self.cycle_count += 1;

        // add 1 to low bits without carry to get address of high bits of final address
        addr_bytes[0] = addr_bytes[0].wrapping_add(1);
        let final_addr_hi = bus.read(u16::from_le_bytes(addr_bytes), self);

        self.pc = u16::from_le_bytes([final_addr_lo, final_addr_hi]);
        self.cycle_count += 1;
    }

    fn jsr(&mut self, bus: &mut dyn CpuAddressBus) {
        let addr = self.fetch_operand_u16(bus);
        self.cycle_count += 2;

        // get return address (next instruction - 1)
        let ret_addr = self.pc.to_le_bytes();

        // push low bits of address to sp - 1
        bus.write(self.sp.wrapping_sub(1) as u16 + 0x100, ret_addr[0], self);
        self.cycle_count += 1;

        // push high bits of address to sp
        bus.write(self.sp as u16 + 0x100, ret_addr[1], self);

        self.sp = self.sp.wrapping_sub(2);
        self.pc = addr;
        self.cycle_count += 1;
    }

    fn lda(&mut self, val: u8) {
        self.set_z_from_val(val);
        self.set_n_from_val(val);
        self.a = val;
    }

    fn lda_imm(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_imm(self, bus);
        self.lda(val);
    }

    fn lda_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page(self, bus);
        self.lda(val);
    }

    fn lda_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page_indexed(self, self.x, bus);
        self.lda(val);
    }

    fn lda_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs(self, bus);
        self.lda(val);
    }

    fn lda_abs_indexed(&mut self, index: u8, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs_indexed(self, index, bus);
        self.lda(val);
    }

    fn lda_indexed_indirect(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_indexed_indirect(self, bus);
        self.lda(val);
    }

    fn lda_indirect_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_indirect_indexed(self, bus);
        self.lda(val);
    }

    fn ldx(&mut self, val: u8) {
        self.set_z_from_val(val);
        self.set_n_from_val(val);
        self.x = val;
    }

    fn ldx_imm(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_imm(self, bus);
        self.ldx(val);
    }

    fn ldx_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page(self, bus);
        self.ldx(val);
    }

    fn ldx_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page_indexed(self, self.y, bus);
        self.ldx(val);
    }

    fn ldx_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs(self, bus);
        self.ldx(val);
    }

    fn ldx_abs_indexed(&mut self, index: u8, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs_indexed(self, index, bus);
        self.ldx(val);
    }

    fn ldy(&mut self, val: u8) {
        self.set_z_from_val(val);
        self.set_n_from_val(val);
        self.y = val;
    }

    fn ldy_imm(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_imm(self, bus);
        self.ldy(val);
    }

    fn ldy_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page(self, bus);
        self.ldy(val);
    }

    fn ldy_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page_indexed(self, self.x, bus);
        self.ldy(val);
    }

    fn ldy_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs(self, bus);
        self.ldy(val);
    }

    fn ldy_abs_indexed(&mut self, index: u8, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs_indexed(self, index, bus);
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

    fn lsr_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_zero_page(self, bus, Self::lsr);
    }

    fn lsr_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_zero_page_indexed(self, self.x, bus, Self::lsr);
    }

    fn lsr_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_abs(self, bus, Self::lsr);
    }

    fn lsr_abs_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_abs_indexed(self, self.x, bus, Self::lsr);
    }

    fn nop(&mut self) {
        self.pc += 1;
        self.cycle_count += 2;
    }

    fn nop_imm(&mut self, bus: &mut dyn CpuAddressBus) {
        let _ = addressing::read_imm(self, bus);
    }

    fn nop_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        let _ = addressing::read_zero_page(self, bus);
    }

    fn nop_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        let _ = addressing::read_zero_page_indexed(self, self.x, bus);
    }

    fn nop_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        // read from address and ignore result (the redundant read
        // must be performed, as it may have side effects)
        let _ = addressing::read_abs(self, bus);
    }

    fn nop_abs_indexed(&mut self, index: u8, bus: &mut dyn CpuAddressBus) {
        let _ = addressing::read_abs_indexed(self, index, bus);
    }

    fn ora(&mut self, val: u8) {
        self.a |= val;
        self.set_z_from_val(self.a);
        self.set_n_from_val(self.a);
    }

    fn ora_imm(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_imm(self, bus);
        self.ora(val);
    }

    fn ora_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page(self, bus);
        self.ora(val);
    }

    fn ora_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page_indexed(self, self.x, bus);
        self.ora(val);
    }

    fn ora_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs(self, bus);
        self.ora(val);
    }

    fn ora_abs_indexed(&mut self, index: u8, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs_indexed(self, index, bus);
        self.ora(val);
    }

    fn ora_indexed_indirect(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_indexed_indirect(self, bus);
        self.ora(val);
    }

    fn ora_indirect_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_indirect_indexed(self, bus);
        self.ora(val);
    }

    // used for pha, php instructions
    fn push_val(&mut self, val: u8, bus: &mut dyn CpuAddressBus) {
        self.cycle_count += 2;

        bus.write(self.sp as u16 + 0x100, val, self);
        self.cycle_count += 1;

        self.sp = self.sp.wrapping_sub(1);
        self.pc += 1;
    }

    fn pha(&mut self, bus: &mut dyn CpuAddressBus) {
        self.push_val(self.a, bus)
    }

    fn php(&mut self, bus: &mut dyn CpuAddressBus) {
        // NOTE: the 'b-flag' bit is set when pushing
        // FIXME: may need to set bit 5 when pushing as well? it should be set by default, but
        self.push_val(self.p | 0b10000, bus);
    }

    // used for pla, plp instructions
    fn pull_val(&mut self, bus: &mut dyn CpuAddressBus) -> u8 {
        self.sp = self.sp.wrapping_add(1);

        self.cycle_count += 3;
        let res = bus.read(self.sp as u16 + 0x100, self);

        self.pc += 1;
        self.cycle_count += 1;
        res
    }

    fn pla(&mut self, bus: &mut dyn CpuAddressBus) {
        self.a = self.pull_val(bus);
        self.set_z_from_val(self.a);
        self.set_n_from_val(self.a);
    }

    fn plp(&mut self, bus: &mut dyn CpuAddressBus) {
        self.p = (self.pull_val(bus) & !0b10000) | 0b100000;

        if self.is_irq_pending() {
            self.bits.delay_irq.set(1);
        }
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

    fn rol_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_zero_page(self, bus, Self::rol);
    }

    fn rol_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_zero_page_indexed(self, self.x, bus, Self::rol);
    }

    fn rol_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_abs(self, bus, Self::rol);
    }

    fn rol_abs_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_abs_indexed(self, self.x, bus, Self::rol);
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

    fn ror_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_zero_page(self, bus, Self::ror);
    }

    fn ror_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_zero_page_indexed(self, self.x, bus, Self::ror);
    }

    fn ror_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_abs(self, bus, Self::ror);
    }

    fn ror_abs_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::read_write_abs_indexed(self, self.x, bus, Self::ror);
    }

    fn rti(&mut self, bus: &mut dyn CpuAddressBus) {
        self.pc += 1;
        self.cycle_count += 1;

        // read next instruction byte and discard it
        let _ = bus.read(self.pc, self);
        self.cycle_count += 1;

        self.sp = self.sp.wrapping_add(1);
        self.cycle_count += 1;

        // pull into status flags
        // NOTE: bit 4 is cleared and bit 5 is set when pulling into status register
        self.p = (bus.read(self.sp as u16 + 0x100, self) & !0b10000) | 0b100000;
        self.sp = self.sp.wrapping_add(1);
        self.cycle_count += 1;

        // get program counter low bits
        let pc_low = bus.read(self.sp as u16 + 0x100, self);
        self.sp = self.sp.wrapping_add(1);
        self.cycle_count += 1;

        // get program counter high bits
        let pc_hi = bus.read(self.sp as u16 + 0x100, self);
        self.cycle_count += 1;

        self.pc = u16::from_le_bytes([pc_low, pc_hi]);
    }

    fn rts(&mut self, bus: &mut dyn CpuAddressBus) {
        self.pc += 1;
        self.cycle_count += 1;

        // read next instruction byte and discard it
        let _ = bus.read(self.pc, self);
        self.cycle_count += 1;

        self.sp = self.sp.wrapping_add(1);
        self.cycle_count += 1;

        let pc_low = bus.read(self.sp as u16 + 0x100, self);
        self.sp = self.sp.wrapping_add(1);
        self.cycle_count += 1;

        let pc_hi = bus.read(self.sp as u16 + 0x100, self);
        self.cycle_count += 1;

        // FIXME: should discard carry from low 8 bits when adding?
        self.pc = u16::from_le_bytes([pc_low, pc_hi]);

        // add 1 since pushed value is expected to be pc - 1 (from the jsr instruction)
        self.pc += 1;
        self.cycle_count += 1;
    }

    fn sbc(&mut self, val: u8) {
        let (res_1, borrow_1) = self.a.overflowing_sub(val);
        let (_, overflow_1) = (self.a as i8).overflowing_sub(val as i8);

        // subtract the not of the carry from the result
        let (res_2, borrow_2) = res_1.overflowing_sub((self.p & 1) ^ 1);
        let (_, overflow_2) = (res_1 as i8).overflowing_sub(((self.p & 1) ^ 1) as i8);

        self.a = res_2;
        self.set_c_from_bool(!(borrow_1 | borrow_2));
        self.set_v_from_bool(overflow_1 ^ overflow_2);
        self.set_z_from_val(res_2);
        self.set_n_from_val(res_2);
    }

    fn sbc_imm(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_imm(self, bus);
        self.sbc(val);
    }

    fn sbc_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page(self, bus);
        self.sbc(val);
    }

    fn sbc_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_zero_page_indexed(self, self.x, bus);
        self.sbc(val);
    }

    fn sbc_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs(self, bus);
        self.sbc(val);
    }

    fn sbc_abs_indexed(&mut self, index: u8, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_abs_indexed(self, index, bus);
        self.sbc(val);
    }

    fn sbc_indexed_indirect(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_indexed_indirect(self, bus);
        self.sbc(val);
    }

    fn sbc_indirect_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        let val = addressing::read_indirect_indexed(self, bus);
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

        if self.is_irq_pending() {
            self.bits.delay_irq.set(1);
        }

        self.cycle_count += 2;
    }

    fn sta_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::write_zero_page(self, self.a, bus);
    }

    fn sta_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::write_zero_page_indexed(self, self.a, self.x, bus);
    }

    fn sta_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::write_abs(self, self.a, bus);
    }

    fn sta_abs_indexed(&mut self, index: u8, bus: &mut dyn CpuAddressBus) {
        addressing::write_abs_indexed(self, self.a, index, bus);
    }

    fn sta_indexed_indirect(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::write_indexed_indirect(self, self.a, bus);
    }

    fn sta_indirect_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::write_indirect_indexed(self, self.a, bus);
    }

    fn stx_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::write_zero_page(self, self.x, bus);
    }

    fn stx_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::write_zero_page_indexed(self, self.x, self.y, bus);
    }

    fn stx_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::write_abs(self, self.x, bus);
    }

    fn sty_zero_page(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::write_zero_page(self, self.y, bus);
    }

    fn sty_zero_page_indexed(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::write_zero_page_indexed(self, self.y, self.x, bus);
    }

    fn sty_abs(&mut self, bus: &mut dyn CpuAddressBus) {
        addressing::write_abs(self, self.y, bus);
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

    fn debug_exec_opcode(&mut self, opc: [u8; 3], bus: &mut dyn CpuAddressBus) -> u8 {
        bus.write(self.pc, opc[0], self);
        bus.write(self.pc + 1, opc[1], self);
        bus.write(self.pc + 2, opc[2], self);

        let prev_cycles = self.cycle_count;
        self.exec_instruction(bus);
        (self.cycle_count - prev_cycles) as u8
    }
}
