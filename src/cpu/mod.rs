use super::memory_map::MemoryMap;
use super::{apu, memory_map as mmap, ppu};

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
            ..Default::default()
        }
    }

    pub fn exec_instruction(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ppu: &mut ppu::Ppu,
        apu: &mut apu::Apu,
    ) -> u8 {
        match [
            memory.read_cpu(ppu, apu, self.pc),
            memory.read_cpu(ppu, apu, self.pc + 1),
            memory.read_cpu(ppu, apu, self.pc + 2),
        ] {
            // ADC #byte_1 (immediate)
            [0x69, byte_1, _] => {
                self.adc(byte_1, 2);
                2
            }
            // ADC $byte_1 (zero page)
            [0x65, byte_1, _] => {
                let val = memory.read_cpu(ppu, apu, byte_1);
                self.adc(val, 2);
                3
            }
            // ADC $byte_1, X (zero page indexed)
            [0x75, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                self.adc(memory.read_cpu(ppu, apu, addr), 2);
                4
            }
            // ADC $bytes (absolute)
            [0x6d, bytes @ ..] => {
                let val = memory.read_cpu(ppu, apu, u16::from_le_bytes(bytes));
                self.adc(val, 3);
                4
            }
            // ADC $bytes, X (absolute indexed)
            [0x7d, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                self.adc(memory.read_cpu(ppu, apu, addr), 3);
                // add 'carry' for one extra cycle if a page boundary was crossed
                4 + carry as u8
            }
            // ADC $bytes, Y (absolute indexed)
            [0x79, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.adc(memory.read_cpu(ppu, apu, addr), 3);
                4 + carry as u8
            }
            // ADC ($byte_1, X) (indexed indirect)
            [0x61, byte_1, _] => {
                let addr = self.get_indexed_indirect(byte_1, memory, ppu, apu);
                self.adc(memory.read_cpu(ppu, apu, addr), 2);
                6
            }
            // ADC ($byte_1), Y (indirect indexed)
            [0x71, byte_1, _] => {
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory, ppu, apu);
                self.adc(memory.read_cpu(ppu, apu, addr), 2);
                5 + carry as u8
            }
            // AND #byte_1 (immediate)
            [0x29, byte_1, _] => {
                self.and(byte_1, 2);
                2
            }
            // AND $byte_1 (zero page)
            [0x25, byte_1, _] => {
                let val = memory.read_cpu(ppu, apu, byte_1);
                self.and(val, 2);
                3
            }
            // AND $byte_1, X (zero page indexed)
            [0x35, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                self.and(memory.read_cpu(ppu, apu, addr), 2);
                4
            }
            // AND $bytes (absolute)
            [0x2d, bytes @ ..] => {
                let val = memory.read_cpu(ppu, apu, u16::from_le_bytes(bytes));
                self.and(val, 3);
                4
            }
            // AND $bytes, X (absolute indexed)
            [0x3d, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                self.and(memory.read_cpu(ppu, apu, addr), 3);
                4 + carry as u8
            }
            // AND $bytes, Y (absolute indexed)
            [0x39, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.and(memory.read_cpu(ppu, apu, addr), 3);
                4 + carry as u8
            }
            // AND ($byte_1, X) (indexed indirect)
            [0x21, byte_1, _] => {
                let addr = self.get_indexed_indirect(byte_1, memory, ppu, apu);
                self.and(memory.read_cpu(ppu, apu, addr), 2);
                6
            }
            // AND ($byte_1), Y (indirect indexed)
            [0x31, byte_1, _] => {
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory, ppu, apu);
                self.and(memory.read_cpu(ppu, apu, addr), 2);
                5 + carry as u8
            }
            // ASL A (accumulator)
            [0x0a, ..] => {
                self.a = self.asl(self.a, 1);
                2
            }
            // ASL $byte_1 (zero page)
            [0x06, byte_1, _] => {
                memory.write_cpu(
                    ppu,
                    apu,
                    byte_1,
                    self.asl(memory.read_cpu(ppu, apu, byte_1), 2),
                );
                5
            }
            // ASL $byte_1, X (zero page indexed)
            [0x16, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                memory.write_cpu(ppu, apu, addr, self.asl(memory.read_cpu(ppu, apu, addr), 2));
                6
            }
            // ASL $bytes (absolute)
            [0x0e, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                memory.write_cpu(ppu, apu, addr, self.asl(memory.read_cpu(ppu, apu, addr), 3));
                6
            }
            // ASL $bytes, X (absolute indexed)
            [0x1e, bytes @ ..] => {
                let (addr, _) = self.get_absolute_indexed(bytes, self.x);
                memory.write_cpu(ppu, apu, addr, self.asl(memory.read_cpu(ppu, apu, addr), 3));
                7
            }
            // BCC $byte_1
            [0x90, byte_1, _] => self.branch_if((self.p & 1) == 0, byte_1),
            // BCS $byte_1
            [0xb0, byte_1, _] => self.branch_if((self.p & 1) != 0, byte_1),
            // BEQ $byte_1
            [0xf0, byte_1, _] => self.branch_if((self.p & 2) != 0, byte_1),
            // BMI $byte_1
            [0x30, byte_1, _] => self.branch_if((self.p as i8) < 0, byte_1),
            // BNE $byte_1
            [0xd0, byte_1, _] => self.branch_if((self.p & 2) == 0, byte_1),
            // BPL $byte_1
            [0x10, byte_1, _] => self.branch_if((self.p as i8) > 0, byte_1),
            // BVC $byte_1
            [0x50, byte_1, _] => self.branch_if((self.p & 0b01000000) == 0, byte_1),
            // BVS $byte_1
            [0x70, byte_1, _] => self.branch_if((self.p & 0b01000000) != 0, byte_1),
            // BIT $byte_1 (zero page)
            [0x24, byte_1, _] => {
                self.bit(memory.read_cpu(ppu, apu, byte_1), 2);
                3
            }
            // BIT $bytes (absolute)
            [0x2c, bytes @ ..] => {
                self.bit(memory.read_cpu(ppu, apu, u16::from_le_bytes(bytes)), 3);
                4
            }
            // BRK
            [0x00, ..] => {
                self.brk(memory, ppu, apu);
                7
            }
            // CLC
            [0x18, ..] => {
                self.pc += 1;
                self.set_c_from_bool(false);
                2
            }
            // CLD
            [0xd8, ..] => {
                self.pc += 1;
                self.p = self.p & !8;
                2
            }
            // CLI
            [0x58, ..] => {
                self.pc += 1;
                self.set_i_from_bit(0);
                2
            }
            // CLV
            [0xb8, ..] => {
                self.pc += 1;
                self.set_v_from_bit(0);
                2
            }
            // CMP #byte_1 (immediate)
            [0xc9, byte_1, _] => {
                self.cmp_register_val(self.a, byte_1, 2);
                2
            }
            // CMP $byte_1 (zero page)
            [0xc5, byte_1, _] => {
                let val = memory.read_cpu(ppu, apu, byte_1);
                self.cmp_register_val(self.a, val, 2);
                3
            }
            // CMP $byte_1, X (zero page indexed)
            [0xd5, byte_1, _] => {
                let val = memory.read_cpu(ppu, apu, byte_1.wrapping_add(self.x));
                self.cmp_register_val(self.a, val, 2);
                4
            }
            // CMP $bytes (absolute)
            [0xcd, bytes @ ..] => {
                let val = memory.read_cpu(ppu, apu, u16::from_le_bytes(bytes));
                self.cmp_register_val(self.a, val, 3);
                4
            }
            // CMP $bytes, X (absolute indexed)
            [0xdd, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                self.cmp_register_val(self.a, memory.read_cpu(ppu, apu, addr), 3);
                4 + carry as u8
            }
            // CMP $bytes, Y (absolute indexed)
            [0xd9, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.cmp_register_val(self.a, memory.read_cpu(ppu, apu, addr), 3);
                4 + carry as u8
            }
            // CMP ($byte_1, X) (indexed indirect)
            [0xc1, byte_1, _] => {
                let addr = self.get_indexed_indirect(byte_1, memory, ppu, apu);
                self.cmp_register_val(self.a, memory.read_cpu(ppu, apu, addr), 2);
                6
            }
            // CMP ($byte_1), Y (indirect indexed)
            [0xd1, byte_1, _] => {
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory, ppu, apu);
                self.cmp_register_val(self.a, memory.read_cpu(ppu, apu, addr), 2);
                5 + carry as u8
            }
            // CPX #byte_1 (immediate)
            [0xe0, byte_1, _] => {
                self.cmp_register_val(self.x, byte_1, 2);
                2
            }
            // CPX $byte_1 (zero page)
            [0xe4, byte_1, _] => {
                self.cmp_register_val(self.x, memory.read_cpu(ppu, apu, byte_1), 2);
                3
            }
            // CPX $bytes (absolute)
            [0xec, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                self.cmp_register_val(self.x, memory.read_cpu(ppu, apu, addr), 3);
                4
            }
            // CPY #byte_1 (immediate)
            [0xc0, byte_1, _] => {
                self.cmp_register_val(self.y, byte_1, 2);
                2
            }
            // CPY $byte_1 (zero page)
            [0xc4, byte_1, _] => {
                self.cmp_register_val(self.y, memory.read_cpu(ppu, apu, byte_1), 2);
                3
            }
            // CPY $bytes (absolute)
            [0xcc, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                self.cmp_register_val(self.y, memory.read_cpu(ppu, apu, addr), 3);
                4
            }
            // DEC $byte_1 (zero page)
            [0xc6, byte_1, _] => {
                memory.write_cpu(
                    ppu,
                    apu,
                    byte_1,
                    self.dec(memory.read_cpu(ppu, apu, byte_1), 2),
                );
                5
            }
            // DEC $byte_1, X (zero page indexed)
            [0xd6, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                memory.write_cpu(ppu, apu, addr, self.dec(memory.read_cpu(ppu, apu, addr), 2));
                6
            }
            // DEC $bytes (absolute)
            [0xce, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                memory.write_cpu(ppu, apu, addr, self.dec(memory.read_cpu(ppu, apu, addr), 3));
                6
            }
            // DEC $bytes, X (absolute indexed)
            [0xde, bytes @ ..] => {
                let (addr, _) = self.get_absolute_indexed(bytes, self.x);
                memory.write_cpu(ppu, apu, addr, self.dec(memory.read_cpu(ppu, apu, addr), 3));
                7
            }
            // DEX
            [0xca, ..] => {
                self.x = self.dec(self.x, 1);
                2
            }
            // DEY
            [0x88, ..] => {
                self.y = self.dec(self.y, 1);
                2
            }
            // EOR #byte_1 (immediate)
            [0x49, byte_1, _] => {
                self.eor(byte_1, 2);
                2
            }
            // EOR $byte_1 (zero page)
            [0x45, byte_1, _] => {
                self.eor(memory.read_cpu(ppu, apu, byte_1), 2);
                3
            }
            // EOR $byte_1, X (zero page indexed)
            [0x55, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                self.eor(memory.read_cpu(ppu, apu, addr), 2);
                4
            }
            // EOR $bytes (absolute)
            [0x4d, bytes @ ..] => {
                self.eor(memory.read_cpu(ppu, apu, u16::from_le_bytes(bytes)), 3);
                4
            }
            // EOR $bytes, X (absolute indexed)
            [0x5d, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                self.eor(memory.read_cpu(ppu, apu, addr), 3);
                4 + carry as u8
            }
            // EOR $bytes, Y (absolute indexed)
            [0x59, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.eor(memory.read_cpu(ppu, apu, addr), 3);
                4 + carry as u8
            }
            // EOR ($bytes, X) (indexed indirect)
            [0x41, byte_1, _] => {
                let addr = self.get_indexed_indirect(byte_1, memory, ppu, apu);
                self.eor(memory.read_cpu(ppu, apu, addr), 2);
                6
            }
            // EOR ($bytes), Y (indirect indexed)
            [0x51, byte_1, _] => {
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory, ppu, apu);
                self.eor(memory.read_cpu(ppu, apu, addr), 2);
                5 + carry as u8
            }
            // INC $byte_1 (zero page)
            [0xe6, byte_1, _] => {
                memory.write_cpu(
                    ppu,
                    apu,
                    byte_1,
                    self.inc(memory.read_cpu(ppu, apu, byte_1), 2),
                );
                5
            }
            // INC $byte_1, X (zero page indexed)
            [0xf6, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                memory.write_cpu(ppu, apu, addr, self.inc(memory.read_cpu(ppu, apu, addr), 2));
                6
            }
            // INC $bytes (absolute)
            [0xee, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                memory.write_cpu(ppu, apu, addr, self.inc(memory.read_cpu(ppu, apu, addr), 3));
                6
            }
            // INC $bytes, X (absolute indexed)
            [0xfe, bytes @ ..] => {
                let (addr, _) = self.get_absolute_indexed(bytes, self.x);
                memory.write_cpu(ppu, apu, addr, self.inc(memory.read_cpu(ppu, apu, addr), 3));
                7
            }
            // INX
            [0xe8, ..] => {
                self.x = self.inc(self.x, 1);
                2
            }
            // INY
            [0xc8, ..] => {
                self.y = self.inc(self.y, 1);
                2
            }
            // JMP $bytes (absolute)
            [0x4c, bytes @ ..] => {
                self.pc = u16::from_le_bytes(bytes);
                3
            }
            // JMP ($bytes) (absolute indirect)
            [0x6c, mut bytes @ ..] => {
                let addr_lo = memory.read_cpu(ppu, apu, u16::from_le_bytes(bytes));
                // add 1 to low bits without carry to get address of high bits of final address
                bytes[0] = bytes[0].wrapping_add(1);
                let addr_hi = memory.read_cpu(ppu, apu, u16::from_le_bytes(bytes));
                self.pc = u16::from_le_bytes([addr_lo, addr_hi]);
                5
            }
            // JSR $bytes (absolute)
            [0x20, bytes @ ..] => {
                self.jsr(u16::from_le_bytes(bytes), memory, ppu, apu);
                6
            }
            // LDA #byte_1 (immediate)
            [0xa9, byte_1, _] => {
                self.lda(byte_1, 2);
                2
            }
            // LDA $byte_1 (zero page)
            [0xa5, byte_1, _] => {
                self.lda(memory.read_cpu(ppu, apu, byte_1), 2);
                3
            }
            // LDA $byte_1, X (zero page indexed)
            [0xb5, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                self.lda(memory.read_cpu(ppu, apu, addr), 2);
                4
            }
            // LDA $bytes (absolute)
            [0xad, bytes @ ..] => {
                self.lda(memory.read_cpu(ppu, apu, u16::from_le_bytes(bytes)), 3);
                4
            }
            // LDA $bytes, X (absolute indexed)
            [0xbd, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                self.lda(memory.read_cpu(ppu, apu, addr), 3);
                4 + carry as u8
            }
            // LDA $bytes, Y (absolute indexed)
            [0xb9, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.lda(memory.read_cpu(ppu, apu, addr), 3);
                4 + carry as u8
            }
            // LDA ($byte_1, X) (indexed indirect)
            [0xa1, byte_1, _] => {
                let addr = self.get_indexed_indirect(byte_1, memory, ppu, apu);
                self.lda(memory.read_cpu(ppu, apu, addr), 2);
                6
            }
            // LDA ($byte_1), Y (indirect indexed)
            [0xb1, byte_1, _] => {
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory, ppu, apu);
                self.lda(memory.read_cpu(ppu, apu, addr), 2);
                5 + carry as u8
            }
            // LDX #byte_1 (immediate)
            [0xa2, byte_1, _] => {
                self.ldx(byte_1, 2);
                2
            }
            // LDX $byte_1 (zero page)
            [0xa6, byte_1, _] => {
                self.ldx(memory.read_cpu(ppu, apu, byte_1), 2);
                3
            }
            // LDX $byte_1, Y (zero page indexed)
            [0xb6, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.y);
                self.ldx(memory.read_cpu(ppu, apu, addr), 2);
                4
            }
            // LDX $bytes (absolute)
            [0xae, bytes @ ..] => {
                self.ldx(memory.read_cpu(ppu, apu, u16::from_le_bytes(bytes)), 3);
                4
            }
            // LDX $bytes, Y (absolute indexed)
            [0xbe, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.ldx(memory.read_cpu(ppu, apu, addr), 3);
                4 + carry as u8
            }
            // LDY #byte_1 (immediate)
            [0xa0, byte_1, _] => {
                self.ldy(byte_1, 2);
                2
            }
            // LDY $byte_1 (zero page)
            [0xa4, byte_1, _] => {
                self.ldy(memory.read_cpu(ppu, apu, byte_1), 2);
                3
            }
            // LDY $byte_1, X (zero page indexed)
            [0xb4, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                self.ldy(memory.read_cpu(ppu, apu, addr), 2);
                4
            }
            // LDY $bytes (absolute)
            [0xac, bytes @ ..] => {
                self.ldy(memory.read_cpu(ppu, apu, u16::from_le_bytes(bytes)), 3);
                4
            }
            // LDY $bytes, X (absolute indexed)
            [0xbc, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                self.ldy(memory.read_cpu(ppu, apu, addr), 3);
                4 + carry as u8
            }
            // LSR A (accumulator)
            [0x4a, ..] => {
                self.a = self.lsr(self.a, 1);
                2
            }
            // LSR $byte_1 (zero page)
            [0x46, byte_1, _] => {
                memory.write_cpu(
                    ppu,
                    apu,
                    byte_1,
                    self.lsr(memory.read_cpu(ppu, apu, byte_1), 2),
                );
                5
            }
            // LSR $byte_1, X (zero page indexed)
            [0x56, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                memory.write_cpu(ppu, apu, addr, self.lsr(memory.read_cpu(ppu, apu, addr), 2));
                6
            }
            // LSR $bytes (absolute)
            [0x4e, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                memory.write_cpu(ppu, apu, addr, self.lsr(memory.read_cpu(ppu, apu, addr), 3));
                6
            }
            // LSR $bytes, X (absolute indexed)
            [0x5e, bytes @ ..] => {
                let (addr, _) = self.get_absolute_indexed(bytes, self.x);
                memory.write_cpu(ppu, apu, addr, self.lsr(memory.read_cpu(ppu, apu, addr), 3));
                7
            }
            // NOP
            [0xea, ..] => {
                self.pc += 1;
                2
            }
            // ORA #byte_1 (immediate)
            [0x09, byte_1, _] => {
                self.ora(byte_1, 2);
                2
            }
            // ORA $byte_1 (zero page)
            [0x05, byte_1, _] => {
                self.ora(memory.read_cpu(ppu, apu, byte_1), 2);
                3
            }
            // ORA $byte_1, X (zero page indexed)
            [0x15, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                self.ora(memory.read_cpu(ppu, apu, addr), 2);
                4
            }
            // ORA $bytes (absolute)
            [0x0d, bytes @ ..] => {
                self.ora(memory.read_cpu(ppu, apu, u16::from_le_bytes(bytes)), 3);
                4
            }
            // ORA $bytes, X (absolute indexed)
            [0x1d, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                self.ora(memory.read_cpu(ppu, apu, addr), 3);
                4 + carry as u8
            }
            // ORA $bytes, Y (absolute indexed)
            [0x19, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.ora(memory.read_cpu(ppu, apu, addr), 3);
                4 + carry as u8
            }
            // ORA ($bytes, X) (indexed indirect)
            [0x01, byte_1, _] => {
                let addr = self.get_indexed_indirect(byte_1, memory, ppu, apu);
                self.ora(memory.read_cpu(ppu, apu, addr), 2);
                6
            }
            // ORA ($bytes), Y (indirect indexed)
            [0x11, byte_1, _] => {
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory, ppu, apu);
                self.ora(memory.read_cpu(ppu, apu, addr), 2);
                5 + carry as u8
            }
            // PHA
            [0x48, ..] => {
                self.push_val(self.a, memory, ppu, apu);
                3
            }
            // PHP
            [0x08, ..] => {
                // NOTE: the 'b-flag' bit is set when pushing
                // FIXME: may need to set bit 5 when pushing as well? it should be set by default, but
                self.push_val(self.p | 0b10000, memory, ppu, apu);
                3
            }
            // PLA
            [0x68, ..] => {
                self.a = self.pull_val(memory, ppu, apu);
                self.set_z_from_val(self.a);
                self.set_n_from_val(self.a);
                4
            }
            // PLP
            [0x28, ..] => {
                // NOTE: the 'b-flag'(bit 4) is cleared when pulling and bit 5 is set
                self.p = (self.pull_val(memory, ppu, apu) & !0b10000) | 0b100000;
                4
            }
            // ROL A (accumulator)
            [0x2a, ..] => {
                self.a = self.rol(self.a, 1);
                2
            }
            // ROL $byte_1 (zero page)
            [0x26, byte_1, _] => {
                memory.write_cpu(
                    ppu,
                    apu,
                    byte_1,
                    self.rol(memory.read_cpu(ppu, apu, byte_1), 2),
                );
                5
            }
            // ROL $byte_1, X (zero page indexed)
            [0x36, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                memory.write_cpu(ppu, apu, addr, self.rol(memory.read_cpu(ppu, apu, addr), 2));
                6
            }
            // ROL $bytes (absolute)
            [0x2e, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                memory.write_cpu(ppu, apu, addr, self.rol(memory.read_cpu(ppu, apu, addr), 3));
                6
            }
            // ROL $bytes, X (absolute indexed)
            [0x3e, bytes @ ..] => {
                let (addr, _) = self.get_absolute_indexed(bytes, self.x);
                memory.write_cpu(ppu, apu, addr, self.rol(memory.read_cpu(ppu, apu, addr), 3));
                7
            }
            // ROR A (accumulator)
            [0x6a, ..] => {
                self.a = self.ror(self.a, 1);
                2
            }
            // ROR $byte_1 (zero page)
            [0x66, byte_1, _] => {
                memory.write_cpu(
                    ppu,
                    apu,
                    byte_1,
                    self.ror(memory.read_cpu(ppu, apu, byte_1), 2),
                );
                5
            }
            // ROR $byte_1, X (zero page indexed)
            [0x76, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                memory.write_cpu(ppu, apu, addr, self.ror(memory.read_cpu(ppu, apu, addr), 2));
                6
            }
            // ROR $bytes (absolute)
            [0x6e, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                memory.write_cpu(ppu, apu, addr, self.ror(memory.read_cpu(ppu, apu, addr), 3));
                6
            }
            // ROR $bytes, X (absolute indexed)
            [0x7e, bytes @ ..] => {
                let (addr, _) = self.get_absolute_indexed(bytes, self.x);
                memory.write_cpu(ppu, apu, addr, self.ror(memory.read_cpu(ppu, apu, addr), 3));
                7
            }
            // RTI
            [0x40, ..] => {
                self.rti(memory, ppu, apu);
                6
            }
            // RTS
            [0x60, ..] => {
                self.rts(memory, ppu, apu);
                6
            }
            // SBC #byte_1 (immediate)
            [0xe9, byte_1, _] => {
                self.sbc(byte_1, 2);
                2
            }
            // SBC $byte_1 (zero page)
            [0xe5, byte_1, _] => {
                let val = memory.read_cpu(ppu, apu, byte_1);
                self.sbc(val, 2);
                3
            }
            // SBC $byte_1, X (zero page indexed)
            [0xf5, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                self.sbc(memory.read_cpu(ppu, apu, addr), 2);
                4
            }
            // SBC $bytes (absolute)
            [0xed, bytes @ ..] => {
                let val = memory.read_cpu(ppu, apu, u16::from_le_bytes(bytes));
                self.sbc(val, 3);
                4
            }
            // SBC $bytes, X (absolute indexed)
            [0xfd, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                self.sbc(memory.read_cpu(ppu, apu, addr), 3);
                // add 'carry' for one extra cycle if a page boundary was crossed
                4 + carry as u8
            }
            // SBC $bytes, Y (absolute indexed)
            [0xf9, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.sbc(memory.read_cpu(ppu, apu, addr), 3);
                4 + carry as u8
            }
            // SBC ($byte_1, X) (indexed indirect)
            [0xe1, byte_1, _] => {
                let addr = self.get_indexed_indirect(byte_1, memory, ppu, apu);
                self.sbc(memory.read_cpu(ppu, apu, addr), 2);
                6
            }
            // SBC ($byte_1), Y (indirect indexed)
            [0xf1, byte_1, _] => {
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory, ppu, apu);
                self.sbc(memory.read_cpu(ppu, apu, addr), 2);
                5 + carry as u8
            }
            // SEC
            [0x38, ..] => {
                self.pc += 1;
                self.set_c_from_bit(1);
                2
            }
            // SED
            [0xf8, ..] => {
                self.pc += 1;
                self.p |= 8;
                2
            }
            // SEI
            [0x78, ..] => {
                self.pc += 1;
                self.set_i_from_bit(4);
                2
            }
            // STA $byte_1 (zero page)
            [0x85, byte_1, _] => {
                self.pc += 2;
                memory.write_cpu(ppu, apu, byte_1, self.a);
                3
            }
            // STA $byte_1, X (zero page indexed)
            [0x95, byte_1, _] => {
                self.pc += 2;
                memory.write_cpu(ppu, apu, byte_1.wrapping_add(self.x), self.a);
                4
            }
            // STA $bytes (absolute)
            [0x8d, bytes @ ..] => {
                self.pc += 3;
                memory.write_cpu(ppu, apu, u16::from_le_bytes(bytes), self.a);
                4
            }
            // STA $bytes, X (absolute indexed)
            [0x9d, bytes @ ..] => {
                self.pc += 3;
                let (addr, _) = self.get_absolute_indexed(bytes, self.x);
                memory.write_cpu(ppu, apu, addr, self.a);
                5
            }
            // STA $bytes, Y (absolute indexed)
            [0x99, bytes @ ..] => {
                self.pc += 3;
                let (addr, _) = self.get_absolute_indexed(bytes, self.y);
                memory.write_cpu(ppu, apu, addr, self.a);
                5
            }
            // STA ($byte_1, X) (indexed indirect)
            [0x81, byte_1, _] => {
                self.pc += 2;
                let addr = self.get_indexed_indirect(byte_1, memory, ppu, apu);
                memory.write_cpu(ppu, apu, addr, self.a);
                6
            }
            // STA ($byte_1), Y (indirect indexed)
            [0x91, byte_1, _] => {
                self.pc += 2;
                let (addr, _) = self.get_indirect_indexed(byte_1, memory, ppu, apu);
                memory.write_cpu(ppu, apu, addr, self.a);
                6
            }
            // STX $byte_1 (zero page)
            [0x86, byte_1, _] => {
                self.pc += 2;
                memory.write_cpu(ppu, apu, byte_1, self.x);
                3
            }
            // STX $byte_1, Y (zero page indexed)
            [0x96, byte_1, _] => {
                self.pc += 2;
                memory.write_cpu(ppu, apu, byte_1.wrapping_add(self.y), self.x);
                4
            }
            // STX $bytes (absolute)
            [0x8e, bytes @ ..] => {
                self.pc += 3;
                memory.write_cpu(ppu, apu, u16::from_le_bytes(bytes), self.x);
                4
            }
            // STY $byte_1 (zero page)
            [0x84, byte_1, _] => {
                self.pc += 2;
                memory.write_cpu(ppu, apu, byte_1, self.y);
                3
            }
            // STY $byte_1, Y (zero page indexed)
            [0x94, byte_1, _] => {
                self.pc += 2;
                memory.write_cpu(ppu, apu, byte_1.wrapping_add(self.x), self.y);
                4
            }
            // STY $bytes (absolute)
            [0x8c, bytes @ ..] => {
                self.pc += 3;
                memory.write_cpu(ppu, apu, u16::from_le_bytes(bytes), self.y);
                4
            }
            // TAX
            [0xaa, ..] => {
                self.pc += 1;
                self.x = self.a;
                self.set_z_from_val(self.x);
                self.set_n_from_val(self.x);
                2
            }
            // TAY
            [0xa8, ..] => {
                self.pc += 1;
                self.y = self.a;
                self.set_z_from_val(self.y);
                self.set_n_from_val(self.y);
                2
            }
            // TSX
            [0xba, ..] => {
                self.pc += 1;
                self.x = self.sp;
                self.set_z_from_val(self.x);
                self.set_n_from_val(self.x);
                2
            }
            // TXA
            [0x8a, ..] => {
                self.pc += 1;
                self.a = self.x;
                self.set_z_from_val(self.a);
                self.set_n_from_val(self.a);
                2
            }
            // TXS
            [0x9a, ..] => {
                self.pc += 1;
                self.sp = self.x;
                2
            }
            // TYA
            [0x98, ..] => {
                self.pc += 1;
                self.a = self.y;
                self.set_z_from_val(self.a);
                self.set_n_from_val(self.a);
                2
            }
            _ => panic!("TODO: handle invalid opcode"),
        }
    }

    // computes the result address from 'addr_bytes' and 'index'.
    // also returns whether a page boundary was crossed
    fn get_absolute_indexed(&self, addr_bytes: [u8; 2], index: u8) -> (u16, bool) {
        let (addr_low, carry) = addr_bytes[0].overflowing_add(index);
        let addr_hi = addr_bytes[1].wrapping_add(carry as u8);
        let addr_indexed = u16::from_le_bytes([addr_low, addr_hi]);

        (addr_indexed, carry)
    }

    fn get_indexed_indirect(
        &self,
        addr: u8,
        memory: &mmap::Nrom128MemoryMap,
        ppu: &mut ppu::Ppu,
        apu: &mut apu::Apu,
    ) -> u16 {
        let addr_indexed = addr.wrapping_add(self.x);
        let dest_addr = u16::from_le_bytes([
            memory.read_cpu(ppu, apu, addr_indexed),
            memory.read_cpu(ppu, apu, addr_indexed.wrapping_add(1)),
        ]);

        dest_addr
    }

    fn get_indirect_indexed(
        &self,
        addr: u8,
        memory: &mmap::Nrom128MemoryMap,
        ppu: &mut ppu::Ppu,
        apu: &mut apu::Apu,
    ) -> (u16, bool) {
        // get address at memory[addr]
        let dest_addr = [
            memory.read_cpu(ppu, apu, addr),
            memory.read_cpu(ppu, apu, addr.wrapping_add(1)),
        ];

        // add index to address while keeping track of whether a page boundary was crossed
        let (indexed_addr_low, carry) = dest_addr[0].overflowing_add(self.y);
        let (indexed_addr_hi, _debug_carry) = dest_addr[1].overflowing_add(carry as u8);

        if cfg!(feature = "logging") {
            if _debug_carry {
                logln!("went past highest address??")
            }
        }

        let indexed_addr = u16::from_le_bytes([indexed_addr_low, indexed_addr_hi]);

        (indexed_addr, carry)
    }

    #[inline]
    fn set_c_from_bool(&mut self, carry: bool) {
        self.p = (self.p & !1) | carry as u8;
    }

    #[inline]
    fn set_c_from_bit(&mut self, bit: u8) {
        self.p = (self.p & !1) | bit;
    }

    #[inline]
    // sets the overflow flag based on the 'overflow' bool
    fn set_v_from_bool(&mut self, overflow: bool) {
        self.p = (self.p & !0b01000000) | ((overflow as u8) << 6);
    }

    #[inline]
    // ors 'bit' directly with the overflow flag
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

    fn adc(&mut self, val: u8, pc_increment: u8) {
        self.pc += pc_increment as u16;

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

    fn and(&mut self, val: u8, pc_increment: u8) {
        self.pc += pc_increment as u16;

        self.a &= val;
        self.set_n_from_val(self.a);
        self.set_z_from_val(self.a);
    }

    fn asl(&mut self, val: u8, pc_increment: u8) -> u8 {
        self.pc += pc_increment as u16;

        self.set_c_from_bool((val >> 7) != 0);
        let res = val << 1;

        self.set_z_from_val(res);
        self.set_n_from_val(res);

        res
    }

    // used for bcc, bcs, beq, bmi, bne, bpl, bvc, bvs instructions.
    // returns the number of cycles the branch will take to execute
    fn branch_if(&mut self, condition: bool, offset: u8) -> u8 {
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

            3 + boundary_crossed as u8
        } else {
            2
        }
    }

    fn bit(&mut self, val: u8, pc_increment: u8) {
        self.pc += pc_increment as u16;

        self.set_v_from_bit(val & 0b01000000);
        self.set_n_from_val(val);

        let res = val & self.a;
        self.set_z_from_val(res);
    }

    fn brk(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ppu: &mut ppu::Ppu,
        apu: &mut apu::Apu, //
    ) {
        // NOTE: pc + 2 is pushed, despite brk being a one byte instruction
        let pc_bytes = (self.pc + 2).to_le_bytes();

        // push high bits of pc + 2
        memory.write_cpu(ppu, apu, self.sp as u16 + 0x100, pc_bytes[1]);
        self.sp = self.sp.wrapping_sub(1);
        // push low bits of pc + 2
        memory.write_cpu(ppu, apu, self.sp as u16 + 0x100, pc_bytes[0]);
        self.sp = self.sp.wrapping_sub(1);

        // push status flags (with the 'b-flag' set)
        memory.write_cpu(ppu, apu, self.sp as u16 + 0x100, self.p | 0b10000);
        self.sp = self.sp.wrapping_sub(1);

        // set interrupt disable flag
        self.set_i_from_bit(4);

        // set pc to address in brk/irq vector
        let brk_vector = u16::from_le_bytes([
            memory.read_cpu(ppu, apu, 0xfffeu16),
            memory.read_cpu(ppu, apu, 0xffffu16),
        ]);
        self.pc = brk_vector;
    }

    // used for cmp, cpx, cpy instructions
    fn cmp_register_val(&mut self, register: u8, val: u8, pc_increment: u8) {
        self.pc += pc_increment as u16;

        let (sub, underflow) = register.overflowing_sub(val);
        self.set_c_from_bool((sub == 0) | !underflow);
        self.set_z_from_val(sub);
        self.set_n_from_val(sub);
    }

    fn dec(&mut self, val: u8, pc_increment: u8) -> u8 {
        self.pc += pc_increment as u16;

        let res = val.wrapping_sub(1);
        self.set_z_from_val(res);
        self.set_n_from_val(res);

        res
    }

    fn eor(&mut self, val: u8, pc_increment: u8) {
        self.pc += pc_increment as u16;

        self.a ^= val;
        self.set_z_from_val(self.a);
        self.set_n_from_val(self.a);
    }

    fn inc(&mut self, val: u8, pc_increment: u8) -> u8 {
        self.pc += pc_increment as u16;

        let res = val.wrapping_add(1);
        self.set_z_from_val(res);
        self.set_n_from_val(res);

        res
    }

    fn jsr(
        &mut self,
        addr: u16,
        memory: &mut mmap::Nrom128MemoryMap,
        ppu: &mut ppu::Ppu,
        apu: &mut apu::Apu,
    ) {
        // get return address (next instruction - 1)
        let ret_addr = (self.pc + 2).to_le_bytes();
        // push low bits of address to sp - 1
        memory.write_cpu(
            ppu,
            apu,
            self.sp.wrapping_sub(1) as u16 + 0x100,
            ret_addr[0],
        );
        // push high bits of address to sp
        memory.write_cpu(ppu, apu, self.sp as u16 + 0x100, ret_addr[1]);

        self.sp = self.sp.wrapping_sub(2);
        self.pc = addr;
    }

    fn lda(&mut self, val: u8, pc_increment: u8) {
        self.pc += pc_increment as u16;

        self.set_z_from_val(val);
        self.set_n_from_val(val);
        self.a = val;
    }

    fn ldx(&mut self, val: u8, pc_increment: u8) {
        self.pc += pc_increment as u16;

        self.set_z_from_val(val);
        self.set_n_from_val(val);
        self.x = val;
    }

    fn ldy(&mut self, val: u8, pc_increment: u8) {
        self.pc += pc_increment as u16;

        self.set_z_from_val(val);
        self.set_n_from_val(val);
        self.y = val;
    }

    fn lsr(&mut self, val: u8, pc_increment: u8) -> u8 {
        self.pc += pc_increment as u16;

        let res = val >> 1;
        self.set_c_from_bit(val & 1);
        self.set_z_from_val(res);
        self.set_n_from_val(res);

        res
    }

    fn ora(&mut self, val: u8, pc_increment: u8) {
        self.pc += pc_increment as u16;

        self.a |= val;
        self.set_z_from_val(self.a);
        self.set_n_from_val(self.a);
    }

    // used for pha, php instructions
    fn push_val(
        &mut self,
        val: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ppu: &mut ppu::Ppu,
        apu: &mut apu::Apu,
    ) {
        self.pc += 1;

        memory.write_cpu(ppu, apu, self.sp as u16 + 0x100, val);
        self.sp = self.sp.wrapping_sub(1);
    }

    // used for pla, plp instructions
    fn pull_val(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ppu: &mut ppu::Ppu,
        apu: &mut apu::Apu,
    ) -> u8 {
        self.pc += 1;

        self.sp = self.sp.wrapping_add(1);
        memory.read_cpu(ppu, apu, self.sp as u16 + 0x100)
    }

    fn rol(&mut self, val: u8, pc_increment: u8) -> u8 {
        self.pc += pc_increment as u16;

        // shift 'val' left and or carry into bit 0
        let res = (val << 1) | (self.p & 1);
        // set carry to bit 7 of 'val'
        self.set_c_from_bit(val >> 7);
        self.set_z_from_val(res);
        self.set_n_from_val(res);

        res
    }

    fn ror(&mut self, val: u8, pc_increment: u16) -> u8 {
        self.pc += pc_increment as u16;

        // shift 'val' right and or carry into bit 7
        let res = (val >> 1) | (self.p << 7);
        // set carry to bit 0 of 'val'
        self.set_c_from_bit(val & 1);
        self.set_z_from_val(res);
        self.set_n_from_val(res);

        res
    }

    fn rti(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ppu: &mut ppu::Ppu,
        apu: &mut apu::Apu, //
    ) {
        // pull into status flags
        self.sp = self.sp.wrapping_add(1);
        // NOTE: bit 4 is cleared and bit 5 is set when pulling into status register
        self.p = (memory.read_cpu(ppu, apu, self.sp as u16 + 0x100) & !0b10000) | 0b100000;

        self.sp = self.sp.wrapping_add(1);
        // get program counter low bits
        let pc_low = memory.read_cpu(ppu, apu, self.sp as u16 + 0x100);
        self.sp = self.sp.wrapping_add(1);
        // get program counter high bits
        let pc_hi = memory.read_cpu(ppu, apu, self.sp as u16 + 0x100);

        self.pc = u16::from_le_bytes([pc_low, pc_hi]);
    }

    fn rts(
        &mut self,
        memory: &mut mmap::Nrom128MemoryMap,
        ppu: &mut ppu::Ppu,
        apu: &mut apu::Apu, //
    ) {
        // pull into program counter
        self.sp = self.sp.wrapping_add(1);
        let pc_low = memory.read_cpu(ppu, apu, self.sp as u16 + 0x100);
        self.sp = self.sp.wrapping_add(1);
        let pc_hi = memory.read_cpu(ppu, apu, self.sp as u16 + 0x100);
        // add 1 since pushed value is expected to be pc - 1 (from the jsr instruction)
        // FIXME: should discard carry from low 8 bits when adding?
        self.pc = u16::from_le_bytes([pc_low, pc_hi]).wrapping_add(1);
    }

    fn sbc(&mut self, val: u8, pc_increment: u8) {
        self.pc += pc_increment as u16;

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

    pub fn debug_exec_opcode(
        &mut self,
        opc: [u8; 3],
        memory: &mut mmap::Nrom128MemoryMap,
        ppu: &mut ppu::Ppu,
        apu: &mut apu::Apu,
    ) -> u8 {
        memory.write_cpu(ppu, apu, self.pc, opc[0]);
        memory.write_cpu(ppu, apu, self.pc + 1, opc[1]);
        memory.write_cpu(ppu, apu, self.pc + 2, opc[2]);

        self.exec_instruction(memory, ppu, apu)
    }

    pub fn log_register_values(&self) {
        log!("A:{:0>2X} ", self.a);
        log!("X:{:0>2X} ", self.x);
        log!("Y:{:0>2X} ", self.y);
        log!("P:{:0>2X} ", self.p);
        log!("SP:{:0>2X}\n", self.sp);
    }
}
