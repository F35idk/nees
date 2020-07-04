mod parse;

use parse::RomInfo;

struct MemoryMap {
    // TODO: reduce size, use custom address calculation
    memory: [u8; 0x10000],
}

impl MemoryMap {
    fn new() -> Self {
        MemoryMap {
            memory: [0; 0x10000],
        }
    }

    #[inline]
    fn get_mut(&mut self, index: u16) -> &mut u8 {
        unsafe { self.memory.get_mut(index as usize).unwrap() }
    }

    #[inline]
    fn get_mut_u8(&mut self, index: u8) -> &mut u8 {
        unsafe { self.memory.get_unchecked_mut(index as usize) }
    }

    #[inline]
    fn get(&self, index: u16) -> u8 {
        unsafe { *self.memory.get_unchecked(index as usize) }
    }

    #[inline]
    fn get_u8(&self, index: u8) -> u8 {
        unsafe { *self.memory.get_unchecked(index as usize) }
    }

    fn test_calc_addr(&self, mut addr: u16) {
        let is_lt_2000 = addr < 0x2000;
        // mask off high bits if address is < 0x2000 (i.e a mirror of 0-0x7ff)
        addr &= !(0b1100000000000 * is_lt_2000 as u16);

        // set to true if addr is between 2000 and 3fff (nes ppu registers)
        let is_ppu = is_lt_2000 ^ (addr < 0x4000);
        // TODO: apply this to addr instead and place ppu registers in 'memory'
        let ppu_register_index = addr & (0b111 | !is_ppu as u16 * 0xffff);

        println!("ppu register index = {:x}", ppu_register_index);
        println!("final addr: {:x}", addr);
    }
}

#[derive(Default)]
struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    p: u8,
    sp: u8,
    pc: u16,
    cycle_count: u32,
}

impl Cpu {
    fn new_nestest() -> Self {
        Self {
            pc: 0xc000,
            ..Default::default()
        }
    }

    fn exec_instruction(&mut self, memory: &mut MemoryMap) -> u8 {
        // FIXME: handle out of bounds????
        // FIXME: need to subtract from clock cycles if last cycle wasn't a write?? (in these
        // cases, the 6502 will fetch the next instruction while the current one is executing)
        match [
            memory.get(self.pc),
            memory.get(self.pc + 1),
            memory.get(self.pc + 2),
        ] {
            // ADC #byte_1 (immediate)
            [0x69, byte_1, _] => {
                self.adc(byte_1, 2);
                2
            }
            // ADC $byte_1 (zero page)
            [0x65, byte_1, _] => {
                let val = memory.get_u8(byte_1);
                self.adc(val, 2);
                3
            }
            // ADC $byte_1, X (zero page indexed)
            [0x75, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                self.adc(memory.get_u8(addr), 2);
                4
            }
            // ADC $bytes (absolute)
            [0x6d, bytes @ ..] => {
                let val = memory.get(u16::from_le_bytes(bytes));
                self.adc(val, 3);
                4
            }
            // ADC $bytes, X (absolute indexed)
            [0x7d, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                self.adc(memory.get(addr), 3);
                // add 'carry' for one extra cycle if a page boundary was crossed
                4 + carry as u8
            }
            // ADC $bytes, Y (absolute indexed)
            [0x79, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.adc(memory.get(addr), 3);
                4 + carry as u8
            }
            // ADC ($byte_1, X) (indexed indirect)
            [0x61, byte_1, _] => {
                let addr = self.get_indexed_indirect(byte_1, memory);
                self.adc(memory.get(addr), 2);
                6
            }
            // ADC ($byte_1), Y (indirect indexed)
            [0x71, byte_1, _] => {
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory);
                self.adc(memory.get(addr), 2);
                5 + carry as u8
            }
            // AND #byte_1 (immediate)
            [0x29, byte_1, _] => {
                self.and(byte_1, 2);
                2
            }
            // AND $byte_1 (zero page)
            [0x25, byte_1, _] => {
                let val = memory.get_u8(byte_1);
                self.and(val, 2);
                3
            }
            // AND $byte_1, X (zero page indexed)
            [0x35, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                self.and(memory.get_u8(addr), 2);
                4
            }
            // AND $bytes (absolute)
            [0x2d, bytes @ ..] => {
                self.pc += 3;
                let val = memory.get(u16::from_le_bytes(bytes));
                self.and(val, 3);
                4
            }
            // AND $bytes, X (absolute indexed)
            [0x3d, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                self.and(memory.get(addr), 3);
                4 + carry as u8
            }
            // AND $bytes, Y (absolute indexed)
            [0x39, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.and(memory.get(addr), 3);
                4 + carry as u8
            }
            // AND ($byte_1, X) (indexed indirect)
            [0x21, byte_1, _] => {
                let addr = self.get_indexed_indirect(byte_1, memory);
                self.and(memory.get(addr), 2);
                6
            }
            // AND ($byte_1), Y (indirect indexed)
            [0x31, byte_1, _] => {
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory);
                self.and(memory.get(addr), 2);
                5 + carry as u8
            }
            // ASL A (accumulator)
            [0x0a, ..] => {
                self.a = self.asl(self.a, 1);
                2
            }
            // ASL $byte_1 (zero page)
            [0x06, byte_1, _] => {
                *memory.get_mut_u8(byte_1) = self.asl(memory.get_u8(byte_1), 2);
                5
            }
            // ASL $byte_1, X (zero page indexed)
            [0x16, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                *memory.get_mut_u8(addr) = self.asl(memory.get_u8(byte_1), 2);
                6
            }
            // ASL $bytes (absolute)
            [0x0e, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                *memory.get_mut(addr) = self.asl(memory.get(addr), 3);
                6
            }
            // ASL $bytes, X (absolute indexed)
            [0x1e, bytes @ ..] => {
                let (addr, _) = self.get_absolute_indexed(bytes, self.x);
                *memory.get_mut(addr) = self.asl(memory.get(addr), 3);
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
                self.bit(memory.get_u8(byte_1), 2);
                3
            }
            // BIT $bytes (absolute)
            [0x2c, bytes @ ..] => {
                self.bit(memory.get(u16::from_le_bytes(bytes)), 3);
                4
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
                let val = memory.get_u8(byte_1);
                self.cmp_register_val(self.a, val, 2);
                3
            }
            // CMP $byte_1, X (zero page indexed)
            [0xd5, byte_1, _] => {
                let val = memory.get_u8(byte_1.wrapping_add(self.x));
                self.cmp_register_val(self.a, val, 2);
                4
            }
            // CMP $bytes (absolute)
            [0xcd, bytes @ ..] => {
                let val = memory.get(u16::from_le_bytes(bytes));
                self.cmp_register_val(self.a, val, 3);
                4
            }
            // CMP $bytes, X (absolute indexed)
            [0xdd, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                self.cmp_register_val(self.a, memory.get(addr), 3);
                4 + carry as u8
            }
            // CMP $bytes, Y (absolute indexed)
            [0xd9, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.cmp_register_val(self.a, memory.get(addr), 3);
                4 + carry as u8
            }
            // CMP ($byte_1, X) (indexed indirect)
            [0xc1, byte_1, _] => {
                let addr = self.get_indexed_indirect(byte_1, memory);
                self.cmp_register_val(self.a, memory.get(addr), 2);
                6
            }
            // CMP ($byte_1), Y (indirect indexed)
            [0xd1, byte_1, _] => {
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory);
                self.cmp_register_val(self.a, memory.get(addr), 2);
                5 + carry as u8
            }
            // CPX #byte_1 (immediate)
            [0xe0, byte_1, _] => {
                self.cmp_register_val(self.x, byte_1, 2);
                2
            }
            // CPX $byte_1 (zero page)
            [0xe4, byte_1, _] => {
                self.cmp_register_val(self.x, memory.get_u8(byte_1), 2);
                3
            }
            // CPX $bytes (absolute)
            [0xec, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                self.cmp_register_val(self.x, memory.get(addr), 3);
                4
            }
            // CPY #byte_1 (immediate)
            [0xc0, byte_1, _] => {
                self.cmp_register_val(self.y, byte_1, 2);
                2
            }
            // CPY $byte_1 (zero page)
            [0xc4, byte_1, _] => {
                self.cmp_register_val(self.y, memory.get_u8(byte_1), 2);
                3
            }
            // CPY $bytes (absolute)
            [0xcc, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                self.cmp_register_val(self.y, memory.get(addr), 3);
                4
            }
            // DEC $byte_1 (zero page)
            [0xc6, byte_1, _] => {
                *memory.get_mut_u8(byte_1) = self.dec(memory.get_u8(byte_1), 2);
                5
            }
            // DEC $byte_1, X (zero page indexed)
            [0xd6, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                *memory.get_mut_u8(addr) = self.dec(memory.get_u8(addr), 2);
                6
            }
            // DEC $bytes (absolute)
            [0xce, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                *memory.get_mut(addr) = self.dec(memory.get(addr), 3);
                6
            }
            // DEC $bytes, X (absolute indexed)
            [0xde, bytes @ ..] => {
                let (addr, _) = self.get_absolute_indexed(bytes, self.x);
                *memory.get_mut(addr) = self.dec(memory.get(addr), 3);
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
                self.eor(memory.get_u8(byte_1), 2);
                3
            }
            // EOR $byte_1, X (zero page indexed)
            [0x55, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                self.eor(memory.get_u8(addr), 2);
                4
            }
            // EOR $bytes (absolute)
            [0x4d, bytes @ ..] => {
                self.eor(memory.get(u16::from_le_bytes(bytes)), 3);
                4
            }
            // EOR $bytes, X (absolute indexed)
            [0x5d, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                self.eor(memory.get(addr), 3);
                4 + carry as u8
            }
            // EOR $bytes, Y (absolute indexed)
            [0x59, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.eor(memory.get(addr), 3);
                4 + carry as u8
            }
            // EOR ($bytes, X) (indexed indirect)
            [0x41, byte_1, _] => {
                let addr = self.get_indexed_indirect(byte_1, memory);
                self.eor(memory.get(addr), 2);
                6
            }
            // EOR ($bytes), Y (indirect indexed)
            [0x51, byte_1, _] => {
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory);
                self.eor(memory.get(addr), 2);
                5 + carry as u8
            }
            // INC $byte_1 (zero page)
            [0xe6, byte_1, _] => {
                *memory.get_mut_u8(byte_1) = self.inc(memory.get_u8(byte_1), 2);
                5
            }
            // INC $byte_1, X (zero page indexed)
            [0xf6, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                *memory.get_mut_u8(addr) = self.inc(memory.get_u8(addr), 2);
                6
            }
            // INC $bytes (absolute)
            [0xee, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                *memory.get_mut(addr) = self.inc(memory.get(addr), 3);
                6
            }
            // INC $bytes, X (absolute indexed)
            [0xfe, bytes @ ..] => {
                let (addr, _) = self.get_absolute_indexed(bytes, self.x);
                *memory.get_mut(addr) = self.inc(memory.get(addr), 3);
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
                let addr_lo = memory.get(u16::from_le_bytes(bytes));
                // add 1 to low bits without carry to get address of high bits of final address
                bytes[0] = bytes[0].wrapping_add(1);
                let addr_hi = memory.get(u16::from_le_bytes(bytes));
                self.pc = u16::from_le_bytes([addr_lo, addr_hi]);
                5
            }
            [0x20, bytes @ ..] => {
                self.jsr(u16::from_le_bytes(bytes), memory);
                6
            }
            _ => panic!("TODO: handle invalid opcode"),
        }
    }

    // computes the result address from 'addr_bytes' and 'index'.
    // also returns whether a page boundary was crossed
    fn get_absolute_indexed(&self, addr_bytes: [u8; 2], index: u8) -> (u16, bool) {
        let (addr_low, carry) = addr_bytes[0].overflowing_add(index);
        let addr_hi = addr_bytes[1] + carry as u8;
        let addr_indexed = u16::from_le_bytes([addr_low, addr_hi]);

        (addr_indexed, carry)
    }

    fn get_indexed_indirect(&self, addr: u8, memory: &MemoryMap) -> u16 {
        let addr_indexed = addr.wrapping_add(self.x);
        let dest_addr =
            u16::from_le_bytes([memory.get_u8(addr_indexed), memory.get_u8(addr_indexed + 1)]);

        dest_addr
    }

    fn get_indirect_indexed(&self, addr: u8, memory: &MemoryMap) -> (u16, bool) {
        // get address at memory[addr]
        let dest_addr = [memory.get_u8(addr), memory.get_u8(addr + 1)];

        // add index to address while keeping track of whether a page boundary was crossed
        let (indexed_addr_low, carry) = dest_addr[0].overflowing_add(self.y);
        let indexed_addr_hi = dest_addr[1] + carry as u8;
        let indexed_addr = u16::from_le_bytes([indexed_addr_low, indexed_addr_hi]);

        (indexed_addr, carry)
    }

    #[inline]
    fn set_c_from_bool(&mut self, carry: bool) {
        self.p = (self.p & !1) | carry as u8;
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

        // add the carry from the status flag to 'val' first
        let (val, mut carry) = val.overflowing_add(self.p & 1);
        let (_, mut overflow) = (val as i8).overflowing_add((self.p & 1) as i8);

        // then add 'val' to the accumulator
        let (res, carry_2) = val.overflowing_add(self.a);
        let (_, overflow_2) = (val as i8).overflowing_add(self.a as i8);

        overflow |= overflow_2;
        carry |= carry_2;

        self.a = res;
        self.set_c_from_bool(carry);
        self.set_v_from_bool(overflow);
        self.set_z_from_val(res);
        self.set_n_from_val(res);
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

    fn brk(&mut self) -> u8 {
        // TODO: implement
        0
    }

    // used for cmp, cpx, cpy instructions
    fn cmp_register_val(&mut self, register: u8, val: u8, pc_increment: u8) {
        self.pc += pc_increment as u16;

        let eq = val == register;
        let lt = val < register;

        self.set_c_from_bool(lt | eq);
        self.set_z_from_bool(eq);
        // NOTE: may want to set from bit by subtracting etc.
        self.set_n_from_bool(!(lt | eq));
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

    fn jsr(&mut self, addr: u16, memory: &mut MemoryMap) {
        // get return address (next instruction - 1)
        let ret_addr = (self.pc + 2).to_le_bytes();
        // push low bytes of address to sp - 1
        *memory.get_mut(self.sp.wrapping_sub(1) as u16 + 0x100) = ret_addr[0];
        // push high bytes of address to sp
        *memory.get_mut(self.sp as u16 + 0x100) = ret_addr[1];

        self.sp = self.sp.wrapping_sub(2);
        self.pc = addr;
    }

    fn debug_exec_opcode(&mut self, opc: [u8; 3], memory: &mut MemoryMap) -> u8 {
        if cfg!(debug_assertions) {
            *memory.get_mut(self.pc) = opc[0];
            *memory.get_mut(self.pc + 1) = opc[1];
            *memory.get_mut(self.pc + 2) = opc[2];

            self.exec_instruction(memory)
        } else {
            0
        }
    }

    fn debug_print_registers(&self) {
        if cfg!(debug_assertions) {
            println!("A: {:x}", self.a);
            println!("X: {:x}", self.x);
            println!("Y: {:x}", self.y);
            println!("P: {:x}", self.p);
            println!("SP: {:x}", self.sp);
            println!("PC: {:x}", self.pc);
        }
    }
}

fn test_adc() {
    let mut cpu = Cpu::default();

    cpu.p = 0x6e;
    cpu.adc(0x69, 2);

    assert_eq!(cpu.a, 0x69);
    assert_eq!(cpu.p, 0x2c);

    cpu.a = 1;
    cpu.p = 0x6d;
    cpu.adc(0x69, 2);

    assert_eq!(cpu.a, 0x6b);
    assert_eq!(cpu.p, 0x2c);

    cpu.a = 0x7f;
    cpu.p = 0x25;
    cpu.adc(0x80, 2);

    assert_eq!(cpu.a, 0);
    assert_eq!(cpu.p, 0x27);

    let mut memory = MemoryMap::new();
    *memory.get_mut(0x80) = 00;
    *memory.get_mut(0x81) = 02;
    *memory.get_mut(0x200) = 0x69;
    cpu.a = 0;
    cpu.p = 0x66;
    // ADC ($80, X) (indexed indirect)
    let cyc = cpu.debug_exec_opcode([0x61, 0x80, 0x00], &mut memory);

    assert_eq!(cyc, 6);
    assert_eq!(cpu.a, 0x69);
    assert_eq!(cpu.p, 0x24);

    *memory.get_mut(0x80) = 0;
    *memory.get_mut(0x81) = 0;
    *memory.get_mut(0x200) = 0;
    *memory.get_mut(0x78) = 0x69;
    cpu.a = 0;
    cpu.p = 0x66;
    // ADC $78 (zero page)
    let cyc = cpu.debug_exec_opcode([0x65, 0x78, 0x00], &mut memory);

    assert_eq!(cyc, 3);
    assert_eq!(cpu.a, 0x69);
    assert_eq!(cpu.p, 0x24);

    *memory.get_mut(0x78) = 0;
    *memory.get_mut(0x678) = 0x69;
    cpu.a = 0;
    cpu.p = 0x66;
    let cyc = cpu.debug_exec_opcode([0x6d, 0x78, 0x06], &mut memory);

    assert_eq!(cyc, 4);
    assert_eq!(cpu.a, 0x69);
    assert_eq!(cpu.p, 0x24);
}

fn test_and() {
    let mut cpu = Cpu::default();

    cpu.a = 0x55;
    cpu.p = 0;
    let _ = cpu.and(0xaa, 2);

    // assert_eq!(cyc, 2);
    assert_eq!(cpu.a, 0);
    assert_eq!(cpu.p, 2); // zero-flag should be set

    let mut memory = MemoryMap::new();
    *memory.get_mut(0x80) = 00;
    *memory.get_mut(0x81) = 02;
    *memory.get_mut(0x200) = 0xaa;
    cpu.a = 0x55;
    cpu.p = 0;
    // AND ($80, X)
    let cyc = cpu.debug_exec_opcode([0x21, 0x80, 00], &mut memory);

    assert_eq!(cyc, 6);
    assert_eq!(cpu.a, 0);
    assert_eq!(cpu.p, 2); // zero-flag should be set
}

fn test_asl() {
    let mut cpu = Cpu::default();
    let mut memory = MemoryMap::new();

    cpu.a = 0x80;
    cpu.p = 0xe5;
    // ASL A
    let cyc = cpu.debug_exec_opcode([0x0a, 00, 00], &mut memory);

    assert_eq!(cyc, 2);
    assert_eq!(cpu.a, 0);
    assert_eq!(cpu.p, 0x67);

    cpu.a = 0;
    cpu.p = 0xe5;
    *memory.get_mut(0x78) = 0x80;
    // ASL $78
    let cyc = cpu.debug_exec_opcode([0x06, 0x78, 00], &mut memory);

    assert_eq!(cyc, 5);
    assert_eq!(memory.get(0x78), 0);
    assert_eq!(cpu.p, 0x67);

    cpu.p = 0xa5;
    *memory.get_mut(0x78) = 0;
    *memory.get_mut(0x678) = 0x55;
    // ASL $0678
    let cyc = cpu.debug_exec_opcode([0x0e, 0x78, 0x06], &mut memory);

    assert_eq!(cyc, 6);
    assert_eq!(memory.get(0x678), 0xaa);
    assert_eq!(cpu.p, 0xa4);
}

fn test_branch_instrs() {
    let mut cpu = Cpu::default();
    let mut memory = MemoryMap::new();
    cpu.p = 0;
    cpu.pc = 0x100;
    let cyc = cpu.debug_exec_opcode([0x90, 0x80, 00], &mut memory);

    assert_eq!(cpu.pc, 0x100 - 0x80 + 2);
    assert_eq!(cyc, 4);

    cpu.pc = 0x100;
    cpu.p = 0;
    let cyc = cpu.debug_exec_opcode([0x90, 0x7f, 00], &mut memory);

    assert_eq!(cpu.pc, 0x100 + 0x7f + 2);
    assert_eq!(cyc, 3);

    cpu.pc = 0x100;
    cpu.p = 0b01000000;
    let cyc = cpu.debug_exec_opcode([0x50, 0xff, 00], &mut memory);

    assert_eq!(cpu.pc, 0x100 + 2);
    assert_eq!(cyc, 2);

    cpu.pc = 0x100;
    cpu.p = 0b01000000;
    let cyc = cpu.debug_exec_opcode([0x70, 0xff, 00], &mut memory);

    assert_eq!(cpu.pc, 0x100 - 1 + 2);
    assert_eq!(cyc, 3);
}

fn test_bit() {
    let mut cpu = Cpu::default();
    let mut memory = MemoryMap::new();

    cpu.p = 0xa4;
    cpu.a = 0xff;
    cpu.pc = 0x40;
    *memory.get_mut(1) = 0xff;
    // BIT $01
    let cyc = cpu.debug_exec_opcode([0x24, 0x01, 00], &mut memory);

    assert_eq!(cyc, 3);
    assert_eq!(cpu.p, 0xe4);
}

fn test_cmp() {
    let mut cpu = Cpu::default();
    cpu.a = 0x40;
    cpu.p = 0x25;
    let _ = cpu.cmp_register_val(cpu.a, 0x41, 2);

    // assert_eq!(cyc, 2);
    assert_eq!(cpu.p, 0xa4);
}

fn test_dec_inc() {
    // TODO: ..
}

fn test_eor() {
    // TODO: ..
}

fn test_jmp() {
    let mut cpu = Cpu::new_nestest();
    let mut memory = MemoryMap::new();

    cpu.debug_exec_opcode([0x4c, 0xf5, 0xc5], &mut memory);

    assert_eq!(cpu.pc, 0xc5f5);

    *memory.get_mut(0x2ff) = 0x00;
    *memory.get_mut(0x200) = 0x03;
    // JMP ($02ff)
    cpu.debug_exec_opcode([0x6c, 0xff, 0x02], &mut memory);

    assert_eq!(cpu.pc, 0x300);
}

fn test_jsr() {
    let mut cpu = Cpu::default();
    let mut memory = MemoryMap::new();

    cpu.pc = 0x300;
    cpu.sp = 0xff;

    let cyc = cpu.debug_exec_opcode([0x20, 00, 00], &mut memory);

    assert_eq!(cyc, 6);
    assert_eq!(memory.get(cpu.sp.wrapping_add(1) as u16 + 0x100), 0x02);
    assert_eq!(memory.get(cpu.sp.wrapping_add(2) as u16 + 0x100), 0x03);

    cpu.pc = 0x300;
    cpu.sp = 0x00;

    // sp = 0x00, so the jsr will cause it to underflow through zero
    cpu.debug_exec_opcode([0x20, 00, 00], &mut memory);

    assert_eq!(memory.get(cpu.sp.wrapping_add(1) as u16 + 0x100), 0x02);
    assert_eq!(memory.get(cpu.sp.wrapping_add(2) as u16 + 0x100), 0x03);

    cpu.pc = 0xc5fd;
    cpu.sp = 0xfd;
    cpu.debug_exec_opcode([0x20, 0x2d, 0xc7], &mut memory);

    assert_eq!(cpu.sp, 0xfb);
    assert_eq!(cpu.pc, 0xc72d);
    assert_eq!(memory.get(cpu.sp.wrapping_add(1) as u16 + 0x100), 0xfd + 2);
    assert_eq!(memory.get(cpu.sp.wrapping_add(2) as u16 + 0x100), 0xc5);
}

fn main() {
    let rom = std::fs::read("nestest.nes").unwrap();
    println!("{}", std::str::from_utf8(&rom[0..=3]).unwrap());

    let info = RomInfo::new(&rom).unwrap();
    println!("is nes 2.0: {}", info.is_nes_2_format());
    println!("has trainer: {}", info.has_trainer());
    println!("mirroring type: {:?}", info.get_mirroring_type());
    println!("mapper number: {}", info.get_mapper_num());
    println!("prg rom size: {}KB", info.get_prg_size() as u32 * 16);
    println!("chr rom size: {}KB", info.get_chr_size() as u32 * 8);

    test_adc();
    test_and();
    test_asl();
    test_branch_instrs();
    test_bit();
    test_cmp();
    test_jmp();
    test_jsr();

    let memory = MemoryMap::new();
    memory.test_calc_addr(0x800);
    memory.test_calc_addr(0xfff);
    memory.test_calc_addr(0x80f);
    memory.test_calc_addr(0xa0e);
    memory.test_calc_addr(0x1000);
    memory.test_calc_addr(0x18f0);
    memory.test_calc_addr(0x48f0);
    memory.test_calc_addr(0x3fff);
    memory.test_calc_addr(0x2001);
}
