mod parse;

use parse::RomInfo;

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

    fn exec_instruction(&mut self, memory: &mut [u8]) -> u8 {
        // FIXME: handle out of bounds????
        // FIXME: need to subtract from clock cycles if last cycle wasn't a write?? (in these
        // cases, the 6502 will fetch the next instruction while the current one is executing)
        // TODO: use get_unchecked() where the index is
        // guaranteed to be smaller than the array size
        match [
            memory[self.pc as usize],
            memory[(self.pc + 1) as usize],
            memory[(self.pc + 2) as usize],
        ] {
            // ADC #byte_1 (immediate)
            [0x69, byte_1, _] => {
                self.adc(byte_1, 2);
                2
            }
            // ADC $byte_1 (zero page)
            [0x65, byte_1, _] => {
                let val = memory[byte_1 as usize];
                self.adc(val, 2);
                3
            }
            // ADC $byte_1, X (zero page indexed)
            [0x75, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                self.adc(memory[addr as usize], 2);
                4
            }
            // ADC $bytes (absolute)
            [0x6d, bytes @ ..] => {
                let val = memory[u16::from_le_bytes(bytes) as usize];
                self.adc(val, 3);
                4
            }
            // ADC $bytes, X (absolute indexed)
            [0x7d, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                // add 'carry' to return value for one extra cycle if a page boundary was crossed
                self.adc(memory[addr as usize], 3);
                4 + carry as u8
            }
            // ADC $bytes, Y (absolute indexed)
            [0x79, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.adc(memory[addr as usize], 3);
                4 + carry as u8
            }
            // ADC ($byte_1, X) (indexed indirect)
            [0x61, byte_1, _] => {
                let addr = self.get_indexed_indirect(byte_1, memory);
                self.adc(memory[addr as usize], 2);
                6
            }
            // ADC ($byte_1), Y (indirect indexed)
            [0x71, byte_1, _] => {
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory);
                self.adc(memory[addr as usize], 2);
                5 + carry as u8
            }
            // AND #byte_1 (immediate)
            [0x29, byte_1, _] => {
                self.and(byte_1, 2);
                2
            }
            // AND $byte_1 (zero page)
            [0x25, byte_1, _] => {
                let val = memory[byte_1 as usize];
                self.and(val, 2);
                3
            }
            // AND $byte_1, X (zero page indexed)
            [0x35, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                self.and(memory[addr as usize], 2);
                4
            }
            // AND $bytes (absolute)
            [0x2d, bytes @ ..] => {
                self.pc += 3;
                let val = memory[u16::from_le_bytes(bytes) as usize];
                self.and(val, 3);
                4
            }
            // AND $bytes, X (absolute indexed)
            [0x3d, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                self.and(memory[addr as usize], 3);
                4 + carry as u8
            }
            // AND $bytes, Y (absolute indexed)
            [0x39, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.and(memory[addr as usize], 3);
                4 + carry as u8
            }
            // AND ($byte_1, X) (indexed indirect)
            [0x21, byte_1, _] => {
                let addr = self.get_indexed_indirect(byte_1, memory);
                self.and(memory[addr as usize], 2);
                6
            }
            // AND ($byte_1), Y (indirect indexed)
            [0x31, byte_1, _] => {
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory);
                self.and(memory[addr as usize], 2);
                5 + carry as u8
            }
            // ASL A (accumulator)
            [0x0a, ..] => {
                self.a = self.asl(self.a, 1);
                2
            }
            // ASL $byte_1 (zero page)
            [0x06, byte_1, _] => {
                memory[byte_1 as usize] = self.asl(memory[byte_1 as usize], 2);
                5
            }
            // ASL $byte_1, X (zero page indexed)
            [0x16, byte_1, _] => {
                let addr = byte_1.wrapping_add(self.x);
                memory[addr as usize] = self.asl(memory[byte_1 as usize], 2);
                6
            }
            // ASL $bytes (absolute)
            [0x0e, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                memory[addr as usize] = self.asl(memory[addr as usize], 3);
                6
            }
            // ASL $bytes, X (absolute indexed)
            [0x1e, bytes @ ..] => {
                let (addr, _) = self.get_absolute_indexed(bytes, self.x);
                memory[addr as usize] = self.asl(memory[addr as usize], 3);
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
                self.bit(memory[byte_1 as usize], 2);
                3
            }
            // BIT $bytes (absolute)
            [0x2c, bytes @ ..] => {
                self.bit(memory[u16::from_le_bytes(bytes) as usize], 3);
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
                let val = memory[byte_1 as usize];
                self.cmp_register_val(self.a, val, 2);
                3
            }
            // CMP $byte_1, X (zero page indexed)
            [0xd5, byte_1, _] => {
                let val = memory[byte_1.wrapping_add(self.x) as usize];
                self.cmp_register_val(self.a, val, 2);
                4
            }
            // CMP $bytes (absolute)
            [0xcd, bytes @ ..] => {
                let val = memory[u16::from_le_bytes(bytes) as usize];
                self.cmp_register_val(self.a, val, 3);
                4
            }
            // CMP $bytes, X (absolute indexed)
            [0xdd, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                self.cmp_register_val(self.a, memory[addr as usize], 3);
                4 + carry as u8
            }
            // CMP $bytes, Y (absolute indexed)
            [0xd9, bytes @ ..] => {
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.cmp_register_val(self.a, memory[addr as usize], 3);
                4 + carry as u8
            }
            // CMP ($byte_1, X) (indexed indirect)
            [0xc1, byte_1, _] => {
                let addr = self.get_indexed_indirect(byte_1, memory);
                self.cmp_register_val(self.a, memory[addr as usize], 2);
                6
            }
            // CMP ($byte_1), Y (indirect indexed)
            [0xd1, byte_1, _] => {
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory);
                self.cmp_register_val(self.a, memory[addr as usize], 2);
                5 + carry as u8
            }
            // CPX #byte_1 (immediate)
            [0xe0, byte_1, _] => {
                self.cmp_register_val(self.x, byte_1, 2);
                2
            }
            // CPX $byte_1 (zero page)
            [0xe4, byte_1, _] => {
                self.cmp_register_val(self.x, memory[byte_1 as usize], 2);
                3
            }
            // CPX $bytes (absolute)
            [0xec, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                self.cmp_register_val(self.x, memory[addr as usize], 3);
                4
            }
            // CPY #byte_1 (immediate)
            [0xc0, byte_1, _] => {
                self.cmp_register_val(self.y, byte_1, 2);
                2
            }
            // CPY $byte_1 (zero page)
            [0xc4, byte_1, _] => {
                self.cmp_register_val(self.y, memory[byte_1 as usize], 2);
                3
            }
            // CPY $bytes (absolute)
            [0xcc, bytes @ ..] => {
                let addr = u16::from_le_bytes(bytes);
                self.cmp_register_val(self.y, memory[addr as usize], 3);
                4
            }
            _ => panic!("TODO: handle invalid opcode"),
        }
    }

    fn get_absolute_indexed(&self, addr_bytes: [u8; 2], index: u8) -> (u16, bool) {
        let (addr_low, carry) = addr_bytes[0].overflowing_add(index);
        let addr_hi = addr_bytes[1] + carry as u8;
        let addr_indexed = u16::from_le_bytes([addr_low, addr_hi]);

        (addr_indexed, carry)
    }

    fn get_indexed_indirect(&self, addr: u8, memory: &[u8]) -> u16 {
        let addr_indexed = addr.wrapping_add(self.x);
        let dest_addr = u16::from_le_bytes([
            memory[addr_indexed as usize],
            memory[(addr_indexed + 1) as usize],
        ]);

        dest_addr
    }

    fn get_indirect_indexed(&self, addr: u8, memory: &[u8]) -> (u16, bool) {
        // get address at memory[addr]
        let dest_addr = [memory[addr as usize], memory[(addr + 1) as usize]];

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

    // NOTE: returns the result of the asl op
    fn asl(&mut self, val: u8, pc_increment: u8) -> u8 {
        self.pc += pc_increment as u16;

        self.set_c_from_bool((val >> 7) != 0);
        let res = val << 1;

        self.set_z_from_val(res);
        self.set_n_from_val(res);

        res
    }

    // used for bcc, bcs, beq, bmi, bne, bpl, bvc, bvs instructions.
    // returns the amount of cycles the branch will take to execute
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

    fn debug_exec_opcode(&mut self, opc: [u8; 3], memory: &mut Vec<u8>) -> u8 {
        if cfg!(debug_assertions) {
            memory[self.pc as usize] = opc[0];
            memory[self.pc as usize + 1] = opc[1];
            memory[self.pc as usize + 2] = opc[2];

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

    fn test_calc_addr(&self, mut addr: u16) {
        let is_lt_2000 = addr < 0x2000;
        // mask off high bits if address is a mirror of 0-0x7ff
        addr &= !(0b1100000000000 * is_lt_2000 as u16);

        // set to true if addr is between 2000 and 3fff
        let is_ppu = is_lt_2000 ^ (addr < 0x4000);
        // TODO: apply this to addr instead and index into memory for ppu register writes
        let ppu_register_index = addr & (0b111 | !is_ppu as u16 * 0xffff);

        println!("ppu register index = {:x}", ppu_register_index);
        println!("final addr: {:x}", addr);
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

    let mut memory = vec![0u8; 0x1000];
    memory[0x80] = 00;
    memory[0x81] = 02;
    memory[0x200] = 0x69;
    cpu.a = 0;
    cpu.p = 0x66;
    // ADC ($80, X) (indexed indirect)
    let cyc = cpu.debug_exec_opcode([0x61, 0x80, 0x00], &mut memory);

    assert_eq!(cyc, 6);
    assert_eq!(cpu.a, 0x69);
    assert_eq!(cpu.p, 0x24);

    memory[0x80] = 0;
    memory[0x81] = 0;
    memory[0x200] = 0;
    memory[0x78] = 0x69;
    cpu.a = 0;
    cpu.p = 0x66;
    // ADC $78 (zero page)
    let cyc = cpu.debug_exec_opcode([0x65, 0x78, 0x00], &mut memory);

    assert_eq!(cyc, 3);
    assert_eq!(cpu.a, 0x69);
    assert_eq!(cpu.p, 0x24);

    memory[0x78] = 0;
    memory[0x678] = 0x69;
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

    let mut memory = vec![0u8; 0x1000];
    memory[0x80] = 00;
    memory[0x81] = 02;
    memory[0x200] = 0xaa;
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
    let mut memory = vec![0u8; 0x1000];

    cpu.a = 0x80;
    cpu.p = 0xe5;
    // ASL A
    let cyc = cpu.debug_exec_opcode([0x0a, 00, 00], &mut memory);

    assert_eq!(cyc, 2);
    assert_eq!(cpu.a, 0);
    assert_eq!(cpu.p, 0x67);

    cpu.a = 0;
    cpu.p = 0xe5;
    memory[0x78] = 0x80;
    // ASL $78
    let cyc = cpu.debug_exec_opcode([0x06, 0x78, 00], &mut memory);

    assert_eq!(cyc, 5);
    assert_eq!(memory[0x78], 0);
    assert_eq!(cpu.p, 0x67);

    cpu.p = 0xa5;
    memory[0x78] = 0;
    memory[0x678] = 0x55;
    // ASL $0678
    let cyc = cpu.debug_exec_opcode([0x0e, 0x78, 0x06], &mut memory);

    assert_eq!(cyc, 6);
    assert_eq!(memory[0x678], 0xaa);
    assert_eq!(cpu.p, 0xa4);
}

fn test_branch_instrs() {
    let mut cpu = Cpu::default();
    let mut memory = vec![0u8; 0x1000];
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
    // TODO: ..
}

fn test_cmp() {
    let mut cpu = Cpu::default();
    cpu.a = 0x40;
    cpu.p = 0x25;
    let _ = cpu.cmp_register_val(cpu.a, 0x41, 2);

    // assert_eq!(cyc, 2);
    assert_eq!(cpu.p, 0xa4);
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
    test_cmp();

    let cpu = Cpu::default();
    cpu.test_calc_addr(0x800);
    cpu.test_calc_addr(0xfff);
    cpu.test_calc_addr(0x80f);
    cpu.test_calc_addr(0xa0e);
    cpu.test_calc_addr(0x1000);
    cpu.test_calc_addr(0x18f0);
    cpu.test_calc_addr(0x48f0);
    cpu.test_calc_addr(0x3fff);
    cpu.test_calc_addr(0x2001);
}
