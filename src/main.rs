mod parse;

use parse::RomInfo;

fn bytes_to_u16(bytes: [u8; 2]) -> u16 {
    if cfg!(target_endian = "big") {
        ((bytes[1] as u16) << 8) + bytes[0] as u16
    } else {
        unsafe { std::mem::transmute(bytes) }
    }
}

fn u16_to_bytes(num: u16) -> [u8; 2] {
    unsafe { std::mem::transmute(num) }
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

    fn exec_instruction(&mut self, memory: &mut [u8]) -> u8 {
        // FIXME: handle out of bounds????
        // FIXME: need to subtract from clock cycles if last cycle wasn't a write
        // TODO: use get_unchecked() where the index is
        // guaranteed to be smaller than the array size
        match [
            memory[self.pc as usize],
            memory[(self.pc + 1) as usize],
            memory[(self.pc + 2) as usize],
        ] {
            // ADC #byte_1 (immediate)
            [0x69, byte_1, _] => {
                self.pc += 2;
                self.adc_immediate(byte_1)
            }
            // ADC $byte_1 (zero page)
            [0x65, byte_1, _] => {
                self.pc += 2;
                let val = memory[byte_1 as usize];
                self.adc_immediate(val) + 1
            }
            // ADC $byte_1, X (zero page indexed)
            [0x75, byte_1, _] => {
                self.pc += 2;
                let addr = byte_1.wrapping_add(self.x);
                self.adc_immediate(memory[addr as usize]) + 2
            }
            // ADC $bytes (absolute)
            [0x6d, bytes @ ..] => {
                self.pc += 3;
                let val = memory[bytes_to_u16(bytes) as usize];
                self.adc_immediate(val) + 2
            }
            // ADC $bytes, X (absolute indexed)
            [0x7d, bytes @ ..] => {
                self.pc += 3;
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                // add 'carry' to return value for one extra cycle if a page boundary was crossed
                self.adc_immediate(memory[addr as usize]) + 2 + carry as u8
            }
            // ADC $bytes, Y (absolute indexed)
            [0x79, bytes @ ..] => {
                self.pc += 3;
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.adc_immediate(memory[addr as usize]) + 2 + carry as u8
            }
            // ADC ($byte_1, X) (indexed indirect)
            [0x61, byte_1, _] => {
                self.pc += 2;
                let addr = self.get_indexed_indirect(byte_1, memory);
                self.adc_immediate(memory[addr as usize]) + 4
            }
            // ADC ($byte_1), Y (indirect indexed)
            [0x71, byte_1, _] => {
                self.pc += 2;
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory);
                self.adc_immediate(memory[addr as usize]) + 3 + carry as u8
            }
            // AND #byte_1 (immediate)
            [0x29, byte_1, _] => {
                self.pc += 2;
                self.and_immediate(byte_1)
            }
            // AND $byte_1 (zero page)
            [0x25, byte_1, _] => {
                self.pc += 2;
                let val = memory[byte_1 as usize];
                self.and_immediate(val) + 1
            }
            // AND $byte_1, X (zero page indexed)
            [0x35, byte_1, _] => {
                self.pc += 2;
                let addr = byte_1.wrapping_add(self.x);
                self.and_immediate(memory[addr as usize]) + 2
            }
            // AND $bytes (absolute)
            [0x2d, bytes @ ..] => {
                self.pc += 3;
                let val = memory[bytes_to_u16(bytes) as usize];
                self.and_immediate(val) + 2
            }
            // AND $bytes, X (absolute indexed)
            [0x3d, bytes @ ..] => {
                self.pc += 3;
                let (addr, carry) = self.get_absolute_indexed(bytes, self.x);
                self.and_immediate(memory[addr as usize]) + 2 + carry as u8
            }
            // AND $bytes, Y (absolute indexed)
            [0x39, bytes @ ..] => {
                self.pc += 3;
                let (addr, carry) = self.get_absolute_indexed(bytes, self.y);
                self.and_immediate(memory[addr as usize]) + 2 + carry as u8
            }
            // AND ($byte_1, X) (indexed indirect)
            [0x21, byte_1, _] => {
                self.pc += 2;
                let addr = self.get_indexed_indirect(byte_1, memory);
                self.and_immediate(memory[addr as usize]) + 4
            }
            // AND ($byte_1), Y (indirect indexed)
            [0x31, byte_1, _] => {
                self.pc += 2;
                let (addr, carry) = self.get_indirect_indexed(byte_1, memory);
                self.and_immediate(memory[addr as usize]) + 3 + carry as u8
            }
            // ASL A (accumulator)
            [0x0a, ..] => {
                self.pc += 1;
                self.a = self.asl_value(self.a);
                2
            }
            // ASL $byte_1 (zero page)
            [0x06, byte_1, _] => {
                self.pc += 2;
                memory[byte_1 as usize] = self.asl_value(memory[byte_1 as usize]);
                5
            }
            // ASL $byte_1, X (zero page indexed)
            [0x16, byte_1, _] => {
                self.pc += 2;
                let addr = byte_1.wrapping_add(self.x);
                memory[addr as usize] = self.asl_value(memory[byte_1 as usize]);
                6
            }
            // ASL $bytes (absolute)
            [0x0e, bytes @ ..] => {
                self.pc += 3;
                let addr = bytes_to_u16(bytes);
                memory[addr as usize] = self.asl_value(memory[addr as usize]);
                6
            }
            // ASL $bytes, X (absolute indexed)
            [0x1e, bytes @ ..] => {
                self.pc += 3;
                let (addr, _) = self.get_absolute_indexed(bytes, self.x);
                memory[addr as usize] = self.asl_value(memory[addr as usize]);
                7
            }
            // BCC $byte_1
            [0x90, byte_1, _] => {
                self.pc += 2;
                self.bcc(byte_1)
            }
            // BCS $byte_1
            [0xb0, byte_1, _] => {
                self.pc += 2;
                self.bcs(byte_1)
            }
            // BEQ $byte_1
            [0xf0, byte_1, _] => {
                self.pc += 2;
                self.beq(byte_1)
            }
            // BMI $byte_1
            [0x30, byte_1, _] => {
                self.pc += 2;
                self.bmi(byte_1)
            }
            // BNE $byte_1
            [0xd0, byte_1, _] => {
                self.pc += 2;
                self.bne(byte_1)
            }
            // BPL $byte_1
            [0x10, byte_1, _] => {
                self.pc += 2;
                self.bpl(byte_1)
            }
            // BVC $byte_1
            [0x50, byte_1, _] => {
                self.pc += 2;
                self.bvc(byte_1)
            }
            // BVS $byte_1
            [0x70, byte_1, _] => {
                self.pc += 2;
                self.bvs(byte_1)
            }
            _ => panic!("TODO: handle invalid opcode"),
        }
    }

    fn get_absolute_indexed(&self, addr_bytes: [u8; 2], index: u8) -> (u16, bool) {
        let (addr_low, carry) = addr_bytes[0].overflowing_add(index);
        let addr_hi = addr_bytes[1] + carry as u8;
        let addr_indexed = bytes_to_u16([addr_low, addr_hi]);

        (addr_indexed, carry)
    }

    fn get_indexed_indirect(&self, addr: u8, memory: &[u8]) -> u16 {
        let addr_indexed = addr.wrapping_add(self.x);
        let dest_addr = bytes_to_u16([
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
        let indexed_addr = bytes_to_u16([indexed_addr_low, indexed_addr_hi]);

        (indexed_addr, carry)
    }

    #[inline]
    fn set_carry(&mut self, carry: bool) {
        self.p = (self.p & !1) | carry as u8;
    }

    #[inline]
    fn set_overflow(&mut self, overflow: bool) {
        self.p = (self.p & !0b01000000) | ((overflow as u8) << 6);
    }

    #[inline]
    fn set_zero(&mut self, val: u8) {
        self.p = (self.p & !2) | (((val == 0) as u8) << 1);
    }

    #[inline]
    fn set_negative(&mut self, val: u8) {
        self.p = (self.p & !0x80) | val & 0x80;
    }

    fn adc_immediate(&mut self, imm: u8) -> u8 {
        // add the carry to the immediate value first
        let (imm, mut carry) = imm.overflowing_add(self.p & 1);
        let (_, mut overflow) = (imm as i8).overflowing_add((self.p & 1) as i8);

        // then add the immediate value to the accumulator
        let (res, carry_2) = imm.overflowing_add(self.a);
        let (_, overflow_2) = (imm as i8).overflowing_add(self.a as i8);

        overflow |= overflow_2;
        carry |= carry_2;

        self.a = res;
        self.set_carry(carry);
        self.set_overflow(overflow);
        self.set_zero(res);
        self.set_negative(res);

        2
    }

    fn and_immediate(&mut self, imm: u8) -> u8 {
        self.a &= imm;
        self.set_negative(self.a);
        self.set_zero(self.a);

        2
    }

    fn asl_value(&mut self, val: u8) -> u8 {
        self.set_carry((val >> 7) != 0);
        let res = val << 1;

        self.set_zero(res);
        self.set_negative(res);

        res
    }

    fn bcc(&mut self, offset: u8) -> u8 {
        self.branch_if((self.p & 1) == 0, offset)
    }

    fn bcs(&mut self, offset: u8) -> u8 {
        self.branch_if((self.p & 1) != 0, offset)
    }

    fn beq(&mut self, offset: u8) -> u8 {
        self.branch_if((self.p & 2) != 0, offset)
    }

    fn bmi(&mut self, offset: u8) -> u8 {
        self.branch_if((self.p as i8) < 0, offset)
    }

    fn bne(&mut self, offset: u8) -> u8 {
        self.branch_if((self.p & 2) == 0, offset)
    }

    fn bpl(&mut self, offset: u8) -> u8 {
        self.branch_if((self.p as i8) > 0, offset)
    }

    fn bvc(&mut self, offset: u8) -> u8 {
        self.branch_if((self.p & 0b01000000) == 0, offset)
    }

    fn bvs(&mut self, offset: u8) -> u8 {
        self.branch_if((self.p & 0b01000000) != 0, offset)
    }

    fn branch_if(&mut self, condition: bool, offset: u8) -> u8 {
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

    fn debug_exec_opcode(&mut self, opc: [u8; 3], memory: &mut Vec<u8>) -> u8 {
        memory.extend_from_slice(&opc);
        self.pc = (memory.len() - 3) as u16;

        let cyc = self.exec_instruction(memory);
        memory.truncate(memory.len() - 3);
        cyc
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
    let mut cpu = Cpu::new_nestest();

    cpu.p = 0x6e;
    cpu.adc_immediate(0x69);

    assert_eq!(cpu.a, 0x69);
    assert_eq!(cpu.p, 0x2c);

    cpu.a = 1;
    cpu.p = 0x6d;
    cpu.adc_immediate(0x69);

    assert_eq!(cpu.a, 0x6b);
    assert_eq!(cpu.p, 0x2c);

    cpu.a = 0x7f;
    cpu.p = 0x25;
    cpu.adc_immediate(0x80);

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
    let mut cpu = Cpu::new_nestest();

    cpu.a = 0x55;
    cpu.p = 0;
    let cyc = cpu.and_immediate(0xaa);

    assert_eq!(cyc, 2);
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
    let mut cpu = Cpu::new_nestest();
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
    let mut cpu = Cpu::new_nestest();
    cpu.pc = 0x100;
    cpu.p = 0;
    let cyc = cpu.bcc(0x80);

    assert_eq!(cpu.pc, 0x100 - 0x80);
    assert_eq!(cyc, 4);

    cpu.pc = 0x100;
    cpu.p = 0;
    let cyc = cpu.bcc(0x7f);

    assert_eq!(cpu.pc, 0x100 + 0x7f);
    assert_eq!(cyc, 3);

    cpu.pc = 0x100;
    cpu.p = 0b01000000;
    let cyc = cpu.bvc(0xff);

    assert_eq!(cpu.pc, 0x100);
    assert_eq!(cyc, 2);

    cpu.pc = 0x100;
    cpu.p = 0b01000000;
    let cyc = cpu.bvs(0xff);

    assert_eq!(cpu.pc, 0x100 - 1);
    assert_eq!(cyc, 4);
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
}
