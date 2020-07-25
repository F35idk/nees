use super::super::memory_map as mmap;
use super::super::memory_map::MemoryMap;
use super::super::PtrsWrapper;
use super::Cpu;

// submodules used for improved readability inside of this module.
// (everything is re-exported, so these are not visible to the outside)
pub use abs::*;
pub use abs_indexed::*;
pub use imm::*;
pub use indexed_indirect::*;
pub use indirect_indexed::*;
pub use zero_page::*;
pub use zero_page_indexed::*;

// TODO: implement dummy reads and writes

// immediate addressing
mod imm {
    use super::*;

    pub fn read_imm(cpu: &mut Cpu, memory: &mmap::Nrom128MemoryMap, ptrs: &mut PtrsWrapper) -> u8 {
        let val = cpu.fetch_operand_byte(memory, ptrs);
        cpu.pc += 2;
        *ptrs.cpu_cycles += 2;
        val
    }
}

// absolute addressing
mod abs {
    use super::*;

    pub fn read_abs(cpu: &mut Cpu, memory: &mmap::Nrom128MemoryMap, ptrs: &mut PtrsWrapper) -> u8 {
        let addr = cpu.fetch_operand_u16(memory, ptrs);
        cpu.pc += 3;
        *ptrs.cpu_cycles += 4;
        memory.read_cpu(ptrs, addr)
    }

    pub fn write_abs(
        cpu: &mut Cpu,
        val: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
    ) {
        let addr = cpu.fetch_operand_u16(memory, ptrs);
        cpu.pc += 3;
        *ptrs.cpu_cycles += 4;

        memory.write_cpu(ptrs, addr, val);
    }

    // fetches the absolute address at pc+1, performs 'operation' on the
    // value at the address and stores it back
    pub fn read_write_abs(
        cpu: &mut Cpu,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_u16(memory, ptrs);
        let val = memory.read_cpu(ptrs, addr);

        cpu.pc += 3;
        *ptrs.cpu_cycles += 6;

        let res = operation(cpu, val);
        memory.write_cpu(ptrs, addr, res);
    }
}

// absolute indexed addressing
mod abs_indexed {
    use super::*;

    pub fn write_abs_indexed(
        cpu: &mut Cpu,
        val: u8,
        index: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
    ) {
        let addr = cpu.fetch_operand_u16(memory, ptrs);
        let addr_indexed = addr.wrapping_add(index as u16);

        cpu.pc += 3;
        *ptrs.cpu_cycles += 5;

        // FIXME: dummy writes
        memory.write_cpu(ptrs, addr_indexed, val);
    }

    pub fn read_abs_indexed(
        cpu: &mut Cpu,
        index: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
    ) -> u8 {
        let addr_bytes = cpu.fetch_operand_bytes(memory, ptrs);
        let (addr_indexed, page_crossed) = self::calc_abs_indexed(addr_bytes, index);

        cpu.pc += 3;
        *ptrs.cpu_cycles += 4 + page_crossed as u64;

        memory.read_cpu(ptrs, addr_indexed)
    }

    pub fn read_write_abs_indexed(
        cpu: &mut Cpu,
        index: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_u16(memory, ptrs);
        let addr_indexed = addr.wrapping_add(index as u16);
        // TODO: dummy reads (read from carry-less address first, etc.)
        let val = memory.read_cpu(ptrs, addr_indexed);

        cpu.pc += 3;
        *ptrs.cpu_cycles += 7;

        let res = operation(cpu, val);
        memory.write_cpu(ptrs, addr_indexed, res);
    }

    fn calc_abs_indexed(addr_bytes: [u8; 2], index: u8) -> (u16, bool) {
        let (addr_low, carry) = addr_bytes[0].overflowing_add(index);
        let addr_hi = addr_bytes[1].wrapping_add(carry as u8);
        let addr_indexed = u16::from_le_bytes([addr_low, addr_hi]);

        (addr_indexed, carry)
    }
}

mod zero_page {
    use super::*;

    pub fn read_zero_page(
        cpu: &mut Cpu,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
    ) -> u8 {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        *ptrs.cpu_cycles += 3;
        cpu.pc += 2;

        memory.read_cpu(ptrs, addr as u16)
    }

    pub fn write_zero_page(
        cpu: &mut Cpu,
        val: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
    ) {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        cpu.pc += 2;
        *ptrs.cpu_cycles += 3;
        memory.write_cpu(ptrs, addr as u16, val);
    }

    pub fn read_write_zero_page(
        cpu: &mut Cpu,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let val = memory.read_cpu(ptrs, addr as u16);

        cpu.pc += 2;
        *ptrs.cpu_cycles += 5;

        let res = operation(cpu, val);
        memory.write_cpu(ptrs, addr as u16, res);
    }
}

mod zero_page_indexed {
    use super::*;

    pub fn write_zero_page_indexed(
        cpu: &mut Cpu,
        val: u8,
        index: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
    ) {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let addr_indexed = addr.wrapping_add(index);
        cpu.pc += 2;
        *ptrs.cpu_cycles += 4;
        memory.write_cpu(ptrs, addr_indexed as u16, val);
    }

    pub fn read_zero_page_indexed(
        cpu: &mut Cpu,
        index: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
    ) -> u8 {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let addr_indexed = addr.wrapping_add(index);

        cpu.pc += 2;
        *ptrs.cpu_cycles += 4;

        memory.read_cpu(ptrs, addr_indexed as u16)
    }

    pub fn read_write_zero_page_indexed(
        cpu: &mut Cpu,
        index: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let addr_indexed = addr.wrapping_add(index);

        cpu.pc += 2;
        *ptrs.cpu_cycles += 6;

        let val = memory.read_cpu(ptrs, addr_indexed as u16);
        let res = operation(cpu, val);

        memory.write_cpu(ptrs, addr_indexed as u16, res);
    }
}

mod indexed_indirect {
    use super::*;

    pub fn read_indexed_indirect(
        cpu: &mut Cpu,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
    ) -> u8 {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let final_addr = self::calc_indexed_indirect(cpu, addr, memory, ptrs);

        cpu.pc += 2;
        *ptrs.cpu_cycles += 6;

        memory.read_cpu(ptrs, final_addr)
    }

    pub fn write_indexed_indirect(
        cpu: &mut Cpu,
        val: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
    ) {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let final_addr = self::calc_indexed_indirect(cpu, addr, memory, ptrs);

        cpu.pc += 2;
        *ptrs.cpu_cycles += 6;

        memory.write_cpu(ptrs, final_addr, val);
    }

    fn calc_indexed_indirect(
        cpu: &mut Cpu,
        addr: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
    ) -> u16 {
        let addr_indexed = addr.wrapping_add(cpu.x);
        u16::from_le_bytes([
            memory.read_cpu(ptrs, addr_indexed as u16),
            memory.read_cpu(ptrs, addr_indexed.wrapping_add(1) as u16),
        ])
    }
}

mod indirect_indexed {
    use super::*;

    pub fn read_indirect_indexed(
        cpu: &mut Cpu,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
    ) -> u8 {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let (final_addr, page_crossed) = self::calc_indirect_indexed(cpu, addr, memory, ptrs);

        cpu.pc += 2;
        *ptrs.cpu_cycles += 5 + page_crossed as u64;

        memory.read_cpu(ptrs, final_addr)
    }

    pub fn write_indirect_indexed(
        cpu: &mut Cpu,
        val: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
    ) {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        // OPTIMIZE: don't use 'calc_indirect_indexed()', since carry doesn't matter
        let (final_addr, _) = self::calc_indirect_indexed(cpu, addr, memory, ptrs);
        // TODO: dummy writes

        cpu.pc += 2;
        *ptrs.cpu_cycles += 6;

        memory.write_cpu(ptrs, final_addr, val);
    }

    fn calc_indirect_indexed(
        cpu: &Cpu,
        addr: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut PtrsWrapper,
    ) -> (u16, bool) {
        // get address at memory[addr]
        let dest_addr = [
            memory.read_cpu(ptrs, addr as u16),
            memory.read_cpu(ptrs, addr.wrapping_add(1) as u16),
        ];

        // add index to address while keeping track of whether a page boundary was crossed
        let (indexed_addr_low, carry) = dest_addr[0].overflowing_add(cpu.y);
        let indexed_addr_hi = dest_addr[1].wrapping_add(carry as u8);

        let indexed_addr = u16::from_le_bytes([indexed_addr_low, indexed_addr_hi]);

        (indexed_addr, carry)
    }
}
