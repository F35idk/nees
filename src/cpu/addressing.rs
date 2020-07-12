use super::super::memory_map as mmap;
use super::super::memory_map::MemoryMap;
use super::Cpu;

pub use abs::*;
pub use abs_indexed::*;
pub use indexed_indirect::*;
pub use indirect_indexed::*;
pub use zero_page::*;
pub use zero_page_indexed::*;

// TODO: implement dummy reads and writes

// absolute addressing
mod abs {
    use super::*;

    pub fn read_abs(
        cpu: &mut Cpu,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) -> u8 {
        let addr = cpu.fetch_operand_u16(memory, ptrs);
        cpu.pc += 3;
        cpu.cycle_count += 4;

        memory.read_cpu(ptrs, addr)
    }

    pub fn write_abs(
        cpu: &mut Cpu,
        val: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let addr = cpu.fetch_operand_u16(memory, ptrs);
        memory.write_cpu(ptrs, addr, val);
    }

    // fetches the absolute address at pc+1, performs 'operation' on the
    // value at the address and stores it back
    pub fn read_write_abs<F>(
        cpu: &mut Cpu,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
        mut operation: F,
    ) where
        F: FnMut(&mut Cpu, u8) -> u8,
    {
        let addr = cpu.fetch_operand_u16(memory, ptrs);
        let val = memory.read_cpu(ptrs, addr);
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
        ptrs: &mut mmap::MemoryMapPtrs,
    ) -> bool {
        let addr_bytes = cpu.fetch_operand_bytes(memory, ptrs);
        let (addr_indexed, carry) = self::calc_abs_indexed(addr_bytes, index);
        memory.write_cpu(ptrs, addr_indexed, val); // FIXME: dummy reads/writes
        carry // whether a page boundary was crossed
    }

    pub fn read_abs_indexed(
        cpu: &mut Cpu,
        index: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) -> u8 {
        let addr_bytes = cpu.fetch_operand_bytes(memory, ptrs);
        let (addr_indexed, page_crossed) = self::calc_abs_indexed(addr_bytes, index);

        cpu.pc += 3;
        cpu.cycle_count += 4 + page_crossed as u32;

        memory.read_cpu(ptrs, addr_indexed)
    }

    pub fn read_write_abs_indexed<F>(
        cpu: &mut Cpu,
        index: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
        mut operation: F,
    ) where
        // NOTE: 'operation' takes a bool indicating if a page boundary was crossed
        F: FnMut(&mut Cpu, u8, bool) -> u8,
    {
        let addr_bytes = cpu.fetch_operand_bytes(memory, ptrs);
        let (addr_indexed, carry) = self::calc_abs_indexed(addr_bytes, index);

        let val = memory.read_cpu(ptrs, addr_indexed);
        let res = operation(cpu, val, carry);

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
        ptrs: &mut mmap::MemoryMapPtrs,
    ) -> u8 {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        cpu.cycle_count += 3;
        cpu.pc += 2;

        memory.read_cpu(ptrs, addr)
    }

    pub fn write_zero_page(
        cpu: &mut Cpu,
        val: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        memory.write_cpu(ptrs, addr, val);
    }

    pub fn read_write_zero_page<F>(
        cpu: &mut Cpu,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
        mut operation: F,
    ) where
        F: FnMut(&mut Cpu, u8) -> u8,
    {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let val = memory.read_cpu(ptrs, addr);

        let res = operation(cpu, val);
        memory.write_cpu(ptrs, addr, res);
    }
}

mod zero_page_indexed {
    use super::*;

    pub fn write_zero_page_indexed(
        cpu: &mut Cpu,
        val: u8,
        index: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let addr_indexed = addr.wrapping_add(index);
        memory.write_cpu(ptrs, addr_indexed, val);
    }

    pub fn read_zero_page_indexed(
        cpu: &mut Cpu,
        index: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) -> u8 {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let addr_indexed = addr.wrapping_add(index);

        cpu.pc += 2;
        cpu.cycle_count += 4;

        memory.read_cpu(ptrs, addr_indexed)
    }

    pub fn read_write_zero_page_indexed<F>(
        cpu: &mut Cpu,
        index: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
        mut operation: F,
    ) where
        F: FnMut(&mut Cpu, u8) -> u8,
    {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let addr_indexed = addr.wrapping_add(index);

        let val = memory.read_cpu(ptrs, addr_indexed);
        let res = operation(cpu, val);

        memory.write_cpu(ptrs, addr_indexed, res);
    }
}

mod indexed_indirect {
    use super::*;

    pub fn read_indexed_indirect(
        cpu: &mut Cpu,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) -> u8 {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let final_addr = self::calc_indexed_indirect(cpu, addr, memory, ptrs);

        cpu.pc += 2;
        cpu.cycle_count += 6;

        memory.read_cpu(ptrs, final_addr)
    }

    pub fn write_indexed_indirect(
        cpu: &mut Cpu,
        val: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let final_addr = self::calc_indexed_indirect(cpu, addr, memory, ptrs);
        memory.write_cpu(ptrs, final_addr, val);
    }

    pub fn read_write_indexed_indirect<F>(
        cpu: &mut Cpu,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
        mut operation: F,
    ) where
        F: FnMut(&mut Cpu, u8) -> u8,
    {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let final_addr = self::calc_indexed_indirect(cpu, addr, memory, ptrs);

        let val = memory.read_cpu(ptrs, final_addr);
        let res = operation(cpu, val);

        memory.write_cpu(ptrs, final_addr, res);
    }

    fn calc_indexed_indirect(
        cpu: &mut Cpu,
        addr: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) -> u16 {
        let addr_indexed = addr.wrapping_add(cpu.x);
        u16::from_le_bytes([
            memory.read_cpu(ptrs, addr_indexed),
            memory.read_cpu(ptrs, addr_indexed.wrapping_add(1)),
        ])
    }
}

mod indirect_indexed {
    use super::*;

    pub fn read_indirect_indexed(
        cpu: &mut Cpu,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) -> u8 {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let (final_addr, page_crossed) = self::calc_indirect_indexed(cpu, addr, memory, ptrs);

        cpu.pc += 2;
        cpu.cycle_count += 5 + page_crossed as u32;

        memory.read_cpu(ptrs, final_addr)
    }

    pub fn write_indirect_indexed(
        cpu: &mut Cpu,
        val: u8,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) -> bool {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let (final_addr, page_crossed) = self::calc_indirect_indexed(cpu, addr, memory, ptrs);
        memory.write_cpu(ptrs, final_addr, val);
        page_crossed
    }

    pub fn read_write_indirect_indexed<F>(
        cpu: &mut Cpu,
        memory: &mut mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
        mut operation: F,
    ) where
        F: FnMut(&mut Cpu, u8, bool) -> u8,
    {
        let addr = cpu.fetch_operand_byte(memory, ptrs);
        let (final_addr, page_crossed) = self::calc_indirect_indexed(cpu, addr, memory, ptrs);

        let val = memory.read_cpu(ptrs, final_addr);
        let res = operation(cpu, val, page_crossed);

        memory.write_cpu(ptrs, final_addr, res);
    }

    fn calc_indirect_indexed(
        cpu: &Cpu,
        addr: u8,
        memory: &mmap::Nrom128MemoryMap,
        ptrs: &mut mmap::MemoryMapPtrs,
    ) -> (u16, bool) {
        // get address at memory[addr]
        let dest_addr = [
            memory.read_cpu(ptrs, addr),
            memory.read_cpu(ptrs, addr.wrapping_add(1)),
        ];

        // add index to address while keeping track of whether a page boundary was crossed
        let (indexed_addr_low, carry) = dest_addr[0].overflowing_add(cpu.y);
        let indexed_addr_hi = dest_addr[1].wrapping_add(carry as u8);

        let indexed_addr = u16::from_le_bytes([indexed_addr_low, indexed_addr_hi]);

        (indexed_addr, carry)
    }
}
