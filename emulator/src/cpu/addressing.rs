use super::super::memory_map as mmap;
use super::super::memory_map::CpuMemoryMap;
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

    pub fn read_imm(cpu: &mut Cpu, memory: &mut mmap::Nrom128CpuMemory) -> u8 {
        let val = cpu.fetch_operand_byte(memory);
        cpu.pc += 2;
        cpu.cycle_count += 2;
        val
    }
}

// absolute addressing
mod abs {
    use super::*;

    pub fn read_abs(cpu: &mut Cpu, memory: &mut mmap::Nrom128CpuMemory) -> u8 {
        let addr = cpu.fetch_operand_u16(memory);
        cpu.pc += 3;
        cpu.cycle_count += 4;
        memory.read(addr, cpu)
    }

    pub fn write_abs(cpu: &mut Cpu, val: u8, memory: &mut mmap::Nrom128CpuMemory) {
        let addr = cpu.fetch_operand_u16(memory);
        cpu.pc += 3;
        cpu.cycle_count += 4;

        memory.write(addr, val, cpu);
    }

    // fetches the absolute address at pc+1, performs 'operation' on the
    // value at the address and stores it back
    pub fn read_write_abs(
        cpu: &mut Cpu,
        memory: &mut mmap::Nrom128CpuMemory,

        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_u16(memory);
        let val = memory.read(addr, cpu);

        cpu.pc += 3;
        cpu.cycle_count += 6;

        let res = operation(cpu, val);
        memory.write(addr, res, cpu);
    }
}

// absolute indexed addressing
mod abs_indexed {
    use super::*;

    pub fn write_abs_indexed(
        cpu: &mut Cpu,
        val: u8,
        index: u8,
        memory: &mut mmap::Nrom128CpuMemory,
    ) {
        let addr = cpu.fetch_operand_u16(memory);
        let addr_indexed = addr.wrapping_add(index as u16);

        cpu.pc += 3;
        cpu.cycle_count += 5;

        // FIXME: dummy writes
        memory.write(addr_indexed, val, cpu);
    }

    pub fn read_abs_indexed(cpu: &mut Cpu, index: u8, memory: &mut mmap::Nrom128CpuMemory) -> u8 {
        let addr_bytes = cpu.fetch_operand_bytes(memory);
        let (addr_indexed, page_crossed) = self::calc_abs_indexed(addr_bytes, index);

        cpu.pc += 3;
        cpu.cycle_count += 4 + page_crossed as u64;

        memory.read(addr_indexed, cpu)
    }

    pub fn read_write_abs_indexed(
        cpu: &mut Cpu,
        index: u8,
        memory: &mut mmap::Nrom128CpuMemory,

        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_u16(memory);
        let addr_indexed = addr.wrapping_add(index as u16);
        // TODO: dummy reads (read from carry-less address first, etc.)
        let val = memory.read(addr_indexed, cpu);

        cpu.pc += 3;
        cpu.cycle_count += 7;

        let res = operation(cpu, val);
        memory.write(addr_indexed, res, cpu);
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

    pub fn read_zero_page(cpu: &mut Cpu, memory: &mut mmap::Nrom128CpuMemory) -> u8 {
        let addr = cpu.fetch_operand_byte(memory);
        cpu.cycle_count += 3;
        cpu.pc += 2;

        memory.read(addr as u16, cpu)
    }

    pub fn write_zero_page(cpu: &mut Cpu, val: u8, memory: &mut mmap::Nrom128CpuMemory) {
        let addr = cpu.fetch_operand_byte(memory);
        cpu.pc += 2;
        cpu.cycle_count += 3;
        memory.write(addr as u16, val, cpu);
    }

    pub fn read_write_zero_page(
        cpu: &mut Cpu,
        memory: &mut mmap::Nrom128CpuMemory,

        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_byte(memory);
        let val = memory.read(addr as u16, cpu);

        cpu.pc += 2;
        cpu.cycle_count += 5;

        let res = operation(cpu, val);
        memory.write(addr as u16, res, cpu);
    }
}

mod zero_page_indexed {
    use super::*;

    pub fn write_zero_page_indexed(
        cpu: &mut Cpu,
        val: u8,
        index: u8,
        memory: &mut mmap::Nrom128CpuMemory,
    ) {
        let addr = cpu.fetch_operand_byte(memory);
        let addr_indexed = addr.wrapping_add(index);
        cpu.pc += 2;
        cpu.cycle_count += 4;
        memory.write(addr_indexed as u16, val, cpu);
    }

    pub fn read_zero_page_indexed(
        cpu: &mut Cpu,
        index: u8,
        memory: &mut mmap::Nrom128CpuMemory,
    ) -> u8 {
        let addr = cpu.fetch_operand_byte(memory);
        let addr_indexed = addr.wrapping_add(index);

        cpu.pc += 2;
        cpu.cycle_count += 4;

        memory.read(addr_indexed as u16, cpu)
    }

    pub fn read_write_zero_page_indexed(
        cpu: &mut Cpu,
        index: u8,
        memory: &mut mmap::Nrom128CpuMemory,

        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_byte(memory);
        let addr_indexed = addr.wrapping_add(index);

        cpu.pc += 2;
        cpu.cycle_count += 6;

        let val = memory.read(addr_indexed as u16, cpu);
        let res = operation(cpu, val);

        memory.write(addr_indexed as u16, res, cpu);
    }
}

mod indexed_indirect {
    use super::*;

    pub fn read_indexed_indirect(cpu: &mut Cpu, memory: &mut mmap::Nrom128CpuMemory) -> u8 {
        let addr = cpu.fetch_operand_byte(memory);
        let final_addr = self::calc_indexed_indirect(cpu, addr, memory);

        cpu.pc += 2;
        cpu.cycle_count += 6;

        memory.read(final_addr, cpu)
    }

    pub fn write_indexed_indirect(cpu: &mut Cpu, val: u8, memory: &mut mmap::Nrom128CpuMemory) {
        let addr = cpu.fetch_operand_byte(memory);
        let final_addr = self::calc_indexed_indirect(cpu, addr, memory);

        cpu.pc += 2;
        cpu.cycle_count += 6;

        memory.write(final_addr, val, cpu);
    }

    fn calc_indexed_indirect(cpu: &mut Cpu, addr: u8, memory: &mut mmap::Nrom128CpuMemory) -> u16 {
        let addr_indexed = addr.wrapping_add(cpu.x);
        u16::from_le_bytes([
            memory.read(addr_indexed as u16, cpu),
            memory.read(addr_indexed.wrapping_add(1) as u16, cpu),
        ])
    }
}

mod indirect_indexed {
    use super::*;

    pub fn read_indirect_indexed(cpu: &mut Cpu, memory: &mut mmap::Nrom128CpuMemory) -> u8 {
        let addr = cpu.fetch_operand_byte(memory);
        let (final_addr, page_crossed) = self::calc_indirect_indexed(cpu, addr, memory);

        cpu.pc += 2;
        cpu.cycle_count += 5 + page_crossed as u64;

        memory.read(final_addr, cpu)
    }

    pub fn write_indirect_indexed(cpu: &mut Cpu, val: u8, memory: &mut mmap::Nrom128CpuMemory) {
        let addr = cpu.fetch_operand_byte(memory);
        // OPTIMIZE: don't use 'calc_indirect_indexed()', since carry doesn't matter
        let (final_addr, _) = self::calc_indirect_indexed(cpu, addr, memory);
        // TODO: dummy writes

        cpu.pc += 2;
        cpu.cycle_count += 6;

        memory.write(final_addr, val, cpu);
    }

    fn calc_indirect_indexed(
        cpu: &mut Cpu,
        addr: u8,
        memory: &mut mmap::Nrom128CpuMemory,
    ) -> (u16, bool) {
        // get address at memory[addr]
        let dest_addr = [
            memory.read(addr as u16, cpu),
            memory.read(addr.wrapping_add(1) as u16, cpu),
        ];

        // add index to address while keeping track of whether a page boundary was crossed
        let (indexed_addr_low, carry) = dest_addr[0].overflowing_add(cpu.y);
        let indexed_addr_hi = dest_addr[1].wrapping_add(carry as u8);

        let indexed_addr = u16::from_le_bytes([indexed_addr_low, indexed_addr_hi]);

        (indexed_addr, carry)
    }
}
