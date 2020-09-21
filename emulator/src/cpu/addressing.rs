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

    pub fn read_imm(cpu: &mut Cpu, memory: &mut dyn CpuMemoryMap) -> u8 {
        let val = cpu.fetch_operand_byte(memory);
        cpu.cycle_count += 1;

        cpu.pc += 1;
        val
    }
}

// absolute addressing
mod abs {
    use super::*;

    pub fn read_abs(cpu: &mut Cpu, memory: &mut dyn CpuMemoryMap) -> u8 {
        let addr = cpu.fetch_operand_u16(memory);
        cpu.cycle_count += 1;

        let val = memory.read(addr, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
        val
    }

    pub fn write_abs(cpu: &mut Cpu, val: u8, memory: &mut dyn CpuMemoryMap) {
        let addr = cpu.fetch_operand_u16(memory);
        cpu.cycle_count += 1;

        memory.write(addr, val, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }

    // fetches the absolute address at pc+1, performs 'operation' on the
    // value at the address and stores it back
    pub fn read_write_abs(
        cpu: &mut Cpu,
        memory: &mut dyn CpuMemoryMap,
        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_u16(memory);
        cpu.cycle_count += 1;

        let val = memory.read(addr, cpu);
        cpu.cycle_count += 2;

        // TODO: dummy write here
        let res = operation(cpu, val);
        memory.write(addr, res, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }
}

// absolute indexed addressing
mod abs_indexed {
    use super::*;

    pub fn write_abs_indexed(cpu: &mut Cpu, val: u8, index: u8, memory: &mut dyn CpuMemoryMap) {
        let addr = cpu.fetch_operand_u16(memory);
        let addr_indexed = addr.wrapping_add(index as u16);
        // TODO: dummy read here (addr_indexed fixup cycle)
        cpu.cycle_count += 2;

        memory.write(addr_indexed, val, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }

    pub fn read_abs_indexed(cpu: &mut Cpu, index: u8, memory: &mut dyn CpuMemoryMap) -> u8 {
        let addr_bytes = cpu.fetch_operand_bytes(memory);
        let (addr_indexed, page_crossed) = {
            let (low, page_crossed) = addr_bytes[0].overflowing_add(index);
            let high = addr_bytes[1].wrapping_add(page_crossed as u8);
            let indexed = u16::from_le_bytes([low, high]);

            (indexed, page_crossed)
        };
        cpu.cycle_count += 1 + page_crossed as i16;

        let res = memory.read(addr_indexed, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
        res
    }

    pub fn read_write_abs_indexed(
        cpu: &mut Cpu,
        index: u8,
        memory: &mut dyn CpuMemoryMap,
        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_u16(memory);
        let addr_indexed = addr.wrapping_add(index as u16);
        // TODO: dummy read here (is invalid if page crossed)
        cpu.cycle_count += 2;

        let val = memory.read(addr_indexed, cpu);
        cpu.cycle_count += 1;

        // TODO: dummy write here
        let res = operation(cpu, val);
        cpu.cycle_count += 1;

        memory.write(addr_indexed, res, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }
}

mod zero_page {
    use super::*;

    pub fn read_zero_page(cpu: &mut Cpu, memory: &mut dyn CpuMemoryMap) -> u8 {
        let addr = cpu.fetch_operand_byte(memory);
        cpu.cycle_count += 1;

        let res = memory.read(addr as u16, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
        res
    }

    pub fn write_zero_page(cpu: &mut Cpu, val: u8, memory: &mut dyn CpuMemoryMap) {
        let addr = cpu.fetch_operand_byte(memory);
        cpu.cycle_count += 1;

        memory.write(addr as u16, val, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }

    pub fn read_write_zero_page(
        cpu: &mut Cpu,
        memory: &mut dyn CpuMemoryMap,
        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_byte(memory);
        cpu.cycle_count += 1;

        let val = memory.read(addr as u16, cpu);
        cpu.cycle_count += 2;
        // TODO: dummy write here

        let res = operation(cpu, val);
        memory.write(addr as u16, res, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }
}

mod zero_page_indexed {
    use super::*;

    pub fn read_zero_page_indexed(cpu: &mut Cpu, index: u8, memory: &mut dyn CpuMemoryMap) -> u8 {
        let addr = cpu.fetch_operand_byte(memory);
        cpu.cycle_count += 1;

        // TODO: dummy read from 'addr' here
        let addr_indexed = addr.wrapping_add(index);
        cpu.cycle_count += 1;

        let res = memory.read(addr_indexed as u16, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
        res
    }

    pub fn write_zero_page_indexed(
        cpu: &mut Cpu,
        val: u8,
        index: u8,
        memory: &mut dyn CpuMemoryMap,
    ) {
        let addr = cpu.fetch_operand_byte(memory);
        cpu.cycle_count += 1;

        // TODO: dummy read from 'addr' here
        let addr_indexed = addr.wrapping_add(index);
        cpu.cycle_count += 1;

        memory.write(addr_indexed as u16, val, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }

    pub fn read_write_zero_page_indexed(
        cpu: &mut Cpu,
        index: u8,
        memory: &mut dyn CpuMemoryMap,
        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_byte(memory);
        cpu.cycle_count += 1;

        // TODO: dummy read from 'addr' here
        let addr_indexed = addr.wrapping_add(index);
        cpu.cycle_count += 1;

        let val = memory.read(addr_indexed as u16, cpu);
        cpu.cycle_count += 1;

        // TODO: dummy write here (write 'val' to 'addr_indexed')
        let res = operation(cpu, val);
        cpu.cycle_count += 1;

        memory.write(addr_indexed as u16, res, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }
}

mod indexed_indirect {
    use super::*;

    pub fn read_indexed_indirect(cpu: &mut Cpu, memory: &mut dyn CpuMemoryMap) -> u8 {
        let addr = cpu.fetch_operand_byte(memory);
        cpu.cycle_count += 1;

        let final_addr = self::calc_indexed_indirect(cpu, addr, memory);

        let res = memory.read(final_addr, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
        res
    }

    pub fn write_indexed_indirect(cpu: &mut Cpu, val: u8, memory: &mut dyn CpuMemoryMap) {
        let addr = cpu.fetch_operand_byte(memory);
        cpu.cycle_count += 1;

        let final_addr = self::calc_indexed_indirect(cpu, addr, memory);

        memory.write(final_addr, val, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }

    fn calc_indexed_indirect(cpu: &mut Cpu, addr: u8, memory: &mut dyn CpuMemoryMap) -> u16 {
        // TODO: dummy read (from 'addr') here
        let addr_indexed = addr.wrapping_add(cpu.x);
        cpu.cycle_count += 1;

        let addr_lo = memory.read(addr_indexed as u16, cpu);
        cpu.cycle_count += 1;

        let addr_hi = memory.read(addr_indexed.wrapping_add(1) as u16, cpu);
        cpu.cycle_count += 1;

        u16::from_le_bytes([addr_lo, addr_hi])
    }
}

mod indirect_indexed {
    use super::*;

    pub fn read_indirect_indexed(cpu: &mut Cpu, memory: &mut dyn CpuMemoryMap) -> u8 {
        let addr = cpu.fetch_operand_byte(memory);
        cpu.cycle_count += 1;

        let (final_addr, page_crossed) = self::calc_indirect_indexed(cpu, addr, memory);
        // TODO: dummy read here (if page crossed)
        cpu.cycle_count += page_crossed as i16;

        let res = memory.read(final_addr, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
        res
    }

    pub fn write_indirect_indexed(cpu: &mut Cpu, val: u8, memory: &mut dyn CpuMemoryMap) {
        let addr = cpu.fetch_operand_byte(memory);
        cpu.cycle_count += 1;

        let (final_addr, _) = self::calc_indirect_indexed(cpu, addr, memory);
        // TODO: dummy read here
        cpu.cycle_count += 1;

        memory.write(final_addr, val, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }

    fn calc_indirect_indexed(
        cpu: &mut Cpu,
        addr: u8,
        memory: &mut dyn CpuMemoryMap,
    ) -> (u16, bool) {
        // get address at memory[addr]
        let dest_addr = {
            let low = memory.read(addr as u16, cpu);
            cpu.cycle_count += 1;

            let high = memory.read(addr.wrapping_add(1) as u16, cpu);
            cpu.cycle_count += 1;

            [low, high]
        };

        // add index to address while keeping track of whether a page boundary was crossed
        let (indexed_addr_low, carry) = dest_addr[0].overflowing_add(cpu.y);
        let indexed_addr_hi = dest_addr[1].wrapping_add(carry as u8);

        let indexed_addr = u16::from_le_bytes([indexed_addr_low, indexed_addr_hi]);

        (indexed_addr, carry)
    }
}
