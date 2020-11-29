use super::Cpu;
use crate::address_bus::CpuAddressBus;

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

    pub fn read_imm(cpu: &mut Cpu, bus: &mut dyn CpuAddressBus) -> u8 {
        let val = cpu.fetch_operand_byte(bus);
        cpu.cycle_count += 1;

        cpu.pc += 1;
        val
    }
}

// absolute addressing
mod abs {
    use super::*;

    pub fn read_abs(cpu: &mut Cpu, bus: &mut dyn CpuAddressBus) -> u8 {
        let addr = cpu.fetch_operand_u16(bus);
        cpu.cycle_count += 1;

        let val = bus.read(addr, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
        val
    }

    pub fn write_abs(cpu: &mut Cpu, val: u8, bus: &mut dyn CpuAddressBus) {
        let addr = cpu.fetch_operand_u16(bus);
        cpu.cycle_count += 1;

        bus.write(addr, val, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }

    // fetches the absolute address at pc+1, performs 'operation' on the
    // value at the address and stores it back
    pub fn read_write_abs(
        cpu: &mut Cpu,
        bus: &mut dyn CpuAddressBus,
        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_u16(bus);
        cpu.cycle_count += 1;

        cpu.cycle_count += 2;
        let val = bus.read(addr, cpu);

        // TODO: dummy write here
        let res = operation(cpu, val);
        bus.write(addr, res, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }
}

// absolute indexed addressing
mod abs_indexed {
    use super::*;

    pub fn write_abs_indexed(cpu: &mut Cpu, val: u8, index: u8, bus: &mut dyn CpuAddressBus) {
        let addr = cpu.fetch_operand_u16(bus);
        let addr_indexed = addr.wrapping_add(index as u16);
        // TODO: dummy read here (addr_indexed fixup cycle)
        cpu.cycle_count += 2;

        bus.write(addr_indexed, val, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }

    pub fn read_abs_indexed(cpu: &mut Cpu, index: u8, bus: &mut dyn CpuAddressBus) -> u8 {
        let addr_bytes = cpu.fetch_operand_bytes(bus);
        let (addr_indexed, page_crossed) = {
            let (low, page_crossed) = addr_bytes[0].overflowing_add(index);
            let high = addr_bytes[1].wrapping_add(page_crossed as u8);
            let indexed = u16::from_le_bytes([low, high]);

            (indexed, page_crossed)
        };
        cpu.cycle_count += 1 + page_crossed as i16;

        let res = bus.read(addr_indexed, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
        res
    }

    pub fn read_write_abs_indexed(
        cpu: &mut Cpu,
        index: u8,
        bus: &mut dyn CpuAddressBus,
        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_u16(bus);
        let addr_indexed = addr.wrapping_add(index as u16);
        // TODO: dummy read here (is invalid if page crossed)
        cpu.cycle_count += 2;

        let val = bus.read(addr_indexed, cpu);
        cpu.cycle_count += 1;

        // TODO: dummy write here
        let res = operation(cpu, val);
        cpu.cycle_count += 1;

        bus.write(addr_indexed, res, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }
}

mod zero_page {
    use super::*;

    pub fn read_zero_page(cpu: &mut Cpu, bus: &mut dyn CpuAddressBus) -> u8 {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.cycle_count += 1;

        let res = bus.read(addr as u16, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
        res
    }

    pub fn write_zero_page(cpu: &mut Cpu, val: u8, bus: &mut dyn CpuAddressBus) {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.cycle_count += 1;

        bus.write(addr as u16, val, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }

    pub fn read_write_zero_page(
        cpu: &mut Cpu,
        bus: &mut dyn CpuAddressBus,
        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.cycle_count += 1;

        let val = bus.read(addr as u16, cpu);
        cpu.cycle_count += 2;
        // TODO: dummy write here

        let res = operation(cpu, val);
        bus.write(addr as u16, res, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }
}

mod zero_page_indexed {
    use super::*;

    pub fn read_zero_page_indexed(cpu: &mut Cpu, index: u8, bus: &mut dyn CpuAddressBus) -> u8 {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.cycle_count += 1;

        // TODO: dummy read from 'addr' here
        let addr_indexed = addr.wrapping_add(index);
        cpu.cycle_count += 1;

        let res = bus.read(addr_indexed as u16, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
        res
    }

    pub fn write_zero_page_indexed(cpu: &mut Cpu, val: u8, index: u8, bus: &mut dyn CpuAddressBus) {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.cycle_count += 1;

        // TODO: dummy read from 'addr' here
        let addr_indexed = addr.wrapping_add(index);
        cpu.cycle_count += 1;

        bus.write(addr_indexed as u16, val, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }

    pub fn read_write_zero_page_indexed(
        cpu: &mut Cpu,
        index: u8,
        bus: &mut dyn CpuAddressBus,
        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.cycle_count += 1;

        // TODO: dummy read from 'addr' here
        let addr_indexed = addr.wrapping_add(index);
        cpu.cycle_count += 1;

        let val = bus.read(addr_indexed as u16, cpu);
        cpu.cycle_count += 1;

        // TODO: dummy write here (write 'val' to 'addr_indexed')
        let res = operation(cpu, val);
        cpu.cycle_count += 1;

        bus.write(addr_indexed as u16, res, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }
}

mod indexed_indirect {
    use super::*;

    pub fn read_indexed_indirect(cpu: &mut Cpu, bus: &mut dyn CpuAddressBus) -> u8 {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.cycle_count += 1;

        let final_addr = self::calc_indexed_indirect(cpu, addr, bus);

        let res = bus.read(final_addr, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
        res
    }

    pub fn write_indexed_indirect(cpu: &mut Cpu, val: u8, bus: &mut dyn CpuAddressBus) {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.cycle_count += 1;

        let final_addr = self::calc_indexed_indirect(cpu, addr, bus);

        bus.write(final_addr, val, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }

    fn calc_indexed_indirect(cpu: &mut Cpu, addr: u8, bus: &mut dyn CpuAddressBus) -> u16 {
        // TODO: dummy read (from 'addr') here
        let addr_indexed = addr.wrapping_add(cpu.x);
        cpu.cycle_count += 1;

        let addr_lo = bus.read(addr_indexed as u16, cpu);
        cpu.cycle_count += 1;

        let addr_hi = bus.read(addr_indexed.wrapping_add(1) as u16, cpu);
        cpu.cycle_count += 1;

        u16::from_le_bytes([addr_lo, addr_hi])
    }
}

mod indirect_indexed {
    use super::*;

    pub fn read_indirect_indexed(cpu: &mut Cpu, bus: &mut dyn CpuAddressBus) -> u8 {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.cycle_count += 1;

        let (final_addr, page_crossed) = self::calc_indirect_indexed(cpu, addr, bus);
        // TODO: dummy read here (if page crossed)
        cpu.cycle_count += page_crossed as i16;

        let res = bus.read(final_addr, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
        res
    }

    pub fn write_indirect_indexed(cpu: &mut Cpu, val: u8, bus: &mut dyn CpuAddressBus) {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.cycle_count += 1;

        let (final_addr, _) = self::calc_indirect_indexed(cpu, addr, bus);
        // TODO: dummy read here
        cpu.cycle_count += 1;

        bus.write(final_addr, val, cpu);
        cpu.cycle_count += 1;

        cpu.pc += 1;
    }

    fn calc_indirect_indexed(cpu: &mut Cpu, addr: u8, bus: &mut dyn CpuAddressBus) -> (u16, bool) {
        // get address at bus[addr]
        let dest_addr = {
            let low = bus.read(addr as u16, cpu);
            cpu.cycle_count += 1;

            let high = bus.read(addr.wrapping_add(1) as u16, cpu);
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
