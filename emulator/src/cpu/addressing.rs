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
        cpu.pc += 1;
        cpu.cycle_count += 1;

        val
    }
}

// absolute addressing
mod abs {
    use super::*;

    pub fn read_abs(cpu: &mut Cpu, bus: &mut dyn CpuAddressBus) -> u8 {
        let addr = cpu.fetch_operand_u16(bus);
        cpu.cycle_count += 1;
        cpu.pc += 1;

        let val = bus.read(addr, cpu);
        cpu.cycle_count += 1;

        val
    }

    pub fn write_abs(cpu: &mut Cpu, val: u8, bus: &mut dyn CpuAddressBus) {
        let addr = cpu.fetch_operand_u16(bus);
        cpu.cycle_count += 1;
        cpu.pc += 1;

        bus.write(addr, val, cpu);
        cpu.cycle_count += 1;
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
        cpu.pc += 1;

        let val = bus.read(addr, cpu);
        cpu.cycle_count += 1;

        bus.write(addr, val, cpu);
        cpu.cycle_count += 1;

        let res = operation(cpu, val);
        bus.write(addr, res, cpu);
        cpu.cycle_count += 1;
    }
}

// absolute indexed addressing
mod abs_indexed {
    use super::*;

    pub fn read_abs_indexed(cpu: &mut Cpu, index: u8, bus: &mut dyn CpuAddressBus) -> u8 {
        let addr_bytes = cpu.fetch_operand_bytes(bus);
        let (addr_indexed_low, page_crossed) = addr_bytes[0].overflowing_add(index);
        let addr_indexed_without_carry = u16::from_le_bytes([addr_indexed_low, addr_bytes[1]]);
        cpu.pc += 1;
        cpu.cycle_count += 1;

        let res = {
            let first_read_attempt = bus.read(addr_indexed_without_carry, cpu);
            cpu.cycle_count += 1;

            if !page_crossed {
                first_read_attempt
            } else {
                let addr_indexed_high = addr_bytes[1].wrapping_add(page_crossed as u8);
                let addr_indexed_with_carry =
                    u16::from_le_bytes([addr_indexed_low, addr_indexed_high]);

                let second_read_attempt = bus.read(addr_indexed_with_carry, cpu);
                cpu.cycle_count += 1;
                second_read_attempt
            }
        };

        res
    }

    pub fn write_abs_indexed(cpu: &mut Cpu, val: u8, index: u8, bus: &mut dyn CpuAddressBus) {
        let addr_bytes = cpu.fetch_operand_bytes(bus);
        let (addr_indexed_low, page_crossed) = addr_bytes[0].overflowing_add(index);
        let addr_indexed_without_carry = u16::from_le_bytes([addr_indexed_low, addr_bytes[1]]);
        cpu.pc += 1;
        cpu.cycle_count += 1;

        // perform dummy read before carry has been added to high byte of address
        let _ = bus.read(addr_indexed_without_carry, cpu);
        let addr_indexed_high = addr_bytes[1].wrapping_add(page_crossed as u8);
        let addr_indexed_with_carry = u16::from_le_bytes([addr_indexed_low, addr_indexed_high]);
        cpu.cycle_count += 1;

        bus.write(addr_indexed_with_carry, val, cpu);
        cpu.cycle_count += 1;
    }

    pub fn read_write_abs_indexed(
        cpu: &mut Cpu,
        index: u8,
        bus: &mut dyn CpuAddressBus,
        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr_bytes = cpu.fetch_operand_bytes(bus);
        let (addr_indexed_low, page_crossed) = addr_bytes[0].overflowing_add(index);
        let addr_indexed_without_carry = u16::from_le_bytes([addr_indexed_low, addr_bytes[1]]);
        cpu.pc += 1;
        cpu.cycle_count += 1;

        let _ = bus.read(addr_indexed_without_carry, cpu);
        let addr_indexed_high = addr_bytes[1].wrapping_add(page_crossed as u8);
        let addr_indexed_with_carry = u16::from_le_bytes([addr_indexed_low, addr_indexed_high]);
        cpu.cycle_count += 1;

        let val = bus.read(addr_indexed_with_carry, cpu);
        cpu.cycle_count += 1;

        // perform dummy write before calling 'operation()' on 'val'
        bus.write(addr_indexed_with_carry, val, cpu);
        let res = operation(cpu, val);
        cpu.cycle_count += 1;

        bus.write(addr_indexed_with_carry, res, cpu);
        cpu.cycle_count += 1;
    }
}

mod zero_page {
    use super::*;

    pub fn read_zero_page(cpu: &mut Cpu, bus: &mut dyn CpuAddressBus) -> u8 {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.pc += 1;
        cpu.cycle_count += 1;

        let res = bus.read(addr as u16, cpu);
        cpu.cycle_count += 1;

        res
    }

    pub fn write_zero_page(cpu: &mut Cpu, val: u8, bus: &mut dyn CpuAddressBus) {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.pc += 1;
        cpu.cycle_count += 1;

        bus.write(addr as u16, val, cpu);
        cpu.cycle_count += 1;
    }

    pub fn read_write_zero_page(
        cpu: &mut Cpu,
        bus: &mut dyn CpuAddressBus,
        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.pc += 1;
        cpu.cycle_count += 1;

        let val = bus.read(addr as u16, cpu);
        cpu.cycle_count += 1;

        // perform dummy write before calling 'operation()' on 'val'
        bus.write(addr as u16, val, cpu);
        let res = operation(cpu, val);
        cpu.cycle_count += 1;

        bus.write(addr as u16, res, cpu);
        cpu.cycle_count += 1;
    }
}

mod zero_page_indexed {
    use super::*;

    pub fn read_zero_page_indexed(cpu: &mut Cpu, index: u8, bus: &mut dyn CpuAddressBus) -> u8 {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.pc += 1;
        cpu.cycle_count += 1;

        // perform dummy read before adding 'index' to 'addr'
        let _ = bus.read(addr as u16, cpu);
        let addr_indexed = addr.wrapping_add(index);
        cpu.cycle_count += 1;

        let res = bus.read(addr_indexed as u16, cpu);
        cpu.cycle_count += 1;

        res
    }

    pub fn write_zero_page_indexed(cpu: &mut Cpu, val: u8, index: u8, bus: &mut dyn CpuAddressBus) {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.pc += 1;
        cpu.cycle_count += 1;

        let _ = bus.read(addr as u16, cpu);
        let addr_indexed = addr.wrapping_add(index);
        cpu.cycle_count += 1;

        bus.write(addr_indexed as u16, val, cpu);
        cpu.cycle_count += 1;
    }

    pub fn read_write_zero_page_indexed(
        cpu: &mut Cpu,
        index: u8,
        bus: &mut dyn CpuAddressBus,
        operation: fn(&mut Cpu, u8) -> u8,
    ) {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.pc += 1;
        cpu.cycle_count += 1;

        let _ = bus.read(addr as u16, cpu);
        let addr_indexed = addr.wrapping_add(index);
        cpu.cycle_count += 1;

        let val = bus.read(addr_indexed as u16, cpu);
        cpu.cycle_count += 1;

        bus.write(addr_indexed as u16, val, cpu);
        let res = operation(cpu, val);
        cpu.cycle_count += 1;

        bus.write(addr_indexed as u16, res, cpu);
        cpu.cycle_count += 1;
    }
}

mod indexed_indirect {
    use super::*;

    pub fn read_indexed_indirect(cpu: &mut Cpu, bus: &mut dyn CpuAddressBus) -> u8 {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.pc += 1;
        cpu.cycle_count += 1;

        let final_addr = self::calc_indexed_indirect(cpu, addr, bus);

        let res = bus.read(final_addr, cpu);
        cpu.cycle_count += 1;

        res
    }

    pub fn write_indexed_indirect(cpu: &mut Cpu, val: u8, bus: &mut dyn CpuAddressBus) {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.pc += 1;
        cpu.cycle_count += 1;

        let final_addr = self::calc_indexed_indirect(cpu, addr, bus);

        bus.write(final_addr, val, cpu);
        cpu.cycle_count += 1;
    }

    fn calc_indexed_indirect(cpu: &mut Cpu, addr: u8, bus: &mut dyn CpuAddressBus) -> u16 {
        // perform dummy read before adding 'x' to 'addr'
        let _ = bus.read(addr as u16, cpu);
        let addr_indexed = addr.wrapping_add(cpu.x);
        cpu.cycle_count += 1;

        let final_addr_lo = bus.read(addr_indexed as u16, cpu);
        cpu.cycle_count += 1;

        let final_addr_hi = bus.read(addr_indexed.wrapping_add(1) as u16, cpu);
        cpu.cycle_count += 1;

        u16::from_le_bytes([final_addr_lo, final_addr_hi])
    }
}

mod indirect_indexed {
    use super::*;

    pub fn read_indirect_indexed(cpu: &mut Cpu, bus: &mut dyn CpuAddressBus) -> u8 {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.pc += 1;
        cpu.cycle_count += 1;

        let (addr_indexed_without_carry, page_crossed) =
            self::calc_addr_without_carry(cpu, addr, bus);

        let res = {
            // read from address before applying carry to high bits
            let first_read_attempt = bus.read(u16::from_le_bytes(addr_indexed_without_carry), cpu);
            cpu.cycle_count += 1;

            if !page_crossed {
                // if a page wasn't crossed, the address read from was the correct one
                first_read_attempt
            } else {
                // if a page was crossed, add carry to high bits of address before reading from it
                let addr_indexed_high =
                    addr_indexed_without_carry[1].wrapping_add(page_crossed as u8);
                let addr_indexed_with_carry =
                    u16::from_le_bytes([addr_indexed_without_carry[0], addr_indexed_high]);
                let second_read_attempt = bus.read(addr_indexed_with_carry, cpu);
                cpu.cycle_count += 1;
                second_read_attempt
            }
        };

        res
    }

    pub fn write_indirect_indexed(cpu: &mut Cpu, val: u8, bus: &mut dyn CpuAddressBus) {
        let addr = cpu.fetch_operand_byte(bus);
        cpu.pc += 1;
        cpu.cycle_count += 1;

        let (addr_indexed_without_carry, page_crossed) =
            self::calc_addr_without_carry(cpu, addr, bus);

        // perform dummy read from address before adding carry to high bits
        let _ = bus.read(u16::from_le_bytes(addr_indexed_without_carry), cpu);
        let addr_indexed_high = addr_indexed_without_carry[1].wrapping_add(page_crossed as u8);
        let addr_indexed_with_carry =
            u16::from_le_bytes([addr_indexed_without_carry[0], addr_indexed_high]);
        cpu.cycle_count += 1;

        bus.write(addr_indexed_with_carry, val, cpu);
        cpu.cycle_count += 1;
    }

    fn calc_addr_without_carry(
        cpu: &mut Cpu,
        addr: u8,
        bus: &mut dyn CpuAddressBus,
    ) -> ([u8; 2], bool) {
        // get address at bus[addr]
        let dest_addr = {
            let low = bus.read(addr as u16, cpu);
            cpu.cycle_count += 1;

            let high = bus.read(addr.wrapping_add(1) as u16, cpu);
            cpu.cycle_count += 1;

            [low, high]
        };

        // add index to low bytes of address and keep track of the carry
        let (dest_addr_indexed_low, carry) = dest_addr[0].overflowing_add(cpu.y);

        ([dest_addr_indexed_low, dest_addr[1]], carry)
    }
}
