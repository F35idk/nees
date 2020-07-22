
    pub fn read_register_by_index(&mut self, index: u8) -> u8 {
        assert!(index < 8);

        // TODO: if reading ppustatus, clear bit 7 and some other stuff
        // get the right register using spooky (but safe) pointer arithmetic
        unsafe {
            let start_ptr = &mut self.registers.ppuctrl as *mut u8;

            if index >= 5 {
                // if index is greater than 5, an offset is needed to compensate
                // for the two-byte width of 'ppuscroll' and 'ppuaddr'
                let mut offset = index - 5;
                // if index == 5 (ppuscroll) or 6 (ppuaddr), add the 'address_latch_toggle'
                // bit to 'offset' to get the high/low byte of the register
                offset += (self.registers.address_latch_toggle as u8) & (index < 7) as u8;

                *((start_ptr as usize + index as usize + offset as usize) as *const u8)
            } else {
                *((start_ptr as usize + index as usize) as *const u8)
            }
        }
    }

    pub fn write_register_by_index(&mut self, index: u8, val: u8) {
        assert!(index < 8);

        unsafe {
            let start_ptr = &mut self.registers.ppuctrl as *mut u8;

            if index >= 5 {
                let mut offset = index - 5;
                offset += (self.registers.address_latch_toggle as u8) & (index < 7) as u8;
                let dest_ptr = (start_ptr as usize + index as usize + offset as usize) as *mut u8;

                *dest_ptr = val;
            } else {
                let dest_ptr = (start_ptr as usize + index as usize) as *mut u8;

                *dest_ptr = val;
            }
        }
    }
