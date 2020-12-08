use crate::win;
#[macro_use]
use crate::util;

#[derive(Default)]
pub struct Controller {
    key_state_bitmap: u8,
    key_state_shift_reg: u8,
    strobe_bits: StrobeBits::BitField,
}

bitfield!(StrobeBits<u8>(
    strobe_enable: 0..0,
    shift_enable: 1..1,
));

impl Controller {
    pub fn set_key(&mut self, key: u32) {
        match key {
            win::Keys::SPACE => self.key_state_bitmap |= 0b1,
            win::Keys::SHIFT => self.key_state_bitmap |= 0b10,
            win::Keys::F => self.key_state_bitmap |= 0b100,
            win::Keys::TAB => self.key_state_bitmap |= 0b1000,
            win::Keys::W => self.key_state_bitmap |= 0b10000,
            win::Keys::S => self.key_state_bitmap |= 0b100000,
            win::Keys::A => self.key_state_bitmap |= 0b1000000,
            win::Keys::D => self.key_state_bitmap |= 0b10000000,
            _ => (),
        }
    }

    pub fn unset_key(&mut self, key: u32) {
        match key {
            win::Keys::SPACE => self.key_state_bitmap &= !0b1,
            win::Keys::SHIFT => self.key_state_bitmap &= !0b10,
            win::Keys::F => self.key_state_bitmap &= !0b100,
            win::Keys::TAB => self.key_state_bitmap &= !0b1000,
            win::Keys::W => self.key_state_bitmap &= !0b10000,
            win::Keys::S => self.key_state_bitmap &= !0b100000,
            win::Keys::A => self.key_state_bitmap &= !0b1000000,
            win::Keys::D => self.key_state_bitmap &= !0b10000000,
            _ => (),
        }
    }

    pub fn write(&mut self, val: u8) {
        match val & 1 {
            0 => {
                if self.strobe_bits.strobe_enable.is_true() {
                    self.strobe_bits.strobe_enable.set(0);
                    self.strobe_bits.shift_enable.set(1);
                    self.key_state_shift_reg = self.key_state_bitmap;
                }
            }
            1 => {
                self.strobe_bits.strobe_enable.set(1);
                self.strobe_bits.shift_enable.set(0);
            }
            _ => (),
        }
    }

    pub fn read(&mut self) -> u8 {
        // TODO: upper bits open bus behavior

        if self.strobe_bits.shift_enable.is_true() {
            let val = (self.key_state_shift_reg & 1) | 0b1000000;

            self.key_state_shift_reg >>= 1;
            self.key_state_shift_reg |= 0b10000000;

            val
        } else {
            (self.key_state_bitmap & 1) | 0b1000000
        }
    }
}
