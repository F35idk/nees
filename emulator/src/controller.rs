use super::win;

#[derive(Default)]
pub struct Controller {
    key_state_bitmap: u8,
    key_state_shift_reg: u8,
    strobe_bits: u8,
}

impl Controller {
    pub fn is_strobe_enable(&self) -> bool {
        self.strobe_bits & 1 != 0
    }

    fn set_strobe(&mut self, strobe: bool) {
        self.strobe_bits = (self.strobe_bits & !1) | (strobe as u8);
    }

    fn is_shift_enable(&self) -> bool {
        self.strobe_bits & 2 != 0
    }

    fn set_shift(&mut self, mode: bool) {
        self.strobe_bits = (self.strobe_bits & !2) | ((mode as u8) << 1);
    }

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
                if self.is_strobe_enable() {
                    self.set_strobe(false);
                    self.set_shift(true);
                    self.key_state_shift_reg = self.key_state_bitmap;
                }
            }
            1 => {
                self.set_strobe(true);
                self.set_shift(false);
            }
            _ => (),
        }
    }

    pub fn read(&mut self) -> u8 {
        if self.is_shift_enable() {
            let val = (self.key_state_shift_reg & 1) | 0b1000000;

            self.key_state_shift_reg >>= 1;
            self.key_state_shift_reg |= 0b10000000;

            val
        } else {
            (self.key_state_bitmap & 1) | 0b1000000
        }
    }
}

mod test {
    // TODO: !!!!!!!!!
}
