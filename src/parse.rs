use std::mem::transmute;

pub struct RomInfo<'a> {
    pub header: &'a [u8; 16],
}

#[derive(Debug)]
pub enum MirroringType {
    Hor = 0,
    Vert = 1,
    FourScreen = 0xf,
}

impl<'a> RomInfo<'a> {
    pub fn new(rom: &'a [u8]) -> Option<RomInfo<'a>> {
        if rom[0..=3] != [b'N', b'E', b'S', 0x1a] {
            // magic number is not present
            return None;
        }

        if rom[12..=15] != [0; 4] && (rom[7] & 0b1100) == 8 {
            // last 4 bytes are not zero and header isn't in nes 2.0 format
            return None;
        }

        Some(Self {
            header: unsafe { transmute::<*const u8, &[u8; 16]>(rom.as_ptr()) },
        })
    }

    pub fn get_prg_size(&self) -> u8 {
        self.header[4]
    }

    // NOTE: if zero, game uses chr /ram/, not chr rom
    pub fn get_chr_size(&self) -> u8 {
        self.header[5]
    }

    pub fn get_mapper_num(&self) -> u8 {
        (self.header[7] & 0xf0) + ((self.header[6] & 0xf0) >> 4)
    }

    pub fn get_mirroring_type(&self) -> MirroringType {
        unsafe { transmute((self.header[6] & 1) | ((self.header[6] & 0b1000) >> 3) * 0xf) }
    }

    // NOTE: all persistent memory is not the same
    pub fn has_persistent_mem(&self) -> bool {
        (self.header[6] & 0b10) != 0
    }

    // NOTE: we ignore trainers anyway
    pub fn has_trainer(&self) -> bool {
        (self.header[6] & 0b100) != 0
    }

    pub fn is_nes_2_format(&self) -> bool {
        (self.header[7] & 0b1100) == 8
    }
}

// fn parse_header(rom: &[u8]) {
//     if rom[0..=3] != [b'N', b'E', b'S', 0x1a] {
//         return;
//     }
//
//     let prg_size = rom[4];
//     // NOTE: if zero, game uses chr /ram/, not chr rom
//     let chr_size = rom[5];
//
//     let flags_6 = rom[6];
//     let flags_7 = rom[7];
//
//     let mapper_num_low = flags_6 & 0b11110000;
//     let mapper_num_hi = flags_7 & 0b11110000;
//     let mapper_num = mapper_num_hi + mapper_num_low >> 4;
//     let vert_mirroring = (flags_6 & 1) != 0;
// }
