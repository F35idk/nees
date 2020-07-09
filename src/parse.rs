use std::mem::transmute;

#[derive(Debug)]
pub enum MirroringType {
    Hor = 0,
    Vert = 1,
    FourScreen = 0xf,
}

pub fn is_valid(rom_header: &[u8]) -> bool {
    if rom_header[0..=3] != [b'N', b'E', b'S', 0x1a] {
        // magic number is not present
        return false;
    }

    if rom_header[12..=15] != [0; 4] && (rom_header[7] & 0b1100) == 8 {
        // last 4 bytes are not zero and header isn't in nes 2.0 format
        return false;
    }

    true
}

pub fn get_prg_size(rom_header: &[u8]) -> u8 {
    rom_header[4]
}

// NOTE: if zero, game uses chr /ram/, not chr rom_header
pub fn get_chr_size(rom_header: &[u8]) -> u8 {
    rom_header[5]
}

pub fn get_mapper_num(rom_header: &[u8]) -> u8 {
    (rom_header[7] & 0xf0) + ((rom_header[6] & 0xf0) >> 4)
}

pub fn get_mirroring_type(rom_header: &[u8]) -> MirroringType {
    unsafe { transmute((rom_header[6] & 1) | ((rom_header[6] & 0b1000) >> 3) * 0xf) }
}

// NOTE: all persistent memory is not the same
pub fn has_persistent_mem(rom_header: &[u8]) -> bool {
    (rom_header[6] & 0b10) != 0
}

// NOTE: we ignore trainers anyway
pub fn has_trainer(rom_header: &[u8]) -> bool {
    (rom_header[6] & 0b100) != 0
}

pub fn is_nes_2_format(rom_header: &[u8]) -> bool {
    (rom_header[7] & 0b1100) == 8
}

// fn parse_header(rom_header: &[u8]) {
//     if rom_header[0..=3] != [b'N', b'E', b'S', 0x1a] {
//         return;
//     }
//
//     let prg_size = rom_header[4];
//     // NOTE: if zero, game uses chr /ram/, not chr rom_header
//     let chr_size = rom_header[5];
//
//     let flags_6 = rom_header[6];
//     let flags_7 = rom_header[7];
//
//     let mapper_num_low = flags_6 & 0b11110000;
//     let mapper_num_hi = flags_7 & 0b11110000;
//     let mapper_num = mapper_num_hi + mapper_num_low >> 4;
//     let vert_mirroring = (flags_6 & 1) != 0;
// }
