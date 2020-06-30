mod parse;

use parse::RomInfo;

fn main() {
    let rom = std::fs::read("nestest.nes").unwrap();
    println!("{}", std::str::from_utf8(&rom[0..=3]).unwrap());

    let info = RomInfo::new(&rom).unwrap();
    println!("is nes 2.0: {}", info.is_nes_2_format());
    println!("has trainer: {}", info.has_trainer());
    println!("mirroring type: {:?}", info.get_mirroring_type());
    println!("mapper number: {}", info.get_mapper_num());
    println!("prg rom size: {}KB", info.get_prg_size() as u32 * 16);
    println!("chr rom size: {}KB", info.get_chr_size() as u32 * 8);
}
