use super::apu;
use super::ppu;

// convenience/helper struct for passing around important pointers,
// to reduce amt. of function arguments everywhere, etc.
pub struct PtrsWrapper<'a, 'b, 'c> {
    pub ppu: &'a mut ppu::Ppu,
    pub apu: &'b mut apu::Apu,
    pub cpu_cycles: &'c mut u64,
}

pub fn pixels_to_u32<'a>(pixel_renderer: &'a mut super::PixelRenderer) -> &'a mut [u32] {
    unsafe {
        std::slice::from_raw_parts_mut(
            pixel_renderer.get_pixels().as_mut_ptr() as *mut u32,
            pixel_renderer.get_pixels().len() / 4,
        )
    }
}

macro_rules! logln {
    ($( $args:expr ),*) => { if cfg!(feature = "logging") { println!( $( $args ),* ); } }
}

macro_rules! log {
    ($( $args:expr ),*) => { if cfg!(feature = "logging") { print!( $( $args ),* ); } }
}
