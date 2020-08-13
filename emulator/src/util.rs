use super::{apu, cpu, ppu};

// FIXME: comment on safety (or lack thereof)

// convenience/helper struct for passing around important pointers,
// to reduce amt. of function arguments everywhere, etc.
pub struct PtrsWrapper<'a, 'b> {
    pub cpu: *mut cpu::Cpu,
    pub ppu: &'a mut ppu::Ppu,
    pub apu: &'b mut apu::Apu,
    pub framebuffer: *mut u32,
}

pub fn pixels_to_u32<'a>(pixel_renderer: &'a mut super::PixelRenderer) -> &'a mut [u32; 256 * 240] {
    unsafe { std::mem::transmute(pixel_renderer.get_pixels().as_mut_ptr() as *mut u32) }
}

macro_rules! logln {
    ($( $args:expr ),*) => { if cfg!(feature = "logging") { println!( $( $args ),* ); } }
}

macro_rules! log {
    ($( $args:expr ),*) => { if cfg!(feature = "logging") { print!( $( $args ),* ); } }
}
