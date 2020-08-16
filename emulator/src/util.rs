use super::PixelRenderer;
use super::{apu, cpu, memory_map as mmap, ppu};
use std::marker::PhantomData;
use std::mem::transmute;
use std::ops::{Deref, DerefMut};

pub fn pixels_to_u32<'a>(pixel_renderer: &'a mut PixelRenderer) -> &'a mut [u32; 256 * 240] {
    unsafe { transmute(pixel_renderer.get_pixels().as_mut_ptr() as *mut u32) }
}

macro_rules! logln {
    ($( $args:expr ),*) => { if cfg!(feature = "logging") { println!( $( $args ),* ); } }
}

macro_rules! log {
    ($( $args:expr ),*) => { if cfg!(feature = "logging") { print!( $( $args ),* ); } }
}
