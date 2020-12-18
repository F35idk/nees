use crate::PixelRenderer;

use std::cell::Cell;

#[allow(clippy::cast_ptr_alignment)]
pub fn pixels_to_u32<'a>(pixel_renderer: &'a PixelRenderer) -> &'a [Cell<u32>; 256 * 240] {
    debug_assert_eq!((pixel_renderer.get_pixels().as_ptr() as usize) % 64, 0);
    // SAFETY: the 'get_pixels()' byte slice is always aligned to at least 64
    // bytes, making the cast safe. this is ensured by the vulkan implementation
    // itself ('PixelRenderer' uses vulkan). see table 52, 'required limits' in:
    // https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-device-hostaccess
    unsafe { &*(pixel_renderer.get_pixels().as_ptr() as *const _) }
}

macro_rules! error_exit {
    ($($arg:tt)*) => { {
        if cfg!(test) {
            panic!($($arg)*);
        } else {
            eprintln!($($arg)*);
            std::process::exit(1);
        }
    } }
}

macro_rules! logln {
    ($( $args:expr ),*) => { if cfg!(feature = "logging") { println!( $( $args ),* ); } }
}

macro_rules! log {
    ($( $args:expr ),*) => { if cfg!(feature = "logging") { print!( $( $args ),* ); } }
}
