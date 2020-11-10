use super::PixelRenderer;
use super::{apu, controller as ctrl, cpu, memory_map as mem, ppu, win};
use std::mem::transmute;

pub fn pixels_to_u32<'a>(pixel_renderer: &'a mut PixelRenderer) -> &'a mut [u32; 256 * 240] {
    unsafe { transmute(pixel_renderer.get_pixels().as_mut_ptr() as *mut u32) }
}

// used by test functions
pub fn init_nes() -> (cpu::Cpu, mem::NromCpuMemory) {
    let mut win = win::XcbWindowWrapper::new("test", 20, 20).unwrap();
    let renderer = PixelRenderer::new(&mut win.connection, win.win, 256, 240).unwrap();

    let ppu_memory = mem::NromPpuMemory::new(false);
    let ppu = ppu::Ppu::new();
    let apu = apu::Apu {};
    let cpu = cpu::Cpu::default();
    let controller = ctrl::Controller::default();
    let cpu_memory =
        mem::NromCpuMemory::new_empty(0x4000, ppu, ppu_memory, apu, controller, renderer);

    (cpu, cpu_memory)
}

pub fn reset_nes_state(cpu: &mut cpu::Cpu, cpu_memory: &mut mem::NromCpuMemory) {
    cpu_memory.base.ppu.reset_state();
    cpu_memory.base.controller = ctrl::Controller::default();
    cpu_memory.base.apu = apu::Apu {};
    *cpu = cpu::Cpu::default();
}

macro_rules! logln {
    ($( $args:expr ),*) => { if cfg!(feature = "logging") { println!( $( $args ),* ); } }
}

macro_rules! log {
    ($( $args:expr ),*) => { if cfg!(feature = "logging") { print!( $( $args ),* ); } }
}

macro_rules! bitfield {
    ( $name:ident<$field_type:ty> ($( $field:ident: $lower:literal..$upper:literal ),*)) => {
        #[allow(non_snake_case)]
        pub mod $name {
            #[derive(Default)]
            pub struct Fields {
                bits: $field_type,
                $(pub $field: $field,)*
            }

            const HIGHEST_BIT: $field_type = (::std::mem::size_of::<$field_type>() * 8 - 1) as $field_type;

            $(
                #[allow(non_camel_case_types)]
                #[derive(Default)]
                pub struct $field {
                    _empty: (),
                }

                impl $field {
                    #[inline]
                    #[allow(dead_code)]
                    pub fn get(&self) -> $field_type {
                        let fields: &Fields = unsafe { &*(self as *const _ as *const Fields)};
                        (fields.bits << (HIGHEST_BIT - $upper)) >> (HIGHEST_BIT - $upper + $lower)
                    }

                    #[inline]
                    #[allow(dead_code)]
                    pub fn set(&mut self, val: $field_type) {
                        let fields: &mut Fields = unsafe { &mut *(self as *mut _ as *mut Fields)};
                        let clear_mask = ((fields.bits << (HIGHEST_BIT - $upper)) >> (HIGHEST_BIT - $upper + $lower)) << $lower;

                        fields.bits ^= clear_mask;
                        fields.bits |= ((val as $field_type) << ($lower));
                    }

                    #[inline]
                    #[allow(dead_code)]
                    pub fn is_true(&self) -> bool {
                        let fields: &Fields = unsafe { &*(self as *const _ as *const Fields)};
                        (fields.bits & (1 << ($lower))) != 0
                    }
                }
            )*

            #[derive(Default)]
            pub struct BitField {
                _inner: Fields,
            }

            impl BitField {
                #[inline]
                #[allow(dead_code)]
                pub fn zeroed() -> Self {
                    Self {
                        _inner: Fields {
                            bits: 0,
                            $($field: $field { _empty: () },)*
                        }
                    }
                }

                #[inline]
                #[allow(dead_code)]
                pub fn new($($field: $field_type,)*) -> Self {
                    let mut new = Self::zeroed();

                    $(
                        new.$field.set($field);
                    )*

                    new
                }
            }

            impl ::std::ops::Deref for BitField {
                type Target = Fields;
                fn deref(&self) -> &Fields {
                    unsafe { &*(self as *const _ as *const Fields)}
                }
            }

            impl ::std::ops::DerefMut for BitField {
                fn deref_mut(&mut self) -> &mut Fields {
                    unsafe { &mut *(self as *mut _ as *mut Fields)}
                }
            }
        }
    };
    // handle trailing commas
    ( $name:ident<$field_type:ty> ($( $field:ident: $lower:literal..$upper:literal ),+ ,)) => {
        bitfield!($name<$field_type>($($field: $lower..$upper),*));
    };
}
