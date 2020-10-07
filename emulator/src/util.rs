use super::PixelRenderer;
use super::{apu, controller as ctrl, cpu, memory_map as mem, ppu, win};
use std::mem::transmute;

pub fn pixels_to_u32<'a>(pixel_renderer: &'a mut PixelRenderer) -> &'a mut [u32; 256 * 240] {
    unsafe { transmute(pixel_renderer.get_pixels().as_mut_ptr() as *mut u32) }
}

// used by test functions
pub fn init_nes() -> (cpu::Cpu, mem::Nrom128CpuMemory<'static>) {
    let mut win = win::XcbWindowWrapper::new("test", 20, 20).unwrap();
    let renderer = PixelRenderer::new(&mut win.connection, win.win, 256, 240).unwrap();

    let ppu_memory = Box::leak(Box::new(mem::NromPpuMemory::new()));
    let ppu = ppu::Ppu::new(renderer, ppu_memory);
    let apu = apu::Apu {};
    let cpu = cpu::Cpu::default();
    let controller = ctrl::Controller::default();
    let cpu_memory = mem::Nrom128CpuMemory::new(ppu, apu, controller);

    (cpu, cpu_memory)
}

pub fn reset_nes_state(cpu: &mut cpu::Cpu, cpu_memory: &mut mem::Nrom128CpuMemory<'static>) {
    cpu_memory.ppu.reset_state();
    cpu_memory.controller = ctrl::Controller::default();
    cpu_memory.apu = apu::Apu {};
    *cpu = cpu::Cpu::default();
}

macro_rules! logln {
    ($( $args:expr ),*) => { if cfg!(feature = "logging") { println!( $( $args ),* ); } }
}

macro_rules! log {
    ($( $args:expr ),*) => { if cfg!(feature = "logging") { print!( $( $args ),* ); } }
}
