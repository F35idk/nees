use super::PixelRenderer;
use super::{apu, cpu, memory_map as mmap, ppu};
use std::marker::PhantomData;
use std::mem::transmute;
use std::ops::{Deref, DerefMut};

// zero-size convenience/helper struct for passing around pointers. this struct is
// never instantiated - only a pointer to it is passed around. this pointer is taken
// from the 'super::Nes' struct (see 'NesContext::new()'). each field on 'NesContext'
// is then used to represent a pointer or some set of pointers to fields on this
// 'super::Nes' struct. when these pointers are dereferenced, they are upcast to the
// field on the 'Nes' struct they correspond to (see the 'Deref' impls for 'ContextPtr'
// below). this is all kept (reasonably) safe by the fact that each field mutably
// borrows what it 'points to' as if it were an ordinary mutable reference.
//
// the primary reason i came up with all this bs is that it provides a convenient way
// of splitting borrows, while still allowing multiple pointers to be grouped together
// to reduce the length of function arguments everywhere. for example, the 'CpuContext'
// field borrows only the 'Ppu', 'Apu' and 'PixelRenderer' structs, while the
// 'MemoryMapContext' borrows all of these, plus the 'Cpu' struct. neither of these
// borrow the 'Nrom128MemoryMap' field, so this can be accessed at the same time as
// either of the others without running into issues with the borrow checker.
pub struct NesContext<'c, 'p, 'a, 'r, 'm> {
    pub cpu_ctx: cpu::CpuContext<'p, 'a, 'r>,
    pub memory_ctx: mmap::MemoryMapContext<'c, 'p, 'a, 'r>,
    pub cpu: ContextPtr<'c, cpu::Cpu>,
    pub ppu: ContextPtr<'p, ppu::Ppu>,
    pub apu: ContextPtr<'a, apu::Apu>,
    pub renderer: ContextPtr<'r, super::PixelRenderer>,
    pub memory: ContextPtr<'m, mmap::Nrom128MemoryMap>,
}

impl<'s> NesContext<'s, 's, 's, 's, 's> {
    pub fn new<'a>(nes: &'a mut super::Nes) -> &'a mut Self {
        unsafe { transmute(nes) }
    }
}

// upcasts 'nes_ptr' to an '*mut Nes' pointer and checks that 'field_ptr'
// matches the address of one of the fields on the this 'Nes' struct
// FIXME: this function does not need to be generic (or even a function at
// all) as it is only used in a single place (by 'CpuContext', so it can
// cast itself to a 'MemoryMapContext')
pub fn is_nes_field<F, N>(nes_ptr: &mut N, field_ptr: &mut F) -> bool {
    let nes: *mut super::Nes = unsafe { transmute(nes_ptr) };
    let field_addr = field_ptr as *mut _ as usize;

    unsafe {
        field_addr == &mut (*nes).cpu as *mut _ as usize
            || field_addr == &mut (*nes).ppu as *mut _ as usize
            || field_addr == &mut (*nes).apu as *mut _ as usize
            || field_addr == &mut (*nes).renderer as *mut _ as usize
    }
}

// struct representing a zero-sized pointer. the 'Deref' impls
// below are what provide the upcasting behavior
pub struct ContextPtr<'a, T> {
    phantom: PhantomData<&'a mut T>,
}

impl<'a> Deref for ContextPtr<'a, cpu::Cpu> {
    type Target = cpu::Cpu;

    fn deref(&self) -> &Self::Target {
        let nes: &super::Nes = unsafe { transmute(self) };
        &nes.cpu
    }
}

impl<'a> DerefMut for ContextPtr<'a, cpu::Cpu> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let nes: &mut super::Nes = unsafe { transmute(self) };
        &mut nes.cpu
    }
}

impl<'a> Deref for ContextPtr<'a, ppu::Ppu> {
    type Target = ppu::Ppu;

    fn deref(&self) -> &Self::Target {
        let nes: &super::Nes = unsafe { transmute(self) };
        &nes.ppu
    }
}

impl<'a> DerefMut for ContextPtr<'a, ppu::Ppu> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let nes: &mut super::Nes = unsafe { transmute(self) };
        &mut nes.ppu
    }
}

impl<'a> Deref for ContextPtr<'a, apu::Apu> {
    type Target = apu::Apu;

    fn deref(&self) -> &Self::Target {
        let nes: &super::Nes = unsafe { transmute(self) };
        &nes.apu
    }
}

impl<'a> DerefMut for ContextPtr<'a, apu::Apu> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let nes: &mut super::Nes = unsafe { transmute(self) };
        &mut nes.apu
    }
}

impl<'a> Deref for ContextPtr<'a, PixelRenderer> {
    type Target = PixelRenderer;

    fn deref(&self) -> &Self::Target {
        let nes: &super::Nes = unsafe { transmute(self) };
        &nes.renderer
    }
}

impl<'a> DerefMut for ContextPtr<'a, PixelRenderer> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let nes: &mut super::Nes = unsafe { transmute(self) };
        &mut nes.renderer
    }
}

impl<'a> Deref for ContextPtr<'a, mmap::Nrom128MemoryMap> {
    type Target = mmap::Nrom128MemoryMap;

    fn deref(&self) -> &Self::Target {
        let nes: &super::Nes = unsafe { transmute(self) };
        &nes.memory
    }
}

impl<'a> DerefMut for ContextPtr<'a, mmap::Nrom128MemoryMap> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let nes: &mut super::Nes = unsafe { transmute(self) };
        &mut nes.memory
    }
}
