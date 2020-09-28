use super::palette;
use super::{Ppu, PpuMemoryMap};

mod bg;
mod sprites;

// struct to hold state needed for drawing (background tile shift
// registers, 8 first sprites on next scanline, etc.). this could've
// all been inside of the main 'Ppu' struct, but I decided to factor
// it out to improve separation of concerns. instead of mutably
// borrowing the entire 'Ppu', the methods on 'DrawState' and its
// fields only borrow what is needed, and use immutable references/
// *const pointers wherever possible
#[derive(Default)]
pub struct DrawState {
    pub bg_state: bg::BgDrawState,
    pub sprite_state: sprites::SpriteDrawState,
}

impl DrawState {
    // draws 8 pixels, without making any state changes to 'Ppu'
    // or 'self'. returns whether sprite zero was hit
    pub fn draw_8_pixels(&self, ppu: *const Ppu, framebuffer: &mut [u32; 256 * 240]) -> bool {
        let ppu: &Ppu = unsafe { std::mem::transmute(ppu) };

        if ppu.is_background_enable() || ppu.is_sprites_enable() {
            self.draw_8_pixels_bg_and_sprites(ppu, framebuffer)
        } else {
            self.draw_8_pixels_backdrop_color(ppu, framebuffer);
            false
        }
    }

    fn draw_8_pixels_bg_and_sprites(&self, ppu: &Ppu, framebuffer: &mut [u32; 256 * 240]) -> bool {
        let mut sprite_zero_hit = false;

        for i in 0..8 {
            let tile_offset = i + ppu.get_fine_x_scroll();

            let bg_color_idx = match (
                ppu.is_background_enable(),
                ppu.current_scanline_dot + i as u16,
                ppu.is_background_left_column_enable(),
            ) {
                // set index to zero if bg is disabled
                (false, _, _) => 0,
                // or if on dots 1-8 and bg is disabled for this area
                (_, 1..=8, false) => 0,
                _ => {
                    let lo = ((self.bg_state.tile_bitplanes_lo.to_u16() >> (15 - tile_offset)) & 1)
                        as u8;
                    let hi = (((self.bg_state.tile_bitplanes_hi.to_u16() >> (15 - tile_offset))
                        << 1)
                        & 2) as u8;
                    lo | hi
                }
            };

            let bg_palette_idx = if tile_offset > 7 {
                self.bg_state.tile_palette_indices & 0b11
            } else {
                (self.bg_state.tile_palette_indices & 0b1100) >> 2
            };

            let mut sprite_zero = false;

            let pixel_color = match (
                ppu.is_sprites_enable(),
                ppu.current_scanline_dot + i as u16,
                ppu.is_sprites_left_column_enable(),
            ) {
                // draw bg color if sprites are disabled
                (false, _, _) => Self::calc_pixel_color(ppu, bg_palette_idx, bg_color_idx),
                // draw bg color if on dots 1-8 and sprite
                // drawing is disabled for the first 8 pixels
                (_, 1..=8, false) => Self::calc_pixel_color(ppu, bg_palette_idx, bg_color_idx),
                // otherwise, search for an active, non-transparent sprite at the current dot
                _ => match self
                    .sprite_state
                    .get_sprite_at_dot_info(ppu.current_scanline_dot + i as u16)
                {
                    // draw bg color if no sprite was found
                    None => Self::calc_pixel_color(ppu, bg_palette_idx, bg_color_idx),
                    Some(info) => {
                        sprite_zero = info.is_sprite_zero;

                        if info.is_in_front || bg_color_idx == 0 {
                            Self::calc_pixel_color(ppu, info.palette_index, info.color_index)
                        } else {
                            Self::calc_pixel_color(ppu, bg_palette_idx, bg_color_idx)
                        }
                    }
                },
            };

            if sprite_zero && bg_color_idx != 0 && (ppu.current_scanline_dot + i as u16 - 1) != 0xff
            {
                sprite_zero_hit = true;
            }

            let screen_x = (ppu.current_scanline_dot - 1) as usize + i as usize;
            let screen_y = ppu.current_scanline as usize;
            // TODO: OPTIMIZE: unchecked indexing
            framebuffer[screen_y * 256 + screen_x] = pixel_color;
        }

        sprite_zero_hit
    }

    // draws 8 pixels of backdrop color (or if 'current_vram_addr'
    // >= 0x3f00, draws the color 'current_vram_addr' points to)
    fn draw_8_pixels_backdrop_color(&self, ppu: &Ppu, framebuffer: &mut [u32; 256 * 240]) {
        for i in 0..8 {
            let pixel_color = {
                let bg_color_addr = if ppu.current_vram_addr.get_addr() >= 0x3f00 {
                    logln!("background palette hack triggered");
                    ppu.current_vram_addr.get_addr()
                } else {
                    0x3f00
                };

                let bg_color_byte = ppu.memory.read(bg_color_addr);
                palette::COLOR_LUT.get(bg_color_byte, ppu.is_greyscale_enabled(), ppu.ppumask >> 5)
            };

            let screen_x = (ppu.current_scanline_dot - 1 + i as u16) as usize;
            let screen_y = ppu.current_scanline as usize;
            framebuffer[screen_y * 256 + screen_x] = pixel_color;
        }
    }

    fn calc_pixel_color(ppu: &Ppu, palette_index: u8, color_index: u8) -> u32 {
        let mut addr = (0x3f00 | ((palette_index << 2) as u16)) | (color_index as u16);

        if color_index == 0 {
            addr = 0x3f00;
        }

        let color_byte = ppu.memory.read(addr);
        palette::COLOR_LUT.get(color_byte, ppu.is_greyscale_enabled(), ppu.ppumask >> 5)
    }
}
