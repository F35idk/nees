use super::palette;
use super::Ppu;
use std::ops::Range;

#[derive(Default)]
pub struct DrawState {}

impl DrawState {
    // draws a horizontal line of pixels starting at 'current_scanline_dot'
    // - 1 and stopping at the end of the current tile in the background
    // nametable (or the end of the screen, if the current tile happens
    // to poke outside of it). returns how many pixels were drawn
    pub fn draw_tile_row(
        &mut self,
        ppu: *const Ppu,
        framebuffer: &mut [u32; 256 * 240],
    ) -> (u8, bool) {
        let ppu: &Ppu = unsafe { std::mem::transmute(ppu) };

        // calculate the index of the current background tile in the current scanline
        let horizontal_bg_tile_count =
            ((ppu.current_scanline_dot - 1 + ppu.fine_x_scroll as u16) >> 3) as u8 & 0x3f;

        let bg_tile_offsets = if horizontal_bg_tile_count == 32 {
            // if on tile 32 (meaning 'fine_x_scroll' is non-zero and
            // this is the last tile in the scanline), start drawing
            // at offset 0 from current tile and stop at end of screen
            let screen_x = (ppu.current_scanline_dot - 1) as u8;
            0..(8 - (screen_x % 8))
        } else if horizontal_bg_tile_count == 0 {
            // if on first tile, start drawing pixel at 'fine_x_scroll'
            (ppu.fine_x_scroll)..8
        } else {
            // if on any other tile, draw all pixels in it
            0..8
        };

        if ppu.is_background_enable() || ppu.is_sprites_enable() {
            self.draw_tile_row_background_and_sprites(ppu, framebuffer, bg_tile_offsets)
        } else {
            self.draw_tile_row_backdrop_color(ppu, framebuffer, bg_tile_offsets.len() as u8);
            (bg_tile_offsets.len() as u8, false)
        }
    }

    fn draw_tile_row_background_and_sprites(
        &mut self,
        ppu: &Ppu,
        framebuffer: &mut [u32; 256 * 240],
        bg_tile_offsets: Range<u8>,
    ) -> (u8, bool) {
        // NOTE: this function is split into multiple subfunctions to help readability

        {
            let current_bg_tile = get_current_tile(ppu);
            let bg_palette_idx = get_current_tile_palette_index(ppu);

            // get the high and low bitplanes for the current row of the current tile
            let fine_y = ppu.current_vram_addr.get_fine_y();
            let bg_bitplane_lo = unsafe { *current_bg_tile.get_unchecked(0 + fine_y as usize) };
            let bg_bitplane_hi = unsafe { *current_bg_tile.get_unchecked(8 + fine_y as usize) };

            let mut pixels_drawn = 0u8;
            let mut sprite_zero_hit = false;

            for bg_tile_offset in bg_tile_offsets {
                let bg_color_idx = match (
                    ppu.is_background_enable(),
                    ppu.current_scanline_dot + pixels_drawn as u16,
                    ppu.is_background_left_column_enable(),
                ) {
                    (false, _, _) => 0,
                    (_, 1..=8, false) => 0,
                    _ => {
                        let lo = (bg_bitplane_lo >> (7 - bg_tile_offset)) & 1;
                        let high = ((bg_bitplane_hi >> (7 - bg_tile_offset)) << 1) & 2;
                        lo | high
                    }
                };

                let mut sprite_zero = false;

                let pixel_color = match (
                    ppu.is_background_enable(),
                    ppu.current_scanline_dot + pixels_drawn as u16,
                    ppu.is_sprites_left_column_enable(),
                ) {
                    // draw bg color if 'draw_sprites' is false
                    (false, _, _) => Self::calc_pixel_color(ppu, bg_palette_idx, bg_color_idx),
                    // draw bg color if on dots 1-8 and sprite
                    // drawing is disabled for the first 8 pixels
                    (_, 1..=8, false) => Self::calc_pixel_color(ppu, bg_palette_idx, bg_color_idx),
                    // otherwise, search for an active, non-transparent sprite at the current dot
                    _ => match ppu
                        .oam
                        .get_sprite_at_dot_info(ppu.current_scanline_dot + pixels_drawn as u16)
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

                if sprite_zero
                    && bg_color_idx != 0
                    && (ppu.current_scanline_dot + pixels_drawn as u16 - 1) != 0xff
                {
                    sprite_zero_hit = true;
                }

                let screen_x = (ppu.current_scanline_dot - 1) as usize + pixels_drawn as usize;
                let screen_y = ppu.current_scanline as usize;
                // TODO: OPTIMIZE: unchecked indexing
                framebuffer[screen_y * 256 + screen_x] = pixel_color;

                pixels_drawn += 1;
            }

            return (pixels_drawn, sprite_zero_hit);
        }

        fn get_current_tile(ppu: &Ppu) -> [u8; 16] {
            // get tile index from nametable using 'current_vram_addr' + 0x2000
            let addr = ppu.current_vram_addr.get_addr() | 0x2000;
            let tile_index = ppu.memory.read(addr);
            let background_table_addr = ppu.get_background_pattern_table_addr() as usize;
            let background_table_ptr = ppu.memory.get_pattern_tables();

            unsafe {
                // get tile from pattern table using the tile index
                // SAFETY: 'current_tile_index' * 16 cannot be
                // larger than 0x1000 (the size of a pattern table)
                *((background_table_ptr
                    .get_unchecked(background_table_addr + tile_index as usize * 16))
                    as *const _ as *const [u8; 16])
            }
        }

        fn get_current_tile_palette_index(ppu: &Ppu) -> u8 {
            let coarse_y = ppu.current_vram_addr.get_coarse_y();
            let coarse_x = ppu.current_vram_addr.get_coarse_x();

            // calculate the address of the current tile's 'attribute' in the attribute table
            let attribute_addr = 0x23c0
                | (ppu.current_vram_addr.get_nametable_select() as u16) << 10
                | (coarse_y << 1) as u16 & 0b111000
                | (coarse_x >> 2) as u16;

            // get the 'attribute' byte from the attribute table
            let attribute = ppu.memory.read(attribute_addr);
            // calculate how much to shift 'attribute' by to get the current tile's palette index
            let shift_amt = ((coarse_y << 1) & 0b100) | (coarse_x & 0b10);

            (attribute >> shift_amt) & 0b11
        }
    }

    // draws 8 pixels of backdrop color (or if 'current_vram_addr'
    // >= 0x3f00, draws the color 'current_vram_addr' points to)
    fn draw_tile_row_backdrop_color(
        &mut self,
        ppu: &Ppu,
        framebuffer: &mut [u32; 256 * 240],
        pixels_to_draw: u8,
    ) {
        for i in 0..pixels_to_draw {
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
