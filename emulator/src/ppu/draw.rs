use super::palette;
use super::Ppu;

#[repr(align(2))]
#[derive(Copy, Clone, Default)]
struct TileBitPlanes([u8; 2]);

impl TileBitPlanes {
    fn to_u16(self) -> u16 {
        unsafe { std::mem::transmute(self) }
    }

    fn as_u16(&mut self) -> &mut u16 {
        unsafe { std::mem::transmute(self) }
    }
}

#[derive(Default)]
pub struct DrawState {
    // equivalent to the two 16-bit pattern table data shift registers
    // on the (irl) ppu
    bg_tile_bitplanes_hi: TileBitPlanes,
    bg_tile_bitplanes_lo: TileBitPlanes,
    // stores the palette indices for the two current tiles in the first 4 bits
    bg_tile_palette_indices: u8,
}

impl DrawState {
    pub fn draw_8_pixels(&mut self, ppu: *const Ppu, framebuffer: &mut [u32; 256 * 240]) -> bool {
        let ppu: &Ppu = unsafe { std::mem::transmute(ppu) };

        if ppu.is_background_enable() || ppu.is_sprites_enable() {
            self.draw_8_pixels_bg_and_sprites(ppu, framebuffer)
        } else {
            self.draw_8_pixels_backdrop_color(ppu, framebuffer);
            false
        }
    }
    pub fn shift_tile_data_by_8(&mut self) {
        *(self.bg_tile_bitplanes_hi.as_u16()) <<= 8;
        *(self.bg_tile_bitplanes_lo.as_u16()) <<= 8;
        self.bg_tile_palette_indices &= 0b11;
        self.bg_tile_palette_indices <<= 2;
    }

    pub fn fetch_current_bg_tile_data(&mut self, ppu: *const Ppu) {
        let ppu: &Ppu = unsafe { std::mem::transmute(ppu) };

        let current_bg_tile = {
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
        };

        let bg_palette_idx = {
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
        };

        // get the high and low bitplanes for the current row of the current tile
        let fine_y = ppu.current_vram_addr.get_fine_y();
        let bg_bitplane_lo = unsafe { *current_bg_tile.get_unchecked(0 + fine_y as usize) };
        let bg_bitplane_hi = unsafe { *current_bg_tile.get_unchecked(8 + fine_y as usize) };

        // store the data
        {
            // NOTE: all of this assumes little endian

            // the tile bitplane byte we want to store our high bg bitplane
            // in should be zero (its contents should have been shifted
            // leftwards into 'self.bg_tile_bitplanes_hi.0[1]' previously)
            debug_assert_eq!(self.bg_tile_bitplanes_hi.0[0], 0);
            self.bg_tile_bitplanes_hi.0[0] = bg_bitplane_hi;

            debug_assert_eq!(self.bg_tile_bitplanes_lo.0[0], 0);
            self.bg_tile_bitplanes_lo.0[0] = bg_bitplane_lo;

            debug_assert_eq!(self.bg_tile_palette_indices & 0b11, 0);
            self.bg_tile_palette_indices |= bg_palette_idx;
        }
    }

    fn draw_8_pixels_bg_and_sprites(
        &mut self,
        ppu: &Ppu,
        framebuffer: &mut [u32; 256 * 240],
    ) -> bool {
        let mut sprite_zero_hit = false;

        for i in 0..8 {
            let tile_offset = i + ppu.fine_x_scroll;
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
                    let lo = ((self.bg_tile_bitplanes_lo.to_u16() >> (15 - tile_offset)) & 1) as u8;
                    let hi = (((self.bg_tile_bitplanes_hi.to_u16() >> (15 - tile_offset)) << 1) & 2)
                        as u8;
                    lo | hi
                }
            };

            let bg_palette_idx = if tile_offset > 7 {
                self.bg_tile_palette_indices & 0b11
            } else {
                (self.bg_tile_palette_indices & 0b1100) >> 2
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
                _ => match ppu
                    .oam
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
    fn draw_8_pixels_backdrop_color(&mut self, ppu: &Ppu, framebuffer: &mut [u32; 256 * 240]) {
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
