use super::Ppu;
use super::{palette, util};
use std::ops::Range;

// draws a horizontal line of pixels starting at 'current_scanline_dot'
// - 1 and stopping at the end of the current tile in the background
// nametable (or the end of the screen, if the current tile happens
// to poke outside of it). returns how many pixels were drawn
pub fn draw_tile_row(ppu: &mut Ppu, draw_background: bool, draw_sprites: bool) -> u8 {
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

    if draw_background {
        draw_tile_row_background_and_sprites(ppu, bg_tile_offsets, draw_sprites)
    } else {
        draw_tile_row_backdrop_color_and_sprites(ppu, bg_tile_offsets.len() as u8, draw_sprites);
        bg_tile_offsets.len() as u8
    }
}

fn draw_tile_row_background_and_sprites(
    ppu: &mut Ppu,
    bg_tile_offsets: Range<u8>,
    draw_sprites: bool,
) -> u8 {
    // NOTE: this function is split into multiple subfunctions to help readability

    {
        let current_bg_tile = get_current_tile(ppu);
        let bg_palette_index = get_tile_palette_index(ppu);

        // get the high and low bitplanes for the current row of the current tile
        let fine_y = ppu.current_vram_addr.get_fine_y();
        let bg_bitplane_lo = unsafe { *current_bg_tile.get_unchecked(0 + fine_y as usize) };
        let bg_bitplane_hi = unsafe { *current_bg_tile.get_unchecked(8 + fine_y as usize) };

        let mut pixels_drawn = 0u8;

        for bg_tile_offset in bg_tile_offsets {
            let sprite_color = match (
                draw_sprites,
                ppu.current_scanline_dot + (pixels_drawn as u16),
                ppu.is_sprites_left_column_enable(),
            ) {
                // don't draw sprite if 'draw_sprites' is false
                (false, _, _) => None,
                // don't draw sprites if on dots 1-8 and sprite
                // drawing is disabled for the first 8 pixels
                (_, 1..=8, false) => None,
                _ => ppu
                    .oam
                    .current_sprites_data
                    .iter()
                    .take_while(|data| !data.is_end_of_array())
                    .find_map(|data| {
                        // don't draw sprites that are partially outside of the screen
                        if data.x >= 0xf9 {
                            return None;
                        }

                        // get distance between current dot and sprite's leftmost x coordinate
                        let tile_offset = (ppu.current_scanline_dot + pixels_drawn as u16)
                            .wrapping_sub(data.x as u16);

                        // if current dot is within x-coords of sprite
                        if tile_offset < 8 {
                            // calculate amount to shift tile bitplanes by
                            // to get the current pixel (depends on whether
                            // the sprite is flipped horizontally)
                            let shift_amt = if data.attributes & 0b01000000 != 0 {
                                tile_offset
                            } else {
                                7 - tile_offset
                            };

                            let color_index = {
                                let lo = (data.tile_bitplane_low >> shift_amt) & 1;
                                let hi = ((data.tile_bitplane_high >> shift_amt) << 1) & 2;
                                lo | hi
                            };

                            // if sprite is in front of background and color index
                            // does not point to a transparent color
                            if color_index != 0 && (data.attributes & 0b100000) != 1 {
                                let palette_index = (data.attributes & 0b11) | 4;
                                return Some(calc_pixel_color(ppu, palette_index, color_index));
                            }
                        }

                        None
                    }),
            };

            let pixel_color = sprite_color.unwrap_or_else(|| {
                match (
                    ppu.current_scanline_dot + pixels_drawn as u16 - 1,
                    ppu.is_background_left_column_enable(),
                ) {
                    (1..=8, false) => calc_pixel_color(ppu, 0, 0),
                    _ => {
                        let bg_color_index = {
                            let lo = (bg_bitplane_lo >> (7 - bg_tile_offset)) & 1;
                            let high = ((bg_bitplane_hi >> (7 - bg_tile_offset)) << 1) & 2;
                            lo | high
                        };

                        calc_pixel_color(ppu, bg_palette_index, bg_color_index)
                    }
                }
            });

            let screen_x = (ppu.current_scanline_dot - 1) as usize + pixels_drawn as usize;
            let screen_y = ppu.current_scanline as usize;
            let framebuffer = util::pixels_to_u32(&mut ppu.renderer);
            // TODO: OPTIMIZE: unchecked indexing
            framebuffer[screen_y * 256 + screen_x] = pixel_color;

            pixels_drawn += 1;
        }

        return pixels_drawn;
    }

    fn get_current_tile(ppu: &mut Ppu) -> [u8; 16] {
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
                .get_unchecked_mut(background_table_addr + tile_index as usize * 16))
                as *mut _ as *mut [u8; 16])
        }
    }

    fn get_tile_palette_index(ppu: &mut Ppu) -> u8 {
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

// draws 8 pixels of sprite and backdrop color (or if 'current_vram_addr'
// >= 0x3f00, draws the color 'current_vram_addr' points to as backdrop)
fn draw_tile_row_backdrop_color_and_sprites(ppu: &mut Ppu, pixels_to_draw: u8, draw_sprites: bool) {
    for i in 0..pixels_to_draw {
        let sprite_color = match (
            draw_sprites,
            ppu.current_scanline_dot + (i as u16),
            ppu.is_sprites_left_column_enable(),
        ) {
            (false, _, _) => None,
            (_, 1..=8, false) => None,
            _ => ppu
                .oam
                .current_sprites_data
                .iter()
                .take_while(|data| !data.is_end_of_array())
                .find_map(|data| {
                    let tile_offset =
                        (ppu.current_scanline_dot + i as u16).wrapping_sub(data.x as u16);

                    if tile_offset < 8 {
                        let shift_amt = if data.attributes & 0b01000000 != 0 {
                            tile_offset
                        } else {
                            7 - tile_offset
                        };

                        let color_index = {
                            let lo = (data.tile_bitplane_low >> shift_amt) & 1;
                            let hi = ((data.tile_bitplane_high >> shift_amt) << 1) & 2;
                            lo | hi
                        };

                        if data.x >= 0xf9 {
                            return None;
                        }

                        // if color index doesn't point to a transparent color
                        if color_index != 0 {
                            let palette_index = (data.attributes & 0b11) | 4;
                            return Some(calc_pixel_color(ppu, palette_index, color_index));
                        }
                    }

                    None
                }),
        };

        let pixel_color = sprite_color.unwrap_or_else(|| {
            let bg_color_addr = if ppu.current_vram_addr.get_addr() >= 0x3f00 {
                logln!("background palette hack triggered");
                ppu.current_vram_addr.get_addr()
            } else {
                0x3f00
            };

            let bg_color_byte = ppu.memory.read(bg_color_addr);
            palette::COLOR_LUT.get(bg_color_byte, ppu.is_greyscale_enabled(), ppu.ppumask >> 5)
        });

        let screen_x = (ppu.current_scanline_dot - 1 + i as u16) as usize;
        let screen_y = ppu.current_scanline as usize;
        let framebuffer = util::pixels_to_u32(&mut ppu.renderer);
        framebuffer[screen_y * 256 + screen_x] = pixel_color;
    }
}

fn calc_pixel_color(ppu: &Ppu, palette_index: u8, color_index: u8) -> u32 {
    let addr = (0x3f00 | ((palette_index << 2) as u16)) | (color_index as u16);
    let color_byte = ppu.memory.read(addr);
    palette::COLOR_LUT.get(color_byte, ppu.is_greyscale_enabled(), ppu.ppumask >> 5)
}
