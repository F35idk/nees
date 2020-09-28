use super::super::{PrimaryOam, SecondaryOam};
use super::PpuMemoryMap;

#[derive(Default)]
pub struct SpriteDrawState {
    // contains data for the 8 sprites to be drawn on the current or
    // next scanline (should be filled with sprite data for the next
    // scanline on cycles 257-320)
    current_sprites_data: [SpriteRenderData; 8],
    // during sprite evaluation (dots 65-256 of each visible scanline),
    // this is used as the index of the current sprite to be evaluated
    // in primary oam. during sprite fetching (dots 257-320 of each
    // visible scanline) it is used as an index into secondary oam to
    // get the current sprite to fetch data for
    pub current_sprite: u8,
    // number of sprites found on the next scanline
    pub sprites_found: u8,
}

// struct used internally in 'SpriteDrawState' to store sprite data between scanlines
#[derive(Copy, Clone, Default)]
struct SpriteRenderData {
    x: u8,
    tile_bitplane_lo: u8,
    tile_bitplane_hi: u8,
    // bit 2 of 'attributes' indicates end of array, bit 3 indicates sprite zero
    attributes: u8,
}

// convenience info struct returned by 'SpriteDrawState::get_sprite_at_dot_info()'.
// isn't stored persistently anywhere. this struct and its associated function is
// (as of writing) only used by 'super::DrawState' in 'draw_8_pixels()' when drawing
// sprites.
pub(super) struct SpritePixelInfo {
    pub color_index: u8,
    pub palette_index: u8,
    pub is_in_front: bool,
    pub is_sprite_zero: bool,
}

impl SpriteDrawState {
    pub fn eval_next_scanline_sprite(
        &mut self,
        primary_oam: &PrimaryOam,
        secondary_oam: &mut SecondaryOam,
        current_scanline: i16,
        current_scanline_dot: u16,
    ) {
        assert!(matches!(current_scanline, -1..=239));
        assert!(matches!(current_scanline_dot, 65..=256));

        if self.sprites_found < 8 {
            let mut sprite = unsafe { primary_oam.get_sprite_unchecked(self.current_sprite) };

            // NOTE: 'sprite.y' is 1 less than the screen y coordinate
            if ((current_scanline) as u16).wrapping_sub(sprite.y as u16) < 8 {
                // clear bits 2-4 of attribute byte
                sprite.attributes &= 0b11100011;

                if self.current_sprite == 0 {
                    // set bit 3 of 'attributes' to indicate sprite zero
                    sprite.attributes |= 0b1000
                }

                // copy sprite into secondary oam
                secondary_oam.entries[self.sprites_found as usize] = sprite;
                self.sprites_found += 1;
            }
        } else {
            // TODO: sprite overflow stuff (if this is reached, sprites_found == 8)
        }

        // increment 'current_sprite'
        self.current_sprite = self.current_sprite.wrapping_add(1 << 2);
        if self.current_sprite == 0 {
            // FIXME: fail the first y-copy?
        }
    }

    pub fn fetch_next_scanline_sprite_data(
        &mut self,
        secondary_oam: &SecondaryOam,
        current_scanline: i16,
        current_scanline_dot: u16,
        pattern_table_addr: u16,
        memory: &mut dyn PpuMemoryMap,
    ) {
        assert!(matches!(current_scanline, -1..=239));
        assert!(matches!(current_scanline_dot, 257..=320));
        assert!(self.sprites_found <= 8);

        if (self.current_sprite >> 2) < self.sprites_found {
            // fill a slot in 'current_sprites_data' with data for the current sprite

            let sprite = unsafe { secondary_oam.get_sprite_unchecked(self.current_sprite) };
            let tile_index = sprite.tile_index;
            let tile = {
                let sprite_table_ptr = memory.get_pattern_tables();
                unsafe {
                    *((sprite_table_ptr
                        .get_unchecked(pattern_table_addr as usize + tile_index as usize * 16))
                        as *const _ as *const [u8; 16])
                }
            };

            let x = sprite.x;
            let attributes = sprite.attributes;
            let y = sprite.y;
            // NOTE: 'sprite.y' is 1 less than the screen y coordinate
            let y_offset = current_scanline - y as i16;

            assert!(y_offset >= 0);
            assert!(y_offset < 8);

            let (tile_bitplane_lo, tile_bitplane_hi) = if attributes & 0b10000000 != 0 {
                // use flipped tile bitplanes if sprite is vertically flipped
                (
                    unsafe { *tile.get_unchecked(7 - y_offset as usize) },
                    unsafe { *tile.get_unchecked(15 - y_offset as usize) },
                )
            } else {
                (
                    unsafe { *tile.get_unchecked(0 + y_offset as usize) },
                    unsafe { *tile.get_unchecked(8 + y_offset as usize) },
                )
            };

            // FIXME: indexing
            self.current_sprites_data[(self.current_sprite >> 2) as usize] = SpriteRenderData {
                tile_bitplane_lo,
                tile_bitplane_hi,
                attributes,
                x,
            };

            self.current_sprite = self.current_sprite.wrapping_add(1 << 2);
        } else {
            // fill a slot in 'current_sprites_data' with sentinel value
            // (bit 2 of 'attributes' being set indicates end of array)
            self.current_sprites_data[(self.current_sprite >> 2) as usize] = SpriteRenderData {
                tile_bitplane_lo: 0,
                tile_bitplane_hi: 0,
                attributes: 0b100,
                x: 0,
            };
        }
    }

    // returns a 'SpritePixelInfo' struct describing the first (highest priority) non-transparent
    // sprite pixel at the current dot, or None if no such sprite pixel was found
    // TODO: since this isn't needed anywhere else than 'super::DrawState::draw_8_pixels()', maybe
    // consider inlining it into there?
    pub(super) fn get_sprite_at_dot_info(
        &self,
        current_scanline_dot: u16,
    ) -> Option<SpritePixelInfo> {
        self.current_sprites_data
            .iter()
            // bit 2 of 'attributes' being set indicates end of array
            .take_while(|data| data.attributes & 0b100 == 0)
            .find_map(|data| {
                // ignore sprites that are partially outside of the screen
                if data.x >= 0xf9 {
                    // FIXME: if a sprite that is otherwise valid is outside of the screen,
                    // should the whole sprite search stop (instead of continuing as if
                    // the sprite had a transparent pixel)???
                    return None;
                }

                // get distance between current dot and sprite's leftmost x coordinate
                let tile_offset = current_scanline_dot.wrapping_sub(data.x as u16 + 1);

                // if current dot is within x-coords of sprite
                if tile_offset < 8 {
                    // calculate amount to shift tile bitplanes by
                    // to get the current pixel (depends on whether
                    // the sprite is flipped horizontally or not)
                    let shift_amt = if data.attributes & 0b01000000 != 0 {
                        tile_offset
                    } else {
                        7 - tile_offset
                    };

                    let color_index = {
                        let lo = (data.tile_bitplane_lo >> shift_amt) & 1;
                        let hi = ((data.tile_bitplane_hi >> shift_amt) << 1) & 2;
                        lo | hi
                    };

                    if color_index != 0 {
                        let palette_index = (data.attributes & 0b11) | 4;
                        let is_in_front = (data.attributes & 0b100000) == 0;
                        // bit 3 of 'attributes' being set means data belongs to sprite zero
                        let is_sprite_zero = (data.attributes & 0b1000) != 0;

                        return Some(SpritePixelInfo {
                            palette_index,
                            color_index,
                            is_in_front,
                            is_sprite_zero,
                        });
                    }
                }

                None
            })
    }
}
