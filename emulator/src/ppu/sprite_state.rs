use super::{PrimaryOam, SecondaryOam, SpriteSize};
use crate::address_bus::PpuAddressBus;
use crate::cpu;

#[derive(Default)]
pub(super) struct SpriteDrawState {
    // contains data for the 8 sprites to be drawn on the current or
    // next scanline (should be filled with sprite data for the next
    // scanline on cycles 257-320)
    current_sprites_data: [SpriteRenderData; 8],
    // holds the bits corresponding to the 'n' and 'm' OAM indices
    // (from nesdev.com 'PPU Sprite Evaluation' page). is used
    // both during sprite evaluation (dots 65-256 of each visible
    // scanline) and sprite fetching (dots 257-320)
    pub current_sprite_idx: u8,
    // number of sprites found on the next scanline
    pub sprites_found: u8,
    // OPTIMIZE: pack 'eval_done' into same byte as 'sprites_found'
    pub eval_done: bool,
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
// isn't stored persistently anywhere.
pub(super) struct SpritePixelInfo {
    pub color_index: u8,
    pub palette_index: u8,
    pub is_in_front: bool,
    pub is_sprite_zero: bool,
}

impl SpriteDrawState {
    pub(super) fn eval_next_scanline_sprite(
        &mut self,
        mut sprite_overflow: bool,
        sprite_height: SpriteSize,
        primary_oam: &PrimaryOam,
        secondary_oam: &mut SecondaryOam,
        current_scanline: i16,
        current_scanline_dot: u16,
    ) -> bool {
        debug_assert!(matches!(current_scanline, 0..=239));
        debug_assert!(matches!(current_scanline_dot, 65..=256));

        if self.eval_done {
            return false;
        }

        if self.sprites_found < 8 {
            // copy the byte pointed to by 'current_sprite_idx' from primary to secondary oam
            let byte = primary_oam.get_byte(self.current_sprite_idx);
            secondary_oam.set_byte(
                (self.sprites_found << 2) | self.current_sprite_idx & 0b11,
                byte,
            );

            if self.current_sprite_idx & 0b11 == 0 {
                // if 'current_sprite_idx' is a multiple of 4, treat 'byte' as a sprite
                // y-coordinate and use it to test whether the current sprite is in range

                // NOTE: sprite y-coordinate is 1 less than the actual screen y-coordinate
                if (current_scanline as u16).wrapping_sub(byte as u16) < sprite_height as u16 {
                    // set index to point to next byte (on the same sprite)
                    self.current_sprite_idx += 1;
                } else {
                    // set index to point to next sprite
                    self.current_sprite_idx = self.current_sprite_idx.wrapping_add(4);

                    if self.current_sprite_idx == 0 {
                        // index has overflown back to zero
                        self.eval_done = true;
                    }
                }
            } else {
                if self.current_sprite_idx & 0b11 == 2 {
                    // the byte just copied into secondary oam is an 'attributes' byte

                    debug_assert_eq!(
                        byte,
                        secondary_oam.entries[self.sprites_found as usize].attributes
                    );

                    // clear unused bits
                    secondary_oam.entries[self.sprites_found as usize].attributes &= 0b11100011;

                    if self.current_sprite_idx & !0b11 == 0 {
                        // if the byte belongs to sprite zero, set bit 3
                        secondary_oam.entries[self.sprites_found as usize].attributes |= 0b1000;
                    }
                } else if self.current_sprite_idx & 0b11 == 0b11 {
                    // the byte just copied is the last byte on the current sprite
                    self.sprites_found += 1;

                    debug_assert!({
                        if self.sprites_found == 8 {
                            (self.current_sprite_idx.wrapping_add(1)) % 4 == 0
                        } else {
                            true
                        }
                    });
                }

                // set index to point to next byte (increment 'm')
                self.current_sprite_idx = self.current_sprite_idx.wrapping_add(1);
            }
        } else if !sprite_overflow {
            let sprite_y = primary_oam.get_byte(self.current_sprite_idx);

            if (current_scanline as u16).wrapping_sub(sprite_y as u16) < sprite_height as u16 {
                sprite_overflow = true;
            } else {
                // increment 'n' (from nesdev.com 'PPU Sprite Evaluation' page)
                self.current_sprite_idx = self.current_sprite_idx.wrapping_add(4);

                if self.current_sprite_idx & !0b11 == 0 {
                    // 'n' has overflown back to zero
                    self.eval_done = true;
                }

                // increment 'm' without carry (from nesdev.com 'PPU Sprite Evaluation' page)
                self.current_sprite_idx = if self.current_sprite_idx & 0b11 == 0b11 {
                    self.current_sprite_idx & 0b11
                } else {
                    self.current_sprite_idx + 1
                };
            }
        }

        sprite_overflow
    }

    pub(super) fn fetch_next_scanline_sprite_data(
        &mut self,
        secondary_oam: &SecondaryOam,
        sprite_height: SpriteSize,
        current_scanline: i16,
        current_scanline_dot: u16,
        sprite_pattern_table_addr: u16,
        cycle_count: i32,
        bus: &mut dyn PpuAddressBus,
        cpu: &mut cpu::Cpu,
    ) {
        debug_assert!(matches!(current_scanline, 0..=239));
        debug_assert!(matches!(current_scanline_dot, 257..=320));
        debug_assert!(self.sprites_found <= 8);
        debug_assert!((self.current_sprite_idx >> 2) < 8);

        // make garbage nametable fetches
        let _ = bus.read(0x2000, cycle_count, cpu);
        let _ = bus.read(0x2000, cycle_count + 2, cpu);

        if (self.current_sprite_idx >> 2) < self.sprites_found {
            // fill a slot in 'current_sprites_data' with data for the current sprite

            let sprite =
                unsafe { secondary_oam.get_sprite_unchecked(self.current_sprite_idx & !0b11) };
            let x = sprite.x;
            let y = sprite.y;
            let attributes = sprite.attributes;
            let is_vert_flipped = attributes & 0b10000000 != 0;
            let tile_index = sprite.tile_index;

            let (tile_bitplane_lo, tile_bitplane_hi) = {
                let tile_addr = match sprite_height {
                    SpriteSize::S8x8 => sprite_pattern_table_addr | ((tile_index as u16) << 4),
                    SpriteSize::S8x16 => {
                        // address of pattern table is stored in the lowest tile index bit
                        let sprite_pattern_table_addr_8x16 = (tile_index as u16 & 1) << 12;
                        let mut tile_index_8x16 = tile_index & !1;

                        debug_assert!((current_scanline as u8).wrapping_sub(sprite.y) < 16);

                        if (current_scanline as u8).wrapping_sub(sprite.y) >= 8 {
                            tile_index_8x16 |= 1;
                        }

                        if is_vert_flipped {
                            tile_index_8x16 ^= 1;
                        }

                        sprite_pattern_table_addr_8x16 | ((tile_index_8x16 as u16) << 4)
                    }
                };

                // NOTE: 'sprite.y' is 1 less than the screen y coordinate
                let y_offset = (current_scanline as u16 - y as u16) % 8;
                if is_vert_flipped {
                    // use flipped tile bitplanes if sprite is vertically flipped
                    (
                        bus.read(tile_addr + 7 - y_offset, cycle_count + 4, cpu),
                        bus.read(tile_addr + 15 - y_offset, cycle_count + 6, cpu),
                    )
                } else {
                    (
                        bus.read(tile_addr + y_offset, cycle_count + 4, cpu),
                        bus.read(tile_addr + 8 + y_offset, cycle_count + 6, cpu),
                    )
                }
            };

            self.current_sprites_data[(self.current_sprite_idx >> 2) as usize] = SpriteRenderData {
                tile_bitplane_lo,
                tile_bitplane_hi,
                attributes,
                x,
            };

            self.current_sprite_idx = self.current_sprite_idx.wrapping_add(4);
        } else {
            // make dummy pattern table fetches (as if sprite was all 0xff-bytes)
            let tile_addr = match sprite_height {
                SpriteSize::S8x8 => sprite_pattern_table_addr | (0xff << 4),
                SpriteSize::S8x16 => 0x1000 | (0xff << 4),
            };
            // TODO: emulate the exact dummy bitplane that would be fetched?
            let _ = bus.read(tile_addr, cycle_count + 4, cpu);
            let _ = bus.read(tile_addr, cycle_count + 6, cpu);

            // fill a slot in 'current_sprites_data' with sentinel value
            // (bit 2 of 'attributes' being set indicates end of array)
            self.current_sprites_data[(self.current_sprite_idx >> 2) as usize] = SpriteRenderData {
                tile_bitplane_lo: 0,
                tile_bitplane_hi: 0,
                attributes: 0b100,
                x: 0,
            };
        }
    }

    // returns a 'SpritePixelInfo' struct describing the first (highest priority) non-transparent
    // sprite pixel at the current dot, or None if no such sprite pixel was found
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
