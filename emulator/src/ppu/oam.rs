use super::super::memory_map::PpuMemoryMap;

#[derive(Default)]
pub struct Oam {
    pub primary: PrimaryOam,
    pub secondary: SecondaryOam,
    // contains data for the 8 sprites to be drawn on the current or
    // next scanline (is filled with sprite data for the next scanline
    // on cycles 257-320)
    pub current_sprites_data: [SpriteRenderData; 8],
    // during sprite evaluation (dots 65-256 of each visible scanline),
    // this is used as the index of the current sprite to be evaluated
    // in primary oam. during sprite fetching (dots 257-320 of each
    // visible scanline) it is used as an index into secondary oam to
    // get the current sprite to fetch data for
    pub current_sprite: u8,
    // number of sprites found on the next scanline
    pub sprites_found: u8,
}

#[derive(Copy, Clone, Default)]
#[repr(C)]
pub struct OamEntry {
    pub y: u8,
    pub tile_index: u8,
    pub attributes: u8,
    pub x: u8,
}

pub struct PrimaryOam {
    pub entries: [OamEntry; 64],
}

pub struct SecondaryOam {
    pub entries: [OamEntry; 8],
}

#[derive(Copy, Clone, Default)]
pub struct SpriteRenderData {
    pub x: u8,
    pub tile_bitplane_low: u8,
    pub tile_bitplane_high: u8,
    pub attributes: u8,
}

// convenience methods for the 'SecondaryOam' and 'PrimaryOam' structs
macro_rules! oam_impl {
    ($oam:ty, $n_entries:literal) => {
        impl $oam {
            pub fn as_bytes<'a>(&'a self) -> &'a [u8; $n_entries * 4] {
                unsafe { std::mem::transmute(self) }
            }

            pub fn as_bytes_mut<'a>(&'a mut self) -> &'a mut [u8; $n_entries * 4] {
                unsafe { std::mem::transmute(self) }
            }

            pub fn get_byte(&self, index: u8) -> u8 {
                unsafe { *self.as_bytes().get_unchecked(index as usize) }
            }

            pub fn set_byte(&mut self, index: u8, val: u8) {
                unsafe { *self.as_bytes_mut().get_unchecked_mut(index as usize) = val };
            }

            #[inline]
            pub unsafe fn get_sprite_unchecked(&mut self, index: u8) -> OamEntry {
                *(self.as_bytes_mut().get_unchecked_mut(index as usize) as *mut _ as *mut _)
            }
        }

        impl Default for $oam {
            fn default() -> Self {
                Self {
                    entries: [OamEntry::default(); $n_entries],
                }
            }
        }
    };
}

oam_impl!(PrimaryOam, 64);
oam_impl!(SecondaryOam, 8);

impl Oam {
    pub fn eval_next_scanline_sprite(&mut self, current_scanline: i16, current_scanline_dot: u16) {
        assert!(matches!(current_scanline, -1..=239));
        assert!(matches!(current_scanline_dot, 65..=256));

        if self.sprites_found < 8 {
            // let y = unsafe { self.oam.bytes[self.current_sprite as usize] };
            // assert_eq!(y, unsafe {
            //     self.oam.entries[self.current_sprite as usize >> 2].y
            // });

            // FIXME: forego the y-copy??
            // unsafe { self.secondary_oam.entries[sprites_found as usize].y = y };

            // FIXME: proper uninit sprite behavior

            let sprite = unsafe { self.primary.get_sprite_unchecked(self.current_sprite) };

            assert_eq!(
                sprite.attributes,
                self.primary.entries[self.current_sprite as usize >> 2].attributes
            );

            // FIXME: 'min()' should not be needed
            let sprite_y_coords = (sprite.y as i16)..=(sprite.y as i16 + 7).min(239);

            if sprite_y_coords.contains(&(current_scanline + 1)) {
                // copy sprite into secondary oam
                self.secondary.entries[self.sprites_found as usize] = sprite;

                assert!(current_scanline + 1 - sprite.y as i16 >= 0);
                assert!((current_scanline + 1 - sprite.y as i16) < 8);

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

            let sprite = unsafe { self.secondary.get_sprite_unchecked(self.current_sprite) };
            let tile_index = sprite.tile_index;
            let tile = {
                let sprite_table_ptr = memory.get_pattern_tables();

                unsafe {
                    *((sprite_table_ptr
                        .get_unchecked_mut(pattern_table_addr as usize + tile_index as usize * 16))
                        as *mut _ as *mut [u8; 16])
                }
            };

            let x = sprite.x;
            let attributes = sprite.attributes;
            let y = sprite.y;
            let y_offset = current_scanline + 1 - y as i16;

            assert!(y_offset >= 0);
            assert!(y_offset < 8);

            let tile_bitplane_low = unsafe { *tile.get_unchecked(0 + y_offset as usize) };
            let tile_bitplane_high = unsafe { *tile.get_unchecked(8 + y_offset as usize) };

            // FIXME: indexing
            self.current_sprites_data[(self.current_sprite >> 2) as usize] = SpriteRenderData {
                tile_bitplane_low,
                tile_bitplane_high,
                attributes,
                x,
            };
        } else {
            // fill a slot in 'current_sprites_data' with transparent values
            self.current_sprites_data[(self.current_sprite >> 2) as usize] = SpriteRenderData {
                tile_bitplane_low: 0,
                tile_bitplane_high: 0,
                attributes: 0,
                x: 0,
            };
        }

        self.current_sprite = self.current_sprite.wrapping_add(1 << 2);
    }
}
