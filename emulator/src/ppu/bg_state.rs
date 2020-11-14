use super::super::memory_map::PpuMemoryMap;
use super::Ppu;

#[derive(Default)]
pub(super) struct BgDrawState {
    // equivalent to the two 16-bit pattern table data shift registers
    // on the (irl) ppu
    pub tile_bitplanes_hi: TileBitPlanes,
    pub tile_bitplanes_lo: TileBitPlanes,
    // holds the palette indices of the two current tiles (in the first 4 bits)
    pub tile_palette_indices: u8,
}

#[repr(align(2))]
#[derive(Copy, Clone, Default)]
pub(super) struct TileBitPlanes([u8; 2]);

impl TileBitPlanes {
    pub(super) fn to_u16(self) -> u16 {
        unsafe { std::mem::transmute(self) }
    }

    fn as_u16(&mut self) -> &mut u16 {
        unsafe { std::mem::transmute(self) }
    }
}

impl BgDrawState {
    pub(super) fn shift_tile_data_by_8(&mut self) {
        *(self.tile_bitplanes_hi.as_u16()) <<= 8;
        *(self.tile_bitplanes_lo.as_u16()) <<= 8;
        self.tile_palette_indices &= 0b11;
        self.tile_palette_indices <<= 2;
    }

    pub(super) fn fetch_current_tile_data(
        &mut self,
        cycle_count: i32,
        background_pattern_table_addr: u16,
        current_vram_addr: super::VramAddrRegister,
        memory: &dyn PpuMemoryMap,
    ) {
        // get the high and low bitplanes for the current row of the current tile
        let (bg_bitplane_lo, bg_bitplane_hi) = {
            // get tile index from nametable using lower 12 bits of 'current_vram_addr' + 0x2000
            let addr = (current_vram_addr.get_addr() & 0xfff) | 0x2000;
            let tile_index = memory.read(addr, cycle_count);

            let tile_addr = background_pattern_table_addr + ((tile_index as u16) << 4);
            let fine_y = current_vram_addr.get_fine_y();

            (
                memory.read(tile_addr + fine_y as u16, cycle_count + 2),
                memory.read(tile_addr + 8 + fine_y as u16, cycle_count + 4),
            )
        };

        let bg_palette_idx = {
            let coarse_y = current_vram_addr.get_coarse_y();
            let coarse_x = current_vram_addr.get_coarse_x();

            // calculate the address of the current tile's 'attribute' in the attribute table
            let attribute_addr = 0x23c0
                | (current_vram_addr.get_nametable_select() as u16) << 10
                | (coarse_y << 1) as u16 & 0b111000
                | (coarse_x >> 2) as u16;

            // get the 'attribute' byte from the attribute table
            let attribute = memory.read(attribute_addr, cycle_count + 6);
            // calculate how much to shift 'attribute' by to get the current tile's palette index
            let shift_amt = ((coarse_y << 1) & 0b100) | (coarse_x & 0b10);

            (attribute >> shift_amt) & 0b11
        };

        // store the data
        {
            // NOTE: all of this assumes little endian

            // the tile bitplane byte we want to store our high bg bitplane
            // in should be zero (its contents should have been shifted
            // leftwards into 'self.tile_bitplanes_hi.0[1]' previously)
            debug_assert_eq!(self.tile_bitplanes_hi.0[0], 0);
            self.tile_bitplanes_hi.0[0] = bg_bitplane_hi;

            debug_assert_eq!(self.tile_bitplanes_lo.0[0], 0);
            self.tile_bitplanes_lo.0[0] = bg_bitplane_lo;

            debug_assert_eq!(self.tile_palette_indices & 0b11, 0);
            self.tile_palette_indices |= bg_palette_idx;
        }
    }
}
