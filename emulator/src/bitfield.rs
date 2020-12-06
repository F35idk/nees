macro_rules! bitfield {
    ( $name:ident<$field_type:ty> ($( $field:ident: $lower:literal..$upper:literal ),*)) => {
        #[allow(non_snake_case)]
        pub mod $name {
            #[derive(Default, PartialEq, Debug, Eq)]
            pub struct Fields {
                bits: $field_type,
                $(pub $field: $field,)*
            }

            const HIGHEST_BIT: $field_type = (::std::mem::size_of::<$field_type>() * 8 - 1) as $field_type;

            $(
                #[allow(non_camel_case_types)]
                #[derive(Default, PartialEq, Debug, Eq)]
                pub struct $field {
                    _empty: (),
                }

                impl $field {
                    #[inline]
                    #[allow(dead_code)]
                    pub fn get(&self) -> $field_type {
                        let fields: &Fields = unsafe { &*(self as *const _ as *const Fields)};
                        (fields.bits << (HIGHEST_BIT - $upper)) >> (HIGHEST_BIT - $upper + $lower)
                    }

                    #[inline]
                    #[allow(dead_code)]
                    pub fn set(&mut self, val: $field_type) {
                        let fields: &mut Fields = unsafe { &mut *(self as *mut _ as *mut Fields)};
                        let clear_mask = ((fields.bits << (HIGHEST_BIT - $upper)) >> (HIGHEST_BIT - $upper + $lower)) << $lower;

                        fields.bits ^= clear_mask;
                        fields.bits |= ((val as $field_type) << ($lower));
                    }

                    #[inline]
                    #[allow(dead_code)]
                    pub fn is_true(&self) -> bool {
                        let fields: &Fields = unsafe { &*(self as *const _ as *const Fields)};
                        (fields.bits & (1 << ($lower))) != 0
                    }
                }
            )*

            #[derive(Default, PartialEq, Debug, Eq)]
            pub struct BitField {
                _inner: Fields,
            }

            impl BitField {
                #[inline]
                #[allow(dead_code)]
                pub fn zeroed() -> Self {
                    Self {
                        _inner: Fields {
                            bits: 0,
                            $($field: $field { _empty: () },)*
                        }
                    }
                }

                #[inline]
                #[allow(dead_code)]
                pub fn new($($field: $field_type,)*) -> Self {
                    let mut new = Self::zeroed();

                    $(
                        new.$field.set($field);
                    )*

                    new
                }
            }

            impl ::std::ops::Deref for BitField {
                type Target = Fields;
                fn deref(&self) -> &Fields {
                    unsafe { &*(self as *const _ as *const Fields)}
                }
            }

            impl ::std::ops::DerefMut for BitField {
                fn deref_mut(&mut self) -> &mut Fields {
                    unsafe { &mut *(self as *mut _ as *mut Fields)}
                }
            }

            impl crate::serialize::Serialize for Fields {
                fn serialize(&self, file: &mut ::std::io::BufWriter<::std::fs::File>) -> Result<(), String> {
                    self.bits.serialize(file)
                }

                fn deserialize(&mut self, file: &mut ::std::io::BufReader<::std::fs::File>) -> Result<(), String> {
                    self.bits.deserialize(file)
                }
            }
        }
    };
    // handle trailing commas
    ( $name:ident<$field_type:ty> ($( $field:ident: $lower:literal..$upper:literal ),+ ,)) => {
        bitfield!($name<$field_type>($($field: $lower..$upper),*));
    };
}

#[test]
fn test() {
    bitfield!(MiscBits<u8>(
        int1: 0..2,
        int2: 3..6,
        bool1: 7..7,
    ));

    let mut bits = MiscBits::BitField::zeroed();

    bits.int1.set(2);
    assert_eq!(bits.int1.get(), 2);

    bits.int2.set(0xf);
    assert_eq!(bits.int2.get(), 0xf);

    bits.int2.set(7);
    assert_eq!(bits.int2.get(), 7);

    bits.bool1.set(1);
    assert!(bits.bool1.is_true());

    bits.bool1.set(0);
    assert!(!bits.bool1.is_true());
}
