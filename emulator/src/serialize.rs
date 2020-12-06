use std::io::{Read, Write};
use std::{fs, io};

#[macro_use]
use derive_serialize::Serialize;

pub trait Serialize {
    fn serialize(&self, to: &mut io::BufWriter<fs::File>) -> Result<(), String>;
    fn deserialize(&mut self, from: &mut io::BufReader<fs::File>) -> Result<(), String>;
}

macro_rules! impl_serialize_for_num {
    ($num_type:ty) => {
        impl Serialize for $num_type {
            fn serialize(&self, file: &mut io::BufWriter<fs::File>) -> Result<(), String> {
                file.write_all(&self.to_le_bytes())
                    .map_or_else(|e| Err(e.to_string()), |_| Ok(()))
            }

            fn deserialize(&mut self, file: &mut io::BufReader<fs::File>) -> Result<(), String> {
                const N_BYTES: usize = ::std::mem::size_of::<$num_type>();
                let mut bytes = [0u8; N_BYTES];

                file.read_exact(&mut bytes)
                    .or_else(|e| Err(e.to_string()))?;

                *self = <$num_type>::from_le_bytes(bytes);
                Ok(())
            }
        }
    };
}

impl_serialize_for_num!(u8);
impl_serialize_for_num!(i8);
impl_serialize_for_num!(u16);
impl_serialize_for_num!(i16);
impl_serialize_for_num!(i32);
impl_serialize_for_num!(usize);

macro_rules! impl_serialize_for_byte_array {
    ($n_bytes:literal) => {
        impl Serialize for [u8; $n_bytes] {
            fn serialize(&self, file: &mut io::BufWriter<fs::File>) -> Result<(), String> {
                file.write_all(self)
                    .map_or_else(|e| Err(e.to_string()), |_| Ok(()))
            }

            fn deserialize(&mut self, file: &mut io::BufReader<fs::File>) -> Result<(), String> {
                file.read_exact(self)
                    .map_or_else(|e| Err(e.to_string()), |_| Ok(()))
            }
        }
    };
}

// NOTE: some u8-arrays are special cased (const generics will fix this)
impl_serialize_for_byte_array!(0x400);
impl_serialize_for_byte_array!(0x800);
impl_serialize_for_byte_array!(0x1000);
impl_serialize_for_byte_array!(0x2000);

// NOTE: this does not serialize the length of the slice!
impl<T> Serialize for [T]
where
    T: Serialize,
{
    fn serialize(&self, file: &mut io::BufWriter<fs::File>) -> Result<(), String> {
        for i in self.iter() {
            i.serialize(file)?;
        }

        Ok(())
    }

    fn deserialize(&mut self, file: &mut io::BufReader<fs::File>) -> Result<(), String> {
        for i in self.iter_mut() {
            i.deserialize(file)?;
        }

        Ok(())
    }
}

// NOTE: this /does/ serialize the length of the slice
impl Serialize for Box<[u8]> {
    fn serialize(&self, file: &mut io::BufWriter<fs::File>) -> Result<(), String> {
        let len = self.len();
        len.serialize(file)?;

        file.write_all(self)
            .map_or_else(|e| Err(e.to_string()), |_| Ok(()))
    }

    fn deserialize(&mut self, file: &mut io::BufReader<fs::File>) -> Result<(), String> {
        let mut len = 0;
        len.deserialize(file)?;

        // OPTIMIZE: no need to pre-initialize vec before writing to it
        let mut new_self = vec![0; len];
        file.read_exact(&mut new_self)
            .or_else(|e| Err(e.to_string()))?;

        *self = new_self.into_boxed_slice();
        Ok(())
    }
}

impl Serialize for bool {
    fn serialize(&self, file: &mut io::BufWriter<fs::File>) -> Result<(), String> {
        file.write_all(&[*self as u8])
            .map_or_else(|e| Err(e.to_string()), |_| Ok(()))
    }

    fn deserialize(&mut self, file: &mut io::BufReader<fs::File>) -> Result<(), String> {
        let mut byte = [0];
        file.read_exact(&mut byte).or_else(|e| Err(e.to_string()))?;

        if byte[0] == 1 {
            *self = true;
            Ok(())
        } else if byte[0] == 0 {
            *self = false;
            Ok(())
        } else {
            Err("TODO: error message".to_string())
        }
    }
}

#[test]
fn serialize_test() {
    use std::io::Seek;
    let file = fs::OpenOptions::new()
        .write(true)
        .read(true)
        .create(true)
        .open("serialize_test")
        .unwrap();

    let (mut reader, mut writer) = (
        io::BufReader::new(file.try_clone().unwrap()),
        io::BufWriter::new(file.try_clone().unwrap()),
    );

    // usize
    {
        let num: usize = 0x1b87a78;
        num.serialize(&mut writer).unwrap();
        writer.flush().unwrap();

        writer.seek(std::io::SeekFrom::Start(0)).unwrap();

        let mut num2: usize = 0;
        num2.deserialize(&mut reader).unwrap();

        assert_eq!(num, num2);
    }

    writer.seek(std::io::SeekFrom::Start(0)).unwrap();

    // i32
    {
        let num: i32 = 0x0a_f0_88_2e;
        num.serialize(&mut writer).unwrap();
        writer.flush().unwrap();

        writer.seek(std::io::SeekFrom::Start(0)).unwrap();

        let mut num2: i32 = 0;
        reader = io::BufReader::new(file.try_clone().unwrap());
        num2.deserialize(&mut reader).unwrap();

        assert_eq!(num, num2);
    }

    writer.seek(std::io::SeekFrom::Start(0)).unwrap();

    // [u8, 7]
    {
        let array = [0xf, 0x1e, 0x38, 0xcc, 0x9a, 0x5e, 0x8a];
        array.serialize(&mut writer).unwrap();
        writer.flush().unwrap();

        writer.seek(std::io::SeekFrom::Start(0)).unwrap();

        let mut array2 = [0; 7];
        reader = io::BufReader::new(file.try_clone().unwrap());
        array2.deserialize(&mut reader).unwrap();

        assert_eq!(array, array2);
    }

    writer.seek(std::io::SeekFrom::Start(0)).unwrap();

    // Box<[u8]>
    {
        let boxed_slice = vec![0xff; 10].into_boxed_slice();
        boxed_slice.serialize(&mut writer).unwrap();
        writer.flush().unwrap();

        writer.seek(std::io::SeekFrom::Start(0)).unwrap();

        let mut boxed_slice2: Box<[u8]> = [0].into();
        reader = io::BufReader::new(file.try_clone().unwrap());
        boxed_slice2.deserialize(&mut reader).unwrap();

        assert_eq!(boxed_slice, boxed_slice2);
    }

    writer.seek(std::io::SeekFrom::Start(0)).unwrap();

    // named-field struct
    {
        #[derive(Serialize, PartialEq, Eq, Debug, Default)]
        struct Thing {
            num1: u8,
            num2: u8,
        }

        let thing = Thing {
            num1: 0xea,
            num2: 0xfb,
        };
        thing.serialize(&mut writer).unwrap();
        writer.flush().unwrap();

        writer.seek(std::io::SeekFrom::Start(0)).unwrap();

        let mut thing2 = Thing::default();
        reader = io::BufReader::new(file.try_clone().unwrap());
        thing2.deserialize(&mut reader).unwrap();

        assert_eq!(thing, thing2);
    }

    writer.seek(std::io::SeekFrom::Start(0)).unwrap();

    // tuple struct
    {
        #[derive(Serialize, PartialEq, Eq, Debug, Default)]
        struct OtherThing(u8);

        let other_thing = OtherThing(0xea);
        other_thing.serialize(&mut writer).unwrap();
        writer.flush().unwrap();

        writer.seek(std::io::SeekFrom::Start(0)).unwrap();

        let mut other_thing2 = OtherThing::default();
        reader = io::BufReader::new(file.try_clone().unwrap());
        other_thing2.deserialize(&mut reader).unwrap();

        assert_eq!(other_thing, other_thing2);
    }

    writer.seek(std::io::SeekFrom::Start(0)).unwrap();

    // [u8; 0x400]
    {
        let byte_array = [0x12u8; 0x400];
        byte_array.serialize(&mut writer).unwrap();
        writer.flush().unwrap();

        writer.seek(std::io::SeekFrom::Start(0)).unwrap();

        let mut byte_array2 = [0u8; 0x400];
        reader = io::BufReader::new(file.try_clone().unwrap());
        byte_array2.deserialize(&mut reader).unwrap();

        assert_eq!(byte_array, byte_array2);
    }

    writer.seek(std::io::SeekFrom::Start(0)).unwrap();

    // bitfield
    {
        bitfield!(Bits<u8>(
            one: 0..0,
            two: 1..2,
            three: 3..5,
        ));

        let bits = Bits::BitField::new(1, 0b11, 0b101);
        bits.serialize(&mut writer).unwrap();
        writer.flush().unwrap();

        writer.seek(std::io::SeekFrom::Start(0)).unwrap();

        let mut bits2 = Bits::BitField::zeroed();
        reader = io::BufReader::new(file.try_clone().unwrap());
        bits2.deserialize(&mut reader).unwrap();

        assert_eq!(bits, bits2);
    }
}
