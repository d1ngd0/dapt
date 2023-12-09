use std::mem;

// Serialize is used to
pub trait Serialize {
    fn size_of(&self) -> usize;
    fn serialize(&self, buf: &mut [u8]) -> u8;
}

// Deserialize is used to turn the type back into the
//
pub trait Deserialize<'a> {
    type Item;
    fn type_of() -> u8;
    fn deserialize(buf: &'a [u8]) -> Self::Item;
}

const TYPE_U8: u8 = 4;
const TYPE_U16: u8 = 5;
const TYPE_U32: u8 = 6;
const TYPE_U64: u8 = 7;
const TYPE_I8: u8 = 8;
const TYPE_I16: u8 = 9;
const TYPE_I32: u8 = 10;
const TYPE_I64: u8 = 11;
const TYPE_STR: u8 = 12;

impl Serialize for u8 {
    fn size_of(&self) -> usize {
        mem::size_of::<u8>()
    }

    fn serialize(&self, buf: &mut [u8]) -> u8 {
        buf[0] = *self;
        TYPE_U8
    }
}

impl<'a> Deserialize<'a> for u8 {
    type Item = u8;

    fn type_of() -> u8 {
        TYPE_U8
    }

    fn deserialize(buf: &'a [u8]) -> u8 {
        *buf.first().expect("bytes in token is less than 1 for u8")
    }
}

// Macro to generate Serialize and Deserialize implementations
macro_rules! impl_serialize_deserialize {
    ($type:ty, $type_id:expr) => {
        impl Serialize for $type {
            fn size_of(&self) -> usize {
                mem::size_of::<$type>()
            }

            fn serialize(&self, buf: &mut [u8]) -> u8 {
                let b: &[u8] = unsafe {
                    std::slice::from_raw_parts(self as *const $type as *const u8, self.size_of())
                };
                buf.copy_from_slice(b);
                $type_id
            }
        }

        impl<'a> Deserialize<'a> for $type {
            type Item = $type;

            fn type_of() -> u8 {
                $type_id
            }

            fn deserialize(buf: &'a [u8]) -> $type {
                let mut result: $type = 0;
                unsafe {
                    let ptr = &mut result as *mut $type as *mut u8;
                    ptr.copy_from_nonoverlapping(buf.as_ptr(), mem::size_of::<$type>());
                }
                result
            }
        }
    };
}

// Implement for u16, u32, u64, i16, i32, i64
impl_serialize_deserialize!(u16, TYPE_U16);
impl_serialize_deserialize!(u32, TYPE_U32);
impl_serialize_deserialize!(u64, TYPE_U64);
impl_serialize_deserialize!(i8, TYPE_I8);
impl_serialize_deserialize!(i16, TYPE_I16);
impl_serialize_deserialize!(i32, TYPE_I32);
impl_serialize_deserialize!(i64, TYPE_I64);

impl Serialize for &str {
    fn size_of(&self) -> usize {
        self.len()
    }

    fn serialize(&self, buf: &mut [u8]) -> u8 {
        buf.copy_from_slice(self.as_bytes());
        TYPE_STR
    }
}

impl<'a> Deserialize<'a> for &'a str {
    type Item = &'a str;

    fn type_of() -> u8 {
        TYPE_STR
    }

    fn deserialize(buf: &'a [u8]) -> Self::Item {
        unsafe { std::str::from_utf8_unchecked(buf) }
    }
}
