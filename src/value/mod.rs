use std::mem;

use crate::{
    binary::Binary,
    error::{DaptResult, Error},
};

use base64::{engine::general_purpose::STANDARD_NO_PAD as Base64Encoder, Engine as _};

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

pub const TYPE_U8: u8 = 4;
pub const TYPE_U16: u8 = 5;
pub const TYPE_U32: u8 = 6;
pub const TYPE_U64: u8 = 7;
pub const TYPE_U128: u8 = 8;
pub const TYPE_USIZE: u8 = 9;
pub const TYPE_I8: u8 = 10;
pub const TYPE_I16: u8 = 11;
pub const TYPE_I32: u8 = 12;
pub const TYPE_I64: u8 = 13;
pub const TYPE_I128: u8 = 14;
pub const TYPE_ISIZE: u8 = 15;
pub const TYPE_STR: u8 = 16;
pub const TYPE_F32: u8 = 17;
pub const TYPE_F64: u8 = 18;
pub const TYPE_CHAR: u8 = 19;
pub const TYPE_BYTES: u8 = 20;
pub const TYPE_BOOL: u8 = 21;

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
                let mut result: $type = Default::default();
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
impl_serialize_deserialize!(u128, TYPE_U128);
impl_serialize_deserialize!(usize, TYPE_USIZE);
impl_serialize_deserialize!(i8, TYPE_I8);
impl_serialize_deserialize!(i16, TYPE_I16);
impl_serialize_deserialize!(i32, TYPE_I32);
impl_serialize_deserialize!(i64, TYPE_I64);
impl_serialize_deserialize!(i128, TYPE_I128);
impl_serialize_deserialize!(isize, TYPE_ISIZE);
impl_serialize_deserialize!(f32, TYPE_F32);
impl_serialize_deserialize!(f64, TYPE_F64);
impl_serialize_deserialize!(char, TYPE_CHAR);
impl_serialize_deserialize!(bool, TYPE_BOOL);

// impl<T: AsRef<str>> Serialize for T {
//     fn size_of(&self) -> usize {
//         self.as_ref().len()
//     }

//     fn serialize(&self, buf: &mut [u8]) -> u8 {
//         buf.copy_from_slice(self.as_ref().as_bytes());
//         TYPE_STR
//     }
// }

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

impl Serialize for &[u8] {
    fn size_of(&self) -> usize {
        self.len()
    }

    fn serialize(&self, buf: &mut [u8]) -> u8 {
        buf.copy_from_slice(self);
        TYPE_BYTES
    }
}

impl<'a> Deserialize<'a> for &'a [u8] {
    type Item = &'a [u8];

    fn type_of() -> u8 {
        TYPE_BYTES
    }

    fn deserialize(buf: &'a [u8]) -> Self::Item {
        buf
    }
}

pub enum Number {
    USize(usize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    ISize(isize),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    F32(f32),
    F64(f64),
}

impl Number {
    pub fn new(b: &Binary, index: usize) -> DaptResult<Number> {
        let tpe = b.type_at(index).unwrap();
        match tpe {
            TYPE_U8 => Ok(Number::U8(b.get::<u8>(index).unwrap())),
            TYPE_U16 => Ok(Number::U16(b.get::<u16>(index).unwrap())),
            TYPE_U32 => Ok(Number::U32(b.get::<u32>(index).unwrap())),
            TYPE_U64 => Ok(Number::U64(b.get::<u64>(index).unwrap())),
            TYPE_U128 => Ok(Number::U128(b.get::<u128>(index).unwrap())),
            TYPE_USIZE => Ok(Number::USize(b.get::<usize>(index).unwrap())),
            TYPE_I8 => Ok(Number::I8(b.get::<i8>(index).unwrap())),
            TYPE_I16 => Ok(Number::I16(b.get::<i16>(index).unwrap())),
            TYPE_I32 => Ok(Number::I32(b.get::<i32>(index).unwrap())),
            TYPE_I64 => Ok(Number::I64(b.get::<i64>(index).unwrap())),
            TYPE_I128 => Ok(Number::I128(b.get::<i128>(index).unwrap())),
            TYPE_ISIZE => Ok(Number::ISize(b.get::<isize>(index).unwrap())),
            TYPE_F32 => Ok(Number::F32(b.get::<f32>(index).unwrap())),
            TYPE_F64 => Ok(Number::F64(b.get::<f64>(index).unwrap())),
            _ => Err(Error::TypeMismatch(tpe, "expected number type".into())),
        }
    }
}

macro_rules! impl_number_into {
    ($type:ty) => {
        impl From<Number> for $type {
            fn from(orig: Number) -> Self {
                match orig {
                    Number::USize(num) => num as $type,
                    Number::U8(num) => num as $type,
                    Number::U16(num) => num as $type,
                    Number::U32(num) => num as $type,
                    Number::U64(num) => num as $type,
                    Number::U128(num) => num as $type,
                    Number::ISize(num) => num as $type,
                    Number::I8(num) => num as $type,
                    Number::I16(num) => num as $type,
                    Number::I32(num) => num as $type,
                    Number::I64(num) => num as $type,
                    Number::I128(num) => num as $type,
                    Number::F32(num) => num as $type,
                    Number::F64(num) => num as $type,
                }
            }
        }
    };
}

impl_number_into!(u8);
impl_number_into!(u16);
impl_number_into!(u32);
impl_number_into!(u64);
impl_number_into!(u128);
impl_number_into!(usize);
impl_number_into!(i8);
impl_number_into!(i16);
impl_number_into!(i32);
impl_number_into!(i64);
impl_number_into!(i128);
impl_number_into!(isize);
impl_number_into!(f32);
impl_number_into!(f64);

pub enum Any<'a> {
    USize(usize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    ISize(isize),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    F32(f32),
    F64(f64),
    Str(&'a str),
    Bytes(&'a [u8]),
    Char(char),
    Bool(bool),
}

impl<'a> Any<'a> {
    pub fn new(b: &'a Binary, index: usize) -> Option<Any<'a>> {
        let tpe = b.type_at(index)?;
        match tpe {
            TYPE_U8 => Some(Any::U8(b.get::<u8>(index).unwrap())),
            TYPE_U16 => Some(Any::U16(b.get::<u16>(index).unwrap())),
            TYPE_U32 => Some(Any::U32(b.get::<u32>(index).unwrap())),
            TYPE_U64 => Some(Any::U64(b.get::<u64>(index).unwrap())),
            TYPE_U128 => Some(Any::U128(b.get::<u128>(index).unwrap())),
            TYPE_USIZE => Some(Any::USize(b.get::<usize>(index).unwrap())),
            TYPE_I8 => Some(Any::I8(b.get::<i8>(index).unwrap())),
            TYPE_I16 => Some(Any::I16(b.get::<i16>(index).unwrap())),
            TYPE_I32 => Some(Any::I32(b.get::<i32>(index).unwrap())),
            TYPE_I64 => Some(Any::I64(b.get::<i64>(index).unwrap())),
            TYPE_I128 => Some(Any::I128(b.get::<i128>(index).unwrap())),
            TYPE_ISIZE => Some(Any::ISize(b.get::<isize>(index).unwrap())),
            TYPE_F32 => Some(Any::F32(b.get::<f32>(index).unwrap())),
            TYPE_F64 => Some(Any::F64(b.get::<f64>(index).unwrap())),
            TYPE_STR => Some(Any::Str(b.get::<&'a str>(index).unwrap())),
            TYPE_BYTES => Some(Any::Bytes(b.get::<&'a [u8]>(index).unwrap())),
            TYPE_CHAR => Some(Any::Char(b.get::<char>(index).unwrap())),
            TYPE_BOOL => Some(Any::Bool(b.get::<bool>(index).unwrap())),
            _ => None,
        }
    }
}

impl From<Any<'_>> for String {
    fn from(value: Any) -> Self {
        match value {
            Any::Str(val) => String::from(val),
            Any::U8(val) => val.to_string(),
            Any::U16(val) => val.to_string(),
            Any::U32(val) => val.to_string(),
            Any::U64(val) => val.to_string(),
            Any::U128(val) => val.to_string(),
            Any::USize(val) => val.to_string(),
            Any::I8(val) => val.to_string(),
            Any::I16(val) => val.to_string(),
            Any::I32(val) => val.to_string(),
            Any::I64(val) => val.to_string(),
            Any::I128(val) => val.to_string(),
            Any::ISize(val) => val.to_string(),
            Any::F32(val) => val.to_string(),
            Any::F64(val) => val.to_string(),
            Any::Bytes(val) => Base64Encoder.encode(val),
            Any::Char(val) => val.to_string(),
            Any::Bool(val) => val.to_string(),
        }
    }
}

impl TryFrom<Any<'_>> for Number {
    type Error = Error;
    fn try_from(value: Any) -> Result<Self, Self::Error> {
        match value {
            Any::Str(val) => {
                if val.contains('.') {
                    Ok(Number::F64(val.parse()?))
                } else {
                    Ok(Number::ISize(val.parse()?))
                }
            }
            Any::U8(val) => Ok(Number::U8(val)),
            Any::U16(val) => Ok(Number::U16(val)),
            Any::U32(val) => Ok(Number::U32(val)),
            Any::U64(val) => Ok(Number::U64(val)),
            Any::U128(val) => Ok(Number::U128(val)),
            Any::USize(val) => Ok(Number::USize(val)),
            Any::I8(val) => Ok(Number::I8(val)),
            Any::I16(val) => Ok(Number::I16(val)),
            Any::I32(val) => Ok(Number::I32(val)),
            Any::I64(val) => Ok(Number::I64(val)),
            Any::I128(val) => Ok(Number::I128(val)),
            Any::ISize(val) => Ok(Number::ISize(val)),
            Any::F32(val) => Ok(Number::F32(val)),
            Any::F64(val) => Ok(Number::F64(val)),
            _ => Err(Error::NumberConversionFailed(
                "Could not convert into number".into(),
            )),
        }
    }
}
