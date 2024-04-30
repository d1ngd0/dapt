use std::{
    cmp::Ordering,
    collections::HashMap,
    mem,
    ops::{Add, Div, Mul, Rem, Sub},
};

use crate::{
    binary::Binary,
    error::{DaptResult, Error},
};

use base64::{engine::general_purpose::STANDARD_NO_PAD as Base64Encoder, Engine as _};

use super::{BArray, BMap, BToken, TYPE_ARRAY, TYPE_MAP};

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
pub const TYPE_NULL: u8 = 22;

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

impl Serialize for String {
    fn size_of(&self) -> usize {
        self.len()
    }

    fn serialize(&self, buf: &mut [u8]) -> u8 {
        buf.copy_from_slice(self.as_bytes());
        TYPE_STR
    }
}

impl Deserialize<'_> for String {
    type Item = String;

    fn type_of() -> u8 {
        TYPE_STR
    }

    fn deserialize(buf: &[u8]) -> Self::Item {
        unsafe { String::from_utf8_unchecked(buf.to_vec()) }
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

impl Serialize for Vec<u8> {
    fn size_of(&self) -> usize {
        self.len()
    }

    fn serialize(&self, buf: &mut [u8]) -> u8 {
        buf.copy_from_slice(self);
        TYPE_BYTES
    }
}

impl Deserialize<'_> for Vec<u8> {
    type Item = Vec<u8>;

    fn type_of() -> u8 {
        TYPE_BYTES
    }

    fn deserialize(buf: &'_ [u8]) -> Self::Item {
        buf.to_vec()
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

impl Serialize for () {
    fn size_of(&self) -> usize {
        0
    }

    fn serialize(&self, _: &mut [u8]) -> u8 {
        TYPE_NULL
    }
}

impl Deserialize<'_> for () {
    type Item = ();

    fn type_of() -> u8 {
        TYPE_NULL
    }

    fn deserialize(_: &[u8]) -> Self::Item {
        ()
    }
}

#[derive(Debug, Clone, Copy)]
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
    pub fn new(b: &Binary, token: BToken) -> DaptResult<Number> {
        match token.get_type(b) {
            TYPE_U8 => Ok(Number::U8(b.get::<u8>(token.get_reference(b)).unwrap())),
            TYPE_U16 => Ok(Number::U16(b.get::<u16>(token.get_reference(b)).unwrap())),
            TYPE_U32 => Ok(Number::U32(b.get::<u32>(token.get_reference(b)).unwrap())),
            TYPE_U64 => Ok(Number::U64(b.get::<u64>(token.get_reference(b)).unwrap())),
            TYPE_U128 => Ok(Number::U128(b.get::<u128>(token.get_reference(b)).unwrap())),
            TYPE_USIZE => Ok(Number::USize(
                b.get::<usize>(token.get_reference(b)).unwrap(),
            )),
            TYPE_I8 => Ok(Number::I8(b.get::<i8>(token.get_reference(b)).unwrap())),
            TYPE_I16 => Ok(Number::I16(b.get::<i16>(token.get_reference(b)).unwrap())),
            TYPE_I32 => Ok(Number::I32(b.get::<i32>(token.get_reference(b)).unwrap())),
            TYPE_I64 => Ok(Number::I64(b.get::<i64>(token.get_reference(b)).unwrap())),
            TYPE_I128 => Ok(Number::I128(b.get::<i128>(token.get_reference(b)).unwrap())),
            TYPE_ISIZE => Ok(Number::ISize(
                b.get::<isize>(token.get_reference(b)).unwrap(),
            )),
            TYPE_F32 => Ok(Number::F32(b.get::<f32>(token.get_reference(b)).unwrap())),
            TYPE_F64 => Ok(Number::F64(b.get::<f64>(token.get_reference(b)).unwrap())),
            _ => Err(Error::TypeMismatch(
                token.get_type(b),
                "expected number type".into(),
            )),
        }
    }
}

macro_rules! impl_number_into {
    ($type:ty) => {
        // TODO: These are not implicitly safe, and we should handle them
        // in a better way.
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

#[derive(Debug, serde::Serialize, serde::Deserialize, Clone)]
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
    Array(Vec<Any<'a>>),
    Map(HashMap<&'a str, Any<'a>>),
    Null,
}

// while this works I kinda hate it, is this idomatic or is there a better way to do
// this?
#[derive(Debug, Clone)]
pub enum OwnedAny {
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
    Str(String),
    Bytes(Vec<u8>),
    Char(char),
    Bool(bool),
    Array(Vec<OwnedAny>),
    Map(HashMap<String, OwnedAny>),
    Null,
}

impl From<Any<'_>> for OwnedAny {
    fn from(value: Any) -> Self {
        match value {
            Any::USize(val) => OwnedAny::USize(val),
            Any::U8(val) => OwnedAny::U8(val),
            Any::U16(val) => OwnedAny::U16(val),
            Any::U32(val) => OwnedAny::U32(val),
            Any::U64(val) => OwnedAny::U64(val),
            Any::U128(val) => OwnedAny::U128(val),
            Any::ISize(val) => OwnedAny::ISize(val),
            Any::I8(val) => OwnedAny::I8(val),
            Any::I16(val) => OwnedAny::I16(val),
            Any::I32(val) => OwnedAny::I32(val),
            Any::I64(val) => OwnedAny::I64(val),
            Any::I128(val) => OwnedAny::I128(val),
            Any::F32(val) => OwnedAny::F32(val),
            Any::F64(val) => OwnedAny::F64(val),
            Any::Str(val) => OwnedAny::Str(val.to_string()),
            Any::Bytes(val) => OwnedAny::Bytes(val.to_vec()),
            Any::Char(val) => OwnedAny::Char(val),
            Any::Bool(val) => OwnedAny::Bool(val),
            Any::Array(val) => {
                let mut items = Vec::with_capacity(val.len());
                for item in val {
                    items.push(OwnedAny::from(item));
                }
                OwnedAny::Array(items)
            }
            Any::Map(val) => {
                let mut items = HashMap::with_capacity(val.len());
                for (key, value) in val {
                    items.insert(key.to_string(), OwnedAny::from(value));
                }
                OwnedAny::Map(items)
            }
            Any::Null => OwnedAny::Null,
        }
    }
}

impl<'a> From<&'a OwnedAny> for Any<'a> {
    fn from(value: &'a OwnedAny) -> Self {
        match value {
            OwnedAny::USize(val) => Any::USize(*val),
            OwnedAny::U8(val) => Any::U8(*val),
            OwnedAny::U16(val) => Any::U16(*val),
            OwnedAny::U32(val) => Any::U32(*val),
            OwnedAny::U64(val) => Any::U64(*val),
            OwnedAny::U128(val) => Any::U128(*val),
            OwnedAny::ISize(val) => Any::ISize(*val),
            OwnedAny::I8(val) => Any::I8(*val),
            OwnedAny::I16(val) => Any::I16(*val),
            OwnedAny::I32(val) => Any::I32(*val),
            OwnedAny::I64(val) => Any::I64(*val),
            OwnedAny::I128(val) => Any::I128(*val),
            OwnedAny::F32(val) => Any::F32(*val),
            OwnedAny::F64(val) => Any::F64(*val),
            OwnedAny::Str(val) => Any::Str(val),
            OwnedAny::Bytes(val) => Any::Bytes(val),
            OwnedAny::Char(val) => Any::Char(*val),
            OwnedAny::Bool(val) => Any::Bool(*val),
            OwnedAny::Array(val) => {
                let mut items = Vec::with_capacity(val.len());
                for item in val {
                    items.push(Any::from(item));
                }
                Any::Array(items)
            }
            OwnedAny::Map(val) => {
                let mut items = HashMap::with_capacity(val.len());
                for (key, value) in val {
                    items.insert(&key[..], Any::from(value));
                }
                Any::Map(items)
            }
            OwnedAny::Null => Any::Null,
        }
    }
}

impl<'a> Any<'a> {
    pub fn new(b: &'a Binary, token: BToken) -> Option<Any<'a>> {
        match token.get_type(b) {
            TYPE_U8 => Some(Any::U8(b.get::<u8>(token.get_reference(b)).unwrap())),
            TYPE_U16 => Some(Any::U16(b.get::<u16>(token.get_reference(b)).unwrap())),
            TYPE_U32 => Some(Any::U32(b.get::<u32>(token.get_reference(b)).unwrap())),
            TYPE_U64 => Some(Any::U64(b.get::<u64>(token.get_reference(b)).unwrap())),
            TYPE_U128 => Some(Any::U128(b.get::<u128>(token.get_reference(b)).unwrap())),
            TYPE_USIZE => Some(Any::USize(b.get::<usize>(token.get_reference(b)).unwrap())),
            TYPE_I8 => Some(Any::I8(b.get::<i8>(token.get_reference(b)).unwrap())),
            TYPE_I16 => Some(Any::I16(b.get::<i16>(token.get_reference(b)).unwrap())),
            TYPE_I32 => Some(Any::I32(b.get::<i32>(token.get_reference(b)).unwrap())),
            TYPE_I64 => Some(Any::I64(b.get::<i64>(token.get_reference(b)).unwrap())),
            TYPE_I128 => Some(Any::I128(b.get::<i128>(token.get_reference(b)).unwrap())),
            TYPE_ISIZE => Some(Any::ISize(b.get::<isize>(token.get_reference(b)).unwrap())),
            TYPE_F32 => Some(Any::F32(b.get::<f32>(token.get_reference(b)).unwrap())),
            TYPE_F64 => Some(Any::F64(b.get::<f64>(token.get_reference(b)).unwrap())),
            TYPE_STR => Some(Any::Str(b.get::<&'a str>(token.get_reference(b)).unwrap())),
            TYPE_BYTES => Some(Any::Bytes(
                b.get::<&'a [u8]>(token.get_reference(b)).unwrap(),
            )),
            TYPE_CHAR => Some(Any::Char(b.get::<char>(token.get_reference(b)).unwrap())),
            TYPE_BOOL => Some(Any::Bool(b.get::<bool>(token.get_reference(b)).unwrap())),
            TYPE_ARRAY => {
                let arr = BArray::from(token);
                let mut items = Vec::with_capacity(arr.length(b));
                for i in 0..arr.length(b) {
                    let val_tok = arr.child_index(b, i).unwrap().val_at(b).unwrap();
                    items.push(Any::new(b, val_tok).unwrap());
                }
                Some(Any::Array(items))
            }
            TYPE_MAP => {
                let map = BMap::from(token);
                let mut items = HashMap::with_capacity(map.length(b));
                for i in 0..map.length(b) {
                    let key_tok = map.child_index(b, i).unwrap().key_at(b).unwrap();
                    let val_tok = key_tok.child(b).unwrap().val_at(b).unwrap();
                    items.insert(key_tok.key(b), Any::new(b, val_tok).unwrap());
                }
                Some(Any::Map(items))
            }
            TYPE_NULL => Some(Any::Null),
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
            Any::Array(val) => {
                let mut result = String::new();
                let mut first = true;
                for item in val {
                    if first {
                        first = false;
                    } else {
                        result.push_str(", ");
                    }

                    result.push_str(&String::from(item));
                }
                result
            }
            Any::Map(val) => {
                let mut result = String::new();
                let mut first = true;
                for (key, value) in val {
                    if first {
                        first = false;
                    } else {
                        result.push_str(", ");
                    }

                    result.push_str(&format!("{}: {}", key, String::from(value)));
                }
                result
            }
            Any::Null => "<null>".to_string(),
        }
    }
}

impl From<Number> for Any<'_> {
    fn from(value: Number) -> Self {
        match value {
            Number::USize(val) => Any::USize(val),
            Number::U8(val) => Any::U8(val),
            Number::U16(val) => Any::U16(val),
            Number::U32(val) => Any::U32(val),
            Number::U64(val) => Any::U64(val),
            Number::U128(val) => Any::U128(val),
            Number::ISize(val) => Any::ISize(val),
            Number::I8(val) => Any::I8(val),
            Number::I16(val) => Any::I16(val),
            Number::I32(val) => Any::I32(val),
            Number::I64(val) => Any::I64(val),
            Number::I128(val) => Any::I128(val),
            Number::F32(val) => Any::F32(val),
            Number::F64(val) => Any::F64(val),
        }
    }
}

macro_rules! number_eq_arm {
    ($type:ty, $a:ident, $other:ident) => {{
        // TODO: Let your heart fill with enough anger that you
        // solve this problem. Then feel a great release once you
        // give up and cry in the corner. Uncomment to see my pain.
        // https://internals.rust-lang.org/t/tryfrom-for-f64/9793
        // let val = match <$type>::try_from(*$other) {
        //     Ok(val) => val,
        //     Err(_) => return false,
        // };

        // this can panic :D but it's fine
        // everything is fine
        // just fine
        // I'm fine
        //
        //
        // I'm not fine
        *$a == *$other as $type
    }};
}

macro_rules! impl_partial_eq_number {
    ($type:ty) => {
        impl PartialEq<$type> for Number {
            fn eq(&self, other: &$type) -> bool {
                match self {
                    Number::U8(a) => number_eq_arm!(u8, a, other),
                    Number::U16(a) => number_eq_arm!(u16, a, other),
                    Number::U32(a) => number_eq_arm!(u32, a, other),
                    Number::U64(a) => number_eq_arm!(u64, a, other),
                    Number::U128(a) => number_eq_arm!(u128, a, other),
                    Number::USize(a) => number_eq_arm!(usize, a, other),
                    Number::I8(a) => number_eq_arm!(i8, a, other),
                    Number::I16(a) => number_eq_arm!(i16, a, other),
                    Number::I32(a) => number_eq_arm!(i32, a, other),
                    Number::I64(a) => number_eq_arm!(i64, a, other),
                    Number::I128(a) => number_eq_arm!(i128, a, other),
                    Number::ISize(a) => number_eq_arm!(isize, a, other),
                    Number::F32(a) => number_eq_arm!(f32, a, other),
                    Number::F64(a) => number_eq_arm!(f64, a, other),
                }
            }
        }
    };
}

impl_partial_eq_number!(u8);
impl_partial_eq_number!(u16);
impl_partial_eq_number!(u32);
impl_partial_eq_number!(u64);
impl_partial_eq_number!(u128);
impl_partial_eq_number!(usize);
impl_partial_eq_number!(i8);
impl_partial_eq_number!(i16);
impl_partial_eq_number!(i32);
impl_partial_eq_number!(i64);
impl_partial_eq_number!(i128);
impl_partial_eq_number!(isize);
impl_partial_eq_number!(f32);
impl_partial_eq_number!(f64);

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Number::U8(a) => other == a,
            Number::U16(a) => other == a,
            Number::U32(a) => other == a,
            Number::U64(a) => other == a,
            Number::U128(a) => other == a,
            Number::USize(a) => other == a,
            Number::I8(a) => other == a,
            Number::I16(a) => other == a,
            Number::I32(a) => other == a,
            Number::I64(a) => other == a,
            Number::I128(a) => other == a,
            Number::ISize(a) => other == a,
            Number::F32(a) => other == a,
            Number::F64(a) => other == a,
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match other {
            Number::USize(other) => self.partial_cmp(other),
            Number::U8(other) => self.partial_cmp(other),
            Number::U16(other) => self.partial_cmp(other),
            Number::U32(other) => self.partial_cmp(other),
            Number::U64(other) => self.partial_cmp(other),
            Number::U128(other) => self.partial_cmp(other),
            Number::ISize(other) => self.partial_cmp(other),
            Number::I8(other) => self.partial_cmp(other),
            Number::I16(other) => self.partial_cmp(other),
            Number::I32(other) => self.partial_cmp(other),
            Number::I64(other) => self.partial_cmp(other),
            Number::I128(other) => self.partial_cmp(other),
            Number::F32(other) => self.partial_cmp(other),
            Number::F64(other) => self.partial_cmp(other),
        }
    }
}

macro_rules! number_ord_arm {
    ($type:ty, $a:ident, $other:ident) => {{
        // here too we must handle converting to unsafe types
        // because this will panic. scoll up to see more context
        // on my disent into madness
        let val = *$other as $type;
        $a.partial_cmp(&val)
    }};
}

macro_rules! impl_partial_ord_number {
    ($type:ty) => {
        impl PartialOrd<$type> for Number {
            fn partial_cmp(&self, other: &$type) -> Option<Ordering> {
                match self {
                    Number::USize(val) => number_ord_arm!(usize, val, other),
                    Number::U8(val) => number_ord_arm!(u8, val, other),
                    Number::U16(val) => number_ord_arm!(u16, val, other),
                    Number::U32(val) => number_ord_arm!(u32, val, other),
                    Number::U64(val) => number_ord_arm!(u64, val, other),
                    Number::U128(val) => number_ord_arm!(u128, val, other),
                    Number::ISize(val) => number_ord_arm!(isize, val, other),
                    Number::I8(val) => number_ord_arm!(i8, val, other),
                    Number::I16(val) => number_ord_arm!(i16, val, other),
                    Number::I32(val) => number_ord_arm!(i32, val, other),
                    Number::I64(val) => number_ord_arm!(i64, val, other),
                    Number::I128(val) => number_ord_arm!(i128, val, other),
                    Number::F32(val) => number_ord_arm!(f32, val, other),
                    Number::F64(val) => number_ord_arm!(f64, val, other),
                }
            }
        }
    };
}

impl_partial_ord_number!(u8);
impl_partial_ord_number!(u16);
impl_partial_ord_number!(u32);
impl_partial_ord_number!(u64);
impl_partial_ord_number!(u128);
impl_partial_ord_number!(usize);
impl_partial_ord_number!(i8);
impl_partial_ord_number!(i16);
impl_partial_ord_number!(i32);
impl_partial_ord_number!(i64);
impl_partial_ord_number!(i128);
impl_partial_ord_number!(isize);
impl_partial_ord_number!(f32);
impl_partial_ord_number!(f64);

macro_rules! impl_math_op_type {
    ($type:ty, $name:ident, $fn:ident, $op:tt) => {
        impl $name<$type> for Number {
            type Output = Number;

            fn $fn(self, other: $type) -> Self::Output {
                match self {
                    Number::U8(a) => Number::U8(a $op other as u8),
                    Number::U16(a) => Number::U16(a $op other as u16),
                    Number::U32(a) => Number::U32(a $op other as u32),
                    Number::U64(a) => Number::U64(a $op other as u64),
                    Number::U128(a) => Number::U128(a $op other as u128),
                    Number::USize(a) => Number::USize(a $op other as usize),
                    Number::I8(a) => Number::I8(a $op other as i8),
                    Number::I16(a) => Number::I16(a $op other as i16),
                    Number::I32(a) => Number::I32(a $op other as i32),
                    Number::I64(a) => Number::I64(a $op other as i64),
                    Number::I128(a) => Number::I128(a $op other as i128),
                    Number::ISize(a) => Number::ISize(a $op other as isize),
                    Number::F32(a) => Number::F32(a $op other as f32),
                    Number::F64(a) => Number::F64(a $op other as f64),
                }
            }
        }
    };
}

macro_rules! impl_math_number {
    ($name:ident, $fn:ident, $op:tt) => {
        impl_math_op_type!(u8, $name, $fn, $op);
        impl_math_op_type!(u16, $name, $fn, $op);
        impl_math_op_type!(u32, $name, $fn, $op);
        impl_math_op_type!(u64, $name, $fn, $op);
        impl_math_op_type!(u128, $name, $fn, $op);
        impl_math_op_type!(usize, $name, $fn, $op);
        impl_math_op_type!(i8, $name, $fn, $op);
        impl_math_op_type!(i16, $name, $fn, $op);
        impl_math_op_type!(i32, $name, $fn, $op);
        impl_math_op_type!(i64, $name, $fn, $op);
        impl_math_op_type!(i128, $name, $fn, $op);
        impl_math_op_type!(isize, $name, $fn, $op);
        impl_math_op_type!(f32, $name, $fn, $op);
        impl_math_op_type!(f64, $name, $fn, $op);

        impl $name<Number> for Number {
            type Output = Number;

            fn $fn(self, other: Number) -> Self::Output {
                match other {
                    Number::U8(a) => self $op a,
                    Number::U16(a) => self $op a,
                    Number::U32(a) => self $op a,
                    Number::U64(a) => self $op a,
                    Number::U128(a) => self $op a,
                    Number::USize(a) => self $op a,
                    Number::I8(a) => self $op a,
                    Number::I16(a) => self $op a,
                    Number::I32(a) => self $op a,
                    Number::I64(a) => self $op a,
                    Number::I128(a) => self $op a,
                    Number::ISize(a) => self $op a,
                    Number::F32(a) => self $op a,
                    Number::F64(a) => self $op a,
                }
            }
        }
    };
}

impl_math_number!(Add, add, +);
impl_math_number!(Sub, sub, -);
impl_math_number!(Mul, mul, *);
impl_math_number!(Div, div, /);
impl_math_number!(Rem, rem, %);

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

impl TryFrom<&Any<'_>> for Number {
    type Error = Error;
    fn try_from(value: &Any) -> Result<Self, Self::Error> {
        match value {
            Any::Str(val) => {
                if val.contains('.') {
                    Ok(Number::F64(val.parse()?))
                } else {
                    Ok(Number::ISize(val.parse()?))
                }
            }
            Any::U8(val) => Ok(Number::U8(*val)),
            Any::U16(val) => Ok(Number::U16(*val)),
            Any::U32(val) => Ok(Number::U32(*val)),
            Any::U64(val) => Ok(Number::U64(*val)),
            Any::U128(val) => Ok(Number::U128(*val)),
            Any::USize(val) => Ok(Number::USize(*val)),
            Any::I8(val) => Ok(Number::I8(*val)),
            Any::I16(val) => Ok(Number::I16(*val)),
            Any::I32(val) => Ok(Number::I32(*val)),
            Any::I64(val) => Ok(Number::I64(*val)),
            Any::I128(val) => Ok(Number::I128(*val)),
            Any::ISize(val) => Ok(Number::ISize(*val)),
            Any::F32(val) => Ok(Number::F32(*val)),
            Any::F64(val) => Ok(Number::F64(*val)),
            _ => Err(Error::NumberConversionFailed(
                "Could not convert into number".into(),
            )),
        }
    }
}

macro_rules! impl_partial_eq_any {
    ($type:ty) => {
        impl PartialEq<$type> for Any<'_> {
            fn eq(&self, other: &$type) -> bool {
                match self {
                    Any::U8(a) => number_eq_arm!(u8, a, other),
                    Any::U16(a) => number_eq_arm!(u16, a, other),
                    Any::U32(a) => number_eq_arm!(u32, a, other),
                    Any::U64(a) => number_eq_arm!(u64, a, other),
                    Any::U128(a) => number_eq_arm!(u128, a, other),
                    Any::USize(a) => number_eq_arm!(usize, a, other),
                    Any::I8(a) => number_eq_arm!(i8, a, other),
                    Any::I16(a) => number_eq_arm!(i16, a, other),
                    Any::I32(a) => number_eq_arm!(i32, a, other),
                    Any::I64(a) => number_eq_arm!(i64, a, other),
                    Any::I128(a) => number_eq_arm!(i128, a, other),
                    Any::ISize(a) => number_eq_arm!(isize, a, other),
                    Any::F32(a) => number_eq_arm!(f32, a, other),
                    Any::F64(a) => number_eq_arm!(f64, a, other),
                    Any::Str(a) => match a.parse::<$type>() {
                        Ok(val) => val == *other,
                        Err(_) => false,
                    },
                    _ => false,
                }
            }
        }
    };
}

impl_partial_eq_any!(u8);
impl_partial_eq_any!(u16);
impl_partial_eq_any!(u32);
impl_partial_eq_any!(u64);
impl_partial_eq_any!(u128);
impl_partial_eq_any!(usize);
impl_partial_eq_any!(i8);
impl_partial_eq_any!(i16);
impl_partial_eq_any!(i32);
impl_partial_eq_any!(i64);
impl_partial_eq_any!(i128);
impl_partial_eq_any!(isize);
impl_partial_eq_any!(f32);
impl_partial_eq_any!(f64);

impl PartialEq<&str> for Any<'_> {
    fn eq(&self, other: &&str) -> bool {
        match self {
            Any::U8(a) => a.to_string() == *other,
            Any::U16(a) => a.to_string() == *other,
            Any::U32(a) => a.to_string() == *other,
            Any::U64(a) => a.to_string() == *other,
            Any::U128(a) => a.to_string() == *other,
            Any::I8(a) => a.to_string() == *other,
            Any::I16(a) => a.to_string() == *other,
            Any::I32(a) => a.to_string() == *other,
            Any::I64(a) => a.to_string() == *other,
            Any::I128(a) => a.to_string() == *other,
            Any::F32(a) => a.to_string() == *other,
            Any::F64(a) => a.to_string() == *other,
            Any::Char(a) => a.to_string() == *other,
            Any::Bool(a) => a.to_string().to_lowercase() == other.to_lowercase(),
            Any::Str(a) => a == other,
            _ => false,
        }
    }
}

impl PartialEq<&[u8]> for Any<'_> {
    fn eq(&self, other: &&[u8]) -> bool {
        match self {
            Any::Bytes(a) => a == other,
            _ => false,
        }
    }
}

impl PartialEq<char> for Any<'_> {
    fn eq(&self, other: &char) -> bool {
        match self {
            Any::Char(a) => a == other,
            _ => false,
        }
    }
}

impl PartialEq<bool> for Any<'_> {
    fn eq(&self, other: &bool) -> bool {
        match self {
            Any::Bool(a) => a == other,
            _ => false,
        }
    }
}

impl PartialEq<Vec<Any<'_>>> for Any<'_> {
    fn eq(&self, other: &Vec<Any<'_>>) -> bool {
        let a = match self {
            Any::Array(a) => a,
            _ => return false,
        };

        if a.len() != other.len() {
            return false;
        }

        for item in a.iter() {
            let mut found = false;
            for other_item in other.iter() {
                if item == other_item {
                    found = true;
                    break;
                }
            }

            if !found {
                return false;
            }
        }

        true
    }
}

impl PartialEq<HashMap<&str, Any<'_>>> for Any<'_> {
    fn eq(&self, other: &HashMap<&str, Any<'_>>) -> bool {
        let a = match self {
            Any::Map(a) => a,
            _ => return false,
        };

        if a.len() != other.len() {
            return false;
        }

        for (key, val) in a.iter() {
            if let Some(other_val) = other.get(key) {
                if val != other_val {
                    return false;
                }
            }
        }

        true
    }
}

impl PartialEq for Any<'_> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Any::U8(a) => other == a,
            Any::U16(a) => other == a,
            Any::U32(a) => other == a,
            Any::U64(a) => other == a,
            Any::U128(a) => other == a,
            Any::USize(a) => other == a,
            Any::I8(a) => other == a,
            Any::I16(a) => other == a,
            Any::I32(a) => other == a,
            Any::I64(a) => other == a,
            Any::I128(a) => other == a,
            Any::ISize(a) => other == a,
            Any::F32(a) => other == a,
            Any::F64(a) => other == a,
            Any::Str(a) => other == a,
            Any::Bytes(a) => other == a,
            Any::Char(a) => other == a,
            Any::Bool(a) => other == a,
            // Arrays are equal if they have the same contents
            // they don't have to be in the same order
            Any::Array(a) => other == a,
            Any::Map(a) => other == a,
            Any::Null => match other {
                Any::Null => true,
                _ => false,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_any() {
        assert!(Any::U8(1) == Any::U8(1));
        assert!(Any::Str("1") == Any::U8(1));
        assert!(Any::I16(100) == Any::Str("100"));
        assert!(Any::I64(100) == Any::F64(100.0));
    }

    #[test]
    fn test_number() {
        assert!(Number::U8(1) == 1);
        assert!(Number::U16(1) == 1);
        assert!(Number::U32(1) == 1);
        assert!(Number::U64(1) == 1);
        assert!(Number::U128(1) == 1);
        assert!(Number::USize(1) == 1);
        assert!(Number::I8(1) == 1);
        assert!(Number::I16(1) == 1);
        assert!(Number::I32(1) == 1);
        assert!(Number::I64(1) == 1);
        assert!(Number::I128(1) == 1);
        assert!(Number::ISize(1) == 1);
        assert!(Number::F32(1.0) == 1);
        assert!(Number::F64(1.0) == 1);

        assert!(Number::U8(40) > Number::U32(2));
        assert!(Number::U128(120000000000000) >= Number::U8(2));
        // panics because of the conversion I can fix this easily
        // if rust makes a choice on how to implement try_from for floats
        // https://internals.rust-lang.org/t/tryfrom-for-f64/9793
        // but it looks like no one wants to touch that.
        // assert!(Number::U8(2) <= Number::U128(120000000000000));
        // I spent a lot of angry brain units on trying to implement this
        // myself, but alas, my brain units are not powerful enough to
        // comprehend the rust trait system set up here. I guess.

        assert_eq!(Number::U128(1200) + 1, Number::U128(1201));
        assert_eq!(Number::U128(1200) - 1, Number::U128(1199));
        assert_eq!(Number::U128(1200) * 2, Number::U128(2400));
        assert_eq!(Number::U128(1200) / 2, Number::U128(600));
        assert_eq!(Number::U128(1200) % 2, Number::U128(0));
    }
}
