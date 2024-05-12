use std::sync::Arc;
use std::{cell::RefCell, fmt};

use serde::ser::Serialize;
use serde::{
    de::{DeserializeSeed, Visitor},
    ser::{SerializeMap, SerializeSeq},
    Deserializer,
};

use super::{
    BArray, BKeyValue, BMap, BReference, Binary, TYPE_ARRAY, TYPE_BOOL, TYPE_BYTES, TYPE_CHAR,
    TYPE_F32, TYPE_F64, TYPE_I128, TYPE_I16, TYPE_I32, TYPE_I64, TYPE_I8, TYPE_ISIZE, TYPE_KEYVAL,
    TYPE_MAP, TYPE_NULL, TYPE_STR, TYPE_U128, TYPE_U16, TYPE_U32, TYPE_U64, TYPE_U8, TYPE_USIZE,
};

pub struct BinaryVisitor {
    bin: RefCell<Binary>,
}

impl BinaryVisitor {
    pub fn consume(self) -> Binary {
        self.bin.into_inner()
    }
}

impl Default for BinaryVisitor {
    fn default() -> Self {
        Self {
            bin: RefCell::new(Binary::default()),
        }
    }
}

impl From<Binary> for BinaryVisitor {
    fn from(bin: Binary) -> Self {
        Self {
            bin: RefCell::new(bin),
        }
    }
}

// macro for implementing the simple types
macro_rules! impl_visit {
    ($method: ident, $ty:ty) => {
        fn $method<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(self.bin.borrow_mut().add(None, v).into())
        }
    };
}

// DeserializeSeed for the Binary Visitor calls back into itself by
// calling deserialize_any. This means everything should return a bookmark
// instead of the underlying value.
impl<'de> DeserializeSeed<'de> for &BinaryVisitor {
    type Value = BReference;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(self)
    }
}

// MapKeyVisitor is a DeserializeSeed that ensures the key is a string
struct MapKeySeed;

impl<'de> DeserializeSeed<'de> for MapKeySeed {
    // I really wanted to be able to use the borrowed str, but there
    // is just no way for us to know how it is set up on the other side
    // and so, to be safe and support most cases, we have to allocate
    // a string. Sigh...
    type Value = String;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct MapKeyVisitor;

        impl<'de> Visitor<'de> for MapKeyVisitor {
            type Value = String;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a string")
            }

            // visit_str is, by default, called by visit_string and visit_str
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.into())
            }
        }

        deserializer.deserialize_str(MapKeyVisitor)
    }
}

// We are kind of hacking the visitor. Instead of returning the value
// we return a bookmark to the value within the binary state. This
// allows us to build the binary, and just return a pointer to the value.
// then when you are done deserializing, you can grab the binary out of
// visitor merge that with the final bookmark given for the outer most
// object and chuck that into a dapt struct. Boom, deserialized.
impl<'de> Visitor<'de> for &BinaryVisitor {
    type Value = BReference;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("data which could be turned into a dapt packet")
    }

    impl_visit!(visit_bool, bool);
    impl_visit!(visit_i8, i8);
    impl_visit!(visit_i16, i16);
    impl_visit!(visit_i32, i32);
    impl_visit!(visit_i64, i64);
    impl_visit!(visit_i128, i128);
    impl_visit!(visit_u8, u8);
    impl_visit!(visit_u16, u16);
    impl_visit!(visit_u32, u32);
    impl_visit!(visit_u64, u64);
    impl_visit!(visit_u128, u128);
    impl_visit!(visit_f32, f32);
    impl_visit!(visit_f64, f64);
    impl_visit!(visit_str, &str);
    impl_visit!(visit_borrowed_str, &str);
    impl_visit!(visit_string, String);
    impl_visit!(visit_bytes, &[u8]);
    impl_visit!(visit_borrowed_bytes, &[u8]);
    impl_visit!(visit_byte_buf, Vec<u8>);
    impl_visit!(visit_char, char);

    fn visit_unit<E>(self) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(self.bin.borrow_mut().add(None, ()).into())
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let mut ptrs = Vec::new();

        while let Some(v) = seq.next_element_seed(self)? {
            ptrs.push(v);
        }

        let mut bin = self.bin.borrow_mut();
        let (bref, _array) = BArray::new(None, &ptrs[..], &mut bin);
        Ok(BReference::from(bref))
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let mut ptrs = Vec::new();

        while let Some((k, v)) = map.next_entry_seed(MapKeySeed, self)? {
            let (bref, _bkey_val) = BKeyValue::new(None, v, &k, &mut self.bin.borrow_mut());
            ptrs.push(bref);
        }

        let mut bin = self.bin.borrow_mut();
        let (bref, _map) = BMap::new(None, &ptrs[..], &mut bin);
        Ok(bref)
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::EnumAccess<'de>,
    {
        // we don't care about the variant, we are all powerfull
        // and can handle ANY TYPE!!!! BOW DOWN BEFORE ME AND TREMBLE
        let (bookmark, _variant) = data.variant_seed(self)?;
        Ok(bookmark)
    }

    fn visit_newtype_struct<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(self)
    }
}

pub struct SerializeBReference<'a> {
    bookmark: BReference,
    bin: &'a Arc<Binary>,
}

impl<'a> SerializeBReference<'a> {
    pub fn new(bookmark: BReference, bin: &'a Arc<Binary>) -> Self {
        Self { bookmark, bin }
    }
}

impl SerializeBReference<'_> {
    // When calling this youd better be sure it is the right type
    // otherwise we panic
    pub fn get<'a, T: crate::binary::Deserialize<'a>>(&'a self) -> T::Item {
        self.bin.get::<T>(self.bookmark).unwrap()
    }
}

impl Serialize for SerializeBReference<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        let token = match self.bookmark.val_at(&self.bin) {
            Some(t) => t,
            None => return serializer.serialize_unit(),
        };

        match token.get_type(&self.bin) {
            TYPE_I8 => serializer.serialize_i8(self.get::<i8>()),
            TYPE_I16 => serializer.serialize_i16(self.get::<i16>()),
            TYPE_I32 => serializer.serialize_i32(self.get::<i32>()),
            TYPE_I64 => serializer.serialize_i64(self.get::<i64>()),
            TYPE_ISIZE => serializer.serialize_i64(self.get::<isize>() as i64),
            TYPE_I128 => serializer.serialize_i128(self.get::<i128>()),
            TYPE_U8 => serializer.serialize_u8(self.get::<u8>()),
            TYPE_U16 => serializer.serialize_u16(self.get::<u16>()),
            TYPE_U32 => serializer.serialize_u32(self.get::<u32>()),
            TYPE_U64 => serializer.serialize_u64(self.get::<u64>()),
            TYPE_USIZE => serializer.serialize_u64(self.get::<usize>() as u64),
            TYPE_U128 => serializer.serialize_u128(self.get::<u128>()),
            TYPE_F32 => serializer.serialize_f32(self.get::<f32>()),
            TYPE_F64 => serializer.serialize_f64(self.get::<f64>()),
            TYPE_STR => serializer.serialize_str(&self.get::<&str>()),
            TYPE_BYTES => serializer.serialize_bytes(&self.get::<&[u8]>()),
            TYPE_CHAR => serializer.serialize_char(self.get::<char>()),
            TYPE_BOOL => serializer.serialize_bool(self.get::<bool>()),
            TYPE_NULL => serializer.serialize_unit(),
            TYPE_ARRAY => {
                let c = BArray::from(self.bookmark.val_at(&self.bin).unwrap());

                let mut seq = serializer.serialize_seq(Some(c.length(&self.bin) as usize))?;
                for i in 0..c.length(&self.bin) {
                    seq.serialize_element(&SerializeBReference::new(
                        c.child_index(&self.bin, i).unwrap(),
                        self.bin,
                    ))?;
                }
                seq.end()
            }
            TYPE_MAP => {
                let c = BMap::from(self.bookmark.val_at(&self.bin).unwrap());

                let mut map = serializer.serialize_map(Some(c.length(&self.bin) as usize))?;
                for i in 0..c.length(&self.bin) {
                    let kv = c
                        .child_index(&self.bin, i)
                        .unwrap()
                        .key_at(&self.bin)
                        .unwrap();

                    map.serialize_entry(
                        &kv.key(&self.bin),
                        &SerializeBReference::new(kv.child(&self.bin).unwrap(), self.bin),
                    )?;
                }
                map.end()
            }
            TYPE_KEYVAL => {
                let kv = self.bookmark.key_at(self.bin).unwrap();
                SerializeBReference::new(kv.child(&self.bin).unwrap(), self.bin)
                    .serialize(serializer)
            }
            unknown => panic!("Unknown type {}", unknown),
        }
    }
}
