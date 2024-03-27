use std::{cell::RefCell, fmt};

use serde::{
    de::{DeserializeSeed, Visitor},
    Deserializer,
};

use crate::bookmark::Bookmark;

use super::{BCollection, BKeyValue, Binary};

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
    type Value = Bookmark;

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
    type Value = Bookmark;

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
            ptrs.push(v.index() as u32);
        }

        let mut bin = self.bin.borrow_mut();
        Ok(Bookmark::new(BCollection::create(
            None,
            &ptrs[..],
            &mut bin,
        )))
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let mut ptrs = Vec::new();

        while let Some((k, v)) = map.next_entry_seed(MapKeySeed, self)? {
            let ptr = BKeyValue::create(None, v.index() as u32, &k, &mut self.bin.borrow_mut());
            ptrs.push(ptr as u32);
        }

        let mut bin = self.bin.borrow_mut();
        Ok(Bookmark::new(BCollection::create(
            None,
            &ptrs[..],
            &mut bin,
        )))
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
