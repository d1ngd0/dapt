use std::convert::From;
use std::default::Default;
use std::mem;
use std::ops::Deref;

use crate::{
    error::{DaptResult, Error},
    value::{Any, Deserialize, Number, Serialize, TYPE_STR},
};

pub const TYPE_REFERENCE: u8 = 0;
pub const TYPE_COLLECTION: u8 = 1;
pub const TYPE_KEYVAL: u8 = 2;

const REFERENCE_LENGTH: usize = 5;
const MAX_REFERENCE_DEPTH: isize = 20;

const TYPE_OFFSET: usize = 0;
const TYPE_OFFSET_END: usize = TYPE_OFFSET + 1;

const LENGTH_OFFSET: usize = TYPE_OFFSET_END;
const LENGTH_OFFSET_END: usize = LENGTH_OFFSET + 2;

const REFERENCE_OFFSET: usize = LENGTH_OFFSET_END;
const REFERENCE_OFFSET_END: usize = REFERENCE_OFFSET + 4;

const PARENT_OFFSET: usize = REFERENCE_OFFSET_END;
const PARENT_OFFSET_END: usize = PARENT_OFFSET + 4;

const CONTENT_OFFSET: usize = PARENT_OFFSET_END;

const PTR_OFFSET: usize = TYPE_OFFSET_END;
const PTR_OFFSET_END: usize = PTR_OFFSET + 4;

const ADD_TOKEN_HEADER_LENGTH: usize = CONTENT_OFFSET + REFERENCE_LENGTH;

pub struct Binary(Vec<u8>);

impl Binary {
    fn resolve(&self, index: usize, max_depth: isize) -> Option<usize> {
        if max_depth <= 0 {
            return None;
        }

        let t = self.0.get(index)?;
        if *t == TYPE_REFERENCE {
            let refer = BReference(self.0.get(index..index + 5)?);
            return self.resolve(refer.get_index()? as usize, max_depth - 1);
        }

        Some(index)
    }

    fn token_bounds(&self, index: usize) -> Option<(usize, usize)> {
        let t = self.0.get(index)?;
        if *t == TYPE_REFERENCE {
            Some((index, index + REFERENCE_LENGTH))
        } else {
            let length = self
                .0
                .get(index + LENGTH_OFFSET..index + LENGTH_OFFSET_END)?;
            let length = u16::deserialize(length);
            Some((index, index + length as usize))
        }
    }

    // token_at will return the token at the given location. If the index is
    // a reference it will resolve to the btoken. At most you can traverse 20
    // references before giving up trying to find the BToken
    pub fn token_at(&self, index: usize) -> Option<BToken> {
        let index = self.resolve(index, MAX_REFERENCE_DEPTH)?;

        let t = self.0.get(index)?;
        if *t == TYPE_REFERENCE {
            None
        } else {
            let (b, e) = self.token_bounds(index)?;
            Some(BToken(self.0.get(b..e)?))
        }
    }

    pub fn add_sized(
        &mut self,
        parent: Option<u32>,
        content_size: usize,
        tpe: u8,
    ) -> (usize, usize) {
        let index = self.0.len();
        let token_index = (index + REFERENCE_LENGTH) as u32;

        // TODO there is likely an unsafe way of doing this which will speed things up.
        // We don't need to preset the values which we do here
        self.0
            .resize(index + ADD_TOKEN_HEADER_LENGTH + content_size, 0);

        BReference::new(
            token_index,
            self.0.get_mut(index..index + REFERENCE_LENGTH).unwrap(),
        );

        let _ = BToken::new(
            index as u32,
            parent,
            content_size,
            tpe,
            self.0
                .get_mut(index + REFERENCE_LENGTH..index + ADD_TOKEN_HEADER_LENGTH + content_size) // intentionally explicit in end
                .unwrap(),
        );

        (index, token_index as usize)
    }

    // add puts a new token and breference into the binary. It then returns the
    // index of the breference for future use
    pub fn add<T: Serialize>(&mut self, parent: Option<u32>, content: T) -> usize {
        let index = self.0.len();
        let token_index = (index + REFERENCE_LENGTH) as u32;
        let content_size = content.size_of();

        // TODO there is likely an unsafe way of doing this which will speed things up.
        // We don't need to preset the values which we do here
        self.0
            .resize(index + ADD_TOKEN_HEADER_LENGTH + content_size, 0);

        BReference::new(
            token_index,
            self.0.get_mut(index..index + REFERENCE_LENGTH).unwrap(),
        );

        // copy the content into the btoken
        let tpe = content.serialize(
            self.0
                .get_mut(
                    token_index as usize + CONTENT_OFFSET
                        ..token_index as usize + CONTENT_OFFSET + content.size_of(),
                )
                .expect("btoken not large enough"),
        );

        BToken::new(
            index as u32,
            parent,
            content.size_of(),
            tpe,
            self.0
                .get_mut(index + REFERENCE_LENGTH..index + ADD_TOKEN_HEADER_LENGTH + content_size) // intentionally explicit in end
                .unwrap(),
        );

        index
    }

    // val_at will grab the value for the current index. If the current
    // pointer is a key value, it will
    pub fn val_at(&self, index: usize) -> Option<usize> {
        let index = self.resolve(index, MAX_REFERENCE_DEPTH)?;
        let token = self.token_at(index).unwrap();

        match token.get_type() {
            TYPE_KEYVAL => {
                let key_value: BKeyValue = token.try_into().ok()?;
                self.val_at(key_value.child_index() as usize)
            }
            TYPE_COLLECTION => None,
            _ => Some(index),
        }
    }

    pub fn type_at(&self, index: usize) -> Option<u8> {
        let index = self.resolve(index, MAX_REFERENCE_DEPTH)?;
        Some(*self.0.get(index)?)
    }

    pub fn get<'a, T: Deserialize<'a>>(&'a self, index: usize) -> Option<T::Item> {
        // why not use the btoken get_content function? well because
        // the borrow checker doesn't understand that the underlying
        // lifetime is longer than the btoken we create, so we have
        // to interface with the binary we have directly to return the
        // underlying type.
        let index = self.val_at(index)?;
        let (s, e) = self.token_bounds(index).unwrap();
        let buf = self.0.get(s..e).unwrap();
        Some(T::deserialize(buf.get(CONTENT_OFFSET..).unwrap()))
    }

    // if you know the value is a string you can get it without
    // taking a heap allocation and reference that value from
    // within the binary. If the value is not a str it will return
    // None
    pub fn str<'a>(&'a self, index: usize) -> Option<&'a str> {
        let index = self.val_at(index)?;
        if self.type_at(index)? != TYPE_STR {
            return None;
        }

        self.get::<&'a str>(index)
    }

    // Number returns an error if the underlying type is not a Number
    pub fn number(&self, index: usize) -> DaptResult<Number> {
        let index = self.val_at(index).ok_or(Error::InvalidIndex(
            "invalid index when finding number".into(),
        ))?;

        Number::new(self, index)
    }

    pub fn any(&self, index: usize) -> Option<Any> {
        let index = self.val_at(index)?;
        Any::new(self, index)
    }
}

impl Default for Binary {
    fn default() -> Self {
        // create default with a breference to help define the root token.
        Binary(vec![TYPE_REFERENCE, 0x0, 0x0, 0x0, 0x0])
    }
}

// BReference is a reference to another location within the dapt packet.
// its use is to act as a modifible location in memory
// 0x0 u8 type
// 0x0, 0x0, 0x0, 0x0 u32 pointer to location
#[derive(PartialEq, Debug)]
pub struct BReference<'a>(&'a [u8]);

impl<'a> BReference<'a> {
    fn new(index: u32, buf: &'a mut [u8]) -> BReference<'a> {
        let t = buf.first_mut().expect("breference buf empty! can't fill.");
        *t = TYPE_REFERENCE;

        index.serialize(
            buf.get_mut(PTR_OFFSET..PTR_OFFSET_END)
                .expect("breference buf not large enough"),
        );

        BReference(buf)
    }

    fn get_index(&self) -> Option<u32> {
        let buf = self.0.get(PTR_OFFSET..PTR_OFFSET_END)?;
        let num = u32::deserialize(buf);

        if num == 0 {
            None
        } else {
            Some(num)
        }
    }
}

impl<'a> From<&'a [u8]> for BReference<'a> {
    fn from(b: &'a [u8]) -> Self {
        BReference(b)
    }
}

// BToken is the most generic token within dapt. All tokens
// in dapt are btokens except breferences. btokens consist of
// 0x0 u8 type
// 0x0, 0x0 u16 length of the token
// 0x0, 0x0, 0x0, 0x0 u32 breference offset
// 0x0, 0x0, 0x0, 0x0 u32 parent offset
// if the parent offset is 0 it is assumed to be
// unset. The first byte in a dapt packet is 0x0
// to ensure an offset of 0 is wrong.
#[derive(PartialEq, Debug)]
pub struct BToken<'a>(&'a [u8]);

impl<'a> From<&'a [u8]> for BToken<'a> {
    fn from(b: &'a [u8]) -> Self {
        BToken(b)
    }
}

impl<'a> Deref for BToken<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a> BToken<'a> {
    fn new(
        reference_index: u32,
        parent_index: Option<u32>,
        content_length: usize,
        tpe: u8,
        buf: &'a mut [u8],
    ) -> BToken<'a> {
        let t = buf
            .first_mut()
            .expect("the very btoken buf empty! can't fill.");
        *t = tpe;

        let size = (CONTENT_OFFSET + content_length) as u16;
        size.serialize(
            buf.get_mut(LENGTH_OFFSET..LENGTH_OFFSET_END)
                .expect("btoken buf not large enough"),
        );

        reference_index.serialize(
            buf.get_mut(REFERENCE_OFFSET..REFERENCE_OFFSET_END)
                .expect("btoken not large enough"),
        );

        parent_index.unwrap_or(0).serialize(
            buf.get_mut(PARENT_OFFSET..PARENT_OFFSET_END)
                .expect("btoken not large enough"),
        );

        BToken(buf)
    }

    pub fn get_parent_index(&self) -> Option<u32> {
        let result = u32::deserialize(self.0.get(PARENT_OFFSET..PARENT_OFFSET_END)?);

        if result == 0 {
            None
        } else {
            Some(result)
        }
    }

    pub fn get_reference_index(&self) -> u32 {
        u32::deserialize(self.0.get(REFERENCE_OFFSET..REFERENCE_OFFSET_END).unwrap())
    }

    pub fn get_type(&self) -> u8 {
        *self
            .0
            .first()
            .expect("btoken was empty when fetching type.")
    }

    pub fn set_parent(buf: &mut Binary, index: usize, parent_index: Option<u32>) {
        let index = buf.resolve(index, MAX_REFERENCE_DEPTH);
        if index.is_none() {
            return;
        }

        let index = index.unwrap();
        let (s, _) = buf.token_bounds(index).unwrap();
        parent_index.unwrap_or(0).serialize(
            buf.0
                .get_mut(s + PARENT_OFFSET..s + PARENT_OFFSET_END)
                .expect("parent is unexpected size"),
        );
    }
}

// BArray houses the links to all the Values within the array
// It's structure is a superset of BToken Where the contents
// are u32 pointers to the locations of values for the array
// these pointers can point to any Serialize and Deserialized
// value, BObjects or BArray. The values within the array do
// not need to house the same type.
#[derive(PartialEq, Debug)]
pub struct BCollection<'a>(&'a [u8]);

impl<'a> TryFrom<BToken<'a>> for BCollection<'a> {
    type Error = Error;

    fn try_from(b: BToken<'a>) -> DaptResult<Self> {
        if b.get_type() != TYPE_COLLECTION {
            return Err(Error::TypeMismatch(
                b.get_type(),
                "Expected BCollection".into(),
            ));
        }

        Ok(BCollection(b.0))
    }
}

impl<'a> Deref for BCollection<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a> BCollection<'a> {
    fn create(parent_index: Option<u32>, children: &[u32], b: &mut Binary) -> usize {
        let size = mem::size_of::<u32>();
        let (index, token_index) =
            b.add_sized(parent_index, size * children.len(), TYPE_COLLECTION);

        for (i, v) in children.iter().enumerate() {
            let child_content_index = token_index + CONTENT_OFFSET + (i * size);
            v.serialize(
                b.0.get_mut(child_content_index..child_content_index + 4)
                    .expect("invalid sized object"),
            );

            BToken::set_parent(b, *v as usize, Some(index as u32));
        }

        index
    }

    fn child_index(&self, index: usize) -> Option<usize> {
        let index = CONTENT_OFFSET + (index * mem::size_of::<u32>());
        let child_index = u32::deserialize(self.get(index..index + mem::size_of::<u32>())?);
        Some(child_index as usize)
    }

    // child_indexes takes an array to populate. Make sure you send it a large enough slice or it will panic
    fn child_indexes(&self, ptrs: &mut [usize]) {
        for i in 0..self.length() {
            let child_content_index = self.child_index(i);

            if let None = child_content_index {
                return;
            }

            ptrs[i] = child_content_index.unwrap();
        }
    }

    pub fn child_key(&self, key: &str, b: &Binary) -> Option<usize> {
        for i in 0..self.length() {
            let child_content_index = self.child_index(i)?;
            let child: BKeyValue = b.token_at(child_content_index)?.try_into().ok()?;
            if child.key() == key {
                return Some(child_content_index);
            }
        }

        None
    }

    fn length(&self) -> usize {
        (self.0.len() - CONTENT_OFFSET) / mem::size_of::<u32>()
    }
}

#[derive(PartialEq, Debug)]
pub struct BKeyValue<'a>(&'a [u8]);

impl<'a> TryFrom<BToken<'a>> for BKeyValue<'a> {
    type Error = Error;

    fn try_from(b: BToken<'a>) -> DaptResult<Self> {
        if b.get_type() != TYPE_KEYVAL {
            return Err(Error::TypeMismatch(
                b.get_type(),
                "Expected BKeyValue".into(),
            ));
        }

        Ok(BKeyValue(b.0))
    }
}

impl<'a> Deref for BKeyValue<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a> BKeyValue<'a> {
    pub fn create(parent_index: Option<u32>, child: u32, key: &str, b: &mut Binary) -> usize {
        let size = mem::size_of::<u32>() + key.len();
        let (index, token_index) = b.add_sized(parent_index, size, TYPE_KEYVAL);

        let child_offset = token_index + CONTENT_OFFSET;
        let key_offset = child_offset + mem::size_of::<u32>();

        child.serialize(
            b.0.get_mut(child_offset..key_offset)
                .expect("invalid sized object"),
        );

        BToken::set_parent(b, child as usize, Some(index as u32));

        key.serialize(
            b.0.get_mut(key_offset..key_offset + key.len())
                .expect("invalid sized object"),
        );

        index
    }

    pub fn child_index(&self) -> u32 {
        u32::deserialize(
            self.get(CONTENT_OFFSET..CONTENT_OFFSET + mem::size_of::<u32>())
                .expect("bKeyValue is not the correct size"),
        )
    }

    fn key(&'a self) -> &'a str {
        let val_offset = CONTENT_OFFSET + mem::size_of::<u32>();
        <&'a str as Deserialize>::deserialize(
            self.get(val_offset..).expect("invalid sized keyvalue"),
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_breference() {
        let mut b: [u8; 5] = [0x0; 5];
        let bref = BReference::new(1, &mut b);
        assert_eq!(bref.get_index().unwrap(), 1);
    }

    #[test]
    fn test_btoken() {
        let mut b: [u8; 11] = [0; 11];
        let tok = BToken::new(1, Some(2), 0, 1, &mut b);
        assert_eq!(tok.get_type(), 1);
        assert_eq!(tok.get_parent_index(), Some(2));
        assert_eq!(tok.get_reference_index(), 1);
    }

    macro_rules! test_binary_obj {
        ($type:ty, $value:expr) => {{
            let mut b = Binary::default();
            let val: $type = $value;
            let i = b.add(None, val);
            let new_val: $type = b.get::<$type>(i).unwrap();
            assert_eq!(val, new_val);
        }};
    }

    macro_rules! test_string_obj {
        ($value:expr) => {{
            let mut b = Binary::default();
            let val = String::from($value);
            let i = b.add(None, val.as_str());
            let new_val: &str = b.get::<&str>(i).unwrap();
            assert_eq!(val.as_str(), new_val);
        }};
    }

    #[test]
    fn test_binary_numbers() {
        test_binary_obj!(u8, 100);
        test_binary_obj!(u16, 100);
        test_binary_obj!(u32, 100);
        test_binary_obj!(u64, 100);
    }

    #[test]
    fn test_binary_strings() {
        test_string_obj!("hello");
        test_string_obj!("This is a very long string that is quite long");
    }

    #[test]
    fn test_collection() {
        let mut children: Vec<u32> = vec![];
        let mut b = Binary::default();
        for i in 0..10 {
            children.push(b.add(None, i as usize) as u32);
        }
        let index = BCollection::create(None, &children[..], &mut b);
        let array: BCollection = b
            .token_at(index)
            .unwrap()
            .try_into()
            .expect("this should be a bcollection, we just made it");
        for i in 0..array.length() {
            println!("{:?}", array.child_index(i));
            assert_eq!(i, b.get::<usize>(array.child_index(i).unwrap()).unwrap())
        }
    }

    #[test]
    fn test_object() {
        let mut children: Vec<u32> = vec![];
        let mut b = Binary::default();

        for i in 0..10 {
            let val_index = b.add(None, i) as u32;
            let key_index = BKeyValue::create(None, val_index, &format!("child_{i}"), &mut b);
            children.push(key_index as u32);
        }

        let index = BCollection::create(None, &children, &mut b);
        let map: BCollection = b.token_at(index).unwrap().try_into().unwrap();

        for i in 0..map.length() {
            let child_index = map.child_key(&format!("child_{i}"), &b).unwrap();
            println!("{}", b.token_at(child_index).unwrap().get_type());
            let child_val: usize = b.number(child_index).unwrap().into();
            assert_eq!(child_val, i);
        }
    }

    macro_rules! test_any_string {
        ($value:expr, $val:expr) => {{
            let mut b = Binary::default();
            let i = b.add(None, $value);
            let new_val: String = b.any(i).unwrap().into();
            assert_eq!($val, new_val);
        }};
    }

    #[test]
    fn test_any() {
        test_any_string!("something", "something");
        test_any_string!(8, "8");
        test_any_string!(5000, "5000");
        test_any_string!(9238492834 as u64, "9238492834");
    }
}
