use byteorder::{BigEndian, ByteOrder};
use std::convert::From;
use std::default::Default;
use std::ops::Deref;

use crate::value::{Deserialize, Serialize};

static TYPE_REFERENCE: u8 = 0;
static REFERENCE_LENGTH: usize = 5;
static MAX_REFERENCE_DEPTH: isize = 20;

static TYPE_OFFSET: usize = 0;
static TYPE_OFFSET_END: usize = TYPE_OFFSET + 1;

static LENGTH_OFFSET: usize = TYPE_OFFSET_END;
static LENGTH_OFFSET_END: usize = LENGTH_OFFSET + 2;

static REFERENCE_OFFSET: usize = LENGTH_OFFSET_END;
static REFERENCE_OFFSET_END: usize = REFERENCE_OFFSET + 4;

static PARENT_OFFSET: usize = REFERENCE_OFFSET_END;
static PARENT_OFFSET_END: usize = PARENT_OFFSET + 4;

static CONTENT_OFFSET: usize = PARENT_OFFSET_END;

static PTR_OFFSET: usize = TYPE_OFFSET_END;
static PTR_OFFSET_END: usize = PTR_OFFSET + 4;

static ADD_TOKEN_HEADER_LENGTH: usize = CONTENT_OFFSET + REFERENCE_LENGTH;

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
            let length = BigEndian::read_u16(length);
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

    // add puts a new token and breference into the binary. It then returns the
    // index of the breference for future use
    pub fn add<T: Serialize>(&mut self, parent: Option<u32>, content: &T) -> usize {
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

        BToken::new(
            index as u32,
            parent,
            content,
            self.0
                .get_mut(index + REFERENCE_LENGTH..index + ADD_TOKEN_HEADER_LENGTH + content_size) // intentionally explicit in end
                .unwrap(),
        );

        index
    }

    pub fn get<'a, T: Deserialize<'a>>(&'a self, index: usize) -> Option<T::Item> {
        // why not use the btoken get_content function? well because
        // the borrow checker doesn't understand that the underlying
        // lifetime is longer than the btoken we create, so we have
        // to interface with the binary we have directly to return the
        // underlying type.
        let index = self.resolve(index, MAX_REFERENCE_DEPTH)?;

        if self.token_at(index).unwrap().get_type() != T::type_of() {
            return None;
        }

        let (s, e) = self.token_bounds(index).unwrap();
        let buf = self.0.get(s..e).unwrap();
        Some(T::deserialize(buf.get(CONTENT_OFFSET..).unwrap()))
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
        BigEndian::write_u32(
            buf.get_mut(PTR_OFFSET..PTR_OFFSET_END)
                .expect("breference buf not large enough"),
            index,
        );

        BReference(buf)
    }

    fn get_index(&self) -> Option<u32> {
        let buf = self.0.get(PTR_OFFSET..PTR_OFFSET_END)?;
        let num = BigEndian::read_u32(buf);

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
    fn new<T: Serialize>(
        reference_index: u32,
        parent_index: Option<u32>,
        content: &T,
        buf: &'a mut [u8],
    ) -> BToken<'a> {
        BigEndian::write_u16(
            buf.get_mut(LENGTH_OFFSET..LENGTH_OFFSET_END)
                .expect("btoken buf not large enough"),
            (CONTENT_OFFSET + content.size_of()) as u16,
        );

        BigEndian::write_u32(
            buf.get_mut(REFERENCE_OFFSET..REFERENCE_OFFSET_END)
                .expect("btoken not large enough"),
            reference_index,
        );

        // write the parent, if the parent was none we can
        // write 0
        BigEndian::write_u32(
            buf.get_mut(PARENT_OFFSET..PARENT_OFFSET_END)
                .expect("btoken not large enough"),
            parent_index.unwrap_or(0),
        );

        // copy the content into the btoken
        let tpe = content.serialize(
            buf.get_mut(CONTENT_OFFSET..CONTENT_OFFSET + content.size_of())
                .expect("btoken not large enough"),
        );

        let t = buf
            .first_mut()
            .expect("the very btoken buf empty! can't fill.");
        *t = tpe;

        BToken(buf)
    }

    pub fn get_parent_index(&self) -> Option<u32> {
        let b = self.0.get(PARENT_OFFSET..PARENT_OFFSET_END)?;
        let num = BigEndian::read_u32(b);
        if num == 0 {
            None
        } else {
            Some(num)
        }
    }

    pub fn get_reference_index(&self) -> u32 {
        let b = self.0.get(REFERENCE_OFFSET..REFERENCE_OFFSET_END).unwrap();
        BigEndian::read_u32(b)
    }

    pub fn get_type(&self) -> u8 {
        *self
            .0
            .first()
            .expect("btoken was empty when fetching type.")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_breference() {
        let b = vec![TYPE_REFERENCE, 0x0, 0x0, 0x0, 0x1];
        let bref = BReference(&b);
        assert_eq!(bref.get_index().unwrap(), 1);
    }

    #[test]
    fn test_btoken() {
        let b = vec![
            0x1, // type
            0x0, 0xb, // length of 11
            0x0, 0x0, 0x0, 0x1, // reference index of 1
            0x0, 0x0, 0x0, 0x2, // parent index of 2
        ];
        let tok = BToken(&b);
        assert_eq!(tok.get_parent_index(), Some(2));
        assert_eq!(tok.get_reference_index(), 1);
    }

    #[test]
    fn test_binary() {
        let b = vec![
            TYPE_REFERENCE, // type
            0x0,
            0x0,
            0x0,
            0x5,            // index at 5
            TYPE_REFERENCE, // type
            0x0,
            0x0,
            0x0,
            0xa, // index at 5
            0x1, // type of btoken
            0x0,
            0xb, // length of 11
            0x0,
            0x0,
            0x0,
            0x5, // reference index of 1
            0x0,
            0x0,
            0x0,
            0x0, // parent index of 0
        ];
        let bin = Binary(b.clone());
        assert_eq!(bin.token_at(0), Some(BToken(&b[10..10 + 11])));
    }

    macro_rules! test_binary_obj {
        ($type:ty, $value:expr) => {{
            let mut b = Binary::default();
            let val: $type = $value;
            let i = b.add(None, &val);
            let new_val: $type = b.get::<$type>(i).unwrap();
            assert_eq!(val, new_val);
        }};
    }

    macro_rules! test_string_obj {
        ($value:expr) => {{
            let mut b = Binary::default();
            let val = String::from($value);
            let i = b.add(None, &val.as_str());
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
}
