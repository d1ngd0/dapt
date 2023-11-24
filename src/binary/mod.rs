use byteorder::{BigEndian, ByteOrder};
use std::convert::From;

static TYPE_REFERENCE: u8 = 0;
static REFERENCE_LENGTH: usize = 5;

pub struct Binary(Vec<u8>);

impl Binary {
    // at will return the primitive type (btoken or reference) at the given location
    fn at(&self, index: usize) -> Option<PrimitiveToken> {
        let t = self.0.get(index)?;
        if *t == TYPE_REFERENCE {
            println!("{:?}", self.0.get(index..index + 5));
            Some(PrimitiveToken::Reference(
                self.0.get(index..index + 5)?.into(),
            ))
        } else {
            let length = self
                .0
                .get(index + LENGTH_OFFSET..index + LENGTH_OFFSET_END)?;
            let length = BigEndian::read_u16(length);
            Some(PrimitiveToken::Token(
                self.0.get(index..index + length as usize)?.into(),
            ))
        }
    }

    fn token_at_depth(&self, index: usize, depth: u8) -> Option<BToken> {
        if depth > 20 {
            return None;
        }

        let tok = self.at(index)?;
        match tok {
            PrimitiveToken::Reference(reference) => {
                self.token_at_depth(reference.get_index()? as usize, depth + 1)
            }
            PrimitiveToken::Token(tok) => Some(tok),
        }
    }

    // token_at will return the token at the given location. If the index is
    // a reference it will resolve to the btoken. At most you can traverse 20
    // references before giving up trying to find the BToken
    pub fn token_at(&self, index: usize) -> Option<BToken> {
        self.token_at_depth(index, 0)
    }
}

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

// PrimitiveToken
enum PrimitiveToken<'a> {
    Reference(BReference<'a>),
    Token(BToken<'a>),
}

// BReference is a reference to another location within the dapt packet.
// its use is to act as a modifible location in memory
// 0x0 u8 type
// 0x0, 0x0, 0x0, 0x0 u32 pointer to location
#[derive(PartialEq, Debug)]
pub struct BReference<'a>(&'a [u8]);

impl<'a> BReference<'a> {
    fn get_index(&self) -> Option<u32> {
        let buf = self.0.get(PTR_OFFSET..PTR_OFFSET_END)?;
        Some(BigEndian::read_u32(buf))
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

impl<'a> BToken<'a> {
    pub fn get_parent_index(&self) -> Option<u32> {
        let b = self.0.get(PARENT_OFFSET..PARENT_OFFSET_END)?;
        println!("{:?}", &b);
        let num = BigEndian::read_u32(b);
        if num == 0 {
            None
        } else {
            Some(num)
        }
    }

    pub fn get_reference_index(&self) -> Option<u32> {
        let b = self.0.get(REFERENCE_OFFSET..REFERENCE_OFFSET_END)?;
        let num = BigEndian::read_u32(b);
        if num == 0 {
            None
        } else {
            Some(num)
        }
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
        assert_eq!(tok.get_reference_index(), Some(1));
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
}
