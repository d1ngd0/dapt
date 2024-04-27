use std::convert::From;
use std::default::Default;
use std::mem;
use std::ops::Deref;

use crate::{
    binary::{Any, Deserialize, Number, Serialize},
    error::{DaptResult, Error},
};

pub const TYPE_REFERENCE: u8 = 0;
pub const TYPE_MAP: u8 = 1;
pub const TYPE_ARRAY: u8 = 2;
pub const TYPE_KEYVAL: u8 = 3;

const REFERENCE_LENGTH: usize = 5;
pub const MAX_REFERENCE_DEPTH: isize = 20;

const TYPE_OFFSET: usize = 0;
const TYPE_WIDTH: usize = 1;
const TYPE_OFFSET_END: usize = TYPE_OFFSET + TYPE_WIDTH;

const LENGTH_OFFSET: usize = TYPE_OFFSET_END;
const LENGTH_WIDTH: usize = 2;
const LENGTH_OFFSET_END: usize = LENGTH_OFFSET + LENGTH_WIDTH;

const REFERENCE_OFFSET: usize = LENGTH_OFFSET_END;
const REFERENCE_WIDTH: usize = 4;
const REFERENCE_OFFSET_END: usize = REFERENCE_OFFSET + REFERENCE_WIDTH;

const PARENT_OFFSET: usize = REFERENCE_OFFSET_END;
const PARENT_WIDTH: usize = 4;
const PARENT_OFFSET_END: usize = PARENT_OFFSET + PARENT_WIDTH;

const CONTENT_OFFSET: usize = PARENT_OFFSET_END;

const PTR_OFFSET: usize = TYPE_OFFSET_END;
const PTR_WIDTH: usize = 4;
const PTR_OFFSET_END: usize = PTR_OFFSET + PTR_WIDTH;

const ADD_TOKEN_HEADER_LENGTH: usize = CONTENT_OFFSET;

const INITAL_BINARY_SIZE: usize = 1024;

#[derive(Debug, Clone)]
pub struct Binary(Vec<u8>);

impl Binary {
    // increase the size of the binary. We return a mutable subslice
    // of the new growth, and the index where that subslice starts
    // within the binary.
    fn grow(&mut self, size: usize) -> (usize, &mut [u8]) {
        let index = self.0.len();
        self.0.resize(index + size, 0);

        (index, self.at_mut(index, size))
    }

    fn copy(&mut self, b: BToken, delta: usize) -> (usize, &mut [u8]) {
        // grab the index
        let index = self.0.len();
        // grab the length of the current token
        let size = b.get_length(self);
        // grow the binary and copy the token to the end of the binary
        for i in 0..size {
            self.0.push(self.0[*b + i]);
        }
        // set the val to the new location
        let b = BToken::from(index);
        // set the new length of the token plus the delta
        b.set_length(self, size + delta);
        // set the new location in the reference to the new token
        BReference::from(b.get_reference(self)).set_index(self, index);

        // grow the end of the token for the delta
        let (_, buf) = self.grow(delta);
        (index, buf)
    }

    fn at(&self, index: usize, width: usize) -> &[u8] {
        self.0.get(index..index + width).unwrap()
    }

    fn at_mut(&mut self, index: usize, width: usize) -> &mut [u8] {
        self.0.get_mut(index..index + width).unwrap()
    }

    // token_at will return the token at the given location.
    // At most you can traverse 20 references before giving up
    // trying to find the BToken
    pub fn token_at(&self, index: BReference) -> Option<BToken> {
        index.resolve(self, MAX_REFERENCE_DEPTH)
    }

    // add puts a new token and breference into the binary. It then returns the
    // index of the breference for future use
    pub fn add<T: Serialize>(&mut self, parent: Option<BReference>, content: T) -> BReference {
        let size = content.size_of();
        let (bref, tok) = BToken::new(parent, size, 0, self);
        let t = content.serialize(self.at_mut(*tok + CONTENT_OFFSET, size));
        tok.set_type(self, t);

        bref
    }

    pub fn add_any(&mut self, parent: Option<BReference>, content: Any) -> BReference {
        match content {
            Any::U8(val) => self.add(parent, val),
            Any::U16(val) => self.add(parent, val),
            Any::U32(val) => self.add(parent, val),
            Any::U64(val) => self.add(parent, val),
            Any::U128(val) => self.add(parent, val),
            Any::USize(val) => self.add(parent, val),
            Any::I8(val) => self.add(parent, val),
            Any::I16(val) => self.add(parent, val),
            Any::I32(val) => self.add(parent, val),
            Any::I64(val) => self.add(parent, val),
            Any::I128(val) => self.add(parent, val),
            Any::ISize(val) => self.add(parent, val),
            Any::F32(val) => self.add(parent, val),
            Any::F64(val) => self.add(parent, val),
            Any::Str(val) => self.add(parent, val),
            Any::Bool(val) => self.add(parent, val),
            Any::Char(val) => self.add(parent, val),
            Any::Bytes(val) => self.add(parent, val),
            Any::Null => self.add(parent, ()),
            Any::Array(val) => {
                let mut children: Vec<BReference> = Vec::with_capacity(val.len());
                for v in val {
                    children.push(self.add_any(parent, v));
                }

                let (bref, _) = BArray::new(parent, &children[..], self);
                bref
            }
            Any::Map(val) => {
                let mut children: Vec<BReference> = Vec::with_capacity(val.len());
                for (k, v) in val {
                    let child = self.add_any(parent, v);
                    let (bref, _token) = BKeyValue::new(parent, child, k, self);
                    children.push(bref);
                }

                let (bref, _) = BMap::new(parent, &children[..], self);
                bref
            }
        }
    }

    // pub fn type_at(&self, index: BReference) -> Option<u8> {
    //     let index = self.resolve(index, MAX_REFERENCE_DEPTH)?;
    //     Some(*self.0.get(index)?)
    // }

    pub fn get<'a, T: Deserialize<'a>>(&'a self, index: BReference) -> Option<T::Item> {
        let token = index.val_at(self)?;
        if token.get_type(self) != T::type_of() {
            return None;
        }

        Some(T::deserialize(token.get_content(self)))
    }

    // get_key is a convenience function to get the key of a BReference
    pub fn get_key<'a>(&'a self, index: BReference) -> Option<&'a str> {
        Some(index.key_at(self)?.key(self))
    }

    // if you know the value is a string you can get it without
    // taking a heap allocation and reference that value from
    // within the binary. If the value is not a str it will return
    // None
    pub fn str<'a>(&'a self, index: BReference) -> Option<&'a str> {
        self.get::<&'a str>(index)
    }

    // Number returns an error if the underlying type is not a Number
    pub fn number(&self, index: BReference) -> DaptResult<Number> {
        let token = index.val_at(self).ok_or(Error::InvalidIndex(
            "invalid index when finding number".into(),
        ))?;

        Number::new(self, token)
    }

    pub fn any(&self, index: BReference) -> Option<Any> {
        let token = index.val_at(self)?;
        Any::new(self, token)
    }
}

impl Default for Binary {
    fn default() -> Self {
        // create default with a breference to help define the root token.
        let mut b = Vec::with_capacity(INITAL_BINARY_SIZE);
        b.push(TYPE_REFERENCE);
        b.resize(REFERENCE_LENGTH, 0);
        Binary(b)
    }
}

// BReference is a reference to another location within the dapt packet.
// its use is to act as a modifible location in memory
// 0x0 u8 type
// 0x0, 0x0, 0x0, 0x0 u32 pointer to location
#[derive(PartialEq, Debug, Copy, Clone)]
pub struct BReference(usize);

impl BReference {
    // new creates a new breference within the binary and returns the
    // pointer within the binary to the newly created object. The reference
    // is initialized with a pointer to 0 or None. You must call set_index
    // for the reference to point to a valid location.
    fn new(bin: &mut Binary) -> BReference {
        // resize the buffer, and get the mutable slice
        let (index, buf) = bin.grow(REFERENCE_LENGTH);

        // Set the type to reference
        let t = buf.first_mut().expect("breference buf empty! can't fill.");
        *t = TYPE_REFERENCE;

        // calculate the next token
        let u = (index + REFERENCE_LENGTH) as u32;
        u.serialize(buf.get_mut(PTR_OFFSET..PTR_OFFSET_END).unwrap());

        BReference(index)
    }

    fn get_index(&self, bin: &Binary) -> Option<BToken> {
        let buf = bin.at(self.0 + PTR_OFFSET, PTR_WIDTH);
        let num = u32::deserialize(buf);

        if num == 0 {
            None
        } else {
            Some(BToken::from(num))
        }
    }

    fn set_index(&self, bin: &mut Binary, index: usize) {
        (index as u32).serialize(bin.at_mut(self.0 + PTR_OFFSET, PTR_WIDTH));
    }

    // This will be needed in the future when we add mutation
    // fn set_index(&self, bin: &mut Binary, index: usize) {
    //     let u = index as u32;
    //     u.serialize(bin.at_mut(self.0 + PTR_OFFSET, PTR_WIDTH));
    // }

    pub fn resolve(&self, bin: &Binary, max_depth: isize) -> Option<BToken> {
        if max_depth <= 0 {
            return None;
        }

        let tok = self.get_index(bin)?;
        if tok.get_type(bin) == TYPE_REFERENCE {
            return BReference::from(tok).resolve(bin, max_depth - 1);
        }

        Some(tok)
    }

    // val_at will grab the value for the current index. If the current
    // pointer is a key value, it will
    pub fn val_at(&self, bin: &Binary) -> Option<BToken> {
        let token = self.resolve(bin, MAX_REFERENCE_DEPTH)?;

        match token.get_type(bin) {
            TYPE_KEYVAL => {
                let key_value: BKeyValue = token.try_into().ok()?;
                key_value.child(bin).val_at(bin)
            }
            _ => Some(token),
        }
    }

    pub fn key_at(&self, bin: &Binary) -> Option<BKeyValue> {
        let token = self.resolve(bin, MAX_REFERENCE_DEPTH)?;

        match token.get_type(bin) {
            TYPE_KEYVAL => Some(BKeyValue::from(token)),
            // work our way up the tree until we find a key value...
            // TODO: think about this more... is this what we actually want
            // to do. In the array case we end up going up until we hit an
            // object. Something like [1,2,3,4] would never be able to generate
            // a key... maybe this is ol
            _ => token.get_parent(bin)?.key_at(bin),
        }
    }

    pub fn walk<'a, F>(&self, bin: &'a Binary, f: &mut F) -> bool
    where
        F: FnMut(BReference) -> bool,
    {
        let t = match self.val_at(bin) {
            None => return true,
            Some(t) => t,
        };

        match t.get_type(bin) {
            TYPE_ARRAY => {
                if !f(*self) {
                    return false;
                }

                let bcoll = BArray::from(t);
                for i in 0..bcoll.length(bin) {
                    let b = bcoll.child_index(bin, i).unwrap();
                    if !b.walk(bin, f) {
                        return false;
                    }
                }

                true
            }
            TYPE_MAP => {
                if !f(*self) {
                    return false;
                }

                let bcoll = BMap::from(t);
                for i in 0..bcoll.length(bin) {
                    let b = bcoll.child_index(bin, i).unwrap();
                    if !b.walk(bin, f) {
                        return false;
                    }
                }

                true
            }
            _ => f(*self),
        }
    }

    pub fn dump<'a>(&self, bin: &'a Binary) -> &'a [u8] {
        let token = self.resolve(bin, MAX_REFERENCE_DEPTH).unwrap();
        token.dump(bin)
    }
}

impl From<BToken> for BReference {
    fn from(tok: BToken) -> Self {
        BReference(tok.0)
    }
}

impl From<usize> for BReference {
    fn from(index: usize) -> Self {
        BReference(index)
    }
}

impl From<u32> for BReference {
    fn from(index: u32) -> Self {
        BReference(index as usize)
    }
}

impl From<i32> for BReference {
    fn from(index: i32) -> Self {
        BReference(index as usize)
    }
}

impl Deref for BReference {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
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
#[derive(PartialEq, Debug, Clone, Copy)]
pub struct BToken(usize);

impl From<usize> for BToken {
    fn from(index: usize) -> Self {
        BToken(index)
    }
}

impl From<u32> for BToken {
    fn from(index: u32) -> Self {
        BToken(index as usize)
    }
}

impl From<BReference> for BToken {
    fn from(bref: BReference) -> Self {
        BToken(bref.0)
    }
}

impl From<BMap> for BToken {
    fn from(b: BMap) -> Self {
        BToken(b.0)
    }
}

impl From<BArray> for BToken {
    fn from(b: BArray) -> Self {
        BToken(b.0)
    }
}

impl From<BKeyValue> for BToken {
    fn from(b: BKeyValue) -> Self {
        BToken(b.0)
    }
}

impl Deref for BToken {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl BToken {
    fn new(
        parent_index: Option<BReference>,
        content_length: usize,
        tpe: u8,
        bin: &mut Binary,
    ) -> (BReference, BToken) {
        // create a Breference for the token
        let bref = BReference::new(bin);

        // expand the binary to fit the new token
        let (index, buf) = bin.grow(ADD_TOKEN_HEADER_LENGTH + content_length);

        // set the type
        let t = buf.first_mut().unwrap();
        *t = tpe;

        // set the breference to our new token
        // bref.set_index(bin, index);
        // When creating a new breference it assumes the token will be written
        // right after, which gives the correct index. So just trust me bro

        // set the size of the token
        let size = (ADD_TOKEN_HEADER_LENGTH + content_length) as u16;
        size.serialize(
            buf.get_mut(LENGTH_OFFSET..LENGTH_OFFSET_END)
                .expect("btoken buf not large enough"),
        );

        // serialize the breference
        // TODO: I don't think I need this with my changes, but lets
        // do that in another commit, this one is getting silly
        (*bref as u32).serialize(
            buf.get_mut(REFERENCE_OFFSET..REFERENCE_OFFSET_END)
                .expect("btoken buf not large enough"),
        );

        // set the parent index
        let parent_index = parent_index.unwrap_or(BReference(0));
        (*parent_index as u32).serialize(
            buf.get_mut(PARENT_OFFSET..PARENT_OFFSET_END)
                .expect("btoken not large enough"),
        );

        (bref, BToken(index))
    }

    pub fn get_parent(&self, bin: &Binary) -> Option<BReference> {
        let result = u32::deserialize(bin.at(**self + PARENT_OFFSET, PARENT_WIDTH));

        if result == 0 {
            None
        } else {
            Some(BReference::from(result))
        }
    }

    pub fn set_parent(&self, bin: &mut Binary, bref: BReference) {
        (*bref as u32).serialize(bin.at_mut(**self + PARENT_OFFSET, PARENT_WIDTH));
    }

    pub fn get_reference(&self, bin: &Binary) -> BReference {
        BReference::from(u32::deserialize(
            bin.at(**self + REFERENCE_OFFSET, REFERENCE_WIDTH),
        ))
    }

    pub fn set_reference(&self, bin: &mut Binary, bref: BReference) {
        (*bref as u32).serialize(bin.at_mut(**self + REFERENCE_OFFSET, REFERENCE_WIDTH));
    }

    pub fn get_type(&self, bin: &Binary) -> u8 {
        bin.at(**self, TYPE_WIDTH)[0]
    }

    pub fn set_type(&self, bin: &mut Binary, t: u8) {
        bin.at_mut(**self, TYPE_WIDTH)[0] = t;
    }

    pub fn get_length(&self, bin: &Binary) -> usize {
        u16::deserialize(bin.at(**self + LENGTH_OFFSET, LENGTH_WIDTH)) as usize
    }

    pub fn set_length(&self, bin: &mut Binary, size: usize) {
        (size as u16).serialize(bin.at_mut(**self + LENGTH_OFFSET, LENGTH_WIDTH));
    }

    pub fn get_content<'a>(&self, bin: &'a Binary) -> &'a [u8] {
        let length = self.get_length(bin) - CONTENT_OFFSET;
        bin.at(**self + CONTENT_OFFSET, length)
    }

    pub fn is_type(&self, bin: &Binary, t: u8) -> bool {
        self.get_type(bin) == t
    }

    pub fn dump<'a>(&self, bin: &'a Binary) -> &'a [u8] {
        bin.at(**self, BToken::from(*self).get_length(bin))
    }
}

// BMap houses a map object in a dapt packet. A BMap children should
// only be KeyValue objects.
#[derive(PartialEq, Debug, Clone, Copy)]
pub struct BMap(usize);

impl From<BToken> for BMap {
    fn from(b: BToken) -> Self {
        BMap(b.0)
    }
}

impl Deref for BMap {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl BMap {
    pub fn new(
        parent_index: Option<BReference>,
        children: &[BReference],
        bin: &mut Binary,
    ) -> (BReference, BMap) {
        let size = mem::size_of::<u32>();
        let (bref, token) = BToken::new(parent_index, size * children.len(), TYPE_MAP, bin);

        for (i, v) in children.iter().enumerate() {
            let child_content_index = *token + CONTENT_OFFSET + (i * size);
            (**v as u32).serialize(bin.at_mut(child_content_index, 4));

            // we unwrap here since an invalid breference should panic
            v.resolve(&bin, MAX_REFERENCE_DEPTH)
                .unwrap()
                .set_parent(bin, bref);
        }

        (bref, token.into())
    }

    pub fn child_index(&self, bin: &Binary, index: usize) -> Option<BReference> {
        if index >= self.length(bin) {
            return None;
        }

        let index = **self + CONTENT_OFFSET + (index * mem::size_of::<u32>());
        let child_index = u32::deserialize(bin.at(index, mem::size_of::<u32>()));
        Some(BReference::from(child_index))
    }

    // child_indexes takes an array to populate. Make sure you send it a large enough slice or it will panic
    pub fn child_indexes(&self, bin: &Binary, ptrs: &mut [BReference]) {
        for i in 0..self.length(bin) {
            let child_content_index = self.child_index(bin, i);

            match child_content_index {
                None => return,
                Some(c) => ptrs[i] = c,
            }
        }
    }

    pub fn child_key(&self, key: &str, bin: &Binary) -> Option<BReference> {
        // we will go until child_index tells returns None, meaning
        // we have exhausted all the children
        for i in 0.. {
            let child_content_index = self.child_index(bin, i)?;
            let child: BKeyValue = child_content_index.key_at(bin)?;

            if child.key(bin) == key {
                return Some(child_content_index);
            }
        }

        None
    }

    pub fn length(&self, bin: &Binary) -> usize {
        (BToken::from(*self).get_length(bin) - CONTENT_OFFSET) / mem::size_of::<u32>()
    }

    pub fn add_child(&self, key: &str, child: BReference, bin: &mut Binary) {
        // TODO: This is a pretty inefficient way to do this since we copy the token
        // any time we add a child. May want to think through having a default size for
        // a map, kind of like how a vector has a cap and len.
        let (index, buf) = bin.copy(BToken::from(*self), mem::size_of::<u32>());
        (*child as u32).serialize(buf);
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct BKeyValue(usize);

impl<'a> From<BToken> for BKeyValue {
    fn from(b: BToken) -> Self {
        BKeyValue(b.0)
    }
}

impl Deref for BKeyValue {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl BKeyValue {
    pub fn new(
        parent_index: Option<BReference>,
        child: BReference,
        key: &str,
        bin: &mut Binary,
    ) -> (BReference, BKeyValue) {
        let size = mem::size_of::<u32>() + key.len();
        let (bref, token) = BToken::new(parent_index, size, TYPE_KEYVAL, bin);

        (*child as u32).serialize(bin.at_mut(*token + CONTENT_OFFSET, PTR_WIDTH));

        // unwrap here because we are getting a bad child which means
        // there is a logic error in the code
        child
            .resolve(bin, MAX_REFERENCE_DEPTH)
            .unwrap()
            .set_parent(bin, bref);

        key.serialize(bin.at_mut(*token + CONTENT_OFFSET + mem::size_of::<u32>(), key.len()));

        (bref, token.into())
    }

    pub fn child(&self, bin: &Binary) -> BReference {
        BReference::from(u32::deserialize(
            bin.at(**self + CONTENT_OFFSET, mem::size_of::<u32>()),
        ))
    }

    pub fn key<'a>(&self, bin: &'a Binary) -> &'a str {
        let header = CONTENT_OFFSET + mem::size_of::<u32>();

        <&'a str as Deserialize>::deserialize(bin.at(
            **self + header,
            BToken::from(*self).get_length(bin) - header,
        ))
    }
}

// BArray houses an array object in a dapt packet. A BArray should not
// house bkeyvalue objects.
#[derive(PartialEq, Debug, Clone, Copy)]
pub struct BArray(usize);

impl From<BToken> for BArray {
    fn from(b: BToken) -> Self {
        BArray(b.0)
    }
}

impl Deref for BArray {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl BArray {
    pub fn new(
        parent_index: Option<BReference>,
        children: &[BReference],
        bin: &mut Binary,
    ) -> (BReference, BArray) {
        let size = mem::size_of::<u32>();
        let (bref, token) = BToken::new(parent_index, size * children.len(), TYPE_ARRAY, bin);

        for (i, v) in children.iter().enumerate() {
            let child_content_index = *token + CONTENT_OFFSET + (i * size);
            (**v as u32).serialize(bin.at_mut(child_content_index, size));

            v.resolve(bin, MAX_REFERENCE_DEPTH)
                .unwrap()
                .set_parent(bin, bref);
        }

        (bref, token.into())
    }

    pub fn child_index(&self, bin: &Binary, index: usize) -> Option<BReference> {
        if index >= self.length(bin) {
            return None;
        }

        let index = **self + CONTENT_OFFSET + (index * mem::size_of::<u32>());
        let child_index = u32::deserialize(bin.at(index, mem::size_of::<u32>()));
        Some(BReference::from(child_index))
    }

    // child_indexes takes an array to populate. Make sure you send it a large enough slice or it will panic
    pub fn child_indexes(&self, bin: &Binary, ptrs: &mut [BReference]) {
        for i in 0..self.length(bin) {
            let child = self.child_index(bin, i);

            match child {
                None => return,
                Some(c) => ptrs[i] = c,
            }
        }
    }

    pub fn length(&self, bin: &Binary) -> usize {
        (BToken::from(*self).get_length(bin) - CONTENT_OFFSET) / mem::size_of::<u32>()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_breference() {
        let mut bin = Binary::default();
        let bref = BReference::new(&mut bin);
        assert_eq!(bref.get_index(&bin).unwrap(), BToken(10));
    }

    #[test]
    fn test_btoken() {
        let mut bin = Binary::default();
        let (_bref, tok) = BToken::new(Some(BReference(2)), 0, 1, &mut bin);
        assert_eq!(tok.get_type(&bin), 1);
        assert_eq!(tok.get_parent(&bin), Some(BReference(2)));
        assert_eq!(tok.get_reference(&bin), BReference(5));
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
        let mut children: Vec<BReference> = vec![];
        let mut b = Binary::default();
        for i in 0..10 {
            children.push(b.add(None, i as usize));
        }

        let (_bref, array) = BArray::new(None, &children[..], &mut b);
        for i in 0..array.length(&b) {
            assert_eq!(
                i,
                b.get::<usize>(array.child_index(&b, i).unwrap()).unwrap()
            )
        }
    }

    #[test]
    fn test_object() {
        let mut children: Vec<BReference> = vec![];
        let mut b = Binary::default();

        for i in 0..10 {
            let bref = b.add(None, i);
            let (bref, _token) = BKeyValue::new(None, bref, &format!("child_{i}"), &mut b);
            children.push(bref);
        }

        let (_bref, map) = BMap::new(None, &children, &mut b);

        for i in 0..map.length(&b) {
            let child_index = map.child_key(&format!("child_{i}"), &b).unwrap();
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
