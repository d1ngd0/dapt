use std::rc::Rc;

use arrayvec::ArrayVec;

use crate::binary::{BKeyValue, BToken, Binary, TYPE_KEYVAL};

#[derive(Debug, Clone, Copy)]
pub struct Bookmark(usize);

pub const MAX_POINTERS: usize = 128;

pub type Ptrs = ArrayVec<Bookmark, MAX_POINTERS>;

impl From<usize> for Bookmark {
    fn from(value: usize) -> Self {
        Bookmark(value)
    }
}

impl From<u32> for Bookmark {
    fn from(value: u32) -> Self {
        Bookmark(value.try_into().unwrap())
    }
}

impl From<i32> for Bookmark {
    fn from(value: i32) -> Self {
        Bookmark(value.try_into().unwrap())
    }
}

impl Bookmark {
    // value_node returns a bookmark pointing to a value. If the
    // bookmark was pointing to a Key value it will traverse down
    // towards the value.
    pub fn value_node<'a>(&self, bin: &'a Rc<Binary>) -> Option<Bookmark> {
        let loc = match self.type_of(bin)? {
            TYPE_KEYVAL => {
                let kv = BKeyValue::try_from(self.token_at(bin)?).unwrap();
                kv.child_index().into()
            }
            _ => *self,
        };

        Some(loc)
    }

    pub fn type_of<'a>(&self, bin: &'a Rc<Binary>) -> Option<u8> {
        bin.type_at(self.0)
    }

    pub fn token_at<'a>(&self, bin: &'a Rc<Binary>) -> Option<BToken<'a>> {
        bin.token_at(self.0)
    }
}
