use std::rc::Rc;

use arrayvec::ArrayVec;

use crate::binary::{BCollection, BKeyValue, BToken, Binary, TYPE_COLLECTION, TYPE_KEYVAL};

#[derive(Debug, Clone, Copy)]
pub struct Bookmark(usize);

impl Bookmark {
    pub fn new(index: usize) -> Self {
        Bookmark(index)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

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

impl From<Bookmark> for usize {
    fn from(value: Bookmark) -> Self {
        value.0
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

    // TODO: This is horribly messy, we need to track the type
    // of the collection (array or map) seperatly.
    pub fn is_array<'a>(&self, bin: &'a Rc<Binary>) -> bool {
        let t = self.token_at(bin);
        if let None = t {
            return false;
        }
        let t = t.unwrap();

        if t.get_type() != TYPE_COLLECTION {
            return false;
        }

        let c = BCollection::try_from(t).unwrap();
        if c.length() == 0 {
            return false;
        }

        let first = bin.token_at(c.child_index(0).unwrap());
        if let None = first {
            return false;
        }

        if first.unwrap().get_type() == TYPE_KEYVAL {
            return false;
        }

        true
    }

    pub fn is_object<'a>(&self, bin: &'a Rc<Binary>) -> bool {
        !self.is_array(bin)
    }

    pub fn type_of<'a>(&self, bin: &'a Rc<Binary>) -> Option<u8> {
        bin.type_at(self.0)
    }

    pub fn token_at<'a>(&self, bin: &'a Rc<Binary>) -> Option<BToken<'a>> {
        bin.token_at(self.0)
    }
}
