use std::sync::Arc;

use arrayvec::ArrayVec;

use crate::binary::{BArray, BKeyValue, BMap, BToken, Binary, TYPE_ARRAY, TYPE_KEYVAL, TYPE_MAP};

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
    pub fn value_node<'a>(&self, bin: &Binary) -> Option<Bookmark> {
        let loc = match self.type_of(bin)? {
            TYPE_KEYVAL => {
                let kv = BKeyValue::try_from(self.token_at(bin)?).unwrap();
                kv.child_index().into()
            }
            _ => *self,
        };

        Some(loc)
    }

    pub fn is_array<'a>(&self, bin: &'a Arc<Binary>) -> bool {
        let t = self.token_at(bin);
        if let None = t {
            return false;
        }

        if t.unwrap().get_type() != TYPE_ARRAY {
            return false;
        }

        true
    }

    pub fn is_object<'a>(&self, bin: &'a Arc<Binary>) -> bool {
        let t = self.token_at(bin);
        if let None = t {
            return false;
        }

        if t.unwrap().get_type() != TYPE_MAP {
            return false;
        }

        true
    }

    pub fn type_of<'a>(&self, bin: &Binary) -> Option<u8> {
        bin.type_at(self.0)
    }

    pub fn token_at<'a>(&self, bin: &'a Binary) -> Option<BToken<'a>> {
        bin.token_at(self.0)
    }

    // pub fn key<'a>(&self, relative: Option<Bookmark>, bin: &'a Arc<Binary>) -> Option<String> {
    //     let mut t = bin.token_at(self.value_node(bin)?.index())?;
    //     let mut path = Path::default();

    //     loop {
    //         let p = bin.token_at(t.get_parent_index()? as usize)?;

    //         match p.get_type() {
    //             TYPE_KEYVAL => {
    //                 let kv = BKeyValue::try_from(p).unwrap();
    //                 path.push(Node::FieldLiteral(FieldLiteral::new(kv.key())));
    //             }
    //             _ => (),
    //         }
    //     }

    //     Some(path.reverse().to_string())
    // }

    pub fn walk<'a, F, T>(&self, bin: &'a Binary, ob: &mut T, f: &F) -> bool
    where
        F: Fn(Bookmark, &mut T) -> bool,
    {
        let t = self.value_node(bin);
        if let None = t {
            return true;
        }

        let t = bin.token_at(t.unwrap().index());
        if let None = t {
            return true;
        }
        let t = t.unwrap();

        match t.get_type() {
            TYPE_ARRAY => {
                if !f(*self, ob) {
                    return false;
                }

                let bcoll = BArray::try_from(t).unwrap();
                for i in 0..bcoll.length() {
                    let b = Bookmark::new(bcoll.child_index(i).unwrap());
                    if !b.walk(bin, ob, f) {
                        return false;
                    }
                }

                true
            }
            TYPE_MAP => {
                if !f(*self, ob) {
                    return false;
                }

                let bcoll = BMap::try_from(t).unwrap();
                for i in 0..bcoll.length() {
                    let b = Bookmark::new(bcoll.child_index(i).unwrap());
                    if !b.walk(bin, ob, f) {
                        return false;
                    }
                }

                true
            }
            _ => f(*self, ob),
        }
    }
}
