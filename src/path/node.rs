use arrayvec::ArrayVec;
use std::fmt;
use std::rc::Rc;

use crate::binary::BCollection;
use crate::binary::Binary;
use crate::binary::TYPE_COLLECTION;
use crate::bookmark::Bookmark;
use crate::bookmark::MAX_POINTERS;
use crate::Ptrs;

// Node is the type that a parser puts out. each
// node should implement the trait functions below
pub trait Discoverable {
    fn find(&self, bin: Rc<Binary>, b: Bookmark) -> Option<Ptrs>;
}

#[derive(Debug, PartialEq)]
pub struct FieldLiteral {
    name: String,
}

impl FieldLiteral {
    pub fn new(name: &str) -> FieldLiteral {
        // name is a string which optionally is wrapped in double quotes.
        // here we remove the double quotes if they exist and remove any
        // escape characters.
        FieldLiteral {
            name: name.trim_matches('"').replace("\\\"", "\""),
        }
    }
}

impl Discoverable for FieldLiteral {
    // find returns a list of pointers to the
    // child that matches the specified name.
    fn find(&self, bin: Rc<Binary>, b: Bookmark) -> Option<Ptrs> {
        let mut res: ArrayVec<Bookmark, MAX_POINTERS> = ArrayVec::new();

        let n = b.value_node(&bin)?;

        match n.type_of(&bin)? {
            TYPE_COLLECTION => {
                let bcoll: BCollection = n.token_at(&bin)?.try_into().unwrap();
                if let Some(child_location) = bcoll.child_key(&self.name, &bin) {
                    res.push(child_location.into());
                }
            }
            _ => (),
        }

        if res.len() > 0 {
            Some(res)
        } else {
            None
        }
    }
}

impl fmt::Display for FieldLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // if there is a . or a " in the name we need to wrap
        // it in double quotes. We will wrap spaces in double
        // quotes too, even though we don't have to.
        if self.name.contains('.') || self.name.contains('"') || self.name.contains(' ') {
            write!(f, "\"{}\"", self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
}
