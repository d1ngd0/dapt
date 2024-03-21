use arrayvec::ArrayVec;

use crate::binary::BCollection;
use crate::binary::TYPE_COLLECTION;
use crate::bookmark::Bookmark;
use crate::bookmark::MAX_POINTERS;
use crate::Dapt;
use crate::Ptrs;

// Node is the type that a parser puts out. each
// node should implement the trait functions below
pub trait Node {
    fn find(&self, d: &Dapt) -> Option<Ptrs>;
}

struct FieldLiteral {
    name: String,
}

impl Node for FieldLiteral {
    fn find(&self, d: &Dapt) -> Option<Ptrs> {
        let mut res: ArrayVec<Bookmark, MAX_POINTERS> = ArrayVec::new();
        for n in d.ptrs.iter() {
            let n = n.value_node(&d.b)?;
            match n.type_of(&d.b)? {
                TYPE_COLLECTION => {
                    let bcoll: BCollection = n.token_at(&d.b)?.try_into().unwrap();
                    if let Some(child_location) = bcoll.child_key(&self.name, &d.b) {
                        res.push(child_location.into());
                    }
                }
                _ => (),
            }
        }

        if res.len() > 0 {
            Some(res)
        } else {
            None
        }
    }
}
