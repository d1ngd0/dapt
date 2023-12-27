use std::ops::Deref;
use std::rc::{Rc, Weak};

use arrayvec::ArrayVec;
use binary::{BCollection, BToken, Binary};

mod binary;
pub mod error;
pub mod value;

const MAX_POINTERS: usize = 128;

pub struct Dapt {
    iter_loc: usize,
    ptrs: ArrayVec<usize, MAX_POINTERS>,
    b: Rc<Binary>,
}

impl Default for Dapt {
    // initialize the default dapt packet and have it
    // point to the root of the document.
    fn default() -> Self {
        let mut d = Dapt {
            iter_loc: 0,
            ptrs: ArrayVec::new(),
            b: Rc::new(Binary::default()),
        };

        d.ptrs.push(0);
        d
    }
}

impl Iterator for Dapt {
    type Item = Dapt;

    fn next(&mut self) -> Option<Self::Item> {
        let ptr = self.ptrs.get(self.iter_loc)?;
        let ptrs = [*ptr; MAX_POINTERS];
        self.iter_loc += 1;

        Some(Dapt {
            iter_loc: 0,
            ptrs: ArrayVec::from(ptrs),
            b: Rc::clone(&self.b),
        })
    }
}

impl Dapt {
    pub fn first(mut self) -> Dapt {
        self.ptrs.truncate(1);
        self
    }

    pub fn children(&self) -> Dapt {
        let d := Dapt::default();
        let offset = 0;

        for dc in d {
            let tok = dc.token_at();
            let tok = BCollection::try_from(tok).ok()?;

        }
    }

    // type returns the type of the first value, and ignores the type
    // of any other values.
    pub fn type_of(&self) -> Option<u8> {
        self.b.type_at(*self.ptrs.get(0)?)
    }

    fn token_at(&self) -> Option<BToken> {
        self.b.token_at(*self.ptrs.get(0)?)
    }

    pub fn walk_iter() -> DaptWalkIterator {}
}

struct DaptWalkIterator {
    stack: DaptStack,
}

impl Iterator for DaptWalkIterator {
    type Item = Dapt;

    fn next(&mut self) -> Option<Self::Item> {
        let d = self.stack.next()?;
        match d.type_of() {
            binary::TYPE_COLLECTION => {
                let tok = self.stack.current.token_at()?;
                let tok = BCollection::try_from(tok).ok()?;
                None
            }
            binary::TYPE_KEYVAL => {
                let tok = self.stack.current.token_at()?;
                None
            }
            None => {
                self.stack.current = self.stack.parent?;
                self.stack.next()
            }
            _ => Some(d),
        }
    }
}

struct DaptStack {
    current: Dapt,
    parent: Option<Dapt>,
}

impl Deref for DaptStack {
    type Target = Dapt;

    fn deref(&self) -> &Self::Target {
        &self.current
    }
}

#[cfg(test)]
mod tests {}
