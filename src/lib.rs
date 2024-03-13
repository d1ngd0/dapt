use std::rc::Rc;

use arrayvec::ArrayVec;
use binary::{BToken, Binary};

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

    // type returns the type of the first value, and ignores the type
    // of any other values.
    pub fn type_of(&self) -> Option<u8> {
        self.b.type_at(*self.ptrs.first()?)
    }

    fn token_at(&self) -> Option<BToken> {
        self.b.token_at(*self.ptrs.get(0)?)
    }
}

#[cfg(test)]
mod tests {}
