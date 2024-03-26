use std::rc::Rc;

use arrayvec::ArrayVec;
use binary::{BToken, Binary};
use bookmark::{Ptrs, MAX_POINTERS};

mod binary;
mod bookmark;
mod error;
mod path;

pub struct Dapt {
    iter_loc: usize,
    ptrs: Ptrs,
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

        d.ptrs.push(0.into());
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

    fn token_at(&self) -> Option<BToken> {
        self.ptrs.first()?.token_at(&self.b)
    }
}

#[cfg(test)]
mod tests {}
