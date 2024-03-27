use std::rc::Rc;

use arrayvec::ArrayVec;
use binary::{BToken, Binary, BinaryVisitor};
use bookmark::{Ptrs, MAX_POINTERS};
use serde::Deserializer;

mod binary;
mod bookmark;
mod error;
mod path;

#[derive(Debug)]
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

impl<'de> serde::de::Deserialize<'de> for Dapt {
    fn deserialize<D>(deserializer: D) -> Result<Dapt, D::Error>
    where
        D: Deserializer<'de>,
    {
        let visitor = BinaryVisitor::default();
        let mut ptrs = ArrayVec::new();
        let bookmark = deserializer.deserialize_any(&visitor)?;
        ptrs.push(bookmark.into());

        Ok(Dapt {
            ptrs,
            iter_loc: 0,
            b: Rc::new(visitor.consume()),
        })
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
mod tests {
    use super::*;

    #[test]
    fn test_deserialize() {
        let data = r#"
            {
                "a": 1,
                "b": "hello",
                "c": [1, 2, 3],
                "d": {
                    "e": 1,
                    "f": "world"
                }
            }
        "#;

        let d: Dapt = serde_json::from_str(data).unwrap();
        println!("{:?}", d);
    }
}
