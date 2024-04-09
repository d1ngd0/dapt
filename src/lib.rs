use std::sync::Arc;

use arrayvec::ArrayVec;
use binary::{Binary, BinaryVisitor, SerializeBookmark};
use bookmark::{Ptrs, MAX_POINTERS};
use path::parser::Node;
use serde::ser::SerializeSeq;
use serde::Deserializer;

mod binary;
mod bookmark;
mod error;
mod path;
mod query;

pub use binary::Any;
pub use binary::Deserialize;
pub use binary::Number;
pub use error::Error;
pub use path::parser::Path;

#[derive(Debug)]
pub struct Dapt {
    iter_loc: usize,
    ptrs: Ptrs,
    b: Arc<Binary>,
}

impl Default for Dapt {
    // initialize the default dapt packet and have it
    // point to the root of the document.
    fn default() -> Self {
        let mut d = Dapt {
            iter_loc: 0,
            ptrs: ArrayVec::new(),
            b: Arc::new(Binary::default()),
        };

        d.ptrs.push(0.into());
        d
    }
}

impl Clone for Dapt {
    fn clone(&self) -> Self {
        Dapt {
            iter_loc: 0,
            ptrs: self.ptrs.clone(),
            b: Arc::clone(&self.b),
        }
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
            b: Arc::new(visitor.consume()),
        })
    }
}

impl serde::ser::Serialize for Dapt {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        match self.ptrs.len() {
            0 => serializer.serialize_none(),
            1 => SerializeBookmark::new(self.ptrs[0], &self.b).serialize(serializer),
            _ => {
                let mut seq = serializer.serialize_seq(Some(self.ptrs.len()))?;
                for ptr in self.ptrs.iter() {
                    seq.serialize_element(&SerializeBookmark::new(*ptr, &self.b))?;
                }
                seq.end()
            }
        }
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
            b: Arc::clone(&self.b),
        })
    }
}

impl Dapt {
    pub fn first(mut self) -> Dapt {
        self.ptrs.truncate(1);
        self
    }

    // get will return the value of the first pointer, if
    // there is one. When calling this function you must know
    // the type of value within the dapt packet
    pub fn val<'a, T: Deserialize<'a>>(&'a self) -> Option<T::Item> {
        if self.ptrs.len() == 0 {
            return None;
        }

        self.b.get::<T>(self.ptrs[0].index())
    }

    // if you know the value is a string, you can grab it here without
    // taking additional heap allocations. This will live as long as
    // the dapt packet does.
    pub fn str<'a>(&'a self) -> Option<&'a str> {
        if self.ptrs.len() == 0 {
            return None;
        }

        self.b.str(self.ptrs[0].index())
    }

    pub fn number(&self) -> Option<Number> {
        if self.ptrs.len() == 0 {
            return None;
        }

        self.b.number(self.ptrs[0].index()).ok()
    }

    pub fn any(&self) -> Option<Any> {
        if self.ptrs.len() == 0 {
            return None;
        }

        self.b.any(self.ptrs[0].index())
    }

    pub fn get(&self, path: &str) -> Result<Dapt, error::Error> {
        let p = Path::try_from(path)?;
        Ok(self.get_path(&p)?)
    }

    pub fn get_path(&self, path: &Path) -> Result<Dapt, error::Error> {
        let mut d = Dapt {
            iter_loc: 0,
            ptrs: self.ptrs.clone(),
            b: Arc::clone(&self.b),
        };

        for node in path.iter() {
            d.step_path(node)?;
        }

        Ok(d)
    }

    fn step_path(&mut self, n: &Node) -> Result<(), error::Error> {
        let mut ptrs = ArrayVec::new();

        for ptr in self.ptrs.iter() {
            let node_ptrs = n.find(self.b.as_ref(), *ptr);
            if let Some(node_ptrs) = node_ptrs {
                ptrs.try_extend_from_slice(&node_ptrs[..])?;
            }
        }

        self.ptrs = ptrs;
        Ok(())
    }
}
