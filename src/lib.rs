use std::sync::Arc;

use arrayvec::ArrayVec;
use binary::{BReference, BToken, Serialize};
use binary::{Binary, BinaryVisitor, SerializeBReference};
use error::DaptResult;
use path::node::Discoverable;
use serde::ser::SerializeSeq;
use serde::Deserializer;

mod binary;
mod error;
mod path;
pub mod query;

pub use binary::Any;
pub use binary::Deserialize;
pub use binary::Number;
pub use error::Error;
pub use path::parser::Path;

pub const MAX_POINTERS: usize = 128;

pub type Ptrs = ArrayVec<BReference, MAX_POINTERS>;

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

        d.ptrs.push(BReference::from(0));
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
            1 => SerializeBReference::new(self.ptrs[0], &self.b).serialize(serializer),
            _ => {
                let mut seq = serializer.serialize_seq(Some(self.ptrs.len()))?;
                for ptr in self.ptrs.iter() {
                    seq.serialize_element(&SerializeBReference::new(*ptr, &self.b))?;
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
        let mut ptrs = ArrayVec::from(ptrs);
        ptrs.truncate(1);

        Some(Dapt {
            iter_loc: 0,
            ptrs,
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
    pub fn val<'a, T: Deserialize<'a>>(&'a self, path: &str) -> DaptResult<T::Item> {
        let p = Path::try_from(path)?;
        self.val_path::<T>(Some(&p))
    }

    pub fn val_path<'a, T: Deserialize<'a>>(&'a self, path: Option<&Path>) -> DaptResult<T::Item> {
        let mut first = None;
        self.find(path, &mut |b| {
            if first.is_none() {
                first = Some(b);
            }
        });

        let first = match first {
            Some(first) => first,
            None => return Err(Error::NotFound),
        };

        Ok(self.b.get::<T>(first).unwrap())
    }

    // if you know the value is a string, you can grab it here without
    // taking additional heap allocations. This will live as long as
    // the dapt packet does.
    pub fn str<'a>(&'a self, path: &str) -> DaptResult<&'a str> {
        self.val::<&str>(path)
    }

    pub fn str_path<'a>(&'a self, path: Option<&Path>) -> DaptResult<&'a str> {
        self.val_path::<&str>(path)
    }

    pub fn number(&self, path: &str) -> DaptResult<Number> {
        let p = Path::try_from(path)?;
        self.number_path(Some(&p))
    }

    pub fn number_path<'a>(&'a self, path: Option<&Path>) -> DaptResult<Number> {
        let mut first = None;
        self.find(path, &mut |b| {
            if first.is_none() {
                first = Some(b);
            }
        });

        let first = match first {
            Some(first) => first,
            None => return Err(Error::NotFound),
        };

        self.b.number(first)
    }

    pub fn any(&self) -> Option<Any<'_>> {
        match self.ptrs.len() {
            0 => None,
            1 => self.b.any(self.ptrs[0]),
            _ => {
                let mut any = Vec::with_capacity(self.ptrs.len());
                for ptr in self.ptrs.iter() {
                    let val = match self.b.any(*ptr) {
                        Some(val) => val,
                        None => continue,
                    };

                    any.push(val);
                }

                Some(Any::Array(any))
            }
        }
    }

    pub fn any_path(&self, path: &Path) -> DaptResult<Any<'_>> {
        let mut ptrs = Vec::new();
        self.find(Some(path), &mut |b| ptrs.push(b));

        match ptrs.len() {
            0 => Err(Error::NotFound),
            1 => self.b.any(ptrs[0]).ok_or(Error::NotFound),
            _ => {
                let mut any = Vec::with_capacity(ptrs.len());
                for ptr in ptrs.iter() {
                    let val = match self.b.any(*ptr) {
                        Some(val) => val,
                        None => continue,
                    };

                    any.push(val);
                }

                Ok(Any::Array(any))
            }
        }
    }

    pub fn sub(&self, path: &str) -> Result<Dapt, error::Error> {
        let p = Path::try_from(path)?;
        Ok(self.sub_path(&p)?)
    }

    pub fn sub_path(&self, path: &Path) -> Result<Dapt, error::Error> {
        let mut d = Dapt {
            iter_loc: 0,
            ptrs: ArrayVec::new(),
            b: Arc::clone(&self.b),
        };

        self.find(Some(path), &mut |b| {
            d.ptrs.push(b);
        });

        Ok(d)
    }

    fn find<F: FnMut(BReference)>(&self, path: Option<&Path>, f: &mut F) {
        match path {
            // if there is a path, execute it to find the appropraite pointers
            Some(path) => self.ptrs.iter().for_each(|p| path.find(&self.b, *p, f)),
            // if there is no path we can call the closure on our current location
            None => self.ptrs.iter().for_each(|p| f(*p)),
        }
    }
}

pub struct DaptBuilder {
    b: Binary,
}

impl From<Dapt> for DaptBuilder {
    fn from(d: Dapt) -> Self {
        DaptBuilder { b: (*d.b).clone() }
    }
}

impl DaptBuilder {
    pub fn new() -> Self {
        DaptBuilder {
            b: Binary::default(),
        }
    }

    pub fn set<T: Serialize>(&mut self, path: &str, value: T) -> DaptResult<()> {
        let p = Path::try_from(path)?;
        self.set_path(&p, value)
    }

    pub fn set_path<T: Serialize>(&mut self, path: &Path, value: T) -> DaptResult<()> {
        let node = path.aquire(&mut self.b, BReference::from(0))?;
        let v = self
            .b
            .add(Some(BToken::from(node).get_reference(&self.b)), value);
        node.set_child(v, &mut self.b);
        Ok(())
    }

    pub fn set_any(&mut self, path: &str, value: Any) -> DaptResult<()> {
        let p = Path::try_from(path)?;
        self.set_any_path(&p, value)
    }

    pub fn set_any_path(&mut self, path: &Path, value: Any) -> DaptResult<()> {
        let node = path.aquire(&mut self.b, BReference::from(0))?;
        let v = self
            .b
            .add_any(Some(BToken::from(node).get_reference(&self.b)), value);
        node.set_child(v, &mut self.b);
        Ok(())
    }

    pub fn build(self) -> Dapt {
        let mut ptrs = ArrayVec::new();
        ptrs.push(BReference::from(0));

        Dapt {
            ptrs,
            iter_loc: 0,
            b: Arc::new(self.b),
        }
    }
}
