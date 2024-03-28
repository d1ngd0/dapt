use std::rc::Rc;

use arrayvec::ArrayVec;
use binary::{Any, Binary, BinaryVisitor, Deserialize, Number};
use bookmark::{Ptrs, MAX_POINTERS};
use path::parser::{Node, Path};
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
            b: Rc::clone(&self.b),
        };

        for node in path.iter() {
            d.step_path(node)?;
        }

        Ok(d)
    }

    fn step_path(&mut self, n: &Node) -> Result<(), error::Error> {
        let mut ptrs = ArrayVec::new();

        for ptr in self.ptrs.iter() {
            let node_ptrs = n.find(Rc::clone(&self.b), *ptr);
            if let Some(node_ptrs) = node_ptrs {
                ptrs.try_extend_from_slice(&node_ptrs[..])?;
            }
        }

        self.ptrs = ptrs;
        Ok(())
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
                    "e": 1000,
                    "f": "world"
                }
            }
        "#;

        let d: Dapt = serde_json::from_str(data).unwrap();
        assert_eq!(d.get("d.f").unwrap().str(), Some("world"));
        assert_eq!(d.get("a").unwrap().val::<usize>(), Some(1));
        assert_eq!(d.get("b").unwrap().str(), Some("hello"));
        assert_eq!(d.get("d.e").unwrap().val::<usize>(), Some(1000));
    }
}
