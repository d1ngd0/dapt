use arrayvec::ArrayVec;
use std::fmt;
use std::rc::Rc;

use crate::binary::{BArray, BMap, Binary, TYPE_ARRAY, TYPE_MAP};
use crate::bookmark::Bookmark;
use crate::bookmark::MAX_POINTERS;
use crate::Ptrs;

use super::parser::Node;

// Node is the type that a parser puts out. each
// node should implement the trait functions below
pub trait Discoverable {
    fn find(&self, bin: Rc<Binary>, b: Bookmark) -> Option<Ptrs>;
}

#[derive(Debug, PartialEq)]
pub struct FieldLiteral {
    name: String,
}

// FieldLiteral is an actual field name in the dapt packet. These can be
// chained together with `.` in order to create a path. For instance
// `log.level` would be two field literals, which point to the json structure
// `{"log": {"level": "info"}}`. Anything that is not interpreted as a different
// type is considered a field literal. Special characters can be escaped by wrapping
// the field name in double quotes. For instance `"log.level"` would be a single
// field literal that points to the json structure `{"log.level": "info"}`.
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
            TYPE_MAP => {
                let bcoll: BMap = n.token_at(&bin)?.try_into().unwrap();
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

// The Array operator `[]` allows you to traverse arrays. When you
// supply an index, like so `[1]`, it will select the specifie element
// at the provided index. Dapt packets can point to multiple locations
// in a document at once, which means supplying no index `[]` will point
// to all elements in the array.
#[derive(Debug, PartialEq)]
pub struct Array {
    index: Option<usize>,
}

impl Array {
    pub fn new(index: Option<usize>) -> Array {
        Array { index }
    }
}

impl Discoverable for Array {
    // find returns a list of pointers to the
    // child that matches the specified name.
    fn find(&self, bin: Rc<Binary>, b: Bookmark) -> Option<Ptrs> {
        let mut res: ArrayVec<Bookmark, MAX_POINTERS> = ArrayVec::new();

        let n = b.value_node(&bin)?;

        match n.type_of(&bin)? {
            TYPE_ARRAY => {
                let bcoll: BArray = n.token_at(&bin)?.try_into().unwrap();
                if let None = self.index {
                    unsafe {
                        res.set_len(bcoll.length()); // set the length to hold the
                                                     // indexes
                        bcoll.child_indexes(&mut res) // add the indexes
                    }
                } else {
                    if let Some(child_location) = bcoll.child_index(self.index.unwrap()) {
                        res.push(child_location.into());
                    }
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

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.index {
            Some(i) => write!(f, "[{}]", i),
            None => write!(f, "[]"),
        }
    }
}

// Wildcard will select all the children within the map we are currently
// in.
#[derive(Debug, PartialEq)]
pub struct Wildcard;

impl Discoverable for Wildcard {
    // find returns a list of pointers to the
    // child that matches the specified name.
    fn find(&self, bin: Rc<Binary>, b: Bookmark) -> Option<Ptrs> {
        let mut res: ArrayVec<Bookmark, MAX_POINTERS> = ArrayVec::new();

        let n = b.value_node(&bin)?;

        match n.type_of(&bin)? {
            TYPE_MAP => {
                let bcoll: BMap = n.token_at(&bin)?.try_into().unwrap();
                unsafe {
                    res.set_len(bcoll.length()); // set the length to hold the
                                                 // indexes
                    bcoll.child_indexes(&mut res) // add the indexes
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

impl fmt::Display for Wildcard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "*")
    }
}

// recursive will traverse the tree until it finds the node that matches.
// `~.message` would match the `message` field in the json structure
// `{"something":{ "something-else": { "message": "hello" }}}`.
#[derive(Debug, PartialEq)]
pub struct Recursive {
    child: Box<Node>,
}

impl Recursive {
    pub fn new(child: Node) -> Recursive {
        Recursive {
            child: Box::new(child),
        }
    }
}

impl Discoverable for Recursive {
    // find returns a list of pointers to the
    // child that matches the specified name.
    fn find(&self, bin: Rc<Binary>, b: Bookmark) -> Option<Ptrs> {
        let mut res: ArrayVec<Bookmark, MAX_POINTERS> = ArrayVec::new();
        b.walk(&bin, &mut res, &|childb, res| {
            if let Some(ptrs) = self.child.find(Rc::clone(&bin), childb) {
                res.try_extend_from_slice(&ptrs[..]).unwrap();
            }
            true
        });

        if res.len() > 0 {
            Some(res)
        } else {
            None
        }
    }
}

impl fmt::Display for Recursive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "~{}", self.child)
    }
}

#[derive(Debug, PartialEq)]
pub struct First {
    paths: Vec<Node>,
}

impl First {
    pub fn new(paths: Vec<Node>) -> First {
        First { paths }
    }
}

impl Discoverable for First {
    // find returns a list of pointers to the
    // child that matches the specified name.
    fn find(&self, bin: Rc<Binary>, b: Bookmark) -> Option<Ptrs> {
        let mut res: ArrayVec<Bookmark, MAX_POINTERS> = ArrayVec::new();

        for path in &self.paths {
            if let Some(ptrs) = path.find(Rc::clone(&bin), b) {
                res.try_extend_from_slice(&ptrs[..]).unwrap();
                break;
            }
        }

        if res.len() > 0 {
            Some(res)
        } else {
            None
        }
    }
}

impl fmt::Display for First {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;

        for (x, path) in self.paths.iter().enumerate() {
            if x > 0 {
                write!(f, "|")?;
            }
            write!(f, "{}", path)?;
        }

        write!(f, "}}")?;
        Ok(())
    }
}
