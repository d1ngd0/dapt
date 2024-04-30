use core::slice;
use std::fmt;

use crate::binary::{
    BArray, BKeyValue, BMap, BReference, BToken, Binary, TYPE_ARRAY, TYPE_KEYVAL, TYPE_MAP,
};
use crate::error::DaptResult;
use crate::{Error, Path, MAX_POINTERS};

use super::parser::Node;

// Aquireable allows you to ask for that token, and if it doesn't exist we will
// create it. The aquire function should only ever create a sinle entity within
// the document, since you can only return a single breference.
pub trait Aquireable {
    fn aquire(&self, bin: &mut Binary, b: BReference) -> DaptResult<BKeyValue>;
}

// Node is the type that a parser puts out. each
// node should implement the trait functions below
pub trait Discoverable {
    fn find<F>(&self, bin: &Binary, b: BReference, f: &mut F)
    where
        F: FnMut(BReference);
}

#[derive(Debug, PartialEq, Clone)]
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
    fn find<F>(&self, bin: &Binary, b: BReference, f: &mut F)
    where
        F: FnMut(BReference),
    {
        let n = match b.val_at(bin) {
            Some(n) => n,
            None => return,
        };

        match n.get_type(bin) {
            TYPE_MAP => {
                let bcoll = BMap::from(n);
                if let Some(child_location) = bcoll.child_key(&self.name, bin) {
                    f(child_location);
                }
            }
            _ => (),
        };
    }
}

impl Aquireable for FieldLiteral {
    fn aquire(&self, bin: &mut Binary, b: BReference) -> DaptResult<BKeyValue> {
        let mut reg = None;
        self.find(bin, b, &mut |x| reg = Some(x));

        if reg.is_some() {
            return Ok(reg
                .unwrap()
                .key_at(bin)
                .ok_or_else(|| Error::CanNotAquire(format!("could not aquire {}", self)))?);
        }

        match b.token_at(bin) {
            // in the event the breference is empty. This can happen on an empty
            // dapt packet build, so we will set it to a map.
            None => {
                let (key_bref, bkv) = BKeyValue::new(None, BReference::from(0), &self.name, bin);
                // create new map
                let (map_bref, _) = BMap::new(Some(b), slice::from_ref(&key_bref), bin);
                // set the index of the breference to the map
                b.set_index(bin, *map_bref);
                Ok(bkv)
            }
            // we have a token which is a map, we can add another keyvalue to it and move on
            Some(tok) if tok.get_type(bin) == TYPE_MAP => {
                let (key_bref, bkv) = BKeyValue::new(
                    Some(tok.get_reference(bin)),
                    BReference::from(0),
                    &self.name,
                    bin,
                );

                // get existing map and extend it
                let bcoll = BMap::from(tok);
                bcoll.add_child(key_bref, bin);

                // return key value
                Ok(bkv)
            }
            // we have found a keyValue, in which case we should check if it has
            // a child and go deeper, or we should set the child value
            Some(tok) if tok.get_type(bin) == TYPE_KEYVAL => {
                let orig_bkv = BKeyValue::from(tok);
                let child = orig_bkv.child(bin);

                // the key has a value, so we should grab the child value and try to aquire that
                if child.is_some() {
                    return self.aquire(bin, child.unwrap());
                }

                let (key_bref, bkv) = BKeyValue::new(None, BReference::from(0), &self.name, bin);
                // create new map
                let (map_bref, _) = BMap::new(Some(b), slice::from_ref(&key_bref), bin);
                // set the index of the breference to the map
                orig_bkv.set_child(map_bref, bin);
                Ok(bkv)
            }
            // this is a case we do not handle
            Some(_) => Err("Cannot add a field to a non map type".into()),
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
#[derive(Debug, PartialEq, Clone)]
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
    fn find<F>(&self, bin: &Binary, b: BReference, f: &mut F)
    where
        F: FnMut(BReference),
    {
        let n = match b.val_at(bin) {
            Some(n) => n,
            None => return,
        };

        match n.get_type(bin) {
            TYPE_ARRAY => {
                let bcoll = BArray::from(n);
                if let None = self.index {
                    for i in 0..bcoll.length(bin) {
                        // we know the child is there because we call for length
                        // in the for loop
                        f(bcoll.child_index(bin, i).unwrap());
                    }
                } else {
                    if let Some(child_location) = bcoll.child_index(bin, self.index.unwrap()) {
                        f(child_location);
                    }
                }
            }
            _ => (),
        };
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
#[derive(Debug, PartialEq, Clone)]
pub struct Wildcard;

impl Discoverable for Wildcard {
    // find returns a list of pointers to the
    // child that matches the specified name.
    fn find<F>(&self, bin: &Binary, b: BReference, f: &mut F)
    where
        F: FnMut(BReference),
    {
        let n = match b.val_at(bin) {
            Some(n) => n,
            None => return,
        };

        match n.get_type(bin) {
            TYPE_MAP => {
                let bcoll = BMap::from(n);
                for i in 0..bcoll.length(bin) {
                    // we know the child is there because we call for length
                    // in the for loop
                    f(bcoll.child_index(bin, i).unwrap());
                }
            }
            _ => (),
        };
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
#[derive(Debug, PartialEq, Clone)]
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
    fn find<F>(&self, bin: &Binary, b: BReference, f: &mut F)
    where
        F: FnMut(BReference),
    {
        b.walk(bin, &mut |childref| {
            self.child.find(bin, childref, f);
            true
        });
    }
}

impl fmt::Display for Recursive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "~.{}", self.child)
    }
}

// First will find the first non empty value and return that.
// `{~.message|~.error}` would match the `message` field in the json
// structure `{"message": "hello"}` and the `error` field in the json
// structure `{"error": "something went wrong"}`.
#[derive(Debug, PartialEq, Clone)]
pub struct First {
    paths: Vec<Path>,
}

impl First {
    pub fn new(paths: Vec<Path>) -> First {
        First { paths }
    }
}

impl Discoverable for First {
    // find returns a list of pointers to the
    // child that matches the specified name.
    fn find<F>(&self, bin: &Binary, b: BReference, f: &mut F)
    where
        F: FnMut(BReference),
    {
        for path in &self.paths {
            let ptrs = path.find_simple(bin, b);
            match ptrs.get(0) {
                Some(p) => {
                    f(*p);
                    return;
                }
                None => (),
            }
        }
    }
}

impl fmt::Display for First {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;

        for (x, path) in self.paths.iter().enumerate() {
            if x > 0 {
                write!(f, ",")?;
            }
            write!(f, "{}", path)?;
        }

        write!(f, "}}")?;
        Ok(())
    }
}

// Multi works much like first, but will match on all the values
// that are not empty. example `(~.message,~.error)` would match
// both fields in the json structure:
// `{"message": "hello", "error": "something went wrong"}`.
#[derive(Debug, PartialEq, Clone)]
pub struct Multi {
    paths: Vec<Path>,
}

impl Multi {
    pub fn new(paths: Vec<Path>) -> Multi {
        Multi { paths }
    }
}

impl Discoverable for Multi {
    // find returns a list of pointers to the
    // child that matches the specified name.
    fn find<F>(&self, bin: &Binary, b: BReference, f: &mut F)
    where
        F: FnMut(BReference),
    {
        for path in &self.paths {
            path.find_simple(bin, b).iter().for_each(|p| f(*p));
        }
    }
}

impl fmt::Display for Multi {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;

        for (x, path) in self.paths.iter().enumerate() {
            if x > 0 {
                write!(f, "|")?;
            }
            write!(f, "{}", path)?;
        }

        write!(f, ")")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Regexp {
    name: regex::Regex,
}

impl Regexp {
    pub fn new(name: &str) -> Regexp {
        // name is a string which optionally is wrapped in double quotes.
        // here we remove the double quotes if they exist and remove any
        // escape characters.
        Regexp {
            name: regex::Regex::new(name).unwrap(),
        }
    }
}

impl PartialEq for Regexp {
    fn eq(&self, other: &Self) -> bool {
        self.name.as_str() == other.name.as_str()
    }
}

impl Discoverable for Regexp {
    // find returns a list of pointers to the
    // child that matches the specified name.
    fn find<F>(&self, bin: &Binary, b: BReference, f: &mut F)
    where
        F: FnMut(BReference),
    {
        let n = match b.val_at(bin) {
            Some(n) => n,
            None => return,
        };

        match n.get_type(bin) {
            TYPE_MAP => {
                let bcoll = BMap::from(n);
                for i in 0..bcoll.length(bin) {
                    // we know we can unwrap because of the length in the for
                    // loop and because a valid bMap will only have key children
                    let child = bcoll.child_index(bin, i).unwrap();
                    if self.name.is_match(child.key_at(bin).unwrap().key(bin)) {
                        f(child);
                    }
                }
            }
            _ => (),
        };
    }
}

impl fmt::Display for Regexp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // if there is a . or a " in the name we need to wrap
        // it in double quotes. We will wrap spaces in double
        // quotes too, even though we don't have to.
        write!(f, "/{}/", self.name)
    }
}
