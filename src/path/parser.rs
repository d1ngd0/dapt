use std::fmt;
use std::ops::Deref;

use crate::Error as DaptError;

use crate::binary::{BKeyValue, BReference, BToken, Binary};
use crate::error::DaptResult;

use super::lexer::Lexer;
use super::node::{
    Aquireable, Array, Discoverable, FieldLiteral, First, Multi, Recursive, Regexp, Wildcard,
};

const NESTING_OPERATOR: &str = ".";
const INDEX_OPERATOR: &str = "[";
const INDEX_OPERATOR_END: &str = "]";
const WILDCARD_OPERATOR: &str = "*";
const RECURSIVE_OPERATOR: &str = "~";
const FIRST_OPERATOR: &str = "{";
const FIRST_OPERATOR_END: &str = "}";
const FIRST_OPERATOR_SEP: &str = ",";
const MULTI_OPERATOR: &str = "(";
const MULTI_OPERATOR_END: &str = ")";
const MULTI_OPERATOR_SEP: &str = "|";
const REGEXP_OPERATOR: &str = "/";
const STRING_WRAP: &str = "\"";

#[derive(Debug)]
pub enum ParseError {
    EOF,
    EOS,
    UnexpectedEOF,
    MalformedPath(String),
    InvalidIndex(String),
}

impl ParseError {
    pub fn to_string(&self) -> String {
        match self {
            ParseError::EOF => "EOF".to_string(),
            // used to signal the end of a section in operators like
            // first or multi.
            ParseError::EOS => "End Of Section".to_string(),
            ParseError::UnexpectedEOF => "Unexpected EOF".to_string(),
            ParseError::MalformedPath(s) => format!("Malformed path: {}", s),
            ParseError::InvalidIndex(s) => format!("Invalid index: {}", s),
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    FieldLiteral(FieldLiteral),
    Array(Array),
    Wildcard(Wildcard),
    Recursive(Recursive),
    First(First),
    Multi(Multi),
    Regexp(Regexp),
}

impl Node {
    pub fn find<F>(&self, bin: &Binary, b: BReference, f: &mut F)
    where
        F: FnMut(BReference),
    {
        match self {
            Node::FieldLiteral(fl) => fl.find(bin, b, f),
            Node::Array(ar) => ar.find(bin, b, f),
            Node::Wildcard(w) => w.find(bin, b, f),
            Node::Recursive(r) => r.find(bin, b, f),
            Node::First(fr) => fr.find(bin, b, f),
            Node::Multi(m) => m.find(bin, b, f),
            Node::Regexp(r) => r.find(bin, b, f),
        }
    }

    pub fn aquire(&self, bin: &mut Binary, b: BReference) -> DaptResult<BKeyValue> {
        match self {
            Node::FieldLiteral(fl) => fl.aquire(bin, b),
            _ => Err(DaptError::CanNotAquire(
                "can not aquire from non field literal".to_string(),
            )),
        }
    }

    pub fn new_field_literal(field: &str) -> ParseResult<Node> {
        Ok(Node::FieldLiteral(FieldLiteral::new(field)))
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::FieldLiteral(fl) => write!(f, "{}", fl),
            Node::Array(ar) => write!(f, "{}", ar),
            Node::Wildcard(wc) => write!(f, "{}", wc),
            Node::Recursive(r) => write!(f, ".{}", r),
            Node::First(fr) => write!(f, "{}", fr),
            Node::Multi(m) => write!(f, "{}", m),
            Node::Regexp(r) => write!(f, "{}", r),
        }
    }
}

pub struct Parser<'a> {
    lex: Lexer<'a>,
}

impl<'a> From<Lexer<'a>> for Parser<'a> {
    fn from(lex: Lexer<'a>) -> Parser<'a> {
        Parser { lex }
    }
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(path: &'a str) -> Parser<'a> {
        Parser {
            lex: Lexer::from(path),
        }
    }
}

// Path is a collection of nodes. It also impelmets the Discoverable
// trait.
#[derive(Debug, PartialEq, Clone)]
pub struct Path(Vec<Node>);

impl Path {
    pub fn new(path: &str) -> Result<Path, ParseError> {
        Path::try_from(path)
    }

    fn from_nodes(nodes: Vec<Node>) -> Path {
        Path(nodes)
    }

    pub fn aquire(&self, bin: &mut Binary, b: BReference) -> DaptResult<BKeyValue> {
        let mut b = b;
        let mut kv = None;

        for node in self.iter() {
            kv = Some(node.aquire(bin, b)?);
            b = BToken::from(kv.unwrap()).get_reference(bin);
        }

        if let Some(kv) = kv {
            Ok(kv)
        } else {
            Err(DaptError::CanNotAquire("empty path".to_string()))
        }
    }

    // https://github.com/rust-lang/rust/issues/43520 here is why we need
    // this, in the future hopefully we can do this without the heap allocations
    pub fn find_simple(&self, bin: &Binary, b: BReference) -> Vec<BReference> {
        let mut cur_ptrs = vec![b];
        let mut next_ptrs = vec![];

        for node in self.iter() {
            cur_ptrs
                .iter()
                .for_each(|b| node.find(bin, *b, &mut |b| next_ptrs.push(b)));
            cur_ptrs.resize(next_ptrs.len(), BReference::from(0));
            cur_ptrs.copy_from_slice(&next_ptrs);
            next_ptrs.clear();
        }

        cur_ptrs
    }

    fn find_depth<F>(&self, bin: &Binary, b: BReference, depth: usize, f: &mut F)
    where
        F: FnMut(BReference),
    {
        let node = match self.0.get(depth) {
            Some(node) => node,
            None => {
                f(b);
                return;
            }
        };

        node.find(bin, b, &mut |b| self.find_depth(bin, b, depth + 1, f));
    }

    pub fn append_key(&mut self, key: &str) {
        self.0.push(Node::FieldLiteral(FieldLiteral::new(key)));
    }
}

impl Default for Path {
    fn default() -> Self {
        Path(vec![])
    }
}

impl Discoverable for Path {
    fn find<F>(&self, bin: &Binary, b: BReference, f: &mut F)
    where
        F: FnMut(BReference),
    {
        self.find_depth(bin, b, 0, f)
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut path = String::new();
        for node in self.iter() {
            match node {
                Node::FieldLiteral(fl) => path.push_str(&format!(".{}", fl)),
                Node::Array(ar) => path.push_str(&format!("{}", ar)),
                Node::Wildcard(wc) => path.push_str(&format!(".{}", wc)),
                Node::Recursive(r) => path.push_str(&format!(".{}", r)),
                Node::First(f) => path.push_str(&format!(".{}", f)),
                Node::Multi(m) => path.push_str(&format!(".{}", m)),
                Node::Regexp(r) => path.push_str(&format!(".{}", r)),
            }
        }

        write!(f, "{}", path.trim_start_matches('.'))
    }
}

impl Deref for Path {
    type Target = Vec<Node>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<&str> for Path {
    type Error = ParseError;

    fn try_from(path: &str) -> Result<Self, Self::Error> {
        let mut p = Parser::from(path);
        let mut nodes = vec![];

        loop {
            let node = p.parse(Node::new_field_literal);
            match node {
                Err(ParseError::EOF) => break,
                Err(err) => Err(err)?,
                Ok(n) => nodes.push(n),
            };
        }

        Ok(Path(nodes))
    }
}

impl From<Vec<Node>> for Path {
    fn from(nodes: Vec<Node>) -> Path {
        Path(nodes)
    }
}

impl Parser<'_> {
    fn parse<F>(&mut self, ext: F) -> ParseResult<Node>
    where
        F: Fn(&str) -> ParseResult<Node>,
    {
        let token = self.lex.token();
        if let None = token {
            return Err(ParseError::EOF);
        }
        let token = token.unwrap();

        match token {
            // if we hit a nesting operator we should just try again
            NESTING_OPERATOR => self.parse(Node::new_field_literal),
            STRING_WRAP => self.parse_wrapped_field_literal(),
            INDEX_OPERATOR => self.parse_index(),
            WILDCARD_OPERATOR => Ok(Node::Wildcard(Wildcard)),
            RECURSIVE_OPERATOR => self.parse_recursive(),
            MULTI_OPERATOR => self.parse_multi(),
            FIRST_OPERATOR => self.parse_first(),
            REGEXP_OPERATOR => self.parse_regexp(),
            _ => ext(token),
        }
    }

    fn parse_wrapped_field_literal(&mut self) -> ParseResult<Node> {
        let node = match self.lex.token() {
            Some(tok) => FieldLiteral::new(tok),
            None => return Err(ParseError::UnexpectedEOF),
        };

        match self.lex.token() {
            Some(STRING_WRAP) => Ok(Node::FieldLiteral(node)),
            Some(token) => Err(ParseError::MalformedPath(format!(
                "unexpected token: {}, expected string wrap",
                token
            ))),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn parse_regexp(&mut self) -> ParseResult<Node> {
        let reg = match self.lex.token() {
            Some(reg) => Regexp::new(reg),
            None => return Err(ParseError::UnexpectedEOF),
        };

        match self.lex.token() {
            Some(REGEXP_OPERATOR) => Ok(Node::Regexp(reg)),
            Some(token) => Err(ParseError::MalformedPath(format!(
                "unexpected token: {}, expected regex operator",
                token
            ))),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn parse_multi(&mut self) -> ParseResult<Node> {
        let mut paths = vec![];

        loop {
            let (path, cont) = self.parse_path(|token| match token {
                MULTI_OPERATOR_SEP => Err(ParseError::EOS),
                MULTI_OPERATOR_END => Err(ParseError::EOF),
                MULTI_OPERATOR => Err(ParseError::MalformedPath("unexpected (".to_string())),
                FIRST_OPERATOR => Err(ParseError::MalformedPath("unexpected {".to_string())),
                FIRST_OPERATOR_END => Err(ParseError::MalformedPath("unexpected }".to_string())),
                FIRST_OPERATOR_SEP => Err(ParseError::MalformedPath("unexpected ,".to_string())),
                _ => Node::new_field_literal(token),
            })?;

            paths.push(path);

            if !cont {
                break;
            }
        }

        Ok(Node::Multi(Multi::new(paths)))
    }

    fn parse_first(&mut self) -> ParseResult<Node> {
        let mut paths = vec![];

        loop {
            let (path, cont) = self.parse_path(|token| match token {
                FIRST_OPERATOR_SEP => Err(ParseError::EOS),
                FIRST_OPERATOR_END => Err(ParseError::EOF),
                FIRST_OPERATOR => Err(ParseError::MalformedPath("unexpected {".to_string())),
                MULTI_OPERATOR => Err(ParseError::MalformedPath("unexpected (".to_string())),
                MULTI_OPERATOR_END => Err(ParseError::MalformedPath("unexpected )".to_string())),
                MULTI_OPERATOR_SEP => Err(ParseError::MalformedPath("unexpected |".to_string())),
                _ => Node::new_field_literal(token),
            })?;

            paths.push(path);

            if !cont {
                break;
            }
        }

        Ok(Node::First(First::new(paths)))
    }

    // parse_path is a helper function that expects you to handle returning
    // EOS when done parsing a path, and EOF when done parsing a list of
    // paths. This is used for first, and multi. If you should continue the
    // function will return true, if you should stop because EOF was returned
    // from the parser it will return false
    fn parse_path<F>(&mut self, ext: F) -> ParseResult<(Path, bool)>
    where
        F: Fn(&str) -> ParseResult<Node>,
    {
        let mut nodes = vec![];
        let mut cont = true;
        loop {
            let node = self.parse(&ext);
            match node {
                Err(ParseError::EOS) => break,
                Err(ParseError::EOF) => {
                    cont = false;
                    break;
                }
                Err(err) => Err(err)?,
                Ok(n) => nodes.push(n),
            };
        }

        if nodes.len() == 0 {
            return Err(ParseError::MalformedPath(
                "empty path in first operator".to_string(),
            ));
        }

        Ok((Path::from_nodes(nodes), cont))
    }

    fn parse_recursive(&mut self) -> ParseResult<Node> {
        let node = self.parse(Node::new_field_literal);
        if let Err(ParseError::EOF) = node {
            return Err(ParseError::UnexpectedEOF);
        }

        Ok(Node::Recursive(Recursive::new(node?)))
    }

    fn parse_index(&mut self) -> ParseResult<Node> {
        let token = self.lex.token();
        match token {
            Some(INDEX_OPERATOR_END) => Ok(Node::Array(Array::new(None))),
            Some(index) => {
                let end_index = self.lex.token();
                if let None = end_index {
                    return Err(ParseError::UnexpectedEOF);
                }

                if end_index.unwrap() != INDEX_OPERATOR_END {
                    return Err(ParseError::MalformedPath(
                        "missing closing bracket for array".to_string(),
                    ));
                }

                let index = match index.parse::<usize>() {
                    Ok(index) => index,
                    Err(_) => return Err(ParseError::InvalidIndex(index.to_string())),
                };

                Ok(Node::Array(Array::new(Some(index))))
            }
            None => Err(ParseError::UnexpectedEOF),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_parse {
        ($path:expr, $($args:tt),*) => {
            let mut p = Parser::from($path);
            let mut nodes = vec![];
            let expected: Vec<&str> = vec![$($args),*];

            let mut node = p.parse(Node::new_field_literal);
            while let Ok(n) = node {
                nodes.push(n.to_string());
                node = p.parse(Node::new_field_literal);
            }

            assert_eq!(nodes, expected);
        };
    }

    #[test]
    fn test_parse() {
        test_parse!("Im.am a.fish", "Im", "\"am a\"", "fish");
        test_parse!("a.b.c.d.e", "a", "b", "c", "d", "e");
        test_parse!("a.\"b.a\".c", "a", "\"b.a\"", "c");
    }
}
