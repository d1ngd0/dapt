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
const ESCAPE: &str = "\\";

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEOF,
    MalformedPath(String),
    InvalidIndex(String),
}

impl ParseError {
    pub fn to_string(&self) -> String {
        match self {
            // used to signal the end of a section in operators like
            // first or multi.
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
        Ok(Node::FieldLiteral(FieldLiteral::from_escaped(field)))
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::FieldLiteral(fl) => write!(f, "{}", fl),
            Node::Array(ar) => write!(f, "{}", ar),
            Node::Wildcard(wc) => write!(f, "{}", wc),
            Node::Recursive(r) => write!(f, "{}", r),
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
        p.parse()
    }
}

impl From<Vec<Node>> for Path {
    fn from(nodes: Vec<Node>) -> Path {
        Path(nodes)
    }
}

impl Parser<'_> {
    fn parse_operator(&mut self) -> ParseResult<Node> {
        let token = self.lex.peak().ok_or(ParseError::UnexpectedEOF)?;

        match token {
            STRING_WRAP => self.parse_field_literal(),
            INDEX_OPERATOR => self.parse_index(),
            WILDCARD_OPERATOR => self.parse_wildcard(),
            RECURSIVE_OPERATOR => self.parse_recursive(),
            MULTI_OPERATOR => self.parse_multi(),
            FIRST_OPERATOR => self.parse_first(),
            REGEXP_OPERATOR => self.parse_regexp(),
            MULTI_OPERATOR_SEP|MULTI_OPERATOR_END|FIRST_OPERATOR_SEP|FIRST_OPERATOR_END|NESTING_OPERATOR => Err(ParseError::MalformedPath(format!("{} is a keyword, if it is part of a keyname you must wrap it with {} or escape it with {}", token, STRING_WRAP, ESCAPE))),
            _ => self.parse_field_literal(),
        }
    }

    // parse wildcard parses a valid wildcard string
    // example *
    fn parse_wildcard(&mut self) -> ParseResult<Node> {
        self.consume_next(WILDCARD_OPERATOR)?;

        return Ok(Node::Wildcard(Wildcard));
    }

    fn parse_field_literal(&mut self) -> ParseResult<Node> {
        let is_wrapped = self.is_next(STRING_WRAP);
        if is_wrapped {
            self.consume();
        }

        let node = self.lex.token().ok_or(ParseError::UnexpectedEOF)?;
        let node = FieldLiteral::from_escaped(node);

        if is_wrapped {
            self.consume_next(STRING_WRAP)?;
        }

        Ok(Node::FieldLiteral(node))
    }

    fn parse_regexp(&mut self) -> ParseResult<Node> {
        self.consume_next(REGEXP_OPERATOR)?;

        let reg = self.lex.token().ok_or(ParseError::UnexpectedEOF)?;
        let reg = Regexp::new(reg);

        self.consume_next(REGEXP_OPERATOR)?;
        Ok(Node::Regexp(reg))
    }

    // parse_multi will parse the string representation of a multi
    // operator
    // example ("me"|"another"."me"|"a"."third"."me")
    fn parse_multi(&mut self) -> ParseResult<Node> {
        self.consume_next(MULTI_OPERATOR)?;

        let mut paths = vec![];

        loop {
            match self.parse() {
                Ok(path) => paths.push(path),
                Err(err) => return Err(err),
            };

            if !self.is_next(MULTI_OPERATOR_SEP) {
                break;
            }
            self.consume()
        }

        // ensure the ending operator is supplied
        self.consume_next(MULTI_OPERATOR_END)?;

        Ok(Node::Multi(Multi::new(paths)))
    }

    // parse_first will parse the string representation of a first
    // operator
    // example: {"path"."one","path"."two","third".path}
    fn parse_first(&mut self) -> ParseResult<Node> {
        self.consume_next(FIRST_OPERATOR)?;

        let mut paths = vec![];

        loop {
            match self.parse() {
                Ok(path) => paths.push(path),
                Err(err) => return Err(err),
            };

            if !self.is_next(FIRST_OPERATOR_SEP) {
                break;
            }
            self.consume();
        }

        // ensure the ending operator is supplied
        self.consume_next(FIRST_OPERATOR_END)?;

        Ok(Node::First(First::new(paths)))
    }

    // parse is a helper function that expects you to handle returning
    // EOS when done parsing a path, and EOF when done parsing a list of
    // paths. This is used for first, and multi. If you should continue the
    // function will return true, if you should stop because EOF was returned
    // from the parser it will return false
    pub fn parse(&mut self) -> ParseResult<Path> {
        let mut nodes = vec![];
        loop {
            match self.parse_operator() {
                Ok(n) => nodes.push(n),
                Err(err) => return Err(err),
            };

            // check for a ., if we see one we know there is more
            // to parse.
            if self.is_next(NESTING_OPERATOR) {
                self.consume();
            } else if self.is_next(INDEX_OPERATOR) {
                // DO NOTHING, we want to continue but not consume
                // the token because it is valid
            } else {
                break;
            }
        }

        if nodes.len() == 0 {
            return Err(ParseError::MalformedPath(
                "empty path in first operator".to_string(),
            ));
        }

        Ok(Path::from_nodes(nodes))
    }

    fn parse_recursive(&mut self) -> ParseResult<Node> {
        self.consume_next(RECURSIVE_OPERATOR)?;

        if self.is_next(NESTING_OPERATOR) {
            self.consume()
        }

        let node = self.parse_operator()?;

        Ok(Node::Recursive(Recursive::new(node)))
    }

    fn parse_index(&mut self) -> ParseResult<Node> {
        self.consume_next(INDEX_OPERATOR)?;

        if self.is_next(INDEX_OPERATOR_END) {
            self.consume();
            return Ok(Node::Array(Array::new(None)));
        }

        let token = self.lex.token().ok_or(ParseError::UnexpectedEOF)?;
        let index = token
            .parse::<usize>()
            .map_err(|_| ParseError::InvalidIndex(token.to_string()))?;
        self.consume_next(INDEX_OPERATOR_END)?;

        Ok(Node::Array(Array::new(Some(index))))
    }

    // is_next will check to see if the next value matches the supplied
    // token. This is case sensitive.
    fn is_next(&mut self, tok: &str) -> bool {
        let seen = match self.lex.peak() {
            Some(seen) => seen,
            None => return false,
        };

        tok == seen
    }

    // consume_next will return an error if the next token consumed is not the
    // one supplied to the parser. This function is case sensitive.
    fn consume_next(&mut self, tok: &str) -> ParseResult<()> {
        let seen = self.lex.token().ok_or(ParseError::UnexpectedEOF)?;
        if seen != tok {
            return Err(ParseError::MalformedPath(format!(
                "expected {} but got {}",
                tok, seen
            )));
        }

        Ok(())
    }

    // consume just consumes the next token, no questions asked.
    fn consume(&mut self) {
        let _ = self.lex.token();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cmp::min;

    macro_rules! test_parse {
        ($path:expr, $($args:tt),*) => {
            // parse the path
            let mut p = Parser::from($path);
            let path = p.parse()?;

            // define the vectors we expect
            let expected: Vec<&str> = vec![$($args),*];

            assert_eq!(path.0.len(), expected.len());

            for x in 0..min(path.0.len(), expected.len()) {
                assert_eq!(path.0[x].to_string(), expected[x].to_string())
            }
        };
    }

    #[test]
    fn test_parse() -> ParseResult<()> {
        test_parse!("Im.am a.fish", "Im", "\"am a\"", "fish");
        test_parse!("a.b.c.d.e", "a", "b", "c", "d", "e");
        test_parse!("a.\"b.a\".c", "a", "\"b.a\"", "c");
        test_parse!("a.b\\.a.c", "a", "\"b.a\"", "c");
        test_parse!("{a,\"a\".\"b\"}.c", "{a,a.b}", "c");
        test_parse!("(a|\"a\".\"b\").c", "(a|a.b)", "c");
        test_parse!("*.c.something", "*", "c", "something");
        test_parse!("~something.yes", "~.something", "yes");
        test_parse!("/something/.hello", "/something/", "hello");
        test_parse!("c[1]", "c", "[1]");
        Ok(())
    }
}
