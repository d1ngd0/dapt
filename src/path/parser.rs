use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

use crate::binary::Binary;
use crate::bookmark::{Bookmark, Ptrs};

use super::lexer::Lexer;
use super::node::{Array, Discoverable, FieldLiteral, Wildcard};

const NESTING_OPERATOR: &str = ".";
const INDEX_OPERATOR: &str = "[";
const INDEX_OPERATOR_END: &str = "]";
const WILDCARD_OPERATOR: &str = "*";

#[derive(Debug)]
pub enum ParseError {
    EOF,
    UnexpectedEOF,
    MalformedPath(String),
    InvalidIndex(String),
}

impl ParseError {
    pub fn to_string(&self) -> String {
        match self {
            ParseError::EOF => "EOF".to_string(),
            ParseError::UnexpectedEOF => "Unexpected EOF".to_string(),
            ParseError::MalformedPath(s) => format!("Malformed path: {}", s),
            ParseError::InvalidIndex(s) => format!("Invalid index: {}", s),
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, PartialEq)]
pub enum Node {
    FieldLiteral(FieldLiteral),
    Array(Array),
    Wildcard(Wildcard),
}

impl Node {
    pub fn find(&self, bin: Rc<Binary>, b: Bookmark) -> Option<Ptrs> {
        match self {
            Node::FieldLiteral(fl) => fl.find(bin, b),
            Node::Array(ar) => ar.find(bin, b),
            Node::Wildcard(w) => w.find(bin, b),
        }
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::FieldLiteral(fl) => write!(f, "{}", fl),
            Node::Array(ar) => write!(f, "{}", ar),
            Node::Wildcard(wc) => write!(f, "{}", wc),
        }
    }
}

pub struct Path(Vec<Node>);

impl Path {
    pub fn new(path: &str) -> Result<Path, ParseError> {
        Path::try_from(path)
    }

    pub fn push(&mut self, node: Node) {
        self.0.push(node);
    }

    pub fn pop(&mut self) -> Option<Node> {
        self.0.pop()
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
            let node = p.parse();
            match node {
                Err(ParseError::EOF) => break,
                Err(err) => Err(err)?,
                Ok(n) => nodes.push(n),
            };
        }

        Ok(Path(nodes))
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

impl Parser<'_> {
    fn parse(&mut self) -> ParseResult<Node> {
        let token = self.lex.token();
        if let None = token {
            return Err(ParseError::EOF);
        }

        match token.unwrap() {
            // if we hit a nesting operator we should just try again
            NESTING_OPERATOR => self.parse(),
            INDEX_OPERATOR => self.parse_index(),
            WILDCARD_OPERATOR => Ok(Node::Wildcard(Wildcard)),
            _ => Ok(Node::FieldLiteral(FieldLiteral::new(token.unwrap()))),
        }
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

            let mut node = p.parse();
            while let Ok(n) = node {
                nodes.push(n.to_string());
                node = p.parse();
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
