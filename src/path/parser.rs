use std::fmt;

use super::lexer::Lexer;
use super::node::FieldLiteral;

const NESTING_OPERATOR: &str = ".";

enum ParseError {
    EOF,
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, PartialEq)]
pub enum Node {
    FieldLiteral(FieldLiteral),
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::FieldLiteral(fl) => write!(f, "{}", fl),
        }
    }
}

struct Parser<'a> {
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
            _ => Ok(Node::FieldLiteral(FieldLiteral::new(token.unwrap()))),
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
