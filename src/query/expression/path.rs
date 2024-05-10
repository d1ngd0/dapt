use std::{fmt::Display, ops::Deref};

use crate::{
    query::{
        parser::{Parser, KEY_WRAP},
        Error, QueryResult,
    },
    Any, Dapt, Path,
};

use super::Expression;

// PathExpression is a wrapper around a path to make it an expression.
// A path expression will return the value of the path, in the dapt packet
// when evaluated.
#[derive(Debug, Clone)]
pub struct PathExpression {
    path: Path,
}

impl From<Path> for PathExpression {
    fn from(path: Path) -> Self {
        PathExpression { path }
    }
}

impl PathExpression {
    pub fn from_parser(parser: &mut Parser) -> QueryResult<Self> {
        parser.consume_token(KEY_WRAP)?;

        let key = match parser.token() {
            Some(tok) => tok,
            None => return Err(Error::unexpected_eof(parser.consumed())),
        };

        // This a bit of a hack so that we can use the \ escaping character in
        // a path. If you have a " in your path you are going to have to do terrible
        // things
        let path = Path::try_from(key.replace("\\\"", "\"").as_str())
            .map_err(|e| Error::with_history(&e.to_string(), parser.consumed()))?;

        // consume the final " token, and return. If we get a different token
        // or hit EOF we can return an error
        match parser.token() {
            Some(KEY_WRAP) => Ok(PathExpression { path }),
            Some(tok) => Err(Error::with_history(
                &format!("expected {KEY_WRAP} but got {tok}"),
                parser.consumed(),
            )),
            None => Err(Error::unexpected_eof(parser.consumed())),
        }
    }

    pub fn new(path: Path) -> Self {
        PathExpression { path }
    }
}

impl Deref for PathExpression {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        &self.path
    }
}

impl Expression for PathExpression {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'a Dapt) -> Option<Any<'a>> {
        d.any_path(self).ok()
    }
}

impl Display for PathExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", format!("{}", self.path).replace("\"", "\\\""))
    }
}
