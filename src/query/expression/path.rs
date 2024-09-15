use std::{fmt::Display, ops::Deref};

use crate::{
    query::{parser::Parser, QueryResult},
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
    // Create a path from a parser
    pub fn from_parser(parser: &mut Parser) -> QueryResult<Self> {
        let path = parser.parse_key()?;
        Ok(PathExpression::from(path))
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
        write!(f, "{}", self.path)
    }
}
