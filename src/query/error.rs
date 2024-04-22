use std::fmt::Display;

use crate::path::parser::ParseError;
use crate::Error as DaptError;

use super::lexor::Lexer;

pub type QueryResult<T> = Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    InvalidQuery(String),
    UnexpectedEOF(String),
    NonExistentKey(String),
    DaptError(String),
}

impl Error {
    pub fn with_history(msg: &str, lex: &Lexer) -> Self {
        Error::InvalidQuery(format!("{}: {}", msg, lex.consumed()))
    }

    pub fn unexpected_eof(lex: &Lexer) -> Self {
        Error::UnexpectedEOF(format!("unexpected EOF at: {}", lex.consumed()))
    }
}

impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Error::InvalidQuery(format!("Key Parsing error: {}", e.to_string()))
    }
}

impl From<DaptError> for Error {
    fn from(e: DaptError) -> Self {
        Error::DaptError(e.to_string())
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidQuery(msg) => write!(f, "Invalid query: {}", msg),
            Error::UnexpectedEOF(msg) => write!(f, "Unexpected EOF: {}", msg),
            Error::NonExistentKey(msg) => write!(f, "Non existent key: {}", msg),
            Error::DaptError(msg) => write!(f, "Dapt error: {}", msg),
        }
    }
}
