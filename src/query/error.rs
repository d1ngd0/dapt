use std::fmt::Display;

use crate::path::parser::ParseError;
use crate::Error as DaptError;

pub type QueryResult<T> = Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    InvalidQuery(String),
    UnexpectedEOF(String),
    NonExistentKey(String),
    DaptError(String),
    NotFound,
}

impl Error {
    pub fn with_history(msg: &str, history: History<'_>) -> Self {
        if history.1.is_empty() {
            Error::InvalidQuery(format!("[ {} ] {}", history.0, msg))
        } else {
            Error::InvalidQuery(format!("[ {} █ {} ]: {}", history.0, history.1, msg))
        }
    }

    pub fn unexpected_eof(history: History<'_>) -> Self {
        Error::UnexpectedEOF(format!("unexpected EOF at: \"{}\"", history))
    }
}

// History is used to wrap the content the lexor has already consumed. By making
// this a type it is more likely that a developer in the future won't supply something
// other than that, causing confusing error messages.
pub struct History<'a>(&'a str, &'a str);

impl Display for History<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> History<'a> {
    pub fn new(past: &'a str, future: &'a str) -> Self {
        Self(past.trim_end(), future.trim_start())
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

impl From<parse_duration::parse::Error> for Error {
    fn from(e: parse_duration::parse::Error) -> Self {
        Error::InvalidQuery(format!("Duration parsing error: {}", e.to_string()))
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidQuery(msg) => write!(f, "Invalid query: {}", msg),
            Error::UnexpectedEOF(msg) => write!(f, "Unexpected EOF: {}", msg),
            Error::NonExistentKey(msg) => write!(f, "Non existent key: {}", msg),
            Error::DaptError(msg) => write!(f, "Dapt error: {}", msg),
            Error::NotFound => write!(f, "Not found"),
        }
    }
}
