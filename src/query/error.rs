use crate::path::parser::ParseError;

use super::lexor::Lexer;

pub type QueryResult<T> = Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    InvalidQuery(String),
    UnexpectedEOF(String),
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
