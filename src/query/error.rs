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
