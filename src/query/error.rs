#[derive(Debug)]

pub type QueryResult<T> = Result<T, Error>;

enum Error {
    InvalidQuery(String),
}

impl Error {
    fn with_history(self, msg: &str, lex: &Lexer) -> Self {
        match self {
            Error::InvalidQuery(msg) => Error::InvalidQuery(format!("{}: {}", msg, lex.consumed())),
        }
    }
}
