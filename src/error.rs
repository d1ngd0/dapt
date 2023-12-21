use std::num::{ParseFloatError, ParseIntError};

pub type DaptResult<T> = Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    TypeMismatch(u8, String),
    IncorrectSize(String),
    InvalidIndex(String),
    GeneralError(String),
    NumberConversionFailed(String),
}

impl From<ParseFloatError> for Error {
    fn from(value: ParseFloatError) -> Self {
        Error::GeneralError(value.to_string())
    }
}

impl From<ParseIntError> for Error {
    fn from(value: ParseIntError) -> Self {
        Error::GeneralError(value.to_string())
    }
}
