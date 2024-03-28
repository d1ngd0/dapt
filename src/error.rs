use std::num::{ParseFloatError, ParseIntError};

use arrayvec::CapacityError;

use crate::path::parser::ParseError;

pub type DaptResult<T> = Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    TypeMismatch(u8, String),
    IncorrectSize(String),
    InvalidIndex(String),
    GeneralError(String),
    NumberConversionFailed(String),
    ParseError(String),
    TooManyPointers,
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

impl From<ParseError> for Error {
    fn from(value: ParseError) -> Self {
        Error::ParseError(value.to_string())
    }
}

impl From<CapacityError> for Error {
    fn from(_: CapacityError) -> Self {
        Error::TooManyPointers
    }
}
