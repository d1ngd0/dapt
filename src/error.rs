use std::{
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
};

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
    NotFound,
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

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::TypeMismatch(expected, got) => {
                write!(f, "Type mismatch: expected {}, got {}", expected, got)
            }
            Error::IncorrectSize(got) => write!(f, "Incorrect size: {}", got),
            Error::InvalidIndex(got) => write!(f, "Invalid index: {}", got),
            Error::GeneralError(got) => write!(f, "General error: {}", got),
            Error::NumberConversionFailed(got) => write!(f, "Number conversion failed: {}", got),
            Error::ParseError(got) => write!(f, "Parse error: {}", got),
            Error::TooManyPointers => write!(f, "Too many pointers"),
            Error::NotFound => write!(f, "Not found"),
        }
    }
}
