use std::fmt::Display;

use crate::{
    query::{
        parser::{Parser, FN_CLOSE, FN_OPEN, FN_SEP, STRING_WRAP},
        QueryResult,
    },
    Any,
};

use super::Expression;

pub const FN_LOWER: &str = "LOWER";
pub const FN_UPPER: &str = "UPPER";
pub const FN_LENGTH: &str = "LENGTH";
pub const FN_TRIM: &str = "TRIM";
pub const FN_TRIM_LEFT: &str = "TRIM_LEFT";
pub const FN_TRIM_RIGHT: &str = "TRIM_RIGHT";
pub const FN_CONCAT: &str = "CONCAT";
pub const FN_SPLIT: &str = "SPLIT";

macro_rules! simple_string_fn {
    ($name:ident, $fn:expr) => {
        #[derive(Clone)]
        pub struct $name {
            value: Box<dyn Expression>,
        }

        impl $name {
            pub fn from_parser<'a>(p: &mut Parser<'a>) -> QueryResult<$name> {
                p.consume_next($fn)?;
                p.consume_next(FN_OPEN)?;
                let value = p.expression()?;
                p.consume_next(FN_CLOSE)?;

                Ok($name { value })
            }
        }

        impl Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({})", $fn, self.value)
            }
        }
    };
}

simple_string_fn!(StringLower, FN_LOWER);
impl Expression for StringLower {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b crate::Dapt) -> Option<Any<'a>> {
        let value = self.value.evaluate(d)?;

        match value {
            Any::String(s) => Some(Any::String(s.to_lowercase())),
            Any::Str(s) => Some(Any::String(s.to_lowercase())),
            _ => None,
        }
    }
}

simple_string_fn!(StringUpper, FN_UPPER);
impl Expression for StringUpper {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b crate::Dapt) -> Option<Any<'a>> {
        let value = self.value.evaluate(d)?;

        match value {
            Any::String(s) => Some(Any::String(s.to_uppercase())),
            Any::Str(s) => Some(Any::String(s.to_uppercase())),
            _ => None,
        }
    }
}

simple_string_fn!(StringLength, FN_LENGTH);
impl Expression for StringLength {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b crate::Dapt) -> Option<Any<'a>> {
        let value = self.value.evaluate(d)?;

        match value {
            Any::String(s) => Some(Any::USize(s.len())),
            Any::Str(s) => Some(Any::USize(s.len())),
            _ => None,
        }
    }
}

simple_string_fn!(StringTrim, FN_TRIM);
impl Expression for StringTrim {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b crate::Dapt) -> Option<Any<'a>> {
        let value = self.value.evaluate(d)?;

        match value {
            Any::String(s) => Some(Any::String(s.trim().to_string())),
            Any::Str(s) => Some(Any::Str(s.trim())),
            _ => None,
        }
    }
}

simple_string_fn!(StringTrimLeft, FN_TRIM_LEFT);
impl Expression for StringTrimLeft {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b crate::Dapt) -> Option<Any<'a>> {
        let value = self.value.evaluate(d)?;

        match value {
            Any::String(s) => Some(Any::String(s.trim_start().to_string())),
            Any::Str(s) => Some(Any::Str(s.trim_start())),
            _ => None,
        }
    }
}

simple_string_fn!(StringTrimRight, FN_TRIM_RIGHT);
impl Expression for StringTrimRight {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b crate::Dapt) -> Option<Any<'a>> {
        let value = self.value.evaluate(d)?;

        match value {
            Any::String(s) => Some(Any::String(s.trim_end().to_string())),
            Any::Str(s) => Some(Any::Str(s.trim_end())),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct StringConcat {
    exprs: Vec<Box<dyn Expression>>,
}

impl StringConcat {
    pub fn from_parser<'a>(p: &mut Parser<'a>) -> QueryResult<StringConcat> {
        p.consume_next(FN_CONCAT)?;
        p.consume_next(FN_OPEN)?;

        let mut exprs = Vec::new();

        exprs.push(p.expression()?);
        while p.continue_if(FN_SEP) {
            exprs.push(p.expression()?);
        }

        p.consume_next(FN_CLOSE)?;

        Ok(StringConcat { exprs })
    }
}

impl Display for StringConcat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", FN_CONCAT)?;
        let mut first = true;
        for e in self.exprs.iter() {
            if !first {
                write!(f, ", ")?;
            } else {
                first = false;
            }

            write!(f, "{}", e)?;
        }
        write!(f, ")")
    }
}

impl Expression for StringConcat {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b crate::Dapt) -> Option<Any<'a>> {
        let mut result = String::new();
        for e in self.exprs.iter() {
            let value = e.evaluate(d)?;

            match value {
                Any::String(s) => result.push_str(&s),
                Any::Str(s) => result.push_str(s),
                _ => return None,
            }
        }

        Some(Any::String(result))
    }
}

#[derive(Clone)]
pub struct StringSplit {
    value: Box<dyn Expression>,
    delimiter: String,
}

impl StringSplit {
    pub fn from_parser<'a>(p: &mut Parser<'a>) -> QueryResult<StringSplit> {
        p.consume_next(FN_SPLIT)?;
        p.consume_next(FN_OPEN)?;

        let value = p.expression()?;
        p.consume_next(FN_SEP)?;
        let delimiter = p.parse_string(STRING_WRAP)?;

        p.consume_next(FN_CLOSE)?;

        Ok(StringSplit { value, delimiter })
    }
}

impl Display for StringSplit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({}, '{}')", FN_SPLIT, self.value, self.delimiter)
    }
}

impl Expression for StringSplit {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b crate::Dapt) -> Option<Any<'a>> {
        let value = self.value.evaluate(d)?;

        match value {
            Any::String(s) => {
                let arr = s
                    .split(&self.delimiter)
                    .map(|s| Any::String(s.to_string()))
                    .collect();
                Some(Any::Array(arr))
            }
            Any::Str(s) => {
                let arr = s.split(&self.delimiter).map(|s| Any::Str(s)).collect();
                Some(Any::Array(arr))
            }
            _ => None,
        }
    }
}
