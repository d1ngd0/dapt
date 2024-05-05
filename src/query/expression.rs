use std::{collections::HashMap, fmt::Display, ops::Deref};

use dyn_clone::DynClone;

use crate::{Any, Dapt, Number, Path};

use super::{
    parser::{
        Parser, ARRAY_CHILD_SEP, ARRAY_WRAP, ARRAY_WRAP_END, FALSE, FN_ADD, FN_CLOSE, FN_DIVIDE,
        FN_MINUS, FN_MODULUS, FN_MULTIPLY, FN_OPEN, FN_SEP, KEY_WRAP, MAP_CHILD_SEP, MAP_CHILD_SET,
        MAP_WRAP, MAP_WRAP_END, NULL, STRING_WRAP, TRUE,
    },
    Error, QueryResult,
};

// Expression is a trait that takes in a dapt packet and returns an
// optional value. This value can be Any type, which is what a dapt packet
// can return.
pub trait Expression: Display + DynClone {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b Dapt) -> Option<Any<'a>>;
}
dyn_clone::clone_trait_object!(Expression);

// Math expressions
macro_rules! impl_math_op {
    ($name:ident, $op:tt, $fn:ident) => {
        #[derive(Clone)]
        pub struct $name {
            left: Box<dyn Expression>,
            right: Box<dyn Expression>,
        }

        impl $name {
            pub fn from_parser(parser: &mut Parser) -> QueryResult<Self> {
                parser.consume_token($fn)?;
                parser.consume_token(FN_OPEN)?;
                let left = parser.parse_expression()?;
                parser.consume_token(FN_SEP)?;
                let right = parser.parse_expression()?;
                parser.consume_token(FN_CLOSE)?;

                Ok($name { left, right })
            }
        }

        impl Expression for $name {
            fn evaluate<'a, 'b: 'a>(&'a self, d: &'b Dapt) -> Option<Any<'a>> {
                let left = Number::try_from(self.left.evaluate(d)?).ok()?;
                let right = Number::try_from(self.right.evaluate(d)?).ok()?;

                Some(Any::from(left $op right))
            }
        }

        impl Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{} {} {}", self.left, stringify!($op), self.right)
            }
        }
    };
}

impl_math_op!(ModulusExpression, %, FN_MODULUS);
impl_math_op!(DivideExpression, /, FN_DIVIDE);
impl_math_op!(MultiplyExpression, *, FN_MULTIPLY);
impl_math_op!(AddExpression, +, FN_ADD);
impl_math_op!(SubtractExpression, -, FN_MINUS);

// PathExpression is a wrapper around a path to make it an expression.
// A path expression will return the value of the path, in the dapt packet
// when evaluated.
#[derive(Debug, Clone)]
pub struct PathExpression {
    path: Path,
}

impl PathExpression {
    pub fn from_parser(parser: &mut Parser) -> QueryResult<Self> {
        parser.consume_token(KEY_WRAP)?;

        let key = match parser.token() {
            Some(tok) => tok,
            None => return Err(Error::unexpected_eof(parser.consumed())),
        };

        let path = Path::try_from(key)
            .map_err(|e| Error::with_history(&e.to_string(), parser.consumed()))?;

        // consume the final " token, and return. If we get a different token
        // or hit EOF we can return an error
        match parser.token() {
            Some(KEY_WRAP) => Ok(PathExpression { path }),
            Some(tok) => Err(Error::with_history(
                &format!("expected {KEY_WRAP} but got {tok}"),
                parser.consumed(),
            )),
            None => Err(Error::unexpected_eof(parser.consumed())),
        }
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

// StringExpression makes a literal string an expression.
#[derive(Debug, Clone)]
pub struct StringExpression {
    value: String,
}

impl StringExpression {
    pub fn from_parser(parser: &mut Parser) -> QueryResult<Self> {
        parser.consume_token(STRING_WRAP)?;
        let value = match parser.token() {
            Some(tok) => tok.to_string(),
            None => return Err(Error::unexpected_eof(parser.consumed())),
        };

        // consume the final " token, and return. If we get a different token
        // or hit EOF we can return an error
        match parser.token() {
            Some(STRING_WRAP) => Ok(StringExpression { value }),
            Some(tok) => Err(Error::with_history(
                &format!("expected {STRING_WRAP} but got {tok}"),
                parser.consumed(),
            )),
            None => Err(Error::unexpected_eof(parser.consumed())),
        }
    }
}

impl Expression for StringExpression {
    fn evaluate<'a, 'b: 'a>(&'a self, _: &'b Dapt) -> Option<Any<'a>> {
        Some(Any::Str(&self.value))
    }
}

impl Display for StringExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}'", self.value)
    }
}

// NullExpression is an expression that returns a null value.
#[derive(Debug, Clone)]
pub struct NullExpression;

impl NullExpression {
    pub fn from_parser(parser: &mut Parser) -> QueryResult<Self> {
        parser.consume_token(NULL)?;
        Ok(NullExpression)
    }
}

impl Expression for NullExpression {
    fn evaluate<'a, 'b: 'a>(&'a self, _: &'b Dapt) -> Option<Any<'a>> {
        Some(Any::Null)
    }
}

impl Display for NullExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NULL")
    }
}

// BoolExpression is an expression that returns a boolean value.
#[derive(Debug, Clone)]
pub struct BoolExpression {
    value: bool,
}

impl BoolExpression {
    pub fn from_parser(parser: &mut Parser) -> QueryResult<Self> {
        let value = match parser.token() {
            Some(tok) => tok,
            None => return Err(Error::unexpected_eof(parser.consumed())),
        };

        let value = match value.to_uppercase().as_str() {
            TRUE => true,
            FALSE => false,
            _ => {
                return Err(Error::with_history(
                    &format!("expected TRUE or FALSE but got {}", value),
                    parser.consumed(),
                ))
            }
        };

        Ok(BoolExpression { value })
    }
}

impl Expression for BoolExpression {
    fn evaluate<'a, 'b: 'a>(&'a self, _: &'b Dapt) -> Option<Any<'a>> {
        Some(Any::Bool(self.value))
    }
}

impl Display for BoolExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", if self.value { "TRUE" } else { "FALSE" })
    }
}

// NumberExpression is an expression that returns a number value.
#[derive(Debug, Clone)]
pub struct NumberExpression(Number);

impl NumberExpression {
    pub fn from_parser(parser: &mut Parser) -> QueryResult<Self> {
        let tok = match parser.token() {
            Some(tok) => tok,
            None => return Err(Error::unexpected_eof(parser.consumed())),
        };

        let chars = tok.chars();

        if chars.filter(|c| *c == '.').count() == 1 {
            let num = tok.parse::<f64>().map_err(|e| {
                Error::with_history(
                    &format!("expected integer but got {}", e),
                    parser.consumed(),
                )
            })?;

            Ok(NumberExpression(Number::F64(num)))
        } else {
            let num = tok.parse::<isize>().map_err(|e| {
                Error::with_history(
                    &format!("expected integer but got {}", e),
                    parser.consumed(),
                )
            })?;

            Ok(NumberExpression(Number::ISize(num)))
        }
    }
}

impl Deref for NumberExpression {
    type Target = Number;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Expression for NumberExpression {
    fn evaluate<'a, 'b: 'a>(&'a self, _: &'b Dapt) -> Option<Any<'a>> {
        Some(Any::from(self.0))
    }
}

impl Display for NumberExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone)]
pub struct MapLiteral(HashMap<String, Box<dyn Expression>>);

impl MapLiteral {
    pub fn from_parser(parser: &mut Parser) -> QueryResult<Self> {
        parser.consume_token(MAP_WRAP)?;
        let mut map = HashMap::new();
        loop {
            // pase 'key': <expression>
            let key = parser.parse_string(KEY_WRAP)?;
            parser.consume_token(MAP_CHILD_SET)?;
            let value = parser.parse_expression()?;

            map.insert(key, value);

            match parser.token() {
                Some(MAP_WRAP_END) => break,
                Some(MAP_CHILD_SEP) => continue,
                Some(tok) => {
                    return Err(Error::with_history(
                        &format!("expected {MAP_CHILD_SEP} or {MAP_WRAP_END} but got {tok}"),
                        parser.consumed(),
                    ))
                }
                None => return Err(Error::unexpected_eof(parser.consumed())),
            }
        }

        Ok(MapLiteral(map))
    }
}

impl Deref for MapLiteral {
    type Target = HashMap<String, Box<dyn Expression>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Expression for MapLiteral {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b Dapt) -> Option<Any<'a>> {
        let mut map = HashMap::new();
        for (k, v) in self.iter() {
            if let Some(val) = v.evaluate(d) {
                map.insert(&k[..], val);
            }
        }

        Some(Any::Map(map))
    }
}

impl Display for MapLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (k, v) in self.iter() {
            write!(f, "'{}': {}, ", k, v)?;
        }
        write!(f, "}}")
    }
}

#[derive(Clone)]
pub struct ArrayLiteral(Vec<Box<dyn Expression>>);

impl ArrayLiteral {
    pub fn from_parser(parser: &mut Parser) -> QueryResult<Self> {
        parser.consume_token(ARRAY_WRAP)?;
        let mut arr = Vec::new();
        loop {
            let value = parser.parse_expression()?;
            arr.push(value);

            match parser.token() {
                Some(ARRAY_WRAP_END) => break,
                Some(ARRAY_CHILD_SEP) => continue,
                Some(tok) => {
                    return Err(Error::with_history(
                        &format!("expected , or {ARRAY_WRAP_END} but got {tok}"),
                        parser.consumed(),
                    ))
                }
                None => return Err(Error::unexpected_eof(parser.consumed())),
            }
        }

        Ok(ArrayLiteral(arr))
    }
}

impl Deref for ArrayLiteral {
    type Target = Vec<Box<dyn Expression>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Expression for ArrayLiteral {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b Dapt) -> Option<Any<'a>> {
        let mut arr = Vec::new();
        for v in self.iter() {
            if let Some(val) = v.evaluate(d) {
                arr.push(val);
            }
        }

        Some(Any::Array(arr))
    }
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let mut first = true;
        for v in self.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }

            write!(f, "{}", v)?;
        }
        write!(f, "]")
    }
}
