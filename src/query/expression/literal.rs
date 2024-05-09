use std::{collections::HashMap, fmt::Display, ops::Deref};

use crate::{
    query::{
        parser::{
            Parser, ARRAY_CHILD_SEP, ARRAY_WRAP, ARRAY_WRAP_END, FALSE, KEY_WRAP, MAP_CHILD_SEP,
            MAP_CHILD_SET, MAP_WRAP, MAP_WRAP_END, NULL, STRING_WRAP, TRUE,
        },
        Error, QueryResult,
    },
    Any, Dapt, Number,
};

use super::Expression;

// StringExpression makes a literal string an expression.
#[derive(Debug, Clone)]
pub struct StringExpression {
    value: String,
}

impl StringExpression {
    pub fn from_parser(parser: &mut Parser) -> QueryResult<Self> {
        parser.consume_token(STRING_WRAP)?;
        let value = match parser.token() {
            Some(tok) if tok == STRING_WRAP => {
                return Ok(StringExpression {
                    value: String::new(),
                })
            }
            Some(tok) => tok.to_string(),
            None => return Err(Error::unexpected_eof(parser.consumed())),
        };

        // consume the final " token, and return. If we get a different token
        // or hit EOF we can return an error
        match parser.token() {
            Some(STRING_WRAP) => Ok(StringExpression { value }),
            Some(tok) => Err(Error::with_history(
                &format!("string expected {STRING_WRAP} but got {tok}"),
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
