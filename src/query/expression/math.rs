use std::fmt::Display;

use crate::{Any, Dapt, Number};

use super::Expression;

use super::super::{
    parser::{
        Parser, FN_ADD, FN_CLOSE, FN_DIVIDE, FN_MINUS, FN_MODULUS, FN_MULTIPLY, FN_OPEN, FN_SEP,
    },
    QueryResult,
};
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
                parser.consume_next($fn)?;
                parser.consume_next(FN_OPEN)?;
                let left = parser.parse_expression()?;
                parser.consume_next(FN_SEP)?;
                let right = parser.parse_expression()?;
                parser.consume_next(FN_CLOSE)?;

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
