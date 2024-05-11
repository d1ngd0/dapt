use std::fmt::Display;

use crate::{
    query::{
        parser::{Parser, FN_DIVIDE, FN_MINUS, FN_MODULUS, FN_MULTIPLY},
        QueryResult,
    },
    Any, Dapt, Number, Path,
};

use super::Aggregation;
use crate::query::parser::{FN_ADD, FN_CLOSE, FN_OPEN, FN_SEP};

macro_rules! math_aggregation {
    ($name:ident, $op:tt, $fn:ident) => {
        #[derive(Clone)]
        pub struct $name {
            left: Box<dyn Aggregation>,
            right: Box<dyn Aggregation>,
        }

        impl $name {
            pub fn new(left: Box<dyn Aggregation>, right: Box<dyn Aggregation>) -> Self {
                Self { left, right }
            }

            pub fn from_parser(parser: &mut Parser) -> QueryResult<Self> {
                parser.consume_token($fn)?;
                parser.consume_token(FN_OPEN)?;
                let left = parser.parse_aggregation()?;
                parser.consume_token(FN_SEP)?;
                let right = parser.parse_aggregation()?;
                parser.consume_token(FN_CLOSE)?;

                Ok($name { left, right })
            }
        }

        impl Aggregation for $name {
            fn process<'a>(&'a mut self, d: &Dapt) {
                self.left.process(d);
                self.right.process(d);
            }

            fn result<'a>(&'a mut self) -> Option<Any<'a>> {
                let left: f64 = Number::try_from(self.left.result()?).ok()?.into();
                let right: f64 = Number::try_from(self.right.result()?).ok()?.into();

                Some(Any::F64(left $op right))
            }

            fn composable(
                &self,
                path: &Path,
            ) -> (Vec<crate::query::parser::Column>, Box<dyn Aggregation>) {
                // create the left handed side
                let mut left_path = path.clone();
                left_path.append_key("left"); // can't return error
                let (left_composite, left_combine) = self.left.composable(&left_path);

                // create the right handed side
                let mut right_path = path.clone();
                right_path.append_key("right"); // can't return error
                let (right_composite, right_combine) = self.right.composable(&right_path);

                // create the combination
                let combine = Box::new($name::new(left_combine, right_combine));
                (vec![left_composite, right_composite].concat(), combine)
            }
        }

        impl Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    f,
                    "{}{FN_OPEN}{}{FN_SEP}{}{FN_CLOSE}",
                    $fn, self.left, self.right
                )
            }
        }
    };
}

math_aggregation!(ModulusAggregation, %, FN_MODULUS);
math_aggregation!(DivideAggregation, /, FN_DIVIDE);
math_aggregation!(MultiplyAggregation, *, FN_MULTIPLY);
math_aggregation!(AddAggregation, +, FN_ADD);
math_aggregation!(SubtractAggregation, -, FN_MINUS);
