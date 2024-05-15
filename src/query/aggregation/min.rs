use std::fmt::Display;

use crate::{
    query::{
        expression::{Expression, PathExpression},
        parser::{Column, Parser, FN_CLOSE, FN_OPEN},
        QueryResult,
    },
    Any, Dapt, Number, Path,
};

use super::Aggregation;

pub const AGGREGATION_MIN: &str = "MIN";

#[derive(Clone)]
pub struct MinAggregation {
    value: Box<dyn Expression>,
    min: Option<Number>,
}

impl MinAggregation {
    pub fn from_parser(parser: &mut Parser) -> QueryResult<MinAggregation> {
        parser.consume_token(AGGREGATION_MIN)?;
        parser.consume_token(FN_OPEN)?;
        let value = parser.parse_expression()?;
        parser.consume_token(FN_CLOSE)?;

        Ok(MinAggregation { value, min: None })
    }

    pub fn new(value: Box<dyn Expression>) -> Self {
        Self { value, min: None }
    }
}

// SumAggregation will sum the values of the expression, if the expression
// returns an array, each item in the array is sumed. If the expression
// returns non numeric types they are ignored without error.
impl Aggregation for MinAggregation {
    fn process<'a>(&'a mut self, d: &Dapt) {
        let expr_val = self.value.evaluate(d);
        let val = match &expr_val {
            Some(v) => v,
            None => return,
        };

        match val {
            Any::Array(a) => {
                for v in a {
                    match Number::try_from(v) {
                        Ok(n) => {
                            self.min = Some(match self.min {
                                Some(s) => {
                                    if s < n {
                                        s
                                    } else {
                                        n
                                    }
                                }
                                None => n,
                            });
                        }
                        Err(_) => (),
                    }
                }
            }
            _ => match Number::try_from(val) {
                Ok(n) => {
                    self.min = Some(match self.min {
                        Some(s) => {
                            if s < n {
                                s
                            } else {
                                n
                            }
                        }
                        None => n,
                    });
                }
                Err(_) => (),
            },
        }
    }

    fn result<'a>(&'a mut self) -> Option<Any<'a>> {
        let v = self.min.take().map(Any::from);
        v
    }

    fn composable(&self, path: &Path) -> (Vec<Column>, Box<dyn Aggregation>) {
        // create the column
        let composite = Column::new(Box::new(self.clone()), path.clone());

        // create the aggregation
        let combine = Box::new(MinAggregation::new(Box::new(PathExpression::from(
            path.clone(),
        ))));

        (vec![composite], combine)
    }
}

impl Display for MinAggregation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{AGGREGATION_MIN}({})", self.value)
    }
}
