use std::fmt::Display;

use crate::{
    query::{
        expression::{Expression, PathExpression},
        parser::{Column, Parser, AGGREGATION_SUM, FN_CLOSE, FN_OPEN},
        QueryResult,
    },
    Any, Dapt, Number, Path,
};

use super::Aggregation;

#[derive(Clone)]
pub struct SumAggregation {
    value: Box<dyn Expression>,
    sum: Number,
}

impl SumAggregation {
    pub fn from_parser(parser: &mut Parser) -> QueryResult<SumAggregation> {
        parser.consume_token(AGGREGATION_SUM)?;
        parser.consume_token(FN_OPEN)?;
        let value = parser.parse_expression()?;
        parser.consume_token(FN_CLOSE)?;

        Ok(SumAggregation {
            value,
            sum: Number::ISize(0),
        })
    }

    pub fn new(value: Box<dyn Expression>) -> Self {
        Self {
            value,
            sum: Number::ISize(0),
        }
    }
}

// SumAggregation will sum the values of the expression, if the expression
// returns an array, each item in the array is sumed. If the expression
// returns non numeric types they are ignored without error.
impl Aggregation for SumAggregation {
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
                        Ok(n) => self.sum = self.sum + n,
                        Err(_) => (),
                    }
                }
            }
            _ => match Number::try_from(val) {
                Ok(n) => self.sum = self.sum + n,
                Err(_) => (),
            },
        }
    }

    fn result<'a>(&'a mut self) -> Option<Any<'a>> {
        Some(self.sum.into())
    }

    fn composable(&self, path: &Path) -> (Vec<Column>, Box<dyn Aggregation>) {
        // create the column
        let composite = Column::new(Box::new(self.clone()), path.clone());

        // create the aggregation
        let combine = Box::new(SumAggregation {
            value: Box::new(PathExpression::from(path.clone())),
            sum: Number::ISize(0),
        });

        (vec![composite], combine)
    }
}

impl Display for SumAggregation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{AGGREGATION_SUM}({})", self.value)
    }
}
