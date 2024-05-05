use std::fmt::Display;

use crate::{
    query::{
        expression::Expression,
        parser::{Parser, AGGREGATION_SUM, FN_CLOSE, FN_OPEN},
        QueryResult,
    },
    Any, Dapt, Number,
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
    fn process<'a>(&'a mut self, d: &Dapt) -> QueryResult<()> {
        let expr_val = self.value.evaluate(d);
        let val = match &expr_val {
            Some(v) => v,
            None => return Ok(()),
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
        Ok(())
    }

    fn result<'a>(&'a self) -> QueryResult<Any<'a>> {
        Ok(self.sum.into())
    }

    fn composable(
        &self,
        expr: Box<dyn Expression>,
    ) -> (Box<dyn Aggregation>, Box<dyn Aggregation>) {
        let sum = SumAggregation::new(expr);
        (Box::new(self.clone()), Box::new(sum))
    }
}

impl Display for SumAggregation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{AGGREGATION_SUM}({})", self.value)
    }
}
