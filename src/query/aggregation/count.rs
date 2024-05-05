use std::{fmt::Display, iter::Sum};

use crate::{
    query::{
        expression::Expression,
        parser::{Parser, AGGREGATION_COUNT, FN_CLOSE, FN_OPEN},
        QueryResult,
    },
    Any, Dapt,
};

use super::{Aggregation, SumAggregation};

// count aggregation will count the number of times an expression has a value
// if no argument is given to count it will just count the number of lines
#[derive(Clone)]
pub struct CountAggregation {
    expr: Option<Box<dyn Expression>>,
    count: usize,
}

impl CountAggregation {
    pub fn from_parser(parser: &mut Parser) -> QueryResult<CountAggregation> {
        parser.consume_token(AGGREGATION_COUNT)?;
        parser.consume_token(FN_OPEN)?;

        let expr = match parser.peak() {
            Some(FN_CLOSE) => None,
            _ => Some(parser.parse_expression()?),
        };

        parser.consume_token(FN_CLOSE)?;

        Ok(CountAggregation { expr, count: 0 })
    }
}

impl Display for CountAggregation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.expr {
            Some(e) => write!(f, "{AGGREGATION_COUNT}({})", e),
            None => write!(f, "{AGGREGATION_COUNT}"),
        }
    }
}

impl Aggregation for CountAggregation {
    fn process<'a>(&'a mut self, d: &Dapt) -> QueryResult<()> {
        match &self.expr {
            Some(e) => {
                if e.evaluate(d).is_some() {
                    self.count += 1;
                }
            }
            None => self.count += 1,
        }

        Ok(())
    }

    fn result<'a>(&'a self) -> QueryResult<Any<'a>> {
        Ok(Any::USize(self.count))
    }

    fn composable(
        &self,
        expr: Box<dyn Expression>,
    ) -> (Box<dyn Aggregation>, Box<dyn Aggregation>) {
        let count = CountAggregation {
            expr: self.expr.clone(),
            count: 0,
        };

        let sum = SumAggregation::new(expr);

        (Box::new(count), Box::new(sum))
    }
}
