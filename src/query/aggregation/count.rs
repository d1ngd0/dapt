use std::fmt::Display;

use crate::{
    query::{
        expression::{Expression, PathExpression},
        parser::{Column, Parser, AGGREGATION_COUNT, FN_CLOSE, FN_OPEN},
        QueryResult,
    },
    Any, Dapt, Path,
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
    pub fn new(expr: Option<Box<dyn Expression>>) -> Self {
        Self { expr, count: 0 }
    }

    pub fn from_parser(parser: &mut Parser) -> QueryResult<CountAggregation> {
        parser.consume_next(AGGREGATION_COUNT)?;
        parser.consume_next(FN_OPEN)?;

        let expr = match parser.peak() {
            Some(FN_CLOSE) => None,
            _ => Some(parser.expression()?),
        };

        parser.consume_next(FN_CLOSE)?;

        Ok(CountAggregation { expr, count: 0 })
    }
}

impl Display for CountAggregation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.expr {
            Some(e) => write!(f, "{AGGREGATION_COUNT}({})", e),
            None => write!(f, "{AGGREGATION_COUNT}()"),
        }
    }
}

impl Aggregation for CountAggregation {
    fn process<'a>(&'a mut self, d: &Dapt) {
        match &self.expr {
            Some(e) => {
                if e.evaluate(d).is_some() {
                    self.count += 1;
                }
            }
            None => self.count += 1,
        }
    }

    fn result<'a>(&'a mut self) -> Option<Any<'a>> {
        let s = Some(Any::USize(self.count));
        self.count = 0;
        s
    }

    fn composable(&self, path: &Path) -> (Vec<Column>, Box<dyn Aggregation>) {
        let composite = Column::new(Box::new(self.clone()), path.clone());
        let combine = SumAggregation::new(Box::new(PathExpression::from(path.clone())));
        (vec![composite], Box::new(combine))
    }
}
