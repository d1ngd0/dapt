use std::fmt::Display;

use crate::{
    query::{
        parser::{Column, Parser, FN_CLOSE, FN_OPEN},
        QueryResult,
    },
    Dapt, Number, Path,
};

use super::Aggregation;

pub const AGGREGATION_CUMULATIVE_SUM: &str = "CUMULATIVE_SUM";

#[derive(Clone)]
pub struct CumulativeSum {
    sum: Option<Number>,
    agg: Box<dyn Aggregation>,
}

impl CumulativeSum {
    pub fn new(agg: Box<dyn Aggregation>) -> Self {
        Self { sum: None, agg }
    }

    pub fn from_parser(parser: &mut Parser) -> QueryResult<Self> {
        parser.consume_token(AGGREGATION_CUMULATIVE_SUM)?;
        parser.consume_token(FN_OPEN)?;
        let agg = parser.parse_aggregation()?;
        parser.consume_token(FN_CLOSE)?;

        Ok(CumulativeSum { sum: None, agg })
    }
}

impl Display for CumulativeSum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", AGGREGATION_CUMULATIVE_SUM, self.agg)
    }
}

impl Aggregation for CumulativeSum {
    fn process<'a>(&'a mut self, d: &Dapt) {
        self.agg.process(d);
    }

    fn result<'a>(&'a mut self) -> Option<crate::Any<'a>> {
        let result: Number = self.agg.result()?.try_into().ok()?;
        self.sum = match self.sum {
            Some(s) => Some(s + result),
            None => Some(Number::ISize(0) + result),
        };
        Some(self.sum?.into())
    }

    // As CumulativeSum takes an aggregation, we take the childs composite columns
    // and do the cumulative sum on the combination.
    fn composable(&self, expr: &Path) -> (Vec<Column>, Box<dyn Aggregation>) {
        let (composable, combine) = self.agg.composable(expr);
        let combine = Box::new(CumulativeSum::new(combine));

        (composable, combine)
    }
}
