use std::{fmt::Display, ops::Deref};

use crate::{
    query::{
        expression::Expression,
        parser::{Column, Parser, AGGREGATION_AVG, AGGREGATION_SUM_COUNT, FN_CLOSE, FN_OPEN},
        Error, QueryResult,
    },
    Any, Dapt, Number, Path,
};

use super::Aggregation;

#[derive(Clone)]
pub struct AvgAggregation {
    expr: Box<dyn Expression>,
    sum_count: Option<(f64, usize)>,
}

impl AvgAggregation {
    pub fn from_parser(parser: &mut Parser) -> QueryResult<AvgAggregation> {
        parser.consume_token(AGGREGATION_AVG)?;
        parser.consume_token(FN_OPEN)?;
        let expr = parser.parse_expression()?;
        parser.consume_token(FN_CLOSE)?;

        Ok(AvgAggregation {
            expr,
            sum_count: None,
        })
    }
}

impl Display for AvgAggregation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{AGGREGATION_AVG}({})", self.expr)
    }
}

impl Aggregation for AvgAggregation {
    fn process<'a>(&'a mut self, d: &Dapt) -> QueryResult<()> {
        let value = match self.expr.evaluate(d) {
            Some(v) => v,
            None => return Ok(()),
        };

        let value: f64 = match Number::try_from(value) {
            Ok(v) => v.into(),
            Err(_) => return Ok(()),
        };

        self.sum_count = match self.sum_count {
            Some((sum, count)) => Some((sum + value, count + 1)),
            None => Some((value, 1)),
        };
        Ok(())
    }

    fn result<'a>(&'a self) -> QueryResult<Any<'a>> {
        if self.sum_count.is_none() {
            return Err(Error::NotFound);
        }

        let (sum, count) = self.sum_count.unwrap();
        Ok(Any::F64(sum / count as f64))
    }

    fn composable(&self, path: &Path) -> (Vec<Column>, Box<dyn Aggregation>) {
        todo!()
    }
}
