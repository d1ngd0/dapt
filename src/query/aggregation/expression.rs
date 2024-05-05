use std::fmt::Display;

use crate::{binary::OwnedAny, query::parser::Parser, Any, Dapt};

use super::{
    super::{expression::Expression, Error, QueryResult},
    Aggregation,
};

#[derive(Clone)]
pub struct ExpressionAggregation {
    expr: Box<dyn Expression>,
    value: Option<OwnedAny>,
}

impl ExpressionAggregation {
    fn new(expr: Box<dyn Expression>) -> Self {
        Self { expr, value: None }
    }

    pub fn from_parser(parser: &mut Parser) -> QueryResult<ExpressionAggregation> {
        let expr = parser.parse_expression()?;
        Ok(ExpressionAggregation::new(expr))
    }
}

impl Aggregation for ExpressionAggregation {
    fn process<'a>(&'a mut self, d: &Dapt) -> QueryResult<()> {
        // just take the first value, that way we can avoid all the
        // heap allocations for each value we process.
        if self.value.is_some() {
            return Ok(());
        }

        self.value = match self.expr.evaluate(d) {
            // the value here will likely outlive the dapt packet
            // it came from, so we clone.
            Some(v) => Some(v.into()),
            None => return Ok(()),
        };

        Ok(())
    }

    fn result<'a>(&'a self) -> QueryResult<Any<'a>> {
        if self.value.is_none() {
            return Err(Error::NotFound);
        }

        Ok(self.value.as_ref().unwrap().into())
    }
}

impl Display for ExpressionAggregation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expr)
    }
}
