use std::fmt::Display;

use crate::{
    query::{
        expression::PathExpression,
        parser::{Column, Parser},
    },
    Any, Dapt, Path,
};

use super::{
    super::{expression::Expression, QueryResult},
    Aggregation,
};

#[derive(Clone)]
pub struct ExpressionAggregation {
    expr: Box<dyn Expression>,
    // value holds the last value seen since the last call to result
    // it gets reset each time, and is the freshest data
    //
    // this is static? no, it's not. We just have make sure not to use
    // actual reference values in here since we can force an any
    // into holding the value
    value: Option<Any<'static>>,
    // fill holds the last result value seen. If no value was seen it
    // keeps it's original value. If the aggregations has ever seen data
    // it will be here.
    fill: Option<Any<'static>>,
}

impl ExpressionAggregation {
    fn new(expr: Box<dyn Expression>) -> Self {
        Self {
            expr,
            value: None,
            fill: None,
        }
    }

    pub fn from_parser(parser: &mut Parser) -> QueryResult<ExpressionAggregation> {
        let expr = parser.expression()?;
        Ok(ExpressionAggregation::new(expr))
    }
}

impl From<Path> for ExpressionAggregation {
    fn from(path: Path) -> Self {
        Self::new(Box::new(PathExpression::new(path)))
    }
}

impl Aggregation for ExpressionAggregation {
    fn process<'a>(&'a mut self, d: &Dapt) {
        // just take the first value, that way we can avoid all the
        // heap allocations for each value we process.
        if self.value.is_some() {
            return;
        }

        self.value = match self.expr.evaluate(d) {
            // the value here will likely outlive the dapt packet
            // it came from, so we force_owned, which will make sure
            // we use no values with enums.
            Some(v) => Some(v.force_owned()),
            None => return,
        };
    }

    // result returns the expression result to the caller. When returning
    // it prefers to return the last value, but if no values were seen
    // previously it returns the last value seen ever.
    fn result<'a>(&'a mut self) -> Option<Any<'a>> {
        let v = self.value.take().or(self.fill.take())?;
        self.fill = Some(v.clone());
        Some(Any::from(v))
    }

    // since this is essentially first, we can just return the first value
    // we see when combining
    fn composable(&self, path: &Path) -> (Vec<Column>, Box<dyn Aggregation>) {
        let composable = Column::new(Box::new(self.clone()), path.clone());
        let combine = Box::new(ExpressionAggregation::from(path.clone()));
        (vec![composable], combine)
    }

    fn expression(&self) -> QueryResult<Box<dyn Expression>> {
        return Ok(self.expr.clone());
    }
}

impl Display for ExpressionAggregation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expr)
    }
}
