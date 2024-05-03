use std::fmt::Display;

use dyn_clone::DynClone;

use crate::{binary::OwnedAny, query::parser::Parser, Any, Dapt, Number};

use super::{
    expression::Expression,
    parser::{AGGREGATION_COUNT, AGGREGATION_SUM, FN_CLOSE, FN_OPEN},
    Error, QueryResult,
};

// an aggregation taks multiple dapt packets through process
// and returns the defined aggregation result as an Any type.
pub trait Aggregation: std::fmt::Display + DynClone {
    // process can be called multiple times
    fn process<'a>(&'a mut self, d: &Dapt) -> QueryResult<()>;

    // result returns the aggregation result, were applicable, we should
    // return NotFound, which is handled by select by not adding the column
    // to the final result.
    fn result<'a>(&'a self) -> QueryResult<Any<'a>>;
}
dyn_clone::clone_trait_object!(Aggregation);

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
            sum: Number::USize(0),
        })
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
}

impl Display for SumAggregation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{AGGREGATION_SUM}({})", self.value)
    }
}

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
}

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
