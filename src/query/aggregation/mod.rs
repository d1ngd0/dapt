mod count;
mod expression;
mod sum;

use dyn_clone::DynClone;
use std::fmt::Display;

use crate::{Any, Dapt};

use super::{expression::Expression, QueryResult};

pub use count::CountAggregation;
pub use expression::ExpressionAggregation;
pub use sum::SumAggregation;

// an aggregation taks multiple dapt packets through process
// and returns the defined aggregation result as an Any type.
pub trait Aggregation: Display + DynClone {
    // process can be called multiple times
    fn process<'a>(&'a mut self, d: &Dapt) -> QueryResult<()>;

    // result returns the aggregation result, were applicable, we should
    // return NotFound, which is handled by select by not adding the column
    // to the final result.
    fn result<'a>(&'a self) -> QueryResult<Any<'a>>;

    // composable will break an aggregation into two seperate aggregations,
    // The first aggregation will be the composable one. Meaning it can be run
    // on multiple data sets concurnently. The second aggregation will be the
    // combining aggregations, which will combine the results of the first
    // into the expected result of the initial query.
    // The expression passed in is for the combining aggregation. Since aliasing will
    // be done higher up.
    fn composable(&self, expr: Box<dyn Expression>)
        -> (Box<dyn Aggregation>, Box<dyn Aggregation>);
}
dyn_clone::clone_trait_object!(Aggregation);
