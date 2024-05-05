mod count;
mod expression;
mod sum;

use dyn_clone::DynClone;
use std::fmt::Display;

use crate::{Any, Dapt};

use super::QueryResult;

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
}
dyn_clone::clone_trait_object!(Aggregation);
