mod avg;
mod count;
mod expression;
mod math;
mod sum;

use dyn_clone::DynClone;
use std::fmt::Display;

use crate::{Any, Dapt, Path};

use super::parser::Column;

pub use avg::AvgAggregation;
pub use count::CountAggregation;
pub use expression::ExpressionAggregation;
pub use math::{
    AddAggregation, DivideAggregation, ModulusAggregation, MultiplyAggregation, SubtractAggregation,
};
pub use sum::SumAggregation;

// an aggregation taks multiple dapt packets through process
// and returns the defined aggregation result as an Any type.
pub trait Aggregation: Display + DynClone {
    // process can be called multiple times
    fn process<'a>(&'a mut self, d: &Dapt);

    // result returns the aggregation result, were applicable, we should
    // return NotFound, which is handled by select by not adding the column
    // to the final result.
    fn result<'a>(&'a self) -> Option<Any<'a>>;

    // composable will break an aggregation into two seperate aggregations,
    // The first aggregation will be the composable one. Meaning it can be run
    // on multiple data sets concurnently. The second aggregation will be the
    // combining aggregations, which will combine the results of the first
    // into the expected result of the initial query.
    // The expression passed in is for the combining aggregation. Since aliasing will
    // be done higher up.
    fn composable(&self, expr: &Path) -> (Vec<Column>, Box<dyn Aggregation>);
}
dyn_clone::clone_trait_object!(Aggregation);
