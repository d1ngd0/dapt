mod avg;
mod count;
mod cumulative_sum;
mod expression;
mod math;
mod max;
mod min;
mod sum;

use dyn_clone::DynClone;
use std::fmt::Display;

use crate::{Any, Dapt, Path};

use super::{expression::Expression, parser::Column, Error, QueryResult};

pub use avg::*;
pub use count::*;
pub use cumulative_sum::*;
pub use expression::*;
pub use math::*;
pub use max::*;
pub use min::*;
pub use sum::*;

// an aggregation taks multiple dapt packets through process
// and returns the defined aggregation result as an Any type.
pub trait Aggregation: Display + DynClone + Send + Sync {
    // process can be called multiple times
    fn process<'a>(&'a mut self, d: &Dapt);

    // result returns the aggregation result, were applicable, we should
    // return NotFound, which is handled by select by not adding the column
    // to the final result.
    fn result<'a>(&'a mut self) -> Option<Any<'a>>;

    // composable will break an aggregation into two seperate aggregations,
    // The first aggregation will be the composable one. Meaning it can be run
    // on multiple data sets concurnently. The second aggregation will be the
    // combining aggregations, which will combine the results of the first
    // into the expected result of the initial query.
    // The expression passed in is for the combining aggregation. Since aliasing will
    // be done higher up.
    fn composable(&self, expr: &Path) -> (Vec<Column>, Box<dyn Aggregation>);

    // expression is implemented to return the underlying expression of an
    // aggregation. This function is used during group bys to grab the
    // expression of an alias in the select clause.
    fn expression(&self) -> QueryResult<Box<dyn Expression>> {
        Err(Error::InvalidQuery("Can not group by aggregation".into()))
    }
}
dyn_clone::clone_trait_object!(Aggregation);

#[cfg(test)]
mod test {
    use super::*;
    use crate::query::parser::Parser;

    macro_rules! assert_aggregation {
        ( $expr:expr, $expected:expr, $($source:expr),+) => {
            let mut parser = Parser::from($expr);
            let mut expr = parser.parse_aggregation().unwrap();
            let sources = vec![$(serde_json::from_str($source).unwrap()),+];
            for d in sources {
                expr.process(&d);
            }
            let result = expr.result().unwrap();
            assert_eq!(result, $expected);
        };
    }

    #[test]
    fn test_aggregation() {
        assert_aggregation!(
            r#"SUM("a")"#,
            Any::USize(6),
            r#"{"a": 1}"#,
            r#"{"a": 2}"#,
            r#"{"a": 3}"#
        );

        assert_aggregation!(
            r#"count()"#,
            Any::USize(3),
            r#"{"a": 1}"#,
            r#"{"a": 2}"#,
            r#"{"a": 3}"#
        );

        assert_aggregation!(
            r#"count("c")"#,
            Any::USize(1),
            r#"{"a": 1}"#,
            r#"{"b": 2}"#,
            r#"{"c": 3}"#
        );

        assert_aggregation!(
            r#""a""#,
            Any::USize(1),
            r#"{"a": 1}"#,
            r#"{"a": 2}"#,
            r#"{"a": 3}"#
        );

        // literal, just to prove it can be done
        assert_aggregation!(
            r#"10"#,
            Any::USize(10),
            r#"{"a": 1}"#,
            r#"{"a": 2}"#,
            r#"{"a": 3}"#
        );

        // max
        assert_aggregation!(
            r#"MAX("a")"#,
            Any::USize(3),
            r#"{"a": 1}"#,
            r#"{"a": 2}"#,
            r#"{"a": 3}"#
        );

        // min
        assert_aggregation!(
            r#"MIN("a")"#,
            Any::USize(1),
            r#"{"a": 1}"#,
            r#"{"a": 2}"#,
            r#"{"a": 3}"#
        );
    }
}
