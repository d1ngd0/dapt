use std::fmt::Display;

use crate::{
    query::{
        expression::{Expression, PathExpression},
        parser::{Column, Parser, AGGREGATION_AVG, FN_CLOSE, FN_OPEN},
        QueryResult,
    },
    Any, Dapt, Number, Path,
};

use super::{math::DivideAggregation, Aggregation, CountAggregation, SumAggregation};

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
    fn process<'a>(&'a mut self, d: &Dapt) {
        let value = match self.expr.evaluate(d) {
            Some(v) => v,
            None => return,
        };

        let value: f64 = match Number::try_from(value) {
            Ok(v) => v.into(),
            Err(_) => return,
        };

        self.sum_count = match self.sum_count {
            Some((sum, count)) => Some((sum + value, count + 1)),
            None => Some((value, 1)),
        };
    }

    fn result<'a>(&'a self) -> Option<Any<'a>> {
        let (sum, count) = self.sum_count?;
        Some(Any::F64(sum / count as f64))
    }

    fn composable(&self, path: &Path) -> (Vec<Column>, Box<dyn Aggregation>) {
        // create a composable sum
        let mut sum_path = path.clone();
        sum_path.append_key("sum");
        let composite_sum = Column::new(
            Box::new(SumAggregation::new(self.expr.clone())),
            sum_path.clone(),
        );

        // create a composable count
        let mut count_path = path.clone();
        count_path.append_key("count");
        let composite_count = Column::new(
            Box::new(CountAggregation::new(Some(self.expr.clone()))),
            count_path.clone(),
        );

        // to combine we will take the sum of the composable sum and the
        // sum of the composable count, then divide across them creating the
        // final answer
        let combine = Box::new(DivideAggregation::new(
            Box::new(SumAggregation::new(Box::new(PathExpression::from(
                sum_path,
            )))),
            Box::new(SumAggregation::new(Box::new(PathExpression::from(
                count_path,
            )))),
        ));

        (vec![composite_sum, composite_count], combine)
    }
}
