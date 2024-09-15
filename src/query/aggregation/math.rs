use std::fmt::Display;

use crate::{
    query::parser::{EXPONENT, SUB_EXPR_CLOSE, SUB_EXPR_OPEN},
    Any, Dapt, Number, Path,
};

use super::Aggregation;

macro_rules! math_aggregation {
    ($name:ident, $op:tt) => {
        #[derive(Clone)]
        pub struct $name {
            left: Box<dyn Aggregation>,
            right: Box<dyn Aggregation>,
        }

        impl $name {
            pub fn new(left: Box<dyn Aggregation>, right: Box<dyn Aggregation>) -> Self {
                Self { left, right }
            }
        }

        impl Aggregation for $name {
            fn process<'a>(&'a mut self, d: &Dapt) {
                self.left.process(d);
                self.right.process(d);
            }

            fn result<'a>(&'a mut self) -> Option<Any<'a>> {
                let left: f64 = Number::try_from(self.left.result()?).ok()?.into();
                let right: f64 = Number::try_from(self.right.result()?).ok()?.into();

                Some(Any::F64(left $op right))
            }

            fn composable(
                &self,
                path: &Path,
            ) -> (Vec<crate::query::parser::Column>, Box<dyn Aggregation>) {
                // create the left handed side
                let mut left_path = path.clone();
                left_path.append_key("left"); // can't return error
                let (left_composite, left_combine) = self.left.composable(&left_path);

                // create the right handed side
                let mut right_path = path.clone();
                right_path.append_key("right"); // can't return error
                let (right_composite, right_combine) = self.right.composable(&right_path);

                // create the combination
                let combine = Box::new($name::new(left_combine, right_combine));
                (vec![left_composite, right_composite].concat(), combine)
            }
        }

        impl Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    f,
                    "{}{}{}",
                    self.left, stringify!($op), self.right
                )
            }
        }
    };
}

math_aggregation!(ModulusAggregation, %);
math_aggregation!(DivideAggregation, /);
math_aggregation!(MultiplyAggregation, *);
math_aggregation!(AddAggregation, +);
math_aggregation!(SubtractAggregation, -);

#[derive(Clone)]
pub struct ExponentAggregation {
    left: Box<dyn Aggregation>,
    right: Box<dyn Aggregation>,
}

impl ExponentAggregation {
    pub fn new(left: Box<dyn Aggregation>, right: Box<dyn Aggregation>) -> Self {
        Self { left, right }
    }
}

impl Aggregation for ExponentAggregation {
    fn process<'a>(&'a mut self, d: &Dapt) {
        self.left.process(d);
        self.right.process(d);
    }

    fn result<'a>(&'a mut self) -> Option<Any<'a>> {
        let left = Number::try_from(self.left.result()?).ok()?;
        let right = Number::try_from(self.right.result()?).ok()?;

        let left: i64 = left.into();
        let right: u32 = right.into();

        Some(Any::I64(left.pow(right)))
    }

    fn composable(&self, path: &Path) -> (Vec<crate::query::parser::Column>, Box<dyn Aggregation>) {
        // create the left handed side
        let mut left_path = path.clone();
        left_path.append_key("left"); // can't return error
        let (left_composite, left_combine) = self.left.composable(&left_path);

        // create the right handed side
        let mut right_path = path.clone();
        right_path.append_key("right"); // can't return error
        let (right_composite, right_combine) = self.right.composable(&right_path);

        // create the combination
        let combine = Box::new(Self::new(left_combine, right_combine));
        (vec![left_composite, right_composite].concat(), combine)
    }
}

impl Display for ExponentAggregation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}{}", self.left, EXPONENT, self.right)
    }
}

#[derive(Clone)]
pub struct SubAggregation {
    expr: Box<dyn Aggregation>,
}

impl SubAggregation {
    pub fn new(expr: Box<dyn Aggregation>) -> Self {
        Self { expr }
    }
}

impl Aggregation for SubAggregation {
    fn process<'a>(&'a mut self, d: &Dapt) {
        self.expr.process(d)
    }

    fn result<'a>(&'a mut self) -> Option<Any<'a>> {
        self.expr.result()
    }

    fn composable(&self, path: &Path) -> (Vec<crate::query::parser::Column>, Box<dyn Aggregation>) {
        let (composite, combine) = self.expr.composable(path);
        (composite, Box::new(Self::new(combine)))
    }
}

impl Display for SubAggregation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{SUB_EXPR_OPEN}{}{SUB_EXPR_CLOSE}", self.expr)
    }
}
