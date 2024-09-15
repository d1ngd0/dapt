use std::fmt::Display;

use crate::{Any, Dapt, Number};

use super::Expression;

// Math expressions
macro_rules! impl_math_op {
    ($name:ident, $op:tt) => {
        #[derive(Clone)]
        pub struct $name {
            left: Box<dyn Expression>,
            right: Box<dyn Expression>,
        }

        impl $name {
            pub fn new(left: Box<dyn Expression>, right: Box<dyn Expression>) -> Self {
                Self { left, right }
            }
        }

        impl Expression for $name {
            fn evaluate<'a, 'b: 'a>(&'a self, d: &'b Dapt) -> Option<Any<'a>> {
                let left = Number::try_from(self.left.evaluate(d)?).ok()?;
                let right = Number::try_from(self.right.evaluate(d)?).ok()?;

                Some(Any::from(left $op right))
            }
        }

        impl Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{} {} {}", self.left, stringify!($op), self.right)
            }
        }
    };
}

impl_math_op!(ModulusExpression, %);
impl_math_op!(DivideExpression, /);
impl_math_op!(MultiplyExpression, *);
impl_math_op!(AddExpression, +);
impl_math_op!(SubtractExpression, -);

#[derive(Clone)]
pub struct ExponentExpression {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
}

impl ExponentExpression {
    pub fn new(left: Box<dyn Expression>, right: Box<dyn Expression>) -> ExponentExpression {
        ExponentExpression { left, right }
    }
}

impl Expression for ExponentExpression {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b Dapt) -> Option<Any<'a>> {
        let left = Number::try_from(self.left.evaluate(d)?).ok()?;
        let right = Number::try_from(self.right.evaluate(d)?).ok()?;

        let left: i64 = left.into();
        let right: u32 = right.into();
        Some(Any::I64(left.pow(right)))
    }
}

impl Display for ExponentExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, stringify!(EXPONENT), self.right)
    }
}

#[derive(Clone)]
pub struct SubExpression {
    expr: Box<dyn Expression>,
}

impl SubExpression {
    pub fn new(expr: Box<dyn Expression>) -> Self {
        Self { expr }
    }
}

impl Expression for SubExpression {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b Dapt) -> Option<Any<'a>> {
        self.expr.evaluate(d)
    }
}

impl Display for SubExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.expr)
    }
}
