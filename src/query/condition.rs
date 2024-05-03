use crate::{Any, Dapt, Number};

use super::{expression::Expression, Error, QueryResult};

// Condition is a trait that defines a where clause condition, such as
// `age = 10` or `name != "John"` though higher level objects implement
// this trait as well.
pub trait Condition {
    fn evaluate(&self, d: &Dapt) -> QueryResult<bool>;
}

// NoopCondition is a condition that always returns true
pub struct NoopCondition {}

impl Condition for NoopCondition {
    fn evaluate(&self, d: &Dapt) -> QueryResult<bool> {
        Ok(true)
    }
}

impl Default for NoopCondition {
    fn default() -> Self {
        Self {}
    }
}

// if we have a naked expression which needs to be a
// condition we can wrap it in this, these are the default
// rules for what is truthy and what is falsey
pub struct DefaultExpressCondition {
    expr: Box<dyn Expression>,
}

impl DefaultExpressCondition {
    pub fn new(expr: Box<dyn Expression>) -> Self {
        Self { expr }
    }
}

impl Condition for DefaultExpressCondition {
    fn evaluate(&self, d: &Dapt) -> QueryResult<bool> {
        Ok(match self.expr.evaluate(d) {
            Some(Any::Null) => false,
            Some(Any::Bool(b)) => b,
            Some(Any::USize(u)) => u != 0,
            Some(Any::ISize(u)) => u != 0,
            Some(Any::U128(u)) => u != 0,
            Some(Any::I128(i)) => i != 0,
            Some(Any::U64(u)) => u != 0,
            Some(Any::I64(i)) => i != 0,
            Some(Any::U32(u)) => u != 0,
            Some(Any::I32(i)) => i != 0,
            Some(Any::U16(u)) => u != 0,
            Some(Any::I16(i)) => i != 0,
            Some(Any::U8(u)) => u != 0,
            Some(Any::I8(i)) => i != 0,
            Some(Any::F64(f)) => f != 0.0,
            Some(Any::F32(f)) => f != 0.0,
            Some(Any::Str(s)) => !s.is_empty(),
            Some(Any::Array(a)) => !a.is_empty(),
            Some(Any::Map(m)) => !m.is_empty(),
            Some(_) => true,
            None => false,
        })
    }
}

// ExistsCondition is a condition that checks if an expression
// returns a value that exists.
pub struct ExistsCondition {
    expr: Box<dyn Expression>,
}

impl Condition for ExistsCondition {
    fn evaluate(&self, d: &Dapt) -> QueryResult<bool> {
        Ok(match self.expr.evaluate(d) {
            Some(_) => true,
            None => false,
        })
    }
}

// InCondition will check if the left expression is in the right expression. If the
// right expression is not a map or array this functions the same as EqualsCondition
pub struct InCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
}

impl InCondition {
    pub fn new(left: Box<dyn Expression>, right: Box<dyn Expression>) -> Self {
        Self { left, right }
    }
}

impl Condition for InCondition {
    fn evaluate(&self, d: &Dapt) -> QueryResult<bool> {
        let left = self.left.evaluate(d);
        let right = self.right.evaluate(d);

        match (left, right) {
            (Some(l), Some(r)) => match r {
                Any::Array(arr) => Ok(arr.contains(&l)),
                Any::Map(map) => Ok({
                    let mut found = false;
                    for v in map.values() {
                        if *v == l {
                            found = true;
                            break;
                        }
                    }
                    found
                }),
                _ => Ok(l == r),
            },
            (Some(_), None) => Ok(false),
            (None, Some(_)) => Ok(false),
            (None, None) => Err(Error::NonExistentKey(format!(
                "both keys {} == {} do not exist",
                self.left, self.right
            ))),
        }
    }
}

pub struct EqualsCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
}

impl EqualsCondition {
    pub fn new(left: Box<dyn Expression>, right: Box<dyn Expression>) -> Self {
        Self { left, right }
    }
}

impl Condition for EqualsCondition {
    fn evaluate(&self, d: &Dapt) -> QueryResult<bool> {
        match (self.left.evaluate(d), self.right.evaluate(d)) {
            (Some(l), Some(r)) => Ok(l == r),
            // if only one side doesn't exist we can assume they don't match
            (Some(_), None) => Ok(false),
            (None, Some(_)) => Ok(false),
            // if both sides don't exist this might be an error, so we should not
            // capture this data
            (None, None) => Err(Error::NonExistentKey(format!(
                "both keys {} == {} do not exist",
                self.left, self.right
            ))),
        }
    }
}

pub struct NotEqualsCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
}

impl NotEqualsCondition {
    pub fn new(left: Box<dyn Expression>, right: Box<dyn Expression>) -> Self {
        Self { left, right }
    }
}

impl Condition for NotEqualsCondition {
    fn evaluate(&self, d: &Dapt) -> QueryResult<bool> {
        match (self.left.evaluate(d), self.right.evaluate(d)) {
            (Some(l), Some(r)) => Ok(l != r),
            (Some(_), None) => Ok(true),
            (None, Some(_)) => Ok(true),
            (None, None) => Err(Error::NonExistentKey(format!(
                "both keys {} == {} do not exist",
                self.left, self.right
            ))),
        }
    }
}

pub struct GreaterThanCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
}

impl GreaterThanCondition {
    pub fn new(left: Box<dyn Expression>, right: Box<dyn Expression>) -> Self {
        Self { left, right }
    }
}

// TODO: maybe we should have conditions return a result
// so we can handle the does not exist condition better.
impl Condition for GreaterThanCondition {
    fn evaluate(&self, d: &Dapt) -> QueryResult<bool> {
        let left = Number::try_from(self.left.evaluate(d).ok_or_else(|| {
            Error::NonExistentKey(format!("expr {} returned no value", self.left))
        })?)?;

        let right = Number::try_from(self.right.evaluate(d).ok_or_else(|| {
            Error::NonExistentKey(format!("expr {} returned no value", self.right))
        })?)?;

        Ok(left > right)
    }
}

pub struct LessThanCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
}

impl LessThanCondition {
    pub fn new(left: Box<dyn Expression>, right: Box<dyn Expression>) -> Self {
        Self { left, right }
    }
}

impl Condition for LessThanCondition {
    fn evaluate(&self, d: &Dapt) -> QueryResult<bool> {
        let left = Number::try_from(self.left.evaluate(d).ok_or_else(|| {
            Error::NonExistentKey(format!("expr {} returned no value", self.left))
        })?)?;

        let right = Number::try_from(self.right.evaluate(d).ok_or_else(|| {
            Error::NonExistentKey(format!("expr {} returned no value", self.right))
        })?)?;

        Ok(left < right)
    }
}

pub struct GreaterThanEqualCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
}

impl GreaterThanEqualCondition {
    pub fn new(left: Box<dyn Expression>, right: Box<dyn Expression>) -> Self {
        Self { left, right }
    }
}

impl Condition for GreaterThanEqualCondition {
    fn evaluate(&self, d: &Dapt) -> QueryResult<bool> {
        let left = Number::try_from(self.left.evaluate(d).ok_or_else(|| {
            Error::NonExistentKey(format!("expr {} returned no value", self.left))
        })?)?;

        let right = Number::try_from(self.right.evaluate(d).ok_or_else(|| {
            Error::NonExistentKey(format!("expr {} returned no value", self.right))
        })?)?;

        Ok(left >= right)
    }
}

pub struct LessThanEqualCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
}

impl LessThanEqualCondition {
    pub fn new(left: Box<dyn Expression>, right: Box<dyn Expression>) -> Self {
        Self { left, right }
    }
}

impl Condition for LessThanEqualCondition {
    fn evaluate(&self, d: &Dapt) -> QueryResult<bool> {
        let left = Number::try_from(self.left.evaluate(d).ok_or_else(|| {
            Error::NonExistentKey(format!("expr {} returned no value", self.left))
        })?)?;

        let right = Number::try_from(self.right.evaluate(d).ok_or_else(|| {
            Error::NonExistentKey(format!("expr {} returned no value", self.right))
        })?)?;

        Ok(left <= right)
    }
}
