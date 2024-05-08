mod literal;
mod math;
mod path;

use dyn_clone::DynClone;
use std::fmt::Display;

pub use literal::*;
pub use math::*;
pub use path::*;

use crate::{Any, Dapt};

// Expression is a trait that takes in a dapt packet and returns an
// optional value. This value can be Any type, which is what a dapt packet
// can return.
pub trait Expression: Display + DynClone {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b Dapt) -> Option<Any<'a>>;
}
dyn_clone::clone_trait_object!(Expression);
