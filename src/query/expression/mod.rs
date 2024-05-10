mod literal;
mod math;
mod path;
mod strings;

use dyn_clone::DynClone;
use std::fmt::Display;

pub use literal::*;
pub use math::*;
pub use path::*;
pub use strings::*;

use crate::{Any, Dapt};

// Expression is a trait that takes in a dapt packet and returns an
// optional value. This value can be Any type, which is what a dapt packet
// can return.
pub trait Expression: Display + DynClone {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b Dapt) -> Option<Any<'a>>;
}
dyn_clone::clone_trait_object!(Expression);

#[cfg(test)]
mod test {
    use super::*;
    use crate::query::parser::Parser;

    macro_rules! assert_expression {
        ( $source:expr, $expr:expr, $expected:expr) => {
            let mut parser = Parser::from($expr);
            let expr = parser.parse_expression().unwrap();
            let d: Dapt = serde_json::from_str($source).unwrap();
            let result = expr.evaluate(&d).unwrap();
            assert_eq!(result, $expected);
        };
    }

    #[test]
    fn test_expression() {
        assert_expression!(r#"{"a": 10}"#, "\"a\"", Any::U64(10));
        assert_expression!(r#"{"a": 10}"#, "add(\"a\", 10)", Any::U64(20));
        assert_expression!(r#"{"a": 10}"#, "neg(\"a\", 10)", Any::U64(0));
        assert_expression!(r#"{"a": 10}"#, "mul(\"a\", 10)", Any::U64(100));
        assert_expression!(r#"{"a": 10}"#, "div(\"a\", 5)", Any::U64(2));
        assert_expression!(r#"{"a": 10}"#, "mod(\"a\", 4)", Any::USize(2));
        assert_expression!(r#"{"a": "HELLO"}"#, "lower(\"a\")", "hello");
        assert_expression!(r#"{"a": "hello"}"#, "upper(\"a\")", "HELLO");
        assert_expression!(r#"{"a": " hello "}"#, "trim(\"a\")", "hello");
        assert_expression!(r#"{"a": " hello "}"#, "trim_left(\"a\")", "hello ");
        assert_expression!(r#"{"a": " hello "}"#, "trim_right(\"a\")", " hello");
        assert_expression!(
            r#"{"a": "hello", "b": "world"}"#,
            "concat(\"a\", ' ', \"b\")",
            "hello world"
        );
        assert_expression!(
            r#"{"a": "hello world"}"#,
            "split(\"a\", ' ')",
            Any::Array(vec![Any::Str("hello"), Any::Str("world")])
        );
        assert_expression!(r#"{"a.b.c": "hello"}"#, r#"length("a\.b\.c")"#, 5);
        // assert_expression!(r#"{"a.b.c": "hello"}"#, r#"length("\"a.b.c\"")"#, 5);
    }
}
