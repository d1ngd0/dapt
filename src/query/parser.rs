use crate::{Any, Dapt, Path};

use super::{
    error::{Error, QueryResult},
    lexor::Lexer,
};

const SELECT: &str = "SELECT";
const FROM: &str = "FROM";
const WHERE: &str = "WHERE";
const HAVING: &str = "HAVING";
const GROUP: &str = "GROUP";
const SUB_CONDITION: &str = "(";
const SUB_CONDITION_END: &str = ")";
const EQUAL: &str = "=";
const EQUAL_DOUBLE: &str = "==";
const NOT_EQUAL: &str = "!=";
const AND: &str = "AND";
const OR: &str = "OR";
const KEY_WRAP: &str = "\"";
const STRING_WRAP: &str = "'";

struct Parser<'a> {
    lex: Lexer<'a>,
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(s: &'a str) -> Parser<'a> {
        Parser {
            lex: Lexer::from(s),
        }
    }
}

// Condition is a trait that defines a where clause condition, such as
// `age = 10` or `name != "John"` though higher level objects implement
// this trait as well.
trait Condition {
    fn evaluate(&self, d: &Dapt) -> bool;
}

// Expression is a trait that takes in a dapt packet and returns an
// optional value. This value can be Any type, which is what a dapt packet
// can return.
trait Expression {
    fn evaluate<'a>(&self, d: &'a Dapt) -> Option<Any<'a>>;
}

// Conjunctions are used to combine conditions
enum Conjunction {
    Single(Box<dyn Condition>),
    And {
        left: Box<dyn Condition>,
        right: Box<dyn Condition>,
    },
    Or {
        left: Box<dyn Condition>,
        right: Box<dyn Condition>,
    },
}

impl Conjunction {
    // promote_and consumes the existing conjunction and places it in
    // the left side of a new AND conjunction.
    fn promote_and(self, right: Box<dyn Condition>) -> Conjunction {
        Conjunction::And {
            left: Box::new(self),
            right,
        }
    }

    // promote_and consumes the existing conjunction and places it in
    // the left side of a new OR conjunction.
    fn promote_or(self, right: Box<dyn Condition>) -> Conjunction {
        Conjunction::Or {
            left: Box::new(self),
            right,
        }
    }
}

impl Condition for Conjunction {
    fn evaluate(&self, d: &Dapt) -> bool {
        match self {
            Conjunction::Single(c) => c.evaluate(d),
            Conjunction::And { left, right } => left.evaluate(d) && right.evaluate(d),
            Conjunction::Or { left, right } => left.evaluate(d) || right.evaluate(d),
        }
    }
}

struct WhereClause {
    condition: Conjunction,
}

impl<'a> Parser<'a> {
    pub fn parse_where(&mut self) -> QueryResult<WhereClause> {
        let tok = self
            .lex
            .token()
            .ok_or_else(|| Error::unexpected_eof(&self.lex))?;

        match tok {
            WHERE => Ok(WhereClause {
                condition: self.parse_conjunction()?,
            }),
            _ => Err(Error::with_history("expected WHERE", &self.lex)),
        }
    }

    // parse_conjunction is a recursive descent parser that parses a conjunction
    // of conditions. It is a simple parser that only supports AND and OR
    // conjunctions.
    pub fn parse_conjunction(&mut self) -> QueryResult<Conjunction> {
        // start by parsing the first condition and placing it in a single conjuction
        // so assume the condition is a == b
        let mut conj = Conjunction::Single(self.parse_condition()?);

        // then we loop to find a chain of AND OR conjunctions
        loop {
            // peak so we don't consume the token
            let tok = match self.lex.peak() {
                Some(t) => t,
                None => break,
            };

            // If we are in an AND or OR we can consume that token, and then
            // parse the right side. Once we do that successfully we can promote
            // whatever conjuction we have to the left.
            match tok {
                AND => {
                    let _ = self.lex.token(); // consume the AND token
                    let right = self.parse_condition()?;
                    conj = conj.promote_and(right);
                }
                OR => {
                    let _ = self.lex.token(); // consume the OR token
                    let right = self.parse_condition()?;
                    conj = conj.promote_or(right);
                }
                // if we get something else we just break... this allows us
                // to validate the exit token outside the context of parsing
                // a conjunction
                _ => break,
            }
        }

        Ok(conj)
    }

    // parse_condition will parse a single condition in the where clause, like
    // "a" == 10 This also supports Sub conditions like (a == 10 AND b == 20) by
    // calling up to parse_conjunction
    pub fn parse_condition(&mut self) -> QueryResult<Box<dyn Condition>> {
        // check for EOF or a subcondition
        match self.lex.peak() {
            None => return Err(Error::unexpected_eof(&self.lex)),
            Some(SUB_CONDITION) => {
                let _ = self.lex.token(); // consume the (
                let condition = self.parse_conjunction()?;

                match self.lex.token() {
                    None => return Err(Error::unexpected_eof(&self.lex)),
                    Some(SUB_CONDITION_END) => (),
                    _ => return Err(Error::with_history("expected )", &self.lex)),
                }

                return Ok(Box::new(condition));
            }
            _ => (),
        };

        let left = self.parse_expression()?;

        // check for EOF (which could be expected this time) or an AND or OR
        match self.lex.peak() {
            None | Some(OR) | Some(AND) => {
                return Ok(Box::new(DefaultExpressCondition { expr: left }))
            }
            _ => (),
        };

        let tok = self.lex.token().unwrap();
        match tok {
            EQUAL | EQUAL_DOUBLE => {
                let right = match self.parse_expression() {
                    Err(Error::UnexpectedEOF(_)) => {
                        return Err(Error::with_history(
                            "equals expects expressions on both sides",
                            &self.lex,
                        ))
                    }
                    Err(e) => return Err(e),
                    Ok(r) => r,
                };

                Ok(Box::new(EqualsCondition { left, right }))
            }
            NOT_EQUAL => {
                let right = match self.parse_expression() {
                    Err(Error::UnexpectedEOF(_)) => {
                        return Err(Error::with_history(
                            "equals expects expressions on both sides",
                            &self.lex,
                        ))
                    }
                    Err(e) => return Err(e),
                    Ok(r) => r,
                };

                Ok(Box::new(NotEqualsCondition { left, right }))
            }
            other => Err(Error::with_history(
                &format!("expected comparison operator, AND or OR got \"{}\"", other),
                &self.lex,
            )),
        }
    }

    pub fn parse_expression(&mut self) -> QueryResult<Box<dyn Expression>> {
        let left = self
            .lex
            .token()
            .ok_or_else(|| Error::unexpected_eof(&self.lex))?;

        match left {
            KEY_WRAP => {
                let key = self
                    .lex
                    .token()
                    .ok_or_else(|| Error::unexpected_eof(&self.lex))?;

                let path = Path::try_from(key)
                    .map_err(|e| Error::with_history(&e.to_string(), &self.lex))?;

                // consume the final " token, and return. If we get a different token
                // or hit EOF we can return an error
                match self.lex.token() {
                    Some(KEY_WRAP) => Ok(Box::new(path)),
                    Some(tok) => Err(Error::with_history(
                        &format!("expected {KEY_WRAP} but got {tok}"),
                        &self.lex,
                    )),
                    None => Err(Error::unexpected_eof(&self.lex)),
                }
            }
            _ => Err(Error::with_history("expected key", &self.lex)),
        }
    }

    // pub fn parse_query(&mut self) -> Option<Node> {
    //     let tok = self.lex.token()?;

    //     match tok {
    //         SELECT => Some(Node::Select),
    //     }
    // }

    // pub fn parse_select(&mut self) -> Option<()> {
    //     let tok = self.lex.token()?;

    //     match tok {
    //         SELECT => todo!(),
    //         _ => None,
    //     }
    // }
}

// if we have a naked expression which needs to be a
// condition we can wrap it in this, these are the default
// rules for what is truthy and what is falsey
struct DefaultExpressCondition {
    expr: Box<dyn Expression>,
}

impl Condition for DefaultExpressCondition {
    fn evaluate(&self, d: &Dapt) -> bool {
        match self.expr.evaluate(d) {
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
        }
    }
}

// ExistsCondition is a condition that checks if an expression
// returns a value that exists.
struct ExistsCondition {
    expr: Box<dyn Expression>,
}

impl Condition for ExistsCondition {
    fn evaluate(&self, d: &Dapt) -> bool {
        match self.expr.evaluate(d) {
            Some(_) => true,
            None => false,
        }
    }
}

struct EqualsCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
}

impl Condition for EqualsCondition {
    fn evaluate(&self, d: &Dapt) -> bool {
        match (self.left.evaluate(d), self.right.evaluate(d)) {
            (Some(l), Some(r)) => l == r,
            _ => false,
        }
    }
}

struct NotEqualsCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
}

impl Condition for NotEqualsCondition {
    fn evaluate(&self, d: &Dapt) -> bool {
        match (self.left.evaluate(d), self.right.evaluate(d)) {
            (Some(l), Some(r)) => l != r,
            _ => false,
        }
    }
}

impl Expression for Path {
    fn evaluate<'a>(&self, d: &'a Dapt) -> Option<Any<'a>> {
        d.any_path(self).ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    }

    macro_rules! assert_condition {
        ( $source:expr, $expr:expr, $expected:expr) => {
            let mut parser = Parser::from($expr);
            let expr = parser.parse_condition().unwrap();
            let d: Dapt = serde_json::from_str($source).unwrap();
            let result = expr.evaluate(&d);
            assert_eq!(result, $expected);
        };
    }

    #[test]
    fn test_condition() {
        assert_condition!(r#"{"a": 10, "b": 9}"#, r#" "a" == "b" "#, false);
        assert_condition!(r#"{"a": 10, "b": "10"}"#, r#" "a" == "b" "#, true);
    }

    macro_rules! assert_conjunction {
        ( $source:expr, $expr:expr, $expected:expr) => {
            let mut parser = Parser::from($expr);
            let expr = parser.parse_conjunction().unwrap();
            let d: Dapt = serde_json::from_str($source).unwrap();
            let result = expr.evaluate(&d);
            assert_eq!(result, $expected);
        };
    }

    #[test]
    fn test_conjunction() {
        assert_conjunction!(
            r#"{"a": 10, "b": 9, "c": 10.0}"#,
            r#" "a" != "b" AND "a" == "c" "#,
            true
        );

        assert_conjunction!(
            r#"{"a": 10, "b": 9, "c": 10.0}"#,
            r#" "a" == "b" AND "a" == "c" "#,
            false
        );
    }

    macro_rules! assert_where {
        ( $source:expr, $expr:expr, $expected:expr) => {
            let mut parser = Parser::from($expr);
            let expr = parser.parse_where().unwrap();
            let d: Dapt = serde_json::from_str($source).unwrap();
            let result = expr.condition.evaluate(&d);
            assert_eq!(result, $expected);
        };
    }

    #[test]
    fn test_where() {
        assert_where!(
            r#"{"a": 10, "b": 9, "c": 10.0}"#,
            r#"WHERE "a" != "b" AND "a" == "c" "#,
            true
        );

        assert_where!(
            r#"{"a": 10, "b": 9, "c": 10.0}"#,
            r#"WHERE "a" == "b" AND ("a" == "c" OR "nope" == "nothere") "#,
            false
        );
    }
}
