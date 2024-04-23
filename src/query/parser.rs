use std::{collections::HashMap, fmt::Display, ops::Deref};

use crate::{Any, Dapt, Number, Path};

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
const GREATHER_THAN: &str = ">";
const LESS_THAN: &str = "<";
const GREATER_THAN_EQUAL: &str = ">=";
const LESS_THAN_EQUAL: &str = "<=";
const AND: &str = "AND";
const OR: &str = "OR";
const KEY_WRAP: &str = "\"";
const STRING_WRAP: &str = "'";
const NULL: &str = "NULL";
const TRUE: &str = "TRUE";
const FALSE: &str = "FALSE";
const MAP_WRAP: &str = "{";
const MAP_WRAP_END: &str = "}";
const MAP_CHILD_SET: &str = ":";
const MAP_CHILD_SEP: &str = ",";
const ARRAY_WRAP: &str = "[";
const ARRAY_WRAP_END: &str = "]";
const ARRAY_CHILD_SEP: &str = ",";

const FN_OPEN: &str = "(";
const FN_CLOSE: &str = ")";
const FN_SEP: &str = ",";

const FN_ADD: &str = "ADD";
const FN_MINUS: &str = "NEG";
const FN_MULTIPLY: &str = "MUL";
const FN_DIVIDE: &str = "DIV";
const FN_MODULUS: &str = "MOD";

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
    fn evaluate(&self, d: &Dapt) -> QueryResult<bool>;
}

// Expression is a trait that takes in a dapt packet and returns an
// optional value. This value can be Any type, which is what a dapt packet
// can return.
trait Expression: std::fmt::Display {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b Dapt) -> Option<Any<'a>>;
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
    fn evaluate(&self, d: &Dapt) -> QueryResult<bool> {
        match self {
            Conjunction::Single(c) => c.evaluate(d),
            Conjunction::And { left, right } => Ok(left.evaluate(d)? && right.evaluate(d)?),
            Conjunction::Or { left, right } => Ok(left.evaluate(d)? || right.evaluate(d)?),
        }
    }
}

pub struct WhereClause {
    condition: Conjunction,
}

impl WhereClause {
    pub fn new(str: &str) -> QueryResult<WhereClause> {
        let mut parser = Parser::from(str);
        parser.parse_where()
    }

    pub fn filter(&self, d: &Dapt) -> QueryResult<bool> {
        self.condition.evaluate(d)
    }
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
            GREATHER_THAN => {
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

                Ok(Box::new(GreaterThanCondition { left, right }))
            }
            GREATER_THAN_EQUAL => {
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

                Ok(Box::new(GreaterThanEqualCondition { left, right }))
            }
            LESS_THAN => {
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

                Ok(Box::new(LessThanCondition { left, right }))
            }
            LESS_THAN_EQUAL => {
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

                Ok(Box::new(LessThanEqualCondition { left, right }))
            }
            other => Err(Error::with_history(
                &format!("expected comparison operator, AND or OR got \"{}\"", other),
                &self.lex,
            )),
        }
    }

    fn parse_expression(&mut self) -> QueryResult<Box<dyn Expression>> {
        let left = self
            .lex
            .token()
            .ok_or_else(|| Error::unexpected_eof(&self.lex))?;

        match left.to_uppercase().as_str() {
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
            STRING_WRAP => {
                let value = self
                    .lex
                    .token()
                    .ok_or_else(|| Error::unexpected_eof(&self.lex))?;

                // consume the final " token, and return. If we get a different token
                // or hit EOF we can return an error
                match self.lex.token() {
                    Some(STRING_WRAP) => Ok(Box::new(StringExpression {
                        value: value.to_string(),
                    })),
                    Some(tok) => Err(Error::with_history(
                        &format!("expected {STRING_WRAP} but got {tok}"),
                        &self.lex,
                    )),
                    None => Err(Error::unexpected_eof(&self.lex)),
                }
            }
            MAP_WRAP => {
                let mut map = HashMap::new();
                loop {
                    // pase 'key': <expression>
                    let key = self.parse_string()?;
                    self.consume_token(MAP_CHILD_SET)?;
                    let value = self.parse_expression()?;

                    map.insert(key, value);

                    match self.lex.token() {
                        Some(MAP_WRAP_END) => break,
                        Some(MAP_CHILD_SEP) => continue,
                        Some(tok) => {
                            return Err(Error::with_history(
                                &format!(
                                    "expected {MAP_CHILD_SEP} or {MAP_WRAP_END} but got {tok}"
                                ),
                                &self.lex,
                            ))
                        }
                        None => return Err(Error::unexpected_eof(&self.lex)),
                    }
                }

                Ok(Box::new(MapLiteral(map)))
            }
            ARRAY_WRAP => {
                let mut arr = Vec::new();
                loop {
                    let value = self.parse_expression()?;
                    arr.push(value);

                    match self.lex.token() {
                        Some(ARRAY_WRAP_END) => break,
                        Some(ARRAY_CHILD_SEP) => continue,
                        Some(tok) => {
                            return Err(Error::with_history(
                                &format!("expected , or {ARRAY_WRAP_END} but got {tok}"),
                                &self.lex,
                            ))
                        }
                        None => return Err(Error::unexpected_eof(&self.lex)),
                    }
                }

                Ok(Box::new(ArrayLiteral(arr)))
            }
            FN_ADD => {
                self.consume_token(FN_OPEN)?;
                let left = self.parse_expression()?;
                self.consume_token(FN_SEP)?;
                let right = self.parse_expression()?;
                self.consume_token(FN_CLOSE)?;

                Ok(Box::new(AddExpression { left, right }))
            }
            FN_MINUS => {
                self.consume_token(FN_OPEN)?;
                let left = self.parse_expression()?;
                self.consume_token(FN_SEP)?;
                let right = self.parse_expression()?;
                self.consume_token(FN_CLOSE)?;

                Ok(Box::new(SubtractExpression { left, right }))
            }
            FN_MULTIPLY => {
                self.consume_token(FN_OPEN)?;
                let left = self.parse_expression()?;
                self.consume_token(FN_SEP)?;
                let right = self.parse_expression()?;
                self.consume_token(FN_CLOSE)?;

                Ok(Box::new(MultiplyExpression { left, right }))
            }
            FN_DIVIDE => {
                self.consume_token(FN_OPEN)?;
                let left = self.parse_expression()?;
                self.consume_token(FN_SEP)?;
                let right = self.parse_expression()?;
                self.consume_token(FN_CLOSE)?;

                Ok(Box::new(DivideExpression { left, right }))
            }
            FN_MODULUS => {
                self.consume_token(FN_OPEN)?;
                let left = self.parse_expression()?;
                self.consume_token(FN_SEP)?;
                let right = self.parse_expression()?;
                self.consume_token(FN_CLOSE)?;

                Ok(Box::new(ModulusExpression { left, right }))
            }
            TRUE => Ok(Box::new(BoolExpression { value: true })),
            FALSE => Ok(Box::new(BoolExpression { value: false })),
            NULL => Ok(Box::new(NullExpression)),
            _ => self.parse_unwrapped_expression(left),
        }
    }

    fn parse_unwrapped_expression(&self, left: &str) -> QueryResult<Box<dyn Expression>> {
        let mut chars = left.chars();
        match chars.next() {
            Some('0'..='9') => {
                if chars.filter(|c| *c == '.').count() == 1 {
                    let num = left.parse::<f64>().map_err(|e| {
                        Error::with_history(&format!("expected integer but got {}", e), &self.lex)
                    })?;

                    Ok(Box::new(Number::F64(num)))
                } else {
                    let num = left.parse::<usize>().map_err(|e| {
                        Error::with_history(&format!("expected integer but got {}", e), &self.lex)
                    })?;

                    Ok(Box::new(Number::USize(num)))
                }
            }
            Some('-') => {
                if chars.filter(|c| *c == '.').count() == 1 {
                    let num = left.parse::<f64>().map_err(|e| {
                        Error::with_history(&format!("expected integer but got {}", e), &self.lex)
                    })?;

                    Ok(Box::new(Number::F64(num)))
                } else {
                    let num = left.parse::<usize>().map_err(|e| {
                        Error::with_history(&format!("expected integer but got {}", e), &self.lex)
                    })?;

                    Ok(Box::new(Number::USize(num)))
                }
            }
            _ => Err(Error::with_history(
                &format!("unexpected token {}", left),
                &self.lex,
            )),
        }
    }

    fn parse_string(&mut self) -> QueryResult<String> {
        match self.lex.token() {
            Some(STRING_WRAP) => (),
            Some(tok) => {
                return Err(Error::with_history(
                    &format!("expected {STRING_WRAP} but got {tok}"),
                    &self.lex,
                ))
            }
            None => return Err(Error::unexpected_eof(&self.lex)),
        }

        let value = self
            .lex
            .token()
            .ok_or_else(|| Error::unexpected_eof(&self.lex))?;

        // consume the final " token, and return. If we get a different token
        // or hit EOF we can return an error
        match self.lex.token() {
            Some(STRING_WRAP) => Ok(value.to_string()),
            Some(tok) => Err(Error::with_history(
                &format!("expected {STRING_WRAP} but got {tok}"),
                &self.lex,
            )),
            None => Err(Error::unexpected_eof(&self.lex)),
        }
    }

    fn consume_token(&mut self, expected: &str) -> QueryResult<()> {
        match self.lex.token() {
            Some(tok) if tok == expected => Ok(()),
            Some(tok) => Err(Error::with_history(
                &format!("expected {} but got {}", expected, tok),
                &self.lex,
            )),
            None => Err(Error::unexpected_eof(&self.lex)),
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

macro_rules! impl_math_op {
    ($name:ident, $op:tt) => {
        struct $name {
            left: Box<dyn Expression>,
            right: Box<dyn Expression>,
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
struct ExistsCondition {
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

struct EqualsCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
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

struct NotEqualsCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
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

struct GreaterThanCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
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

struct LessThanCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
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

struct GreaterThanEqualCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
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

struct LessThanEqualCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
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

impl Expression for Path {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'a Dapt) -> Option<Any<'a>> {
        d.any_path(self).ok()
    }
}

struct StringExpression {
    value: String,
}

impl Expression for StringExpression {
    fn evaluate<'a, 'b: 'a>(&'a self, _: &'b Dapt) -> Option<Any<'a>> {
        Some(Any::Str(&self.value))
    }
}

impl Display for StringExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}'", self.value)
    }
}

struct NullExpression;

impl Expression for NullExpression {
    fn evaluate<'a, 'b: 'a>(&'a self, _: &'b Dapt) -> Option<Any<'a>> {
        Some(Any::Null)
    }
}

impl Display for NullExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NULL")
    }
}

struct BoolExpression {
    value: bool,
}

impl Expression for BoolExpression {
    fn evaluate<'a, 'b: 'a>(&'a self, _: &'b Dapt) -> Option<Any<'a>> {
        Some(Any::Bool(self.value))
    }
}

impl Display for BoolExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", if self.value { "TRUE" } else { "FALSE" })
    }
}

impl Expression for Number {
    fn evaluate<'a, 'b: 'a>(&'a self, _: &'b Dapt) -> Option<Any<'a>> {
        Some(Any::from(*self))
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::USize(u) => write!(f, "{}", u),
            Number::ISize(i) => write!(f, "{}", i),
            Number::U128(u) => write!(f, "{}", u),
            Number::I128(i) => write!(f, "{}", i),
            Number::U64(u) => write!(f, "{}", u),
            Number::I64(i) => write!(f, "{}", i),
            Number::U32(u) => write!(f, "{}", u),
            Number::I32(i) => write!(f, "{}", i),
            Number::U16(u) => write!(f, "{}", u),
            Number::I16(i) => write!(f, "{}", i),
            Number::U8(u) => write!(f, "{}", u),
            Number::I8(i) => write!(f, "{}", i),
            Number::F64(fl) => write!(f, "{}", fl),
            Number::F32(fl) => write!(f, "{}", fl),
        }
    }
}

struct MapLiteral(HashMap<String, Box<dyn Expression>>);

impl Deref for MapLiteral {
    type Target = HashMap<String, Box<dyn Expression>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Expression for MapLiteral {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b Dapt) -> Option<Any<'a>> {
        let mut map = HashMap::new();
        for (k, v) in self.iter() {
            if let Some(val) = v.evaluate(d) {
                map.insert(&k[..], val);
            }
        }

        Some(Any::Map(map))
    }
}

impl Display for MapLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (k, v) in self.iter() {
            write!(f, "'{}': {}, ", k, v)?;
        }
        write!(f, "}}")
    }
}

struct ArrayLiteral(Vec<Box<dyn Expression>>);

impl Deref for ArrayLiteral {
    type Target = Vec<Box<dyn Expression>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Expression for ArrayLiteral {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b Dapt) -> Option<Any<'a>> {
        let mut arr = Vec::new();
        for v in self.iter() {
            if let Some(val) = v.evaluate(d) {
                arr.push(val);
            }
        }

        Some(Any::Array(arr))
    }
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let mut first = true;
        for v in self.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }

            write!(f, "{}", v)?;
        }
        write!(f, "]")
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
        assert_expression!(r#"{"a": 10}"#, "add(\"a\", 10)", Any::U64(20));
        assert_expression!(r#"{"a": 10}"#, "neg(\"a\", 10)", Any::U64(0));
        assert_expression!(r#"{"a": 10}"#, "mul(\"a\", 10)", Any::U64(100));
        assert_expression!(r#"{"a": 10}"#, "div(\"a\", 5)", Any::U64(2));
        assert_expression!(r#"{"a": 10}"#, "mod(\"a\", 4)", Any::USize(2));
    }

    macro_rules! assert_condition {
        ( $source:expr, $expr:expr, $expected:expr) => {
            let mut parser = Parser::from($expr);
            let expr = parser.parse_condition().unwrap();
            let d: Dapt = serde_json::from_str($source).unwrap();
            let result = expr.evaluate(&d).unwrap();
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
            let result = expr.evaluate(&d).unwrap();
            assert_eq!(result, $expected);
        };
    }

    #[test]
    fn test_conjunction() {
        // test key equality
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

        // test string literal
        assert_conjunction!(
            r#"{"a": "hello world"}"#,
            r#" "a" == 'goodbye world' "#,
            false
        );

        assert_conjunction!(r#"{"a": "hello world"}"#, r#" "a" == 'hello world' "#, true);

        // test raw expression
        assert_conjunction!(r#" {"a": true} "#, r#" "a" "#, true);
        assert_conjunction!(r#" {"a": false} "#, r#" "a" "#, false);
        assert_conjunction!(r#" {"a": 0} "#, r#" "a" "#, false);
        assert_conjunction!(r#" {"a": 100} "#, r#" "a" "#, true);
        assert_conjunction!(r#" {"a": "hello"} "#, r#" "a" "#, true);
        assert_conjunction!(r#" {"a": ""} "#, r#" "a" "#, false);
        assert_conjunction!(r#" {"a": ["a", "b"]} "#, r#" "a" "#, true);
        assert_conjunction!(r#" {"a": []} "#, r#" "a" "#, false);
        assert_conjunction!(r#" {"a": null} "#, r#" "a" "#, false);
        assert_conjunction!(r#" {"a": {"a":1, "b":2}} "#, r#" "a" "#, true);
        assert_conjunction!(r#" {"a": {}} "#, r#" "a" "#, false);
        // this key doesn't exist
        assert_conjunction!(r#" {"a": {}} "#, r#" "no_existy" "#, false);

        // handling null
        assert_conjunction!(r#"{"a": null}"#, r#" "a" == NULL "#, true);
        assert_conjunction!(r#"{"a": null}"#, r#" "a" != NULL "#, false);
        // "a" has no value, null is a valid value... not sure what I think about
        // that. Currently you could ` "a" ` alone to test for it's existance
        assert_conjunction!(r#"{"b": "something"}"#, r#" "a" != NULL "#, true);

        // test bool
        assert_conjunction!(r#"{"a": true}"#, r#" "a" == true "#, true);
        assert_conjunction!(r#"{"a": false}"#, r#" "a" == false "#, true);
        assert_conjunction!(r#"{"a": true}"#, r#" "a" != true "#, false);
        assert_conjunction!(r#"{"a": false}"#, r#" "a" != false "#, false);

        // test numbers
        assert_conjunction!(r#"{"a": 10}"#, r#" "a" == 10 "#, true);
        assert_conjunction!(r#"{"a": 10}"#, r#" "a" != 10 "#, false);
        assert_conjunction!(r#"{"a": 10.98}"#, r#" "a" == 10.98 "#, true);

        // test map
        assert_conjunction!(
            r#"{"a": {"b": 10, "c": 20}}"#,
            r#" "a" == {'b': 10, 'c': 20} "#,
            true
        );

        assert_conjunction!(
            r#"{"a": {"b": {"c": 20}}}"#,
            r#" "a" == {'b': {'c': 20}} "#,
            true
        );

        assert_conjunction!(r#"{"a": [1,2,3]}"#, r#" "a" == [3,2,1] "#, true);
        assert_conjunction!(r#"{"a": [1,2,3,4]}"#, r#" "a" == [3,2,1] "#, false);

        // because you are selecting multiple things, the returned value is an array
        // so we can compare that to an array literal
        assert_conjunction!(
            r#"{"a": {"b": "hello", "c": "world"}}"#,
            r#" "a.*" == ['hello', 'world'] "#,
            true
        );

        // this is silly but is the same thing we are doing above, just using
        // keys directly. This is allowed because the value can be any expression
        // same with maps
        assert_conjunction!(
            r#"{"a": {"b": "hello", "c": "world"}}"#,
            r#" "a.*" == ["a.b", "a.c"] "#,
            true
        );

        // Number Operators
        assert_conjunction!(r#"{"a": 10}"#, r#" "a" > 9 "#, true);
        assert_conjunction!(r#"{"a": 10}"#, r#" "a" < 9 "#, false);
        assert_conjunction!(r#"{"a": 10}"#, r#" "a" < 100 "#, true);
        assert_conjunction!(r#"{"a": 10}"#, r#" "a" > 100 "#, false);
        assert_conjunction!(r#"{"a": 10}"#, r#" "a" >= 10 "#, true);
        assert_conjunction!(r#"{"a": 100}"#, r#" "a" >= 10 "#, true);
        assert_conjunction!(r#"{"a": 1}"#, r#" "a" >= 10 "#, false);
        assert_conjunction!(r#"{"a": 10}"#, r#" "a" <= 10 "#, true);
        assert_conjunction!(r#"{"a": 100}"#, r#" "a" <= 10 "#, false);
        assert_conjunction!(r#"{"a": 1}"#, r#" "a" <= 10 "#, true);
    }

    macro_rules! assert_where {
        ( $source:expr, $expr:expr, $expected:expr) => {
            let mut parser = Parser::from($expr);
            let expr = parser.parse_where().unwrap();
            let d: Dapt = serde_json::from_str($source).unwrap();
            let result = expr.condition.evaluate(&d).unwrap();
            assert_eq!(result, $expected);
        };
    }

    macro_rules! assert_where_error {
        ( $source:expr, $expr:expr, $expected:expr) => {
            let mut parser = Parser::from($expr);
            let expr = parser.parse_where().unwrap();
            let d: Dapt = serde_json::from_str($source).unwrap();
            match expr.condition.evaluate(&d) {
                Ok(_) => panic!("expected error"),
                Err(e) => assert_eq!(e.to_string(), $expected),
            }
        };
    }

    #[test]
    fn test_where() {
        assert_where!(
            r#"{
                "a": 10,
                "b": 9,
                "c": 10.0
            }"#,
            r#"WHERE "a" != "b" AND "a" == "c" "#,
            true
        );

        // wait shouldn't this be an error. Well, I guess not, because the OR is
        // never evaluated because the left side is true.
        assert_where!(
            r#"{
                "a": 10,
                "b": 9,
                "c": 10.0
            }"#,
            r#"WHERE "a" != "b" AND ("a" == "c" OR "nope" == "nothere") "#,
            true
        );

        assert_where_error!(
            r#"{
                "a": 10,
                "b": 9,
                "c": 10.0
            }"#,
            r#"WHERE "non_existant" == "other_nonexistant" "#,
            "Non existent key: both keys non_existant == other_nonexistant do not exist"
        );

        assert_where_error!(
            r#"{
                "a": 10,
                "b": 9,
                "c": 10.0
            }"#,
            r#"WHERE "a" != "b" AND ("nope" == "nothere" OR "a" == "c") "#,
            "Non existent key: both keys nope == nothere do not exist"
        );
    }
}
