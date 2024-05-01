use std::{collections::HashMap, fmt::Display, ops::Deref};

use cityhash::city_hash_64;

use dyn_clone::DynClone;

use crate::{
    binary::OwnedAny,
    path::{node::FieldLiteral, parser::Node},
    Any, Dapt, DaptBuilder, Number, Path,
};

use super::{
    error::{Error, QueryResult},
    lexor::Lexer,
};

const SELECT: &str = "SELECT";
const SELECT_SEP: &str = ",";
const SELECT_ALIAS: &str = "AS";
const FROM: &str = "FROM";
const FROM_SEP: &str = ",";
const WHERE: &str = "WHERE";
const HAVING: &str = "HAVING";
const GROUP: &str = "GROUP";
const ORDER: &str = "ORDER";
const BY: &str = "BY";
const SUB_CONDITION: &str = "(";
const SUB_CONDITION_END: &str = ")";
const EQUAL: &str = "=";
const EQUAL_DOUBLE: &str = "==";
const NOT_EQUAL: &str = "!=";
const IN: &str = "IN";
const GREATER_THAN: &str = ">";
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

const AGGREGATION_SUM: &str = "SUM";
const AGGREGATION_COUNT: &str = "COUNT";

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

#[derive(Clone)]
struct Column {
    agg: Box<dyn Aggregation>,
    alias: Path,
}

struct GroupBy {
    fields: Vec<Box<dyn Expression>>,
    // here we keep a key for the series of fields, and the
    // aggregation set we need to run for this series.
    groups: HashMap<u64, SelectClause>,
    template: SelectClause,
}

struct OrderBy {
    fields: Vec<Box<dyn Expression>>,
}

impl OrderBy {
    pub fn sort(&self, ds: &mut [Dapt]) {
        if self.fields.is_empty() {
            return;
        }

        ds.sort_by(|a, b| {
            for field in self.fields.iter() {
                let a = field.evaluate(a).unwrap();
                let b = field.evaluate(b).unwrap();
                match a.partial_cmp(&b) {
                    Some(ord) => match ord {
                        std::cmp::Ordering::Equal => continue,
                        _ => return ord,
                    },
                    None => continue,
                }
            }

            std::cmp::Ordering::Equal
        });
    }
}

impl GroupBy {
    pub fn process(&mut self, d: &Dapt) -> QueryResult<()> {
        let mut hasher = Hasher::new();

        for field in self.fields.iter() {
            match field.evaluate(d) {
                Some(val) => hasher.hash(&val),
                None => hasher.hash(&Any::Null),
            }
        }

        let hash = hasher.finish();
        let group = if self.groups.contains_key(&hash) {
            self.groups.get_mut(&hash).unwrap()
        } else {
            let group = self.template.clone();
            self.groups.insert(hash, group);
            self.groups.get_mut(&hash).unwrap()
        };

        group.process(d)
    }

    pub fn collect(&self, having: &WhereClause) -> QueryResult<Vec<Dapt>> {
        let mut results = Vec::new();

        for group in self.groups.values() {
            let d = group.collect()?;
            match having.filter(&d) {
                Ok(true) => results.push(d),
                Ok(false) => (),
                // TODO: handle the error here.
                Err(e) => (),
            }
        }

        Ok(results)
    }
}

pub struct Query {
    from: FromClause,
    wherre: WhereClause,
    having: WhereClause,
    group: GroupBy,
    order: OrderBy,
}

struct FromClause(Vec<String>);

impl FromClause {
    pub fn new(str: &str) -> QueryResult<FromClause> {
        let mut parser = Parser::from(str);
        parser.parse_from()
    }
}

impl Query {
    pub fn new(str: &str) -> QueryResult<Query> {
        let mut parser = Parser::from(str);
        parser.parse_query()
    }

    pub fn process(&mut self, d: &Dapt) -> QueryResult<()> {
        if self.wherre.filter(d)? {
            self.group.process(d)?;
        }
        Ok(())
    }

    pub fn collect(&self) -> QueryResult<Vec<Dapt>> {
        let mut set = self.group.collect(&self.having)?;
        self.order.sort(&mut set);
        Ok(set)
    }
}

// SELECT takes both expressions and aggregations. Any expressions will be wrapped
// in an ExpressionAggregation which will grab the first value expressed and then
// return OK from then on. If your query is a transformation, or in other words, only
// expressions, you should call collect on each call to process. If your query is an
// aggregation you can call process whenever you want the calculation to be done.
// Many aggregations are set back to 0 or the default value after process is called.
// so you can continue to use Select after collect is called.
#[derive(Clone)]
pub struct SelectClause {
    fields: Vec<Column>,
}

impl SelectClause {
    pub fn new(str: &str) -> QueryResult<SelectClause> {
        let mut parser = Parser::from(str);
        parser.parse_select()
    }

    pub fn process(&mut self, d: &Dapt) -> QueryResult<()> {
        for col in self.fields.iter_mut() {
            col.agg.process(d)?;
        }

        Ok(())
    }

    pub fn collect(&self) -> QueryResult<Dapt> {
        let mut d = DaptBuilder::new();
        for col in self.fields.iter() {
            let value = col.agg.result()?;
            d.set_any_path(&col.alias, value)?;
        }

        Ok(d.build())
    }
}

// an aggregation taks multiple dapt packets through process
// and returns the defined aggregation result as an Any type.
trait Aggregation: std::fmt::Display + DynClone {
    // process can be called multiple times
    fn process<'a>(&'a mut self, d: &Dapt) -> QueryResult<()>;

    // result returns the aggregation result, were applicable, we should
    // return NotFound, which is handled by select by not adding the column
    // to the final result.
    fn result<'a>(&'a self) -> QueryResult<Any<'a>>;
}
dyn_clone::clone_trait_object!(Aggregation);

// Condition is a trait that defines a where clause condition, such as
// `age = 10` or `name != "John"` though higher level objects implement
// this trait as well.
trait Condition {
    fn evaluate(&self, d: &Dapt) -> QueryResult<bool>;
}

// Expression is a trait that takes in a dapt packet and returns an
// optional value. This value can be Any type, which is what a dapt packet
// can return.
trait Expression: std::fmt::Display + DynClone {
    fn evaluate<'a, 'b: 'a>(&'a self, d: &'b Dapt) -> Option<Any<'a>>;
}
dyn_clone::clone_trait_object!(Expression);

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
    pub fn parse_query(&mut self) -> QueryResult<Query> {
        let select = self.parse_select()?;

        // from is optional, so we can create an empty from cluase
        // if there is no value.
        let from = if let Some(FROM) = self.lex.peak() {
            self.parse_from()?
        } else {
            FromClause(Vec::new())
        };

        // where is optional, so we can create an empty where cluase
        // if there is no value.
        let where_clause = if let Some(WHERE) = self.lex.peak() {
            self.parse_where()?
        } else {
            // this always evaluates to true
            WhereClause {
                condition: Conjunction::Single(Box::new(DefaultExpressCondition {
                    expr: Box::new(BoolExpression { value: true }),
                })),
            }
        };

        // having is also optional
        let having = if let Some(HAVING) = self.lex.peak() {
            self.parse_having()?
        } else {
            // this always evaluates to true
            WhereClause {
                condition: Conjunction::Single(Box::new(DefaultExpressCondition {
                    expr: Box::new(BoolExpression { value: true }),
                })),
            }
        };

        let group = if let Some(GROUP) = self.lex.peak() {
            self.parse_group(select)?
        } else {
            let mut groups = HashMap::new();
            groups.insert(0, select.clone());

            GroupBy {
                fields: Vec::new(),
                groups,
                template: select,
            }
        };

        let order = if let Some(ORDER) = self.lex.peak() {
            self.parse_order()?
        } else {
            OrderBy { fields: Vec::new() }
        };

        Ok(Query {
            from,
            wherre: where_clause,
            having,
            group,
            order,
        })
    }

    pub fn parse_group(&mut self, select: SelectClause) -> QueryResult<GroupBy> {
        self.consume_token(GROUP)?;
        self.consume_token(BY)?;

        let mut fields = Vec::new();
        let groups = HashMap::new();
        loop {
            fields.push(self.parse_expression()?);
            if let Some(FROM_SEP) = self.lex.peak() {
                self.consume_token(FROM_SEP)?;
            } else {
                break;
            }
        }

        Ok(GroupBy {
            fields,
            groups,
            template: select,
        })
    }

    pub fn parse_order(&mut self) -> QueryResult<OrderBy> {
        self.consume_token(ORDER)?;
        self.consume_token(BY)?;

        let mut fields = Vec::new();
        loop {
            fields.push(self.parse_expression()?);
            if let Some(FROM_SEP) = self.lex.peak() {
                self.consume_token(FROM_SEP)?;
            } else {
                break;
            }
        }

        Ok(OrderBy { fields })
    }

    pub fn parse_from(&mut self) -> QueryResult<FromClause> {
        self.consume_token(FROM)?;

        let mut sources = Vec::new();
        loop {
            sources.push(self.parse_string(KEY_WRAP)?);
            if let Some(FROM_SEP) = self.lex.peak() {
                self.consume_token(FROM_SEP)?;
            } else {
                break;
            }
        }

        Ok(FromClause(sources))
    }

    pub fn parse_select(&mut self) -> QueryResult<SelectClause> {
        self.consume_token(SELECT)?;

        let mut fields = Vec::new();
        loop {
            fields.push(self.parse_column()?);
            // peak at the next token to see what we should do
            match self.lex.peak() {
                Some(FROM) => break,
                Some(WHERE) => break,
                Some(SELECT_SEP) => self.consume_token(SELECT_SEP)?,
                None => break,
                Some(tok) => {
                    return Err(Error::with_history(
                        &format!("expecting {SELECT_SEP} or end of select but found {tok}"),
                        &self.lex,
                    ))
                }
            }
        }

        Ok(SelectClause { fields })
    }

    pub fn parse_column(&mut self) -> QueryResult<Column> {
        let agg = self.parse_aggregation()?;

        let alias = match self.lex.peak() {
            Some(val) if val.to_string().to_uppercase() == SELECT_ALIAS => {
                self.consume_token(SELECT_ALIAS)?;
                let path = self.parse_string(KEY_WRAP)?;
                Path::new(&path)?
            }
            _ => {
                let field = FieldLiteral::new(&format!("{}", agg));
                Path::from(vec![Node::FieldLiteral(field)])
            }
        };

        Ok(Column { agg, alias })
    }

    pub fn parse_aggregation(&mut self) -> QueryResult<Box<dyn Aggregation>> {
        let tok = self
            .lex
            .peak()
            .ok_or_else(|| Error::unexpected_eof(&self.lex))?;

        match tok.to_uppercase().as_str() {
            AGGREGATION_SUM => Ok(Box::new(SumAggregation::from_parser(self)?)),
            AGGREGATION_COUNT => Ok(Box::new(CountAggregation::from_parser(self)?)),
            _ => Ok(Box::new(ExpressionAggregation::from_parser(self)?)),
        }
    }

    pub fn parse_having(&mut self) -> QueryResult<WhereClause> {
        let tok = self
            .lex
            .token()
            .ok_or_else(|| Error::unexpected_eof(&self.lex))?;

        match tok {
            HAVING => Ok(WhereClause {
                condition: self.parse_conjunction()?,
            }),
            _ => Err(Error::with_history("expected HAVING", &self.lex)),
        }
    }

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
        match tok.to_uppercase().as_str() {
            EQUAL | EQUAL_DOUBLE => {
                let right = match self.parse_expression() {
                    Err(Error::UnexpectedEOF(_)) => {
                        return Err(Error::with_history(
                            format!("{EQUAL} expects expressions on both sides").as_str(),
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
                            format!("{NOT_EQUAL} expects expressions on both sides").as_str(),
                            &self.lex,
                        ))
                    }
                    Err(e) => return Err(e),
                    Ok(r) => r,
                };

                Ok(Box::new(NotEqualsCondition { left, right }))
            }
            GREATER_THAN => {
                let right = match self.parse_expression() {
                    Err(Error::UnexpectedEOF(_)) => {
                        return Err(Error::with_history(
                            format!("{GREATER_THAN} expects expressions on both sides").as_str(),
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
                            format!("{GREATER_THAN_EQUAL} expects expressions on both sides")
                                .as_str(),
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
                            format!("{LESS_THAN_EQUAL} expects expressions on both sides").as_str(),
                            &self.lex,
                        ))
                    }
                    Err(e) => return Err(e),
                    Ok(r) => r,
                };

                Ok(Box::new(LessThanEqualCondition { left, right }))
            }
            IN => {
                let right = match self.parse_expression() {
                    Err(Error::UnexpectedEOF(_)) => {
                        return Err(Error::with_history(
                            format!("{IN} expects expressions on both sides").as_str(),
                            &self.lex,
                        ))
                    }
                    Err(e) => return Err(e),
                    Ok(r) => r,
                };

                Ok(Box::new(InCondition { left, right }))
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
                    let key = self.parse_string(KEY_WRAP)?;
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

    fn parse_string(&mut self, wrap: &str) -> QueryResult<String> {
        match self.lex.token() {
            Some(val) if val == wrap => (),
            Some(tok) => {
                return Err(Error::with_history(
                    &format!("expected {wrap} but got {tok}"),
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
            Some(val) if val == wrap => Ok(value.to_string()),
            Some(tok) => Err(Error::with_history(
                &format!("expected {wrap} but got {tok}"),
                &self.lex,
            )),
            None => Err(Error::unexpected_eof(&self.lex)),
        }
    }

    fn consume_token(&mut self, expected: &str) -> QueryResult<()> {
        match self.lex.token() {
            Some(tok) if tok.to_uppercase() == expected => Ok(()),
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
        #[derive(Clone)]
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

// InCondition will check if the left expression is in the right expression. If the
// right expression is not a map or array this functions the same as EqualsCondition
struct InCondition {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
struct SumAggregation {
    value: Box<dyn Expression>,
    sum: Number,
}

impl SumAggregation {
    fn from_parser(parser: &mut Parser) -> QueryResult<SumAggregation> {
        parser.consume_token(AGGREGATION_SUM)?;
        parser.consume_token(FN_OPEN)?;
        let value = parser.parse_expression()?;
        parser.consume_token(FN_CLOSE)?;

        Ok(SumAggregation {
            value,
            sum: Number::USize(0),
        })
    }
}

// SumAggregation will sum the values of the expression, if the expression
// returns an array, each item in the array is sumed. If the expression
// returns non numeric types they are ignored without error.
impl Aggregation for SumAggregation {
    fn process<'a>(&'a mut self, d: &Dapt) -> QueryResult<()> {
        let expr_val = self.value.evaluate(d);
        let val = match &expr_val {
            Some(v) => v,
            None => return Ok(()),
        };

        match val {
            Any::Array(a) => {
                for v in a {
                    match Number::try_from(v) {
                        Ok(n) => self.sum = self.sum + n,
                        Err(_) => (),
                    }
                }
            }
            _ => match Number::try_from(val) {
                Ok(n) => self.sum = self.sum + n,
                Err(_) => (),
            },
        }
        Ok(())
    }

    fn result<'a>(&'a self) -> QueryResult<Any<'a>> {
        Ok(self.sum.into())
    }
}

impl Display for SumAggregation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{AGGREGATION_SUM}({})", self.value)
    }
}

// count aggregation will count the number of times an expression has a value
// if no argument is given to count it will just count the number of lines
#[derive(Clone)]
struct CountAggregation {
    expr: Option<Box<dyn Expression>>,
    count: usize,
}

impl CountAggregation {
    fn from_parser(parser: &mut Parser) -> QueryResult<CountAggregation> {
        parser.consume_token(AGGREGATION_COUNT)?;
        parser.consume_token(FN_OPEN)?;

        let expr = match parser.lex.peak() {
            Some(FN_CLOSE) => None,
            _ => Some(parser.parse_expression()?),
        };

        parser.consume_token(FN_CLOSE)?;

        Ok(CountAggregation { expr, count: 0 })
    }
}

impl Display for CountAggregation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.expr {
            Some(e) => write!(f, "{AGGREGATION_COUNT}({})", e),
            None => write!(f, "{AGGREGATION_COUNT}"),
        }
    }
}

impl Aggregation for CountAggregation {
    fn process<'a>(&'a mut self, d: &Dapt) -> QueryResult<()> {
        match &self.expr {
            Some(e) => {
                if e.evaluate(d).is_some() {
                    self.count += 1;
                }
            }
            None => self.count += 1,
        }

        Ok(())
    }

    fn result<'a>(&'a self) -> QueryResult<Any<'a>> {
        Ok(Any::USize(self.count))
    }
}

#[derive(Clone)]
struct ExpressionAggregation {
    expr: Box<dyn Expression>,
    value: Option<OwnedAny>,
}

impl ExpressionAggregation {
    fn new(expr: Box<dyn Expression>) -> Self {
        Self { expr, value: None }
    }

    fn from_parser(parser: &mut Parser) -> QueryResult<ExpressionAggregation> {
        let expr = parser.parse_expression()?;
        Ok(ExpressionAggregation::new(expr))
    }
}

impl Aggregation for ExpressionAggregation {
    fn process<'a>(&'a mut self, d: &Dapt) -> QueryResult<()> {
        // just take the first value, that way we can avoid all the
        // heap allocations for each value we process.
        if self.value.is_some() {
            return Ok(());
        }

        self.value = match self.expr.evaluate(d) {
            // the value here will likely outlive the dapt packet
            // it came from, so we clone.
            Some(v) => Some(v.into()),
            None => return Ok(()),
        };

        Ok(())
    }

    fn result<'a>(&'a self) -> QueryResult<Any<'a>> {
        if self.value.is_none() {
            return Err(Error::NotFound);
        }

        Ok(self.value.as_ref().unwrap().into())
    }
}

impl Display for ExpressionAggregation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expr)
    }
}

struct Hasher {
    state: u64,
}

impl Hasher {
    fn new() -> Self {
        Self { state: 0 }
    }

    fn hash(&mut self, a: &Any<'_>) {
        match a {
            Any::Null => self.hash_combine(0),
            Any::Bool(b) => self.hash_combine(city_hash_64(&(*b as u8).to_ne_bytes())),
            Any::USize(u) => self.hash_combine(city_hash_64(&u.to_ne_bytes())),
            Any::ISize(i) => self.hash_combine(city_hash_64(&i.to_ne_bytes())),
            Any::U128(u) => self.hash_combine(city_hash_64(&u.to_be_bytes())),
            Any::I128(i) => self.hash_combine(city_hash_64(&i.to_be_bytes())),
            Any::U64(u) => self.hash_combine(city_hash_64(&u.to_be_bytes())),
            Any::I64(i) => self.hash_combine(city_hash_64(&i.to_be_bytes())),
            Any::U32(u) => self.hash_combine(city_hash_64(&u.to_be_bytes())),
            Any::I32(i) => self.hash_combine(city_hash_64(&i.to_be_bytes())),
            Any::U16(u) => self.hash_combine(city_hash_64(&u.to_be_bytes())),
            Any::I16(i) => self.hash_combine(city_hash_64(&i.to_be_bytes())),
            Any::U8(u) => self.hash_combine(city_hash_64(&u.to_be_bytes())),
            Any::I8(i) => self.hash_combine(city_hash_64(&i.to_be_bytes())),
            Any::Char(c) => self.hash_combine(city_hash_64(&((*c) as u64).to_ne_bytes())),
            Any::Bytes(b) => self.hash_combine(city_hash_64(*b)),
            Any::F64(f) => self.hash_combine(city_hash_64(&f.to_ne_bytes())),
            Any::F32(f) => self.hash_combine(city_hash_64(&f.to_ne_bytes())),
            Any::Str(s) => self.hash_combine(city_hash_64(s.as_bytes())),
            Any::Array(a) => {
                for v in a {
                    self.hash(v);
                }
            }
            Any::Map(m) => {
                for (k, v) in m.iter() {
                    self.hash_combine(city_hash_64(k.as_bytes()));
                    self.hash(v);
                }
            }
        }
    }

    fn finish(self) -> u64 {
        self.state
    }

    // https://stackoverflow.com/questions/5889238/why-is-xor-the-default-way-to-combine-hashes
    fn hash_combine(&mut self, rhs: u64) {
        self.state = self.state
            ^ (rhs
                .wrapping_add(0x517cc1b727220a95)
                .wrapping_add(self.state << 6)
                .wrapping_add(self.state >> 2))
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
            r#" "a" == {"b": 10, "c": 20} "#,
            true
        );

        assert_conjunction!(
            r#"{"a": {"b": {"c": 20}}}"#,
            r#" "a" == {"b": {"c": 20}} "#,
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

        // in
        assert_conjunction!(r#"{"a": 10}"#, r#" "a" IN [1,2,3,4,5,6,7,8,9,10] "#, true);
        // here we are showing how in can be used to find an object in an array
        // also we are using a as the right side since it has the set
        assert_conjunction!(
            r#"{"a": [{"a": 1}, {"b": 2}, {"c": 11}]}"#,
            r#" {"c":11} in "a" "#,
            true
        );
        // we can use in on a map as well, making it easy to look for a child if you don't
        // know the name
        assert_conjunction!(
            r#"{"a": {"a":{"a": 1}, "b":{"b": 2}, "c":{"c": 11}}}"#,
            r#" {"c":12} in "a" "#,
            true
        );

        // finally in is very helpful when we don't know how many things our key matches
        // but we want to evaluate to true if one of them matches
        assert_conjunction!(
            r#"{"a": {"a": 1, "b": 2, "c": 3}}"#,
            r#" 2 in "a.*" "#,
            true
        );
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

    macro_rules! assert_aggregation {
        ( $expr:expr, $expected:expr, $($source:expr),+) => {
            let mut parser = Parser::from($expr);
            let mut expr = parser.parse_aggregation().unwrap();
            let sources = vec![$(serde_json::from_str($source).unwrap()),+];
            for d in sources {
                expr.process(&d).unwrap();
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
    }

    macro_rules! assert_select {
        ( $expr:expr, $expected:expr, $($source:expr),+) => {
            let mut parser = Parser::from($expr);
            let mut expr = parser.parse_select().unwrap();
            let sources = vec![$(serde_json::from_str($source).unwrap()),+];
            for d in sources {
                expr.process(&d).unwrap();
            }
            let result = expr.collect().unwrap();
            assert_eq!(serde_json::to_string(&result).unwrap(), $expected);
        };
    }

    #[test]
    fn test_select() {
        // literal, just to prove it can be done
        assert_select!(
            r#"SELECT 10 as "number" "#,
            r#"{"number":10}"#,
            // values
            r#"{"a": 1}"#,
            r#"{"a": 2}"#,
            r#"{"a": 3}"#
        );

        assert_select!(
            r#"SELECT sum("a") as "sum" "#,
            r#"{"sum":6}"#,
            // values
            r#"{"a": 1}"#,
            r#"{"a": 2}"#,
            r#"{"a": 3}"#
        );

        assert_select!(
            r#"SELECT sum("a") "#,
            r#"{"SUM(a)":6}"#,
            // values
            r#"{"a": 1}"#,
            r#"{"a": 2}"#,
            r#"{"a": 3}"#
        );

        assert_select!(
            r#"SELECT sum("a") as "a.b.c" "#,
            r#"{"a":{"b":{"c":6}}}"#,
            // values
            r#"{"a": 1}"#,
            r#"{"a": 2}"#,
            r#"{"a": 3}"#
        );

        assert_select!(
            r#"SELECT sum("a") as "sum", count("a") as "count" "#,
            r#"{"sum":6,"count":3}"#,
            // values
            r#"{"a": 1}"#,
            r#"{"a": 2}"#,
            r#"{"a": 3}"#
        );
    }

    macro_rules! assert_select {
        ( $expr:expr, $expected:expr, $($source:expr),+) => {
            let mut parser = Parser::from($expr);
            let mut expr = parser.parse_query().unwrap();
            let sources = vec![$(serde_json::from_str($source).unwrap()),+];
            for d in sources {
                expr.process(&d).unwrap();
            }
            let result = expr.collect().unwrap();
            assert_eq!(serde_json::to_string(&result).unwrap(), $expected);
        };
    }

    #[test]
    fn test_query() {
        assert_select!(
            r#"SELECT sum("a") as "sum", count("a") as "count", "b" WHERE "a" > 1 GROUP BY "b" ORDER BY "b" "#,
            r#"[{"sum":7,"count":2,"b":"hello"},{"sum":2,"count":1,"b":"hi"}]"#,
            // values
            r#"{"a": 1, "b": "hi"}"#,
            r#"{"a": 2, "b": "hi"}"#,
            r#"{"a": 3, "b": "hello"}"#,
            r#"{"a": 4, "b": "hello"}"#
        );
    }
}
