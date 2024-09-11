use core::fmt;
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    time::{Duration, SystemTime},
};

use cityhash::city_hash_64;

use crate::{
    path::{
        node::FieldLiteral,
        parser::{Node, Parser as PathParser},
    },
    Any, Dapt, DaptBuilder, Path,
};

use super::{
    aggregation::*,
    condition::*,
    error::{Error, History, QueryResult},
    expression::*,
    lexor::Lexer,
};

pub const SELECT: &str = "SELECT";
pub const SELECT_SEP: &str = ",";
pub const SELECT_ALIAS: &str = "AS";
pub const FROM: &str = "FROM";
pub const FROM_SEP: &str = ",";
pub const WHERE: &str = "WHERE";
pub const HAVING: &str = "HAVING";
pub const GROUP: &str = "GROUP";
pub const ORDER: &str = "ORDER";
pub const BY: &str = "BY";
pub const ORDER_ASC: &str = "ASC";
pub const ORDER_DESC: &str = "DESC";
pub const LIMIT: &str = "LIMIT";
pub const INTERVAL: &str = "INTERVAL";
pub const SUB_CONDITION: &str = "(";
pub const SUB_CONDITION_END: &str = ")";
pub const EQUAL: &str = "=";
pub const EQUAL_DOUBLE: &str = "==";
pub const NOT_EQUAL: &str = "!=";
pub const IN: &str = "IN";
pub const GREATER_THAN: &str = ">";
pub const LESS_THAN: &str = "<";
pub const GREATER_THAN_EQUAL: &str = ">=";
pub const LESS_THAN_EQUAL: &str = "<=";
pub const AND: &str = "AND";
pub const OR: &str = "OR";
pub const KEY_WRAP: &str = "`";
pub const IDENTIFIER_WRAP: &str = "\"";
pub const STRING_WRAP: &str = "'";
pub const NULL: &str = "NULL";
pub const TRUE: &str = "TRUE";
pub const FALSE: &str = "FALSE";
pub const MAP_WRAP: &str = "{";
pub const MAP_WRAP_END: &str = "}";
pub const MAP_CHILD_SET: &str = ":";
pub const MAP_CHILD_SEP: &str = ",";
pub const ARRAY_WRAP: &str = "[";
pub const ARRAY_WRAP_END: &str = "]";
pub const ARRAY_CHILD_SEP: &str = ",";

pub const FN_OPEN: &str = "(";
pub const FN_CLOSE: &str = ")";
pub const FN_SEP: &str = ",";

pub const FN_ADD: &str = "ADD";
pub const FN_MINUS: &str = "NEG";
pub const FN_MULTIPLY: &str = "MUL";
pub const FN_DIVIDE: &str = "DIV";
pub const FN_MODULUS: &str = "MOD";
pub const FN_EXISTS: &str = "EXISTS";

pub const AGGREGATION_SUM: &str = "SUM";
pub const AGGREGATION_COUNT: &str = "COUNT";
pub const AGGREGATION_AVG: &str = "AVG";

// Column holds the aggregation and it's alias. It is the sum("key") as "sum"
// part of a query. The alias is a path, since paths have the `Aquire` trait which
// allows them to be created in a new dapt packet
#[derive(Clone)]
pub struct Column {
    agg: Box<dyn Aggregation>,
    alias: Path,
}

impl Column {
    pub fn new(agg: Box<dyn Aggregation>, alias: Path) -> Self {
        Self { agg, alias }
    }

    fn composable(&self) -> (Vec<Column>, Column) {
        let (composable, combine) = self.agg.composable(&self.alias);

        (
            composable,
            Column {
                agg: combine,
                alias: self.alias.clone(),
            },
        )
    }
}

impl Display for Column {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} AS {}", self.agg, self.alias)
    }
}

// Group by holds an array of expressions, which are the values we will group by,
// a template which holds a select clause to be cloned for each new unique set of
// group by values, and finally a hashmap, which holds a select clause for each group
// the key is a hash of the group by values.
#[derive(Clone)]
struct GroupBy {
    fields: Vec<Box<dyn Expression>>,
    // here we keep a key for the series of fields, and the
    // aggregation set we need to run for this series.
    template: SelectClause,
    groups: HashMap<u64, SelectClause>,
}

impl GroupBy {
    fn process(&mut self, d: &Dapt) {
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

        group.process(d);
    }

    fn collect(&mut self, having: &HavingClause) -> QueryResult<Vec<Dapt>> {
        let mut results = Vec::new();
        for group in self.groups.values_mut() {
            let d = group.collect()?;
            match having.filter(&d) {
                Ok(true) => {
                    if !d.empty() {
                        results.push(d)
                    }
                }
                Ok(false) => (),
                // TODO: handle the error here.
                Err(_) => (),
            }
        }

        Ok(results)
    }

    fn composable(&self) -> (GroupBy, GroupBy) {
        let (composable, combine) = self.template.composable();
        (
            GroupBy {
                fields: self.fields.clone(),
                template: composable,
                groups: HashMap::new(),
            },
            GroupBy {
                fields: self.fields.clone(),
                template: combine,
                groups: HashMap::new(),
            },
        )
    }
}

impl Display for GroupBy {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.fields.is_empty() {
            return Ok(());
        }

        let mut first = true;
        write!(f, "GROUP BY ")?;

        for field in self.fields.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }

            write!(f, "{}", field)?;
        }

        Ok(())
    }
}

// OrderBy is used to sort the final result set. It holds a vector of OrderByColumn
// which is an expression and a direction
#[derive(Clone)]
struct OrderBy {
    fields: Vec<OrderByColumn>,
}

impl OrderBy {
    pub fn sort(&self, ds: &mut [Dapt]) {
        if self.fields.is_empty() {
            return;
        }

        ds.sort_by(|a, b| {
            for order_column in self.fields.iter() {
                let a = order_column.field.evaluate(a).unwrap_or(Any::Null);
                let b = order_column.field.evaluate(b).unwrap_or(Any::Null);

                let cmp = match order_column.direction {
                    OrderDirection::Ascending => a.partial_cmp(&b),
                    OrderDirection::Descending => b.partial_cmp(&a),
                };

                match cmp {
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

impl Display for OrderBy {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.fields.is_empty() {
            return Ok(());
        }

        write!(f, "ORDER BY ")?;

        let mut first = true;
        for field in self.fields.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }

            write!(f, "{}", field.field)?;
        }

        Ok(())
    }
}

// OrderByColumn holds an expression and a direction, which is either ascending or
// descending
#[derive(Clone)]
struct OrderByColumn {
    field: Box<dyn Expression>,
    direction: OrderDirection,
}

impl Display for OrderByColumn {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {}", self.field, self.direction)
    }
}

// OrderDirection is used for order by, Ascending or Descending
#[derive(Clone)]
enum OrderDirection {
    Ascending,
    Descending,
}

impl Display for OrderDirection {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            OrderDirection::Ascending => write!(f, "ASC"),
            OrderDirection::Descending => write!(f, "DESC"),
        }
    }
}

// Limit is used at the very end, when used with Order By it can be used to get
// a limited subset of the values returned by the query.
#[derive(Clone)]
struct Limit {
    count: usize,
}

impl Display for Limit {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {}", LIMIT, self.count)
    }
}

// From isn't really used, at least within the query, but it is optionally
// available in the query so it can be used outside to pull in appropriate
// data. It allows for specifying multiple sources.
#[derive(Clone)]
struct FromClause(Vec<String>);

impl FromClause {
    #[allow(dead_code)]
    pub fn new(str: &str) -> QueryResult<FromClause> {
        let mut parser = Parser::from(str);
        parser.parse_from()
    }
}

impl Display for FromClause {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.0.is_empty() {
            return Ok(());
        }

        write!(f, "FROM ")?;

        let mut first = true;
        for source in self.0.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }

            write!(f, "{}", source)?;
        }

        Ok(())
    }
}

#[derive(Clone)]
struct Interval {
    last_output: SystemTime,
    duration: Duration,
}

impl Interval {
    fn should_fire_and_reset(&mut self) -> bool {
        let now = SystemTime::now();
        let elapsed = now.duration_since(self.last_output).unwrap();

        if elapsed >= self.duration {
            self.last_output = now;
            return true;
        }

        false
    }
}

// Query is the parsed representation of a query. It holds a from, where, having
// group by (which houses the select clause), order by and limit. The only thing required
// to parse a query is the `SELECT` portion of the query. The rest is optional.
// if not specified a No Op where, having, group by, order by are created.
#[derive(Clone)]
pub struct Query {
    from: FromClause,
    wherre: WhereClause,
    having: HavingClause,
    group: GroupBy,
    order: OrderBy,
    limit: Option<Limit>,
    interval: Interval,
}

impl Query {
    pub fn new(str: &str) -> QueryResult<Query> {
        let mut parser = Parser::from(str);
        parser.parse_query()
    }

    pub fn process(&mut self, d: &Dapt) -> QueryResult<()> {
        if self.wherre.filter(d)? {
            self.group.process(d);
        }
        Ok(())
    }

    pub fn collect(&mut self) -> QueryResult<Vec<Dapt>> {
        let mut set = self.group.collect(&self.having)?;
        self.order.sort(&mut set);

        if let Some(limit) = &self.limit {
            set.truncate(limit.count);
        }

        Ok(set)
    }

    // process_and_collect utilizes the defined interval to determine if the
    // query should return data. If there is no data to return option will be
    // none
    pub fn process_and_collect(&mut self, d: &Dapt) -> QueryResult<Option<Vec<Dapt>>> {
        self.process(d)?;
        if self.interval.should_fire_and_reset() {
            Ok(Some(self.collect()?))
        } else {
            Ok(None)
        }
    }

    // composite returns two query objects, The first query can be
    // run on multiple data sets concurrently, the second query
    // is then used to combine the responses of the first query into
    // the final answer the initial query was looking for.
    pub fn composite(&self) -> (Query, Query) {
        let (composable, combine) = self.group.composable();
        (
            // having, order by and limit can only be done at the combine step
            Query {
                from: self.from.clone(),
                wherre: self.wherre.clone(),
                having: HavingClause {
                    condition: Conjunction::Single(Box::new(NoopCondition::default())),
                },
                group: composable,
                order: OrderBy { fields: Vec::new() },
                limit: None,
                interval: self.interval.clone(),
            },
            // where can only be done at the composable step
            Query {
                from: self.from.clone(),
                wherre: WhereClause {
                    condition: Conjunction::Single(Box::new(NoopCondition::default())),
                },
                having: self.having.clone(),
                group: combine,
                order: self.order.clone(),
                limit: self.limit.clone(),
                interval: self.interval.clone(),
            },
        )
    }
}

impl Display for Query {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.group.template)?;

        if !self.from.0.is_empty() {
            write!(f, " {}", self.from)?;
        }

        write!(
            f,
            " {} {} {} {}",
            self.wherre, self.having, self.group, self.order
        )?;

        if self.limit.is_some() {
            write!(f, " {}", self.limit.as_ref().unwrap())?;
        }

        Ok(())
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

    pub fn process(&mut self, d: &Dapt) {
        for col in self.fields.iter_mut() {
            col.agg.process(d);
        }
    }

    pub fn collect(&mut self) -> QueryResult<Dapt> {
        let mut d = DaptBuilder::new();
        for col in self.fields.iter_mut() {
            if let Some(value) = col.agg.result() {
                d.set_any_path(&col.alias, value)?;
            }
        }

        Ok(d.build())
    }

    pub fn composable(&self) -> (SelectClause, SelectClause) {
        let mut composable = SelectClause { fields: Vec::new() };
        let mut combine = SelectClause { fields: Vec::new() };

        for col in self.fields.iter() {
            let (com, comb) = col.composable();
            for c in com {
                composable.fields.push(c);
            }
            combine.fields.push(comb);
        }

        (composable, combine)
    }

    // alias_of returns a clone of the underlying expression that the
    // expression passed in is aliasing. It returns nothing if the
    // expressions provided does not match a selected column
    pub fn alias_of(&self, expr: &Box<dyn Expression>) -> QueryResult<Box<dyn Expression>> {
        for col in self.fields.iter() {
            println!("{} == {}", col.alias.to_string(), expr.to_string());
            if col.alias.to_string() == expr.to_string() {
                return col.agg.expression();
            }
        }

        Err(Error::NotFound)
    }
}

impl Display for SelectClause {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "SELECT ")?;

        let mut first = true;
        for field in self.fields.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }

            write!(f, "{}", field)?;
        }

        Ok(())
    }
}

// Conjunctions are used to combine conditions. So you can have a == b AND c == d
// Conjuctions are a single condition, or an AND or OR of two conditions.
#[derive(Clone)]
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

impl Display for Conjunction {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Conjunction::Single(c) => write!(f, "{}", c),
            Conjunction::And { left, right } => write!(f, "{} AND {}", left, right),
            Conjunction::Or { left, right } => write!(f, "{} OR {}", left, right),
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

// WhereClause is used to filter dapt packets before passing into an aggregation.
// The clause is just a conjuntion, which creates a left right tree of conditions.
// it exists to provide a public interface to the conjunction
#[derive(Clone)]
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

impl Display for WhereClause {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "WHERE {}", self.condition)
    }
}

// HavingClause is used to filter dapt packets after passing through an aggregation.
// The clause is just a conjuntion, which creates a left right tree of conditions.
// it exists to provide a public interface to the conjunction
#[derive(Clone)]
pub struct HavingClause {
    condition: Conjunction,
}

impl HavingClause {
    pub fn filter(&self, d: &Dapt) -> QueryResult<bool> {
        self.condition.evaluate(d)
    }
}

impl Display for HavingClause {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "HAVING {}", self.condition)
    }
}

// Parser is used to parse a query string into a query struct, it produces all
// sorts of interior structs as well.
pub struct Parser<'a> {
    lex: Lexer<'a>,
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(s: &'a str) -> Parser<'a> {
        Parser {
            lex: Lexer::from(s),
        }
    }
}

impl<'a> Parser<'a> {
    pub fn peak(&mut self) -> Option<&str> {
        self.lex.peak()
    }

    pub fn token(&mut self) -> Option<&str> {
        self.lex.token()
    }

    pub fn consumed(&self) -> History {
        History::new(self.lex.consumed(), self.lex.future())
    }

    pub fn parse_query(&mut self) -> QueryResult<Query> {
        let select = self.parse_select()?;

        // from is optional, so we can create an empty from cluase
        // if there is no value.
        let from = match self.lex.peak() {
            Some(v) if v.to_uppercase() == FROM => self.parse_from()?,
            _ => FromClause(Vec::new()),
        };

        // where is optional, so we can create an empty where cluase
        // if there is no value.
        let where_clause = match self.lex.peak() {
            Some(v) if v.to_uppercase() == WHERE => self.parse_where()?,
            _ => WhereClause {
                condition: Conjunction::Single(Box::new(NoopCondition::default())),
            },
        };

        // having is also optional
        let having = match self.lex.peak() {
            Some(v) if v.to_uppercase() == HAVING => self.parse_having()?,
            _ => HavingClause {
                condition: Conjunction::Single(Box::new(NoopCondition::default())),
            },
        };

        let group = match self.lex.peak() {
            Some(v) if v.to_uppercase() == GROUP => self.parse_group(select)?,
            _ => {
                let mut groups = HashMap::new();
                groups.insert(0, select.clone());

                GroupBy {
                    fields: Vec::new(),
                    groups,
                    template: select,
                }
            }
        };

        let order = match self.lex.peak() {
            Some(v) if v.to_uppercase() == ORDER => self.parse_order()?,
            _ => OrderBy { fields: Vec::new() },
        };

        let limit = match self.lex.peak() {
            Some(v) if v.to_uppercase() == LIMIT => Some(self.parse_limit()?),
            _ => None,
        };

        let interval = match self.lex.peak() {
            Some(v) if v.to_uppercase() == INTERVAL => self.parse_interval()?,
            _ => Interval {
                last_output: SystemTime::now(),
                duration: Duration::from_secs(0),
            },
        };

        let leftovers = self.lex.future().trim();
        if !leftovers.is_empty() {
            return Err(Error::with_history(
                &format!("unexpected trailing content: {}", leftovers),
                self.consumed(),
            ));
        }

        Ok(Query {
            from,
            wherre: where_clause,
            having,
            group,
            order,
            limit,
            interval,
        })
    }

    fn parse_interval(&mut self) -> QueryResult<Interval> {
        self.consume_next(INTERVAL)?;
        let duration = self.parse_string(STRING_WRAP)?;

        Ok(Interval {
            last_output: SystemTime::now(),
            duration: parse_duration::parse(&duration)?,
        })
    }

    fn parse_limit(&mut self) -> QueryResult<Limit> {
        self.consume_next(LIMIT)?;
        let count = self.parse_positive_number()?;
        Ok(Limit { count })
    }

    pub fn continue_if(&mut self, tok: &str) -> bool {
        let peaked = self.lex.peak().unwrap_or_default();
        if peaked.to_uppercase() == tok {
            self.consume();
            true
        } else {
            false
        }
    }

    fn parse_group(&mut self, select: SelectClause) -> QueryResult<GroupBy> {
        self.consume_next(GROUP)?;
        self.consume_next(BY)?;

        let mut fields = Vec::new();
        let groups = HashMap::new();

        loop {
            let alias = self.parse_expression()?;
            let expr = match select.alias_of(&alias) {
                Ok(expr) => Ok(expr), // the alias was of an expression in the select clause
                Err(Error::NotFound) => Ok(alias), // the alias is an expression, not in the select
                Err(err) => Err(err), // there was a real error
            }?;

            println!("{}", expr);

            fields.push(expr);

            if !self.continue_if(FROM_SEP) {
                break;
            }
        }

        Ok(GroupBy {
            fields,
            groups,
            template: select,
        })
    }

    fn parse_order(&mut self) -> QueryResult<OrderBy> {
        self.consume_next(ORDER)?;
        self.consume_next(BY)?;

        let mut fields = Vec::new();
        loop {
            let expr = self.parse_expression()?;

            match self.lex.peak() {
                Some(ORDER_ASC) => {
                    fields.push(OrderByColumn {
                        field: expr,
                        direction: OrderDirection::Ascending,
                    });
                    self.consume_next(ORDER_ASC)?;
                }
                Some(ORDER_DESC) => {
                    fields.push(OrderByColumn {
                        field: expr,
                        direction: OrderDirection::Descending,
                    });
                    self.consume_next(ORDER_DESC)?;
                }
                _ => fields.push(OrderByColumn {
                    field: expr,
                    direction: OrderDirection::Ascending,
                }),
            }

            if !self.continue_if(FROM_SEP) {
                break;
            }
        }

        Ok(OrderBy { fields })
    }

    fn parse_from(&mut self) -> QueryResult<FromClause> {
        self.consume_next(FROM)?;

        let mut sources = Vec::new();

        sources.push(self.parse_string(IDENTIFIER_WRAP)?);
        while self.continue_if(FROM_SEP) {
            sources.push(self.parse_string(IDENTIFIER_WRAP)?);
        }

        Ok(FromClause(sources))
    }

    pub fn parse_select(&mut self) -> QueryResult<SelectClause> {
        self.consume_next(SELECT)?;

        let mut fields = Vec::new();
        fields.push(self.parse_column()?);

        while self.continue_if(SELECT_SEP) {
            fields.push(self.parse_column()?);
        }

        Ok(SelectClause { fields })
    }

    // TODO: you need to change this to a key.
    pub fn parse_column(&mut self) -> QueryResult<Column> {
        let agg = self.parse_aggregation()?;

        let alias = if self.is_next(SELECT_ALIAS) {
            self.consume();
            self.parse_key()?
        } else {
            let field = FieldLiteral::from_escaped(&format!("{}", agg));
            Path::from(vec![Node::FieldLiteral(field)])
        };

        if !alias.is_settable() {
            return Err(Error::InvalidQuery(format!(
                "{} is not a settable alias",
                alias
            )));
        }

        Ok(Column { agg, alias })
    }

    // parse_key will parse a valid key from the parser.
    pub fn parse_key(&mut self) -> QueryResult<Path> {
        let is_wrapped = self.is_next(KEY_WRAP);
        if is_wrapped {
            self.consume();
        }

        let mut path_parser = PathParser::from(self.future());
        let path = path_parser.parse()?;
        self.advance_by(path_parser.chars_consumed());

        if is_wrapped {
            self.consume_next(KEY_WRAP)?;
        }

        Ok(path)
    }

    pub fn parse_aggregation(&mut self) -> QueryResult<Box<dyn Aggregation>> {
        let tok = self
            .lex
            .peak()
            .ok_or_else(|| Error::unexpected_eof(self.consumed()))?;

        match tok.to_uppercase().as_str() {
            AGGREGATION_SUM => Ok(Box::new(SumAggregation::from_parser(self)?)),
            AGGREGATION_COUNT => Ok(Box::new(CountAggregation::from_parser(self)?)),
            AGGREGATION_AVG => Ok(Box::new(AvgAggregation::from_parser(self)?)),
            AGGREGATION_CUMULATIVE_SUM => Ok(Box::new(CumulativeSum::from_parser(self)?)),
            AGGREGATION_MIN => Ok(Box::new(MinAggregation::from_parser(self)?)),
            AGGREGATION_MAX => Ok(Box::new(MaxAggregation::from_parser(self)?)),
            FN_ADD => Ok(Box::new(AddAggregation::from_parser(self)?)),
            FN_MINUS => Ok(Box::new(SubtractAggregation::from_parser(self)?)),
            FN_MULTIPLY => Ok(Box::new(MultiplyAggregation::from_parser(self)?)),
            FN_DIVIDE => Ok(Box::new(DivideAggregation::from_parser(self)?)),
            FN_MODULUS => Ok(Box::new(ModulusAggregation::from_parser(self)?)),
            _ => Ok(Box::new(ExpressionAggregation::from_parser(self)?)),
        }
    }

    pub fn parse_having(&mut self) -> QueryResult<HavingClause> {
        let tok = self
            .lex
            .token()
            .ok_or_else(|| Error::unexpected_eof(self.consumed()))?;

        match tok {
            HAVING => Ok(HavingClause {
                condition: self.parse_conjunction()?,
            }),
            _ => Err(Error::with_history("expected HAVING", self.consumed())),
        }
    }

    pub fn parse_where(&mut self) -> QueryResult<WhereClause> {
        let tok = self
            .lex
            .token()
            .ok_or_else(|| Error::unexpected_eof(self.consumed()))?;

        match tok {
            WHERE => Ok(WhereClause {
                condition: self.parse_conjunction()?,
            }),
            _ => Err(Error::with_history("expected WHERE", self.consumed())),
        }
    }

    // parse_conjunction is a recursive descent parser that parses a conjunction
    // of conditions. It is a simple parser that only supports AND and OR
    // conjunctions.
    fn parse_conjunction(&mut self) -> QueryResult<Conjunction> {
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
            None => return Err(Error::unexpected_eof(self.consumed())),
            Some(SUB_CONDITION) => {
                let _ = self.lex.token(); // consume the (
                let condition = self.parse_conjunction()?;

                match self.lex.token() {
                    None => return Err(Error::unexpected_eof(self.consumed())),
                    Some(SUB_CONDITION_END) => (),
                    _ => return Err(Error::with_history("expected )", self.consumed())),
                }

                return Ok(Box::new(condition));
            }
            _ => (),
        };

        let left = self.parse_expression()?;

        let tok = match self.lex.peak() {
            None => return Ok(Box::new(DefaultExpressCondition::new(left))),
            Some(t) => t,
        };

        match tok.to_uppercase().as_str() {
            EQUAL | EQUAL_DOUBLE => {
                let _ = self.token(); // consume the token
                let right = match self.parse_expression() {
                    Err(Error::UnexpectedEOF(_)) => {
                        return Err(Error::with_history(
                            format!("{EQUAL} expects expressions on both sides").as_str(),
                            self.consumed(),
                        ))
                    }
                    Err(e) => return Err(e),
                    Ok(r) => r,
                };

                Ok(Box::new(EqualsCondition::new(left, right)))
            }
            NOT_EQUAL => {
                let _ = self.token(); // consume the token
                let right = match self.parse_expression() {
                    Err(Error::UnexpectedEOF(_)) => {
                        return Err(Error::with_history(
                            format!("{NOT_EQUAL} expects expressions on both sides").as_str(),
                            self.consumed(),
                        ))
                    }
                    Err(e) => return Err(e),
                    Ok(r) => r,
                };

                Ok(Box::new(NotEqualsCondition::new(left, right)))
            }
            GREATER_THAN => {
                let _ = self.token(); // consume the token
                let right = match self.parse_expression() {
                    Err(Error::UnexpectedEOF(_)) => {
                        return Err(Error::with_history(
                            format!("{GREATER_THAN} expects expressions on both sides").as_str(),
                            self.consumed(),
                        ))
                    }
                    Err(e) => return Err(e),
                    Ok(r) => r,
                };

                Ok(Box::new(GreaterThanCondition::new(left, right)))
            }
            GREATER_THAN_EQUAL => {
                let _ = self.token(); // consume the token
                let right = match self.parse_expression() {
                    Err(Error::UnexpectedEOF(_)) => {
                        return Err(Error::with_history(
                            format!("{GREATER_THAN_EQUAL} expects expressions on both sides")
                                .as_str(),
                            self.consumed(),
                        ))
                    }
                    Err(e) => return Err(e),
                    Ok(r) => r,
                };

                Ok(Box::new(GreaterThanEqualCondition::new(left, right)))
            }
            LESS_THAN => {
                let _ = self.token(); // consume the token
                let right = match self.parse_expression() {
                    Err(Error::UnexpectedEOF(_)) => {
                        return Err(Error::with_history(
                            "equals expects expressions on both sides",
                            self.consumed(),
                        ))
                    }
                    Err(e) => return Err(e),
                    Ok(r) => r,
                };

                Ok(Box::new(LessThanCondition::new(left, right)))
            }
            LESS_THAN_EQUAL => {
                let _ = self.token(); // consume the token
                let right = match self.parse_expression() {
                    Err(Error::UnexpectedEOF(_)) => {
                        return Err(Error::with_history(
                            format!("{LESS_THAN_EQUAL} expects expressions on both sides").as_str(),
                            self.consumed(),
                        ))
                    }
                    Err(e) => return Err(e),
                    Ok(r) => r,
                };

                Ok(Box::new(LessThanEqualCondition::new(left, right)))
            }
            IN => {
                let _ = self.token(); // consume the token
                let right = match self.parse_expression() {
                    Err(Error::UnexpectedEOF(_)) => {
                        return Err(Error::with_history(
                            format!("{IN} expects expressions on both sides").as_str(),
                            self.consumed(),
                        ))
                    }
                    Err(e) => return Err(e),
                    Ok(r) => r,
                };

                Ok(Box::new(InCondition::new(left, right)))
            }
            // if this doesn't match any of these, we can assume this is a unary operation
            _ => Ok(Box::new(DefaultExpressCondition::new(left))),
        }
    }

    pub fn parse_expression(&mut self) -> QueryResult<Box<dyn Expression>> {
        let left = self
            .lex
            .peak()
            .ok_or_else(|| Error::unexpected_eof(self.consumed()))?;

        match left.to_uppercase().as_str() {
            KEY_WRAP => Ok(Box::new(PathExpression::from_parser(self)?)),
            STRING_WRAP => Ok(Box::new(StringExpression::from_parser(self)?)),
            MAP_WRAP => Ok(Box::new(MapLiteral::from_parser(self)?)),
            ARRAY_WRAP => Ok(Box::new(ArrayLiteral::from_parser(self)?)),
            FN_ADD => Ok(Box::new(AddExpression::from_parser(self)?)),
            FN_MINUS => Ok(Box::new(SubtractExpression::from_parser(self)?)),
            FN_MULTIPLY => Ok(Box::new(MultiplyExpression::from_parser(self)?)),
            FN_DIVIDE => Ok(Box::new(DivideExpression::from_parser(self)?)),
            FN_MODULUS => Ok(Box::new(ModulusExpression::from_parser(self)?)),
            FN_LOWER => Ok(Box::new(StringLower::from_parser(self)?)),
            FN_UPPER => Ok(Box::new(StringUpper::from_parser(self)?)),
            FN_LENGTH => Ok(Box::new(StringLength::from_parser(self)?)),
            FN_TRIM => Ok(Box::new(StringTrim::from_parser(self)?)),
            FN_TRIM_LEFT => Ok(Box::new(StringTrimLeft::from_parser(self)?)),
            FN_TRIM_RIGHT => Ok(Box::new(StringTrimRight::from_parser(self)?)),
            FN_CONCAT => Ok(Box::new(StringConcat::from_parser(self)?)),
            FN_SPLIT => Ok(Box::new(StringSplit::from_parser(self)?)),
            TRUE => Ok(Box::new(BoolExpression::from_parser(self)?)),
            FALSE => Ok(Box::new(BoolExpression::from_parser(self)?)),
            NULL => Ok(Box::new(NullExpression::from_parser(self)?)),
            _ => self.parse_unwrapped_expression(left),
        }
    }

    fn parse_unwrapped_expression(&mut self, left: &str) -> QueryResult<Box<dyn Expression>> {
        let mut chars = left.chars();
        match chars.next() {
            Some('0'..='9') | Some('-') => Ok(Box::new(NumberExpression::from_parser(self)?)),
            _ => Ok(Box::new(PathExpression::from_parser(self)?)),
        }
    }

    fn parse_positive_number(&mut self) -> QueryResult<usize> {
        let tok = self
            .lex
            .token()
            .ok_or_else(|| Error::unexpected_eof(self.consumed()))?;

        match tok.parse::<usize>() {
            Ok(num) => Ok(num),
            Err(e) => Err(Error::with_history(
                &format!("expected number but got {}", e),
                self.consumed(),
            )),
        }
    }

    pub fn parse_string(&mut self, wrap: &str) -> QueryResult<String> {
        match self.lex.token() {
            Some(val) if val == wrap => (),
            Some(tok) => {
                return Err(Error::with_history(
                    &format!("expected {wrap} but got {tok}"),
                    self.consumed(),
                ))
            }
            None => return Err(Error::unexpected_eof(self.consumed())),
        }

        let value = self
            .lex
            .token()
            .ok_or_else(|| Error::unexpected_eof(self.consumed()))?;

        // consume the final " token, and return. If we get a different token
        // or hit EOF we can return an error
        match self.lex.token() {
            Some(val) if val == wrap => Ok(value.to_string()),
            Some(tok) => Err(Error::with_history(
                &format!("expected {wrap} but got {tok}"),
                self.consumed(),
            )),
            None => Err(Error::unexpected_eof(self.consumed())),
        }
    }

    // is_next will check to see if the next value matches the supplied
    // token.
    pub fn is_next(&mut self, tok: &str) -> bool {
        let seen = match self.lex.peak() {
            Some(seen) => seen,
            None => return false,
        };

        tok == seen.to_uppercase()
    }

    // consume_next will return an error if the next token consumed is not the
    // one supplied to the parser.
    pub fn consume_next(&mut self, expected: &str) -> QueryResult<()> {
        let seen = self
            .lex
            .token()
            .ok_or_else(|| Error::UnexpectedEOF(self.consumed().to_string()))?;

        if seen.to_uppercase() != expected {
            return Err(Error::with_history(
                &format!("expected \"{}\" but got \"{}\"", expected, seen),
                self.consumed(),
            ));
        }

        Ok(())
    }

    // consume just consumes the next token, no questions asked.
    pub fn consume(&mut self) {
        let _ = self.lex.token();
    }

    // future returns tokens in the future.
    pub fn future(&self) -> &str {
        self.lex.future()
    }

    // advance_by moves the head of the lexor forward by the defined
    // number of characters.
    pub fn advance_by(&mut self, delta: usize) {
        self.lex.advance_head(delta)
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
            Any::VecBytes(b) => self.hash_combine(city_hash_64(b.as_slice())),
            Any::F64(f) => self.hash_combine(city_hash_64(&f.to_ne_bytes())),
            Any::F32(f) => self.hash_combine(city_hash_64(&f.to_ne_bytes())),
            Any::Str(s) => self.hash_combine(city_hash_64(s.as_bytes())),
            Any::String(s) => self.hash_combine(city_hash_64(s.as_bytes())),
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
            Any::StringMap(m) => {
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
        assert_condition!(
            r#"{"a": "hello world"}"#,
            r#" split("a", ' ') == ['hello', 'world'] "#,
            true
        );

        // adding this because it's cool, we fine a word in the string
        assert_condition!(
            r#"{"a": "hello world we are going to find a word in this string"}"#,
            r#" 'going' in split("a", ' ') "#,
            true
        );
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
            r#" a.* == ['hello', 'world'] "#,
            true
        );

        // this is silly but is the same thing we are doing above, just using
        // keys directly. This is allowed because the value can be any expression
        // same with maps
        assert_conjunction!(
            r#"{"a": {"b": "hello", "c": "world"}}"#,
            r#" a.* == ["a"."b", a.c] "#,
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
        assert_conjunction!(r#"{"a": {"a": 1, "b": 2, "c": 3}}"#, r#" 2 in a.* "#, true);
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

    macro_rules! assert_select {
        ( $expr:expr, $expected:expr, $($source:expr),+) => {
            let mut parser = Parser::from($expr);
            let mut expr = parser.parse_select().unwrap();
            let sources = vec![$(serde_json::from_str($source).unwrap()),+];
            for d in sources {
                expr.process(&d);
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
            r#"SELECT sum("a") as "a"."b"."c" "#,
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
            r#"SELECT sum("a") as "sum", count("a") as "count", "b" as banana WHERE "a" > 1 GROUP BY "banana" ORDER BY "banana" "#,
            r#"[{"sum":7,"count":2,"banana":"hello"},{"sum":2,"count":1,"banana":"hi"}]"#,
            // values
            r#"{"a": 1, "b": "hi"}"#,
            r#"{"a": 2, "b": "hi"}"#,
            r#"{"a": 3, "b": "hello"}"#,
            r#"{"a": 4, "b": "hello"}"#
        );

        assert_select!(
            r#" select sum("a") as "sum", "b" GROUP BY "b" ORDER BY "sum" DESC "#,
            r#"[{"sum":95,"b":"hi"},{"sum":3,"b":"hello"}]"#,
            // values
            r#"{"a": 1, "b": "hello"}"#,
            r#"{"a": 2, "b": "hello"}"#,
            r#"{"a": 3, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#
        );

        assert_select!(
            r#" select sum("a") as "sum", "b" GROUP BY "b" ORDER BY "sum" DESC LIMIT 1"#,
            r#"[{"sum":95,"b":"hi"}]"#,
            // values
            r#"{"a": 1, "b": "hello"}"#,
            r#"{"a": 2, "b": "hello"}"#,
            r#"{"a": 3, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#
        );

        // most simple
        assert_select!(
            r#" select count() as "count" "#,
            r#"[{"count":26}]"#,
            // values
            r#"{"a": 1, "b": "hello"}"#,
            r#"{"a": 2, "b": "hello"}"#,
            r#"{"a": 3, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#,
            r#"{"a": 4, "b": "hi"}"#
        );
    }

    macro_rules! assert_composite_query {
        ( $query:expr, $expected:expr, $($source:expr),+) => {
            // create our query
            let query = Query::new($query).unwrap();
            // make it composable
            let (composable, combine) = query.composite();
            let mut combine = combine;

            // each source coming in here will be an array of values, this emulates
            // having multiple datasets.
            let sources = vec![$(serde_json::from_str::<Dapt>($source).unwrap()),+];
            for d in sources {
                // create a new composable aggregation for each dataset and
                // have it aggregate data
                let mut c = composable.clone();
                for sd in d.sub("[]").unwrap() {
                    c.process(&sd).unwrap();
                }

                // take the aggregates and pass it into the combiner
                let res = c.collect().unwrap();
                for sd in res {
                    combine.process(&sd).unwrap();
                }
            }

            // collect the results of the combiner, which should be the same
            // as running the orignal query with only one dataset.
            let result = combine.collect().unwrap();
            assert_eq!(serde_json::to_string(&result).unwrap(), $expected);
        };
    }

    #[test]
    fn test_composite_queries() {
        // expression aggregate
        assert_composite_query!(
            "select \"a\"",
            r#"[{"a":1}]"#,
            r#"[{"a": 1, "b": "hello"},{"a": 2, "b": "hello"}]"#,
            r#"[{"a": 3}]"#
        );

        // sum
        assert_composite_query!(
            "select sum(\"a\") as \"sum\"",
            r#"[{"sum":6}]"#,
            r#"[{"a": 1, "b": "hello"},{"a": 2, "b": "hello"}]"#,
            r#"[{"a": 3}]"#
        );

        // count
        assert_composite_query!(
            "select count() as \"count\"",
            r#"[{"count":3}]"#,
            r#"[{"a": 1, "b": "hello"},{"a": 2, "b": "hello"}]"#,
            r#"[{"a": 3}]"#
        );

        // avg
        assert_composite_query!(
            "select avg(\"a\") as \"avg\"",
            r#"[{"avg":2.0}]"#,
            r#"[{"a": 1, "b": "hello"},{"a": 2, "b": "hello"}]"#,
            r#"[{"a": 3}]"#
        );

        // add
        assert_composite_query!(
            "select add(sum(\"a\"), sum(\"b\")) as \"sums\"",
            r#"[{"sums":20.0}]"#,
            r#"[{"a": 1, "b": 12},{"a": 2, "b": 2}]"#,
            r#"[{"a": 3}]"#
        );

        // subtract
        assert_composite_query!(
            "select neg(sum(\"a\"), sum(\"b\")) as \"subtract\"",
            r#"[{"subtract":-8.0}]"#,
            r#"[{"a": 1, "b": 12},{"a": 2, "b": 2}]"#,
            r#"[{"a": 3}]"#
        );

        // multiply
        assert_composite_query!(
            "select mul(sum(\"a\"), sum(\"b\")) as \"multiply\"",
            r#"[{"multiply":84.0}]"#,
            r#"[{"a": 1, "b": 12},{"a": 2, "b": 2}]"#,
            r#"[{"a": 3}]"#
        );

        // divide
        assert_composite_query!(
            "select div(sum(\"a\"), sum(\"b\")) as \"divide\"",
            r#"[{"divide":0.5}]"#,
            r#"[{"a": 1, "b": 10},{"a": 2, "b": 2}]"#,
            r#"[{"a": 3}]"#
        );

        // mod
        assert_composite_query!(
            "select mod(sum(\"a\"), sum(\"b\")) as \"modulus\"",
            r#"[{"modulus":6.0}]"#,
            r#"[{"a": 1, "b": 12},{"a": 2, "b": 2}]"#,
            r#"[{"a": 3}]"#
        );

        assert_composite_query!(
            "select sum(\"a\") as \"sum\", count() as \"count\", \"b\" WHERE \"a\" > 1 GROUP BY \"b\" ORDER BY \"sum\" DESC LIMIT 2",
            r#"[{"sum":13,"count":3,"b":"what"},{"sum":6,"count":1,"b":"goodbye"}]"#,
            r#"[{"a": 1, "b": "hello"},{"a": 2, "b": "hello"},{"a":6, "b": "goodbye"}]"#,
            r#"[{"a": 3, "b":"hello"},{"a":5, "b": "what"},{"a": 3, "b":"what"},{"a":5, "b": "what"}]"#
        );
    }

    macro_rules! assert_display_query {
        ($input:expr, $output:expr) => {
            let mut parser = Parser::from($input);
            let query = parser.parse_query().unwrap();
            let query_str = format!("{}", query);
            assert_eq!(query_str, $output);
        };
    }

    #[test]
    fn test_display_query() {
        assert_display_query!(
            r#"SELECT sum("a") as "sum", count("a") as "count", "b" WHERE "a" > 1 GROUP BY "b" ORDER BY "b""#,
            "SELECT SUM(a) AS sum, COUNT(a) AS count, b AS b WHERE a > 1 HAVING true GROUP BY b ORDER BY b"
        );
    }

    macro_rules! assert_query_error {
        ($query:expr, $error:expr) => {
            let mut parser = Parser::from($query);
            let err = match parser.parse_query() {
                Ok(_) => panic!("expected error"),
                Err(e) => format!("{}", e),
            };
            assert_eq!(err, $error);
        };
    }

    #[test]
    fn test_qeury_error() {
        assert_query_error!(
            r#"something"#,
            "Invalid query: [ something ] expected \"SELECT\" but got \"something\""
        );
        assert_query_error!(
            r#"SELECT "a" WHERE "chicken" == 'a' WHERE "turkey" > 19 "#,
            "Invalid query: [ SELECT \"a\" WHERE \"chicken\" == 'a' █ WHERE \"turkey\" > 19  ]: unexpected trailing content: WHERE \"turkey\" > 19"
        );
    }
}
