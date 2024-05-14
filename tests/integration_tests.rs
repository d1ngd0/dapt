use dapt::{
    query::{Query, SelectClause, WhereClause},
    Dapt, Path,
};

#[test]
fn test_deserialize() {
    let data = r#"
            {
                "a": 1,
                "b": "hello",
                "c": [1, 2, 3],
                "d": {
                    "e": 1000,
                    "f": "world",
                    "deep": {
                        "deeper": {
                            "deepest": "hello"
                        }
                    }
                },
                "empty_array": [],
                "empty_object": {}
            }
        "#;

    // value testing
    let d: Dapt = serde_json::from_str(data).unwrap();
    assert_eq!(d.sub("d.f").unwrap().str_path(None).unwrap(), "world");
    assert_eq!(d.sub("a").unwrap().val_path::<u64>(None).unwrap(), 1);
    assert_eq!(d.str("b").unwrap(), "hello");
    assert_eq!(d.val::<u64>("d.e").unwrap(), 1000);
    assert_eq!(d.str("~.deepest").unwrap(), "hello");

    // field literal tests
    assert_eq!(
        r#"[1,2,3]"#,
        serde_json::to_string(&d.sub("c").unwrap()).unwrap()
    );

    // array index tests
    assert_eq!("2", serde_json::to_string(&d.sub("c[1]").unwrap()).unwrap());

    // array test
    assert_eq!(
        "[1,2,3]",
        serde_json::to_string(&d.sub("c[]").unwrap()).unwrap()
    );

    // empty array test
    assert_eq!(
        "[]",
        serde_json::to_string(&d.sub("empty_array").unwrap()).unwrap()
    );

    // empty object test
    assert_eq!(
        "{}",
        serde_json::to_string(&d.sub("empty_object").unwrap()).unwrap()
    );

    // wildcard tests
    assert_eq!(
        "[1000,\"world\",{\"deeper\":{\"deepest\":\"hello\"}}]",
        serde_json::to_string(&d.sub("d.*").unwrap()).unwrap()
    );

    // recursive find tests
    assert_eq!(
        "{\"deepest\":\"hello\"}",
        serde_json::to_string(&d.sub("~.deeper").unwrap()).unwrap()
    );

    // first match test
    assert_eq!(
        "{\"deepest\":\"hello\"}",
        serde_json::to_string(&d.sub("{m,d.deep.deeper,a}").unwrap()).unwrap()
    );

    // multiple matches test
    assert_eq!(
        "[{\"deepest\":\"hello\"},1]",
        serde_json::to_string(&d.sub("(m|d.deep.deeper|a)").unwrap()).unwrap()
    );

    // regex match
    assert_eq!(
        "[[],{}]",
        serde_json::to_string(&d.sub("/empty.*/").unwrap()).unwrap()
    );

    // recursive regex
    assert_eq!(
        "[{\"deeper\":{\"deepest\":\"hello\"}},{\"deepest\":\"hello\"},\"hello\"]",
        serde_json::to_string(&d.sub("~./deep.*/").unwrap()).unwrap()
    );
}

macro_rules! test_path {
    ($path:expr) => {
        let p = Path::try_from($path).unwrap();
        assert_eq!($path, p.to_string());
    };
}

#[test]
fn test_path() {
    test_path!("a");
    test_path!("a.{a,b}");
    test_path!("a.(a|b)");
    test_path!("/regex/");
    test_path!("re.~.crusive");
    test_path!("a[1].*");
    test_path!("a[1].{a,b}");
    test_path!("a[1].(a|b)");
    test_path!("a[1].~.b");

    let p = Path::try_from("a.{invalid|path}");
    assert!(p.is_err());
}

#[test]
fn test_filter() {
    let data = r#"
            {
                "a": 1,
                "b": "hello",
                "c": [1, 2, 3],
                "d": {
                    "e": 1000,
                    "f": "world",
                    "deep": {
                        "deeper": {
                            "deepest": "hello"
                        }
                    }
                },
                "empty_array": [],
                "empty_object": {}
            }
        "#;

    let d: Dapt = serde_json::from_str(data).unwrap();

    let f = WhereClause::new("WHERE \"~.deepest\" == 'hello'").unwrap();
    assert_eq!(f.filter(&d).unwrap(), true);

    // c[1] == 2
    let f =
        WhereClause::new("WHERE add(\"c[1]\", 4) > 5 AND \"d.*.deeper\" == {\"deepest\": 'hello'}")
            .unwrap();
    assert_eq!(f.filter(&d).unwrap(), true);
}

macro_rules! assert_select {
    ( $expr:expr, $expected:expr, $($source:expr),+) => {
        let mut select = SelectClause::new($expr).unwrap();
        let sources = vec![$(serde_json::from_str($source).unwrap()),+];
        for d in sources {
            let _ = select.process(&d);
        }
        let result = select.collect().unwrap();
        assert_eq!(serde_json::to_string(&result).unwrap(), $expected);
    };
}

#[test]
fn test_select() {
    // transform and aggregation
    assert_select!(
        r#" SELECT "a" as "data.first", "b" as "data.second", sum("c") as "sum" "#,
        r#"{"data":{"first":1,"second":"hello"},"sum":6}"#,
        r#"{"a":1,"b":"hello","c":[1,2,3]}"#,
        r#"{"a":1,"b":"hello"}"#
    );

    assert_select!(
        r#" SELECT count(), count("c") as "c_count", sum("c") as "sum" "#,
        r#"{"COUNT()":2,"c_count":1,"sum":6}"#,
        r#"{"a":1,"b":"hello","c":[1,2,3]}"#,
        r#"{"a":1,"b":"hello"}"#
    );
}

macro_rules! assert_query {
    ( $expr:expr, $expected:expr, $($source:expr),+) => {
        let mut query = Query::new($expr).unwrap();
        let sources = vec![$(serde_json::from_str($source).unwrap()),+];
        for d in sources {
            let _ = query.process(&d);
        }
        let result = query.collect().unwrap();
        assert_eq!(serde_json::to_string(&result).unwrap(), $expected);
    };
}

#[test]
fn test_query() {
    // Simple
    assert_query!(
        r#" SELECT "a" as "data.first""#,
        r#"[{"data":{"first":1}}]"#,
        r#"{"a":1,"b":"hello","c":[1,2,3],"test":"simple"}"#,
        r#"{"a":1,"b":"hello","test":"simple"}"#
    );

    // Filter
    assert_query!(
        r#" SELECT "a" as "data.first" WHERE "b" == 'bye' "#,
        r#"[{"data":{"first":2}}]"#,
        r#"{"a":1,"b":"hello","c":[1,2,3],"test":"filter"}"#,
        r#"{"a":1,"b":"hello","test":"filter"}"#,
        r#"{"a":2,"b":"bye","test":"filter"}"#
    );

    // expression Filter
    assert_query!(
        r#" SELECT "a" as "data.first" WHERE true "#,
        r#"[{"data":{"first":1}}]"#,
        r#"{"a":1,"b":"hello","c":[1,2,3],"test":"expression_filter"}"#,
        r#"{"a":1,"b":"hello","test":"expression_filter"}"#,
        r#"{"a":2,"b":"bye","test":"expression_filter"}"#
    );

    // Group and order
    assert_query!(
        r#" SELECT "a" as "data.first", "b" GROUP BY "b" ORDER BY "b" DESC"#,
        r#"[{"data":{"first":1},"b":"hello"},{"data":{"first":2},"b":"bye"}]"#,
        r#"{"a":1,"b":"hello","c":[1,2,3],"test":"group_and_order"}"#,
        r#"{"a":1,"b":"hello","test":"group_and_order"}"#,
        r#"{"a":2,"b":"bye","test":"group_and_order"}"#
    );

    // having
    assert_query!(
        r#" SELECT sum("a") as "sum", "b" HAVING "sum" > 5 GROUP BY "b" ORDER BY "b" "#,
        r#"[{"sum":6,"b":"hello"}]"#,
        r#"{"a":5,"b":"hello","test":"having"}"#,
        r#"{"a":1,"b":"hello","test":"having"}"#,
        r#"{"a":2,"b":"bye","test":"having"}"#
    );

    // Sum non existant (empty dapt packets don't get added)
    assert_query!(
        r#" SELECT sum("nope") as "sum""#,
        r#"[]"#,
        r#"{"a":1,"b":"hello","c":[1,2,3],"test":"simple"}"#,
        r#"{"a":1,"b":"hello","test":"simple"}"#
    );

    // Sum non existant with b (empty dapt packets don't get added)
    assert_query!(
        r#" SELECT sum("nope") as "sum", "b" GROUP BY "b" ORDER BY "b" DESC"#,
        r#"[{"b":"hello"},{"b":"bye"}]"#,
        r#"{"a":1,"b":"hello","c":[1,2,3],"test":"simple"}"#,
        r#"{"a":1,"b":"hello","test":"simple"}"#,
        r#"{"a":2,"b":"bye","test":"having"}"#
    );

    // Where non existant
    assert_query!(
        r#" SELECT sum("a") as "sum" WHERE "nope" == 'hey' "#,
        r#"[]"#,
        r#"{"a":1,"b":"hello","c":[1,2,3],"test":"simple"}"#,
        r#"{"a":1,"b":"hello","test":"simple"}"#,
        r#"{"a":2,"b":"bye","test":"having"}"#
    );

    // having non existant
    assert_query!(
        r#" SELECT sum("a") as "sum" HAVING "nope" > 45 "#,
        r#"[]"#,
        r#"{"a":1,"b":"hello","c":[1,2,3],"test":"simple"}"#,
        r#"{"a":1,"b":"hello","test":"simple"}"#,
        r#"{"a":2,"b":"bye","test":"having"}"#
    );
}
