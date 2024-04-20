use dapt::{Dapt, Path};

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
