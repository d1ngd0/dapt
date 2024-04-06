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
    assert_eq!(d.get("d.f").unwrap().str(), Some("world"));
    assert_eq!(d.get("a").unwrap().val::<usize>(), Some(1));
    assert_eq!(d.get("b").unwrap().str(), Some("hello"));
    assert_eq!(d.get("d.e").unwrap().val::<usize>(), Some(1000));
    assert_eq!(d.get("~.deepest").unwrap().str(), Some("hello"));

    // field literal tests
    assert_eq!(
        r#"[1,2,3]"#,
        serde_json::to_string(&d.get("c").unwrap()).unwrap()
    );

    // array index tests
    assert_eq!("2", serde_json::to_string(&d.get("c[1]").unwrap()).unwrap());

    // array test
    assert_eq!(
        "[1,2,3]",
        serde_json::to_string(&d.get("c[]").unwrap()).unwrap()
    );

    // empty array test
    assert_eq!(
        "[]",
        serde_json::to_string(&d.get("empty_array").unwrap()).unwrap()
    );

    // empty object test
    assert_eq!(
        "{}",
        serde_json::to_string(&d.get("empty_object").unwrap()).unwrap()
    );

    // wildcard tests
    assert_eq!(
        "[1000,\"world\",{\"deeper\":{\"deepest\":\"hello\"}}]",
        serde_json::to_string(&d.get("d.*").unwrap()).unwrap()
    );

    // recursive find tests
    assert_eq!(
        "{\"deepest\":\"hello\"}",
        serde_json::to_string(&d.get("~.deeper").unwrap()).unwrap()
    );

    // first match test
    assert_eq!(
        "{\"deepest\":\"hello\"}",
        serde_json::to_string(&d.get("{m,d.deep.deeper,a}").unwrap()).unwrap()
    );

    // multiple matches test
    assert_eq!(
        "[{\"deepest\":\"hello\"},1]",
        serde_json::to_string(&d.get("(m|d.deep.deeper|a)").unwrap()).unwrap()
    );

    // regex match
    assert_eq!(
        "[[],{}]",
        serde_json::to_string(&d.get("/empty.*/").unwrap()).unwrap()
    );

    // recursive regex
    assert_eq!(
        "[{\"deeper\":{\"deepest\":\"hello\"}},{\"deepest\":\"hello\"},\"hello\"]",
        serde_json::to_string(&d.get("~./deep.*/").unwrap()).unwrap()
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
    println!("{:?}", p);
    assert!(p.is_err());
}
