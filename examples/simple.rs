use dapt::Dapt;

fn main() {
    let data = r#"
    {
        "name": "John Doe",
        "age": 30,
        "phones": [
            {
                "type": "home",
                "number": "212 555-1234"
            },
            {
                "type": "office",
                "number": "646 555-4567"
            }
        ]
    }"#;

    let d: Dapt = serde_json::from_str(data).unwrap();
    let name = d.sub("phones[].number").unwrap();
    println!("{}", serde_json::to_string_pretty(&name).unwrap());
}
