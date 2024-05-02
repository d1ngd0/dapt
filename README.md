## Dapt

Dapt is in heavy development, any use in production might see breaking changes. If you have any suggestions or issues please open an issue or PR.

![example workflow](https://github.com/d1ngd0/dapt/actions/workflows/rust.yml/badge.svg)
[![Crates.io](https://img.shields.io/crates/v/dapt)](https://crates.io/crates/dapt)
[![Docs.rs](https://docs.rs/dapt/badge.svg)](https://docs.rs/dapt)

Dapt (data packet) is a rust object that allows for serialization and deserialization of dynamic data utilizing the `serde` crate. Dapt allows you to traverse your data structure with a `jq` ish syntax. The intent behind the project is to act as the data packet for a stream processing engine which handles unstructured data, though it is generic enough to be used for many things.

## Example

```rust
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
    }

    // use serde_json to parse the data into a Dapt object
    let d: Dapt = serde_json::from_str(data).unwrap();

    // get just the phones numbers
    let name = d.get("phones[].number").unwrap();

    // write the data back out
    println!("{}", serde_json::to_string_pretty(&name).unwrap();
}

// output will be:
// [
//   "212 555-1234",
//   "646 555-4567"
// ]
```

run it yourself with `cargo run --example simple`

## Matching Strategy

Dapt is made up of two key parts:

- A byte slice which holds the data in a binary format
- A vector of indexes to locations within that binary data

This means that a dapt packet can be pointing to multiple locations in a dapt packet at once. As a user you can traverse the dapt packet using the `get` method. This method traverses the packet, returning a new packet with new indexes that match the path you provided. The underlying binary data is not copied during this process, reducing heap allocations.

Dapt paths are nodes seperated by `.`. Each part of the path looks at the document to find **ALL** matching values. Lets step through the previous example to get a better idea of how dapt works. Our path was `phones[].number`. First dapt uses the node `phone` to traverse down the document:

```json
[
  {
    "type": "home",
    "number": "212 555-1234"
  },
  {
    "type": "office",
    "number": "646 555-4567"
  }
]
```

Next dapt uses `[]` to traverse the array. The array node allows you to specify an index, though because we didn't our dapt packet will now point to both indexes in the array.

```json
{
  "type": "home",
  "number": "212 555-1234"
}

{
  "type": "office",
  "number": "646 555-4567"
}
```

Now when we use the `number` node, it will return on the `number` field of both objects. This is because each location we point to is traversed with the same node.

```json
"212 555-1234"

"646 555-4567"
```

When we ask serde to serialize this, dapt realizes that it points to multiple locations, and will serialize an array of it's pointers. These values don't have to be the same type, check out the `mixed` example to see how dapt handles this.

## Available Path Nodes

- Field Literal: ex. `host.name` This is the most basic node, it matches a field with the exact name.
- Array: ex. `hosts[0].name` Matches all indexes in an array. Index is optional, if supplied it will only match items with that index, otherwise matches everything
- Array Wildcard: ex. `*.name` Matches all children of a map. Is only one level deep
- Recursive: ex. `~.name` Recursively searches downward for the matching node. Any Node can follow a recursive node, the only requirement is it has some child
- Regex: ex. `/^host.*/.name` Matches all fields that match the regex. The regex is a rust regex, and is matched against the field name
- First: ex. `host.{name,ip.*}` Matches the first node that returns values. Each child of a match is a full path. If the child *could* match multiple values, it will still only match the first value.
- Multi: ex. `host.(name|ip.*)` Matches all paths specified

# Query Features

dapt has query features which are defined in the `query` module. You can use the `Select` struct to aggregate data, the `Filter` struct to filter data, or the `Query` filter to utilize an implementation of SQL like queries.

## Query

I could tease you with filters and aggregations first, but lets just show off what we can do. Let's say you have data coming in which has something like the following structure:

```json
{
  "tickets_purchased": "3",
  "state": "NY",
  "name": "John Doe",
  "purchase_date": "2021-01-01",
}
```

You could then write a query such as:

```SQL
SELECT
  sum("tickets_purchased") as "total.tickets",
  count() as "total.purchases",
  "name"
WHERE
  "state" IN ['NY', 'CA']
HAVING "total_tickets" > 20
GROUP BY "name"
ORDER BY "total_tickets" DESC
TOP 3
```

This query would return something similar to:

```json
[
  {
    "total": {
      "tickets": 30,
      "purchases": 10
    },
    "name": "John Doe"
  },
  {
    "total": {
      "tickets": 25,
      "purchases": 5
    },
    "name": "Jane Doe"
  },
  {
    "total": {
      "tickets": 20,
      "purchases": 5
    },
    "name": "John Smith"
  }
]
```

The interface for creating a query and collecting data is quite easy:

```rust
use dapt::query::Query;
use dapt::Dapt;

fn main() {
  let q = Query::new("SELECT sum(\"tickets_purchased\") as \"total.tickets\", count() as \"total.purchases\", \"name\" WHERE \"state\" IN ['NY', 'CA'] HAVING \"total.tickets\" > 20 GROUP BY \"name\" ORDER BY \"total.tickets\" DESC TOP 3");

  // load up some data
  let data: Vec<Dapt> = vec![
    serde_json::from_str(r#"{"tickets_purchased": "3", "state": "NY", "name": "John Doe", "purchase_date": "2021-01-01"}"#).unwrap(),
    serde_json::from_str(r#"{"tickets_purchased": "30", "state": "NY", "name": "John Doe", "purchase_date": "2021-01-01"}"#).unwrap(),
    serde_json::from_str(r#"{"tickets_purchased": "25", "state": "NY", "name": "Jane Doe", "purchase_date": "2021-01-01"}"#).unwrap(),
    serde_json::from_str(r#"{"tickets_purchased": "20", "state": "NY", "name": "John Smith", "purchase_date": "2021-01-01"}"#).unwrap(),
  ];

  for d in data {
    q.process(d);
  }

  let results = q.results();
}
```
