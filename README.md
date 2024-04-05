## Dapt

Dapt (data packet) is a rust object that allows for serialization and deserialization of dynamic data utilizing the `serde` crate. Dapt allows you to traverse your data structure with a `jq` ish syntax. The intent behind the project is to act as the data packet for a stream processing engine which handles unstructured data, though it is generic enough to be used for many things.

## Example

```rust
use dapt::Dapt;

fn main() {
}
```
