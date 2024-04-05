use std::env;

use dapt::Dapt;

fn main() {
    let args: Vec<String> = env::args().collect();
    // deserialize the json into a dapt object
    let d: Dapt = serde_json::from_str(&args[1]).unwrap();

    // get the path specified
    let d = d.get(&args[2]).unwrap();

    // re serialize the data and print it out
    println!("{}", serde_json::to_string(&d).unwrap());
}
