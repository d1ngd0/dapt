use std::{
    env,
    io::{self, BufRead},
    process,
};

use dapt::{query::Query, Dapt};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} query", args[0]);
        process::exit(1);
    }

    let mut q = match Query::new(&args[1]) {
        Ok(q) => q,
        Err(e) => {
            eprintln!("Error: {}", e);
            process::exit(1);
        }
    };

    let stdin = io::stdin();
    let lines = stdin.lock().lines();

    for line in lines {
        let d: Dapt = match serde_json::from_str(line.unwrap().as_str()) {
            Ok(d) => d,
            Err(_) => continue,
        };

        let res = match q.process_and_collect(&d) {
            Ok(res) => res,
            Err(_) => continue,
        };

        for r in res.iter().flatten() {
            println!("{}", serde_json::to_string(&r).unwrap());
        }
    }

    let res = match q.collect() {
        Ok(res) => res,
        Err(_) => return,
    };

    for r in res.iter() {
        println!("{}", serde_json::to_string(&r).unwrap());
    }
}
