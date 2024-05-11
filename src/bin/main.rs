use std::{
    env,
    io::{self, BufRead},
    process,
    time::SystemTime,
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

    let mut n = SystemTime::now();

    let stdin = io::stdin();
    let lines = stdin.lock().lines();

    for line in lines {
        let d: Dapt = match serde_json::from_str(line.unwrap().as_str()) {
            Ok(d) => d,
            Err(_) => continue,
        };

        let _ = q.process(&d);
        if SystemTime::now().duration_since(n).unwrap().as_secs() > 1 {
            let res = match q.collect() {
                Ok(res) => res,
                Err(_) => continue,
            };

            for r in res.iter() {
                println!("{}", serde_json::to_string(&r).unwrap());
            }
            n = SystemTime::now();
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
