// mod evaluator;
mod reader;
mod repl;
mod tree;
// use crate::evaluator::Evaluator;
use crate::reader::Reader;
use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.len() == 1 {
        let file = args[0].clone();
        let contents = match fs::read_to_string(file) {
            Ok(c) => c,
            Err(e) => {
                println!("error {}", e);
                process::exit(1);
            }
        };
        let mut reader = Reader::new();
        let res = match reader.parse(&contents) {
            Ok(t) => t,
            Err(_) => process::exit(2),
        };
        println!("{}", res.borrow().to_string());
        // let mut evaluator = Evaluator::new();
        // match evaluator.eval(&res) {
        //     Ok(res) => Evaluator::print(&res),
        //     Err(_) => process::exit(3),
        // }
    } else if args.is_empty() {
        repl::run();
    }
}
