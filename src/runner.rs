use crate::evaluator::Evaluator;
use crate::reader::Reader;
use std::env;
use std::fs;

const VERSION: Option<&'static str> = option_env!("CARGO_PKG_VERSION");

pub fn run() -> Result<(), u32> {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.len() == 1 {
        let file = args[0].clone();
        let contents = match fs::read_to_string(file) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("error {}", e);
                return Err(1);
            }
        };
        let mut reader = Reader::new();
        let res = match reader.parse(&contents) {
            Ok(t) => t,
            Err(e) => {
                eprintln!("read error: {}", e);
                return Err(2);
            }
        };
        let mut evaluator = Evaluator::new();
        match evaluator.eval(&res) {
            Ok(res) => Evaluator::print(&res),
            Err(e) => {
                eprintln!("read error: {}", e);
                return Err(3);
            }
        }
    } else if args.is_empty() {
        repl();
    }
    Ok(())
}

fn repl() {
    eprint!("Welcome to GRScheme");
    if let Some(v) = VERSION {
        eprintln!(" v{}.",v);
    } else {
        eprintln!(".");
    }

    let mut reader = Reader::new();
    let mut evaluator = Evaluator::new();
    loop {
        let expression = match Reader::balanced_read("> ") {
            Ok(expr) => expr,
            Err(e) => {
                eprintln!("read error: {}", e);
                continue;
            }
        };
        if !expression.is_empty() {
            let expression = expression.trim().to_owned();
            if !expression.is_empty() {
                match reader.parse(&expression) {
                    Ok(t) => {
                        for subexpr in t.borrow_mut().siblings.iter().skip(1) {
                            subexpr.borrow_mut().parent = None;
                            match evaluator.eval(subexpr) {
                                Ok(res) => Evaluator::print(&res),
                                Err(e) => eprintln!("evaluation error: {}", e),
                            }
                        }
                    }
                    Err(e) => eprintln!("read error: {}", e),
                }
            }
        } else {
            println!();
            break;
        }
    }
}
