use std::io;
use std::io::Write;

// use crate::evaluator::{EvalError, Evaluator};
use crate::reader::{ParseError, Reader};

pub enum ReplError {
    InvalidInput {
        character: char,
        line: u32,
        column: u32,
    },
}

pub fn run() {
    let mut reader = Reader::new();
    // let mut evaluator = Evaluator::new();
    loop {
        let expression = match read_balanced_input("> ") {
            Ok(expr) => expr,
            Err(ReplError::InvalidInput {
                character,
                line,
                column,
            }) => {
                println!(
                    "read error: unexpected character {} at line {}, column: {}",
                    character, line, column
                );
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
                            // match evaluator.eval(subexpr) {
                            //     Ok(res) => Evaluator::print(&res),
                            //     Err(e) => match e {
                            //         EvalError::GeneralError { message } => {
                            //             println!("eval error: {}", message)
                            //         }
                            //         EvalError::UnknownProc { name } => {
                            //             println!("unknown procedure \"{}\"", name)
                            //         }
                            //         EvalError::WrongArgAmount {
                            //             procedure,
                            //             expected,
                            //             fact,
                            //         } => println!(
                            //             "wrong amount of arguments to \"{}\": expected {}, got {}",
                            //             procedure, expected, fact
                            //         ),
                            //         EvalError::UnboundIdentifier { name } => {
                            //             println!("unbound identifier \"{}\"", name)
                            //         }
                            //     },
                            // }
                        }
                    }
                    Err(e) => match e {
                        ParseError::InvalidSyntax { message } => {
                            println!("parse error: {}", message);
                            continue;
                        }
                        ParseError::UnmatchedParenthesis { line, column } => {
                            println!(
                                "parse error: unmatched parenthesis at line {}, column {}",
                                line, column
                            );
                            continue;
                        }
                        ParseError::MismatchedParenthesis {
                            line,
                            column,
                            expected,
                            fact,
                        } => {
                            println!("parse error: mismatched parenthesis: expected {}, got {} at line {}, column {}", expected, fact, line, column);
                            continue;
                        }
                        ParseError::UnexpectedExpressionEnd { line, column } => println!(
                            "unexpected expression end reached at line: {}, column: {}",
                            line, column
                        ),
                    },
                }
            }
        } else {
            println!();
            break;
        }
    }
}
