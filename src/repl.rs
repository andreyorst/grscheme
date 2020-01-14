use std::io;
use std::io::Write;

use crate::evaluator::{EvalError, Evaluator};
use crate::parser::{ParseError, Parser};

pub enum ReplError {
    InvalidInput {
        character: char,
        line: u32,
        column: u32,
    },
}

pub fn read_balanced_input(prompt: &str) -> Result<String, ReplError> {
    let mut paren_count: i32 = 0;
    let mut bracket_count: i32 = 0;
    let mut curly_count: i32 = 0;
    let mut escaped = false;
    let mut inside_string = false;
    let mut comment = false;
    let mut line = String::new();
    let mut expression = String::new();

    print!("{}", prompt);
    io::stdout().flush().ok();

    let mut line_n = 0;
    loop {
        line_n += 1;
        io::stdin().read_line(&mut line).unwrap_or_default();
        for (column_n, c) in line.chars().enumerate() {
            if !escaped && !inside_string && !comment {
                match c {
                    '(' => paren_count += 1,
                    ')' => paren_count -= 1,
                    '[' => bracket_count += 1,
                    ']' => bracket_count -= 1,
                    '{' => curly_count += 1,
                    '}' => curly_count -= 1,
                    '"' => inside_string = true,
                    '\\' => escaped = true,
                    ';' => {
                        comment = true;
                        continue;
                    }
                    _ => (),
                }
            } else if escaped {
                escaped = false;
            } else if inside_string && c == '"' {
                inside_string = false;
            }
            if paren_count < 0 || curly_count < 0 || bracket_count < 0 {
                return Err(ReplError::InvalidInput {
                    character: c,
                    line: line_n,
                    column: column_n as u32 + 1,
                });
            }
        }
        comment = false;
        if !line.is_empty() {
            expression.push_str(&line);
            line.clear();
        }
        if paren_count == 0 && curly_count == 0 && bracket_count == 0 && !inside_string {
            break;
        } else {
            for _ in 0..prompt.len() {
                print!(" ");
            }
            io::stdout().flush().ok();
        }
    }
    Ok(expression)
}

pub fn run() {
    let mut parser = Parser::new();
    let mut evaluator = Evaluator::new();
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
                match parser.parse(&expression) {
                    Ok(t) => {
                        for subexpr in t.borrow_mut().childs.iter().skip(1) {
                            subexpr.borrow_mut().parent = None;
                            match evaluator.eval(subexpr) {
                                Ok(res) => Evaluator::print(&res),
                                Err(e) => match e {
                                    EvalError::GeneralError { message } => {
                                        println!("eval error: {}", message)
                                    }
                                    EvalError::UnknownProc { name } => {
                                        println!("unknown procedure \"{}\"", name)
                                    }
                                    EvalError::WrongArgAmount {
                                        procedure,
                                        expected,
                                        fact,
                                    } => println!(
                                        "wrong amount of arguments to \"{}\": expected {}, got {}",
                                        procedure, expected, fact
                                    ),
                                    EvalError::UnboundIdentifier { name } => {
                                        println!("unbound identifier \"{}\"", name)
                                    }
                                },
                            }
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
