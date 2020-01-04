use std::io;
use std::io::Write;

use crate::evaluator::Evaluator;
use crate::parser::{ParseError, Parser};

enum ReplError {
    InvalidInput { message: String },
}

fn read_balanced_input() -> Result<String, ReplError> {
    let mut paren_count: i32 = 0;
    let mut bracket_count: i32 = 0;
    let mut curly_count: i32 = 0;
    let mut angle_count: i32 = 0;
    let mut escaped = false;
    let mut inside_string = false;
    let mut comment = false;
    let mut line = String::new();
    let mut expression = String::new();

    print!("> ");
    io::stdout().flush().ok();

    let mut current_line = 0;
    loop {
        current_line += 1;
        io::stdin().read_line(&mut line).unwrap_or_default();
        for (current_column, c) in line.chars().enumerate() {
            if !escaped && !inside_string && !comment {
                match c {
                    '(' => paren_count += 1,
                    ')' => paren_count -= 1,
                    '[' => bracket_count += 1,
                    ']' => bracket_count -= 1,
                    '{' => curly_count += 1,
                    '}' => curly_count -= 1,
                    '<' => angle_count += 1,
                    '>' => angle_count -= 1,
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
            if paren_count < 0 || curly_count < 0 || bracket_count < 0 || angle_count < 0 {
                return Err(ReplError::InvalidInput {
                    message: format!(
                        "unexpected \"{}\", line: {}, col: {}",
                        c,
                        current_line,
                        current_column + 1
                    ),
                });
            }
        }
        comment = false;
        if !line.is_empty() {
            expression = format!("{}{}", expression, line);
            line.clear();
        }
        if paren_count == 0
            && curly_count == 0
            && bracket_count == 0
            && angle_count == 0
            && !inside_string
        {
            break;
        }
    }
    Ok(expression)
}

pub fn run() {
    let mut parser = Parser::new();
    let mut evaluator = Evaluator::new();
    loop {
        let expression = match read_balanced_input() {
            Ok(expr) => expr,
            Err(ReplError::InvalidInput { message }) => {
                println!("read error: {}", message);
                continue;
            }
        };
        if !expression.is_empty() {
            let expression = expression.trim().to_owned();
            if !expression.is_empty() {
                match parser.parse(&expression) {
                    Ok(t) => evaluator.eval(&t),
                    Err(ParseError::InvalidSyntax { message }) => {
                        println!("parse error: {}", message);
                        continue;
                    }
                }
            }
        } else {
            println!();
            break;
        }
    }
}
