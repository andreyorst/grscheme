use std::io;
use std::io::Write;

use crate::parser::{Parser, ParseError};
use crate::tree::Tree;

fn read_balanced_input() -> String {
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

    loop {
        io::stdin().read_line(&mut line).unwrap_or_default();
        for c in line.chars() {
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
                println!("read_balanced_input: error, unexpected '{}'", c);
                return String::from("\n");
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
    expression
}

pub fn run() {
    let mut parser = Parser::new();
    loop {
        let expression = read_balanced_input();
        if !expression.is_empty() {
            match parser.parse(&expression) {
                Ok(t) => Tree::print_tree(&t),
                Err(ParseError::InvalidSyntax { message }) => {
                    println!("parse error: {}", message);
                    continue;
                }
            }
        } else {
            println!();
            break;
        }
    }
}
