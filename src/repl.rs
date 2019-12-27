use std::io;
use std::io::Write;

use crate::interpreter::{Interpreter, InterpreterError};
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
    let mut program = String::new();

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
            program = format!("{}{}", program, line);
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
    program
}

pub fn run() {
    let mut interpreter = Interpreter::new();
    loop {
        let program = read_balanced_input();
        if !program.is_empty() {
            let tree = Tree::root("progn".to_owned());
            match interpreter.parse(tree, &program) {
                Ok(_) => println!(),
                Err(InterpreterError::InvalidSyntax { message }) => {
                    println!("parse error: {}", message);
                    continue;
                }
                _ => continue,
            }
        } else {
            println!();
            break;
        }
    }
}
