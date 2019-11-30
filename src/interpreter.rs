use std::collections::HashMap;
use std::io;
use std::io::Write;

use crate::identifier::{Identifier, Type};
use crate::stack_item::StackItem;
use crate::token::*;

#[derive(Debug)]
pub struct Interpreter {
    pub stack: Vec<StackItem>,
    pub scope_stack: Vec<HashMap<String, Identifier>>,
}

enum InterpreterError {
    ArgAmount,
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            stack: vec![],
            scope_stack: vec![HashMap::new()],
        }
    }

    fn parse(&mut self, program: &str) -> Option<String> {
        let mut comment = false;
        let mut inside_word;
        let mut word = String::new();

        for c in program.chars() {
            if !comment {
                match c {
                    '(' => {
                        self.stack_push(&c.to_string());
                        continue;
                    }
                    ')' => {
                        if !word.is_empty() {
                            self.stack_push(&word);
                            word.clear();
                        }
                        self.stack_push(&c.to_string());
                        inside_word = false;
                    }
                    '\'' => {
                        self.stack_push(&c.to_string());
                        continue;
                    }
                    ' ' | '\t' | '\n' => inside_word = false,
                    ';' => {
                        comment = true;
                        continue;
                    }
                    _ => inside_word = true,
                }
                if inside_word {
                    word.push(c);
                } else if !word.is_empty() {
                    self.stack_push(&word);
                    word.clear();
                }
            } else if c == '\n' {
                comment = false;
            }

            if let Token::Apply = self.get_last_token() {
                self.apply();
            };
        }

        match self.stack.pop() {
            Some(x) => Some(x.data),
            None => None,
        }
    }

    fn stack_push(&mut self, item: &str) {
        let token = self.tokenize(&item);
        self.stack.push(StackItem::from(token, &item));
    }

    fn get_last_token(&self) -> Token {
        match self.stack.last() {
            Some(x) => x.token.clone(),
            None => Token::None,
        }
    }

    fn apply(&mut self) {
        let mut operands: Vec<String> = Vec::new();
        let mut procedure = String::new();

        loop {
            let item = match self.stack.pop() {
                Some(x) => x,
                None => panic!("Empty stack"),
            };
            match item.token {
                Token::Apply => {
                    if let Token::List { .. } = self.get_last_token() {
                        operands.push(item.data)
                    }
                }
                Token::Eval => break,
                Token::Quote => {
                    procedure = "quote".to_owned();
                    break;
                }
                Token::List { .. } => operands.push(item.data),
                Token::Procedure
                | Token::Lambda {
                    paren_count: 0,
                    state: State::Wait,
                } => procedure = item.data,
                Token::Id | _ => operands.push(item.data),
            }
        }
        self.eval(&procedure, &operands);
    }

    fn eval(&mut self, procedure: &str, operands: &[String]) {
        let operands: Vec<&String> = operands.iter().rev().collect();
        println!("apply '{}' to {:?}", procedure, operands);
        let res: Option<String> = match procedure {
            "define" => {
                let _ = self.define(&operands);
                None
            }
            "lambda" => None,
            "quote" => Interpreter::quote(&operands),
            "list" => Interpreter::list(&operands),
            "+" | "-" | "*" | "/" => Interpreter::eval_math(&operands, procedure),
            "<" | "<=" | "=" | "=>" | ">" => Interpreter::eval_cmp(&operands, procedure),
            &_ => None,
        };
        match res {
            Some(x) => self.stack_push(&x),
            None => panic!("error evaluating the procedure {}", procedure),
        }
    }

    fn define(&mut self, operands: &[&String]) -> Result<(), InterpreterError> {
        if operands.len() == 2 {
            let name = operands[0].clone();
            let _data = self.get_id_value(name);
            Ok(())
        } else {
            Err(InterpreterError::ArgAmount)
        }
    }

    fn quote(operands: &[&String]) -> Option<String> {
        let mut res = String::from("'");
        for (i, item) in operands.iter().enumerate() {
            res.push_str(item);
            if i + 1 < operands.len() {
                let next = operands[i + 1];
                match next.to_owned().as_ref() {
                    ")" => (),
                    &_ => match item.to_owned().as_ref() {
                        "(" => (),
                        &_ => res.push(' '),
                    },
                }
            }
        }
        Some(res)
    }

    fn list(_operands: &[&String]) -> Option<String> {
        Some("".to_owned())
    }

    fn eval_math(operands: &[&String], op: &str) -> Option<String> {
        let mut res: i32;
        if operands.len() >= 2 || (operands.len() == 1 && op == "-") {
            res = match operands[0].trim().parse() {
                Ok(x) => x,
                Err(_) => return None,
            };
        } else {
            return None;
        }
        if operands.len() == 1 && op == "-" {
            res = -res;
        } else {
            for value in operands.iter().skip(1) {
                let val: i32 = match value.trim().parse() {
                    Ok(x) => x,
                    Err(_) => return None,
                };

                match op {
                    "+" => res += val,
                    "-" => {
                        if operands.len() == 1 {
                            res = -val;
                        } else {
                            res -= val;
                        }
                    }
                    "*" => res *= val,
                    "/" => {
                        if val != 0 {
                            res /= val;
                        } else {
                            return None;
                        }
                    }
                    &_ => return None,
                }
            }
        }
        Some(res.to_string())
    }

    fn eval_cmp(operands: &[&String], op: &str) -> Option<String> {
        if operands.len() < 2 {
            return None;
        }
        let mut res = false;
        let mut left: i32 = match operands[0].trim().parse() {
            Ok(x) => x,
            Err(_) => return None,
        };
        for value in operands.iter().skip(1) {
            let right: i32 = match value.trim().parse() {
                Ok(x) => x,
                Err(_) => return None,
            };
            res = match op {
                "=" => left == right,
                "<" => left < right,
                "<=" => left <= right,
                ">" => left > right,
                ">=" => left >= right,
                &_ => panic!("wrong operator"),
            };
            left = right;
        }

        if res {
            Some("#t".to_string())
        } else {
            Some("#f".to_string())
        }
    }

    fn tokenize(&mut self, word: &str) -> Token {
        let last_token = self.get_last_token();

        match word {
            "(" => match last_token {
                Token::Quote => Token::List {
                    paren_count: 0,
                    state: State::Wait,
                },
                Token::List { paren_count, .. } => Token::List {
                    paren_count: paren_count + 1,
                    state: State::Body,
                },
                Token::Lambda { paren_count, state } => Token::Lambda {
                    paren_count: paren_count + 1,
                    state: match state {
                        State::Wait => State::Args,
                        State::Args => State::Body,
                        State::Body => State::Body,
                    },
                },
                _ => Token::Eval,
            },
            ")" => match last_token {
                Token::List { paren_count, .. } => {
                    if paren_count == 0 {
                        Token::Apply
                    } else {
                        Token::List {
                            paren_count: paren_count - 1,
                            state: State::Body,
                        }
                    }
                }
                Token::Lambda { paren_count, state } => {
                    if paren_count == 0 {
                        Token::Apply
                    } else {
                        Token::Lambda {
                            paren_count: paren_count - 1,
                            state,
                        }
                    }
                }

                _ => Token::Apply,
            },
            "lambda" => Token::Lambda {
                paren_count: 0,
                state: State::Wait,
            },
            "'" | "quote" => Token::Quote,
            &_ => match last_token {
                Token::Eval => Token::Procedure,
                Token::None | Token::Procedure => {
                    let item_type = Interpreter::get_item_type(word);
                    match item_type {
                        Type::Name => Token::Id,
                        _ => Token::Value { r#type: item_type },
                    }
                }
                Token::Quote => Token::Symbol,
                Token::List { paren_count, .. } => Token::List {
                    paren_count,
                    state: State::Body,
                },
                _ => last_token,
            },
        }
    }

    fn get_id_value(&self, id: String) -> Option<String> {
        if id.trim().parse::<u32>().is_ok() {
            Some("1".to_owned())
        } else if id.trim().parse::<i32>().is_ok() {
            Some("2".to_owned())
        } else if id.trim().parse::<f32>().is_ok() {
            Some("3".to_owned())
        } else {
            self.lookup_id(&id);
            Some("4".to_owned())
        }
    }

    fn lookup_id(&self, id: &str) -> Option<&Identifier> {
        for scope in self.scope_stack.iter().rev() {
            if scope.contains_key(id) {
                return scope.get(id);
            }
        }
        None
    }

    fn read_balanced_input() -> String {
        let mut paren_count = 0;
        let mut bracket_count = 0;
        let mut curly_count = 0;
        let mut angle_count = 0;
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

    pub fn repl() {
        let mut interpreter = Interpreter::new();
        loop {
            let program = Interpreter::read_balanced_input();
            if !program.is_empty() {
                match interpreter.parse(&program) {
                    Some(res) => println!("{}", res),
                    None => continue,
                }
            } else {
                println!();
                break;
            }
        }
    }
}
