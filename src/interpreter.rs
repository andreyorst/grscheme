#![warn(clippy::all)]

use std::collections::HashMap;
use std::io;
use std::io::Write;

use crate::identifier::{Identifier, Type};
use crate::stack_item::StackItem;
use crate::token::*;
use crate::evaluator;

#[derive(Debug)]
pub struct Interpreter {
    pub stack: Vec<StackItem>,
    pub scope_stack: Vec<HashMap<String, Identifier>>,
}

enum ErrorKind {
    ArgAmount,
    StackError,
    EvalError,
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
        let mut inside_word = false;
        let mut inside_string = false;
        let mut word = String::new();

        for c in program.chars() {
            if !comment || !inside_string {
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
                        if inside_word {
                            panic!("' not allowed as word char");
                        } else {
                            self.stack_push(&c.to_string());
                            inside_word = false;
                            continue;
                        }
                    }
                    '"' => {
                        inside_string = true;
                        inside_word = true;
                    }
                    ' ' | '\t' | '\n' => {
                        inside_word = false;
                    }
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

            match self.get_last_token() {
                Token::Apply => {
                    let _ = self.apply();
                }
                Token::Symbol => {
                    let _ = self.quote_symbol();
                }
                _ => (),
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
        println!("{:?}", self.stack.last().unwrap());
    }

    pub fn get_item_type(s: &str) -> Type {
        if s.trim().parse::<u32>().is_ok() {
            Type::U32
        } else if s.trim().parse::<i32>().is_ok() {
            Type::I32
        } else if s.trim().parse::<f32>().is_ok() {
            Type::F32
        } else if s.starts_with('"') && s.ends_with('"') {
            Type::Str
        } else {
            Type::Name
        }
    }

    fn get_last_token(&self) -> Token {
        match self.stack.last() {
            Some(x) => x.token.clone(),
            None => Token::None,
        }
    }

    fn apply(&mut self) -> Result<(), ErrorKind> {
        let mut operands: Vec<String> = Vec::new();
        let mut proc = String::new();

        loop {
            let item = match self.stack.pop() {
                Some(x) => x,
                None => return Err(ErrorKind::StackError),
            };
            match item.token {
                Token::Apply => (),
                Token::Eval => break,
                Token::Quote { procedure } => {
                    proc = "quote".to_owned();
                    if !procedure {
                        operands.insert(0, ")".to_owned());
                        break;
                    }
                },
                Token::List { .. } => operands.push(item.data),
                Token::Procedure
                | Token::Lambda {
                    paren_count: 0,
                    state: State::Wait,
                } => proc = item.data,
                Token::Id { .. } | _ => operands.push(item.data),
            }
        }
        self.eval(&proc, &operands)
    }

    fn eval(&mut self, procedure: &str, operands: &[String]) -> Result<(), ErrorKind> {
        let operands: Vec<&String> = operands.iter().rev().collect();
        println!("apply '{}' to {:?}", procedure, operands);
        let res: Option<String> = match procedure {
            "define" => {
                let _ = self.define(&operands);
                None
            }
            "lambda" => None,
            "quote" => evaluator::quote(&operands),
            "list" => evaluator::list(&operands),
            "+" | "-" | "*" | "/" => evaluator::eval_math(&operands, procedure),
            "<" | "<=" | "=" | "=>" | ">" => evaluator::eval_cmp(&operands, procedure),
            &_ => None,
        };
        match res {
            Some(x) => self.stack_push(&x),
            None => return Err(ErrorKind::EvalError),
        };
        Ok(())
    }

    fn tokenize(&mut self, word: &str) -> Token {
        let last_token = self.get_last_token();

        match word {
            "(" => match last_token {
                Token::Quote { procedure: false } => Token::List {
                    paren_count: 0,
                    state: State::Wait,
                },
                Token::Quote { procedure: true } => Token::List {
                    paren_count: 1,
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
                Token::List { paren_count, state: State::Body } => {
                    if paren_count == 0 {
                        Token::Apply
                    } else {
                        Token::List {
                            paren_count: paren_count - 1,
                            state: State::Body,
                        }
                    }
                }
                Token::List { state: State::Wait, .. } => Token::Apply,
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
            "'" => Token::Quote { procedure: false },
            "quote" => Token::Quote { procedure: true },
            &_ => match last_token {
                Token::Eval => Token::Procedure,
                Token::None | Token::Procedure => {
                    let item_type = Interpreter::get_item_type(word);
                    match item_type {
                        Type::Name => Token::Id,
                        _ => Token::Value { item_type },
                    }
                }
                Token::Quote { .. } => Token::Symbol,
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
    fn define(&mut self, operands: &[&String]) -> Result<(), ErrorKind> {
        if operands.len() == 2 {
            let name = operands[0].clone();
            let _data = self.get_id_value(name);
            Ok(())
        } else {
            Err(ErrorKind::ArgAmount)
        }
    }

    fn quote_symbol(&mut self) -> Result<(), ErrorKind> {
        let mut item = match self.stack.pop() {
            Some(i) => i.data,
            None => return Err(ErrorKind::StackError),
        };
        let mut token = Token::None;
        while let Some(StackItem {
            token: Token::Quote { procedure: false },
            ..
        }) = self.stack.last()
        {
            let item_type = Interpreter::get_item_type(&item);
            item = match item_type {
                Type::Name => format!("'{}", item),
                _ => {
                    token = Token::Value { item_type };
                    item.to_owned()
                }
            };
            self.stack.pop();
        }
        self.stack.push(StackItem { token, data: item });
        Ok(())
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
