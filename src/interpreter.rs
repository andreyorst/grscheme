use std::collections::HashMap;
use std::io;
use std::io::Write;

use crate::evaluator;
use crate::identifier::{Identifier, Type};
use crate::stack_item::{StackItem, State};

#[derive(Debug, Clone)]
pub enum Token {
    Value { item_type: Type },
    Lambda,
    ExprStart,
    ExprEnd,
    Quote,
    Symbol,
    None,
}

#[derive(Debug)]
pub struct Interpreter {
    pub stack: Vec<StackItem>,
    pub scope_stack: Vec<HashMap<String, Identifier>>,
}

enum ErrorKind {
    _ArgAmount,
    StackError,
    _EvalError,
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            stack: vec![],
            scope_stack: vec![HashMap::new()],
        }
    }

    fn parse(&mut self, expression: &str) -> Result<String, ErrorKind> {
        let mut comment = false;
        let mut inside_word = false;
        let mut inside_string = false;
        let mut item = String::new();

        for c in expression.chars() {
            if !comment && !inside_string {
                match c {
                    '(' => {
                        self.push_to_stack(&c.to_string());
                        continue;
                    }
                    ')' => {
                        if !item.is_empty() {
                            self.push_to_stack(&item);
                            item.clear();
                        }
                        self.push_to_stack(&c.to_string());
                        inside_word = false;
                    }
                    '\'' => {
                        if inside_word {
                            panic!("' not allowed as word char");
                        } else {
                            self.push_to_stack(&c.to_string());
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
                    item.push(c);
                } else if !item.is_empty() {
                    self.push_to_stack(&item);
                    item.clear();
                }
            } else if inside_string {
                item.push(c);
                if c == '"' {
                    inside_string = false;
                    self.push_to_stack(&item);
                    item.clear();
                }
            } else if comment && c == '\n' {
                comment = false;
            }
        }

        match self.stack.pop() {
            Some(item) => Ok(item.data),
            None => Err(ErrorKind::StackError),
        }
    }

    fn push_to_stack(&mut self, item: &str) {
        let token = self.tokenize(&item);
        self.stack.push(StackItem::from(token, &item));
        println!("{:?}", self.stack.last().unwrap());
    }

    fn get_last_token(&self) -> Token {
        match self.stack.last() {
            Some(x) => x.token.clone(),
            None => Token::None,
        }
    }

    fn _apply(&mut self) -> Result<(), ErrorKind> {
        let operands: Vec<String> = Vec::new();
        let proc = String::new();

        self._eval(&proc, &operands)
    }

    fn _eval(&mut self, procedure: &str, operands: &[String]) -> Result<(), ErrorKind> {
        let operands: Vec<&String> = operands.iter().rev().collect();
        println!("apply '{}' to {:?}", procedure, operands);
        let res: Option<String> = match procedure {
            "define" => None,
            "lambda" => None,
            "quote" => evaluator::quote(&operands),
            "list" => evaluator::list(&operands),
            "+" | "-" | "*" | "/" => evaluator::calculate(&operands, procedure),
            "<" | "<=" | "=" | "=>" | ">" => evaluator::compare(&operands, procedure),
            &_ => None,
        };
        match res {
            Some(x) => self.push_to_stack(&x),
            None => return Err(ErrorKind::_EvalError),
        };
        Ok(())
    }

    fn tokenize(&mut self, word: &str) -> Token {
        let last_token = self.get_last_token();

        match word {
            "(" => Token::ExprStart,
            ")" => Token::ExprEnd,
            "lambda" => match last_token {
                Token::ExprStart => Token::Lambda,
                Token::Quote => Token::Symbol,
                _ => Token::Value { item_type: Type::Name },
            },
            "'" | "quote" => Token::Quote,
            &_ => match last_token {
                Token::Quote => Token::Symbol,
                _ => Token::Value {
                    item_type: get_item_type(word),
                },
            },
        }
    }

    fn _quote_symbol(&mut self) -> Result<(), ErrorKind> {
        let mut item = match self.stack.pop() {
            Some(i) => i.data,
            None => return Err(ErrorKind::StackError),
        };
        let mut token = Token::None;
        while let Some(StackItem {
            token: Token::Quote,
            ..
        }) = self.stack.last()
        {
            let item_type = get_item_type(&item);
            item = match item_type {
                Type::Name => format!("'{}", item),
                _ => {
                    token = Token::Value { item_type };
                    item.to_owned()
                }
            };
            self.stack.pop();
        }
        self.stack.push(StackItem { token, data: item, state: State::Arg });
        Ok(())
    }
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
        let program = read_balanced_input();
        if !program.is_empty() {
            match interpreter.parse(&program) {
                Ok(res) => println!("{}", res),
                _ => continue,
            }
        } else {
            println!();
            break;
        }
    }
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
