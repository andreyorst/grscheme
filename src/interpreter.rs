use std::collections::HashMap;
use std::io;
use std::io::Write;

use crate::evaluator;
use crate::identifier::{Identifier, Type};
use crate::stack_item::StackItem;

#[derive(Debug, Clone)]
pub enum State {
    Wait,
    Args,
    Body,
}

#[derive(Debug, Clone)]
pub enum Token {
    Value { item_type: Type },
    Lambda { paren_count: u32, state: State },
    ExprStart,
    ExprEnd,
    Quote,
    Symbol,
    List { paren_count: u32 },
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
    SyntaxError { message: &'static str },
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
        let mut error = Ok(());

        for c in expression.chars() {
            if !comment && !inside_string {
                match c {
                    '(' => {
                        error = self.push_to_stack(&c.to_string());
                        continue;
                    }
                    ')' => {
                        if !item.is_empty() {
                            error = self.push_to_stack(&item);
                            if let Err(e) = error {
                                return Err(e);
                            };
                            item.clear();
                        }
                        error = self.push_to_stack(&c.to_string());
                        inside_word = false;
                    }
                    '\'' => {
                        if inside_word {
                            return Err(ErrorKind::SyntaxError {
                                message: "qoute is not a valid word character",
                            });
                        } else {
                            error = self.push_to_stack(&c.to_string());
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
                    error = self.push_to_stack(&item);
                    item.clear();
                }
            } else if inside_string {
                item.push(c);
                if c == '"' {
                    inside_string = false;
                    error = self.push_to_stack(&item);
                    item.clear();
                }
            } else if comment && c == '\n' {
                comment = false;
            }

            if let Err(e) = error {
                return Err(e);
            }
        }

        match self.stack.pop() {
            Some(item) => Ok(item.data),
            None => Err(ErrorKind::StackError),
        }
    }

    fn push_to_stack(&mut self, item: &str) -> Result<(), ErrorKind> {
        let token = match self.tokenize(&item) {
            Ok(t) => t,
            Err(e) => return Err(e),
        };
        self.stack.push(StackItem {
            token,
            data: String::from(item),
        });
        println!("{:?}", self.stack.last().unwrap());
        Ok(())
    }

    fn get_token(&self) -> Token {
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
            Some(x) => {
                if let Err(e) = self.push_to_stack(&x) {
                    return Err(e);
                }
            }
            None => return Err(ErrorKind::_EvalError),
        };
        Ok(())
    }

    fn tokenize(&self, word: &str) -> Result<Token, ErrorKind> {
        let last_token = self.get_token();

        match word {
            "(" => match last_token {
                Token::Quote => Ok(Token::List { paren_count: 1 }),
                Token::List { paren_count } => Ok(Token::List {
                    paren_count: paren_count + 1,
                }),
                Token::Lambda { paren_count, state } => {
                    let state = match state {
                        State::Wait => State::Args,
                        State::Args => State::Body,
                        State::Body => State::Body,
                    };
                    Ok(Token::Lambda {
                        paren_count: paren_count + 1,
                        state,
                    })
                }
                _ => Ok(Token::ExprStart),
            },
            ")" => match last_token {
                Token::Quote => Err(ErrorKind::SyntaxError {
                    message: "unexpected ')'",
                }),
                Token::List { paren_count } => {
                    if paren_count == 1 {
                        Ok(Token::List { paren_count: 0 })
                    } else if paren_count == 0 {
                        Ok(Token::ExprEnd)
                    } else {
                        Ok(Token::List {
                            paren_count: paren_count - 1,
                        })
                    }
                }
                Token::Lambda { paren_count, state } => {
                    if paren_count == 1 {
                        let state = match state {
                            State::Body => State::Wait,
                            _ => {
                                return Err(ErrorKind::SyntaxError {
                                    message: "unexpected ')'",
                                })
                            }
                        };
                        match state {
                            State::Wait => Ok(Token::ExprEnd),
                            _ => Ok(Token::Lambda {
                                paren_count: paren_count - 1,
                                state,
                            }),
                        }
                    } else {
                        Ok(Token::Lambda {
                            paren_count: paren_count - 1,
                            state,
                        })
                    }
                }
                _ => Ok(Token::ExprEnd),
            },
            "lambda" => match last_token {
                Token::ExprStart => Ok(Token::Lambda {
                    paren_count: 1,
                    state: State::Wait,
                }),
                Token::Quote => Ok(Token::Symbol),
                _ => Ok(Token::Value {
                    item_type: Type::Name,
                }),
            },
            "'" | "quote" => Ok(Token::Quote),
            &_ => match last_token {
                Token::Quote => Ok(Token::Symbol),
                Token::List { paren_count } => Ok(Token::List { paren_count }),
                Token::Lambda { paren_count, state } => match state {
                    State::Wait => Err(ErrorKind::SyntaxError {
                        message: "unexpected token, expected parameter list",
                    }),
                    State::Args => {
                        if paren_count == 1 {
                            Ok(Token::Lambda {
                                paren_count,
                                state: State::Body,
                            })
                        } else {
                            Ok(Token::Lambda { paren_count, state })
                        }
                    }
                    _ => Ok(Token::Lambda { paren_count, state }),
                },
                _ => Ok(Token::Value {
                    item_type: get_item_type(word),
                }),
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
        self.stack.push(StackItem { token, data: item });
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
                Err(ErrorKind::SyntaxError { message }) => {
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
