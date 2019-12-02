use std::collections::HashMap;
use std::io;
use std::io::Write;

use crate::evaluator;
use crate::identifier::{Identifier, Type};

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
    Procedure,
    Eval,
    Apply,
    Quote,
    QuoteProc { paren_count: u32, state: State },
    Symbol,
    List { paren_count: u32 },
    None,
}

#[derive(Debug)]
pub struct StackItem {
    pub token: Token,
    pub data: String,
}

#[derive(Debug)]
pub struct Interpreter {
    pub stack: Vec<StackItem>,
    pub scope_stack: Vec<HashMap<String, Identifier>>,
}

enum Error {
    _ArgAmount,
    StackExhausted,
    Eval,
    InvalidSyntax { message: &'static str },
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            stack: vec![],
            scope_stack: vec![HashMap::new()],
        }
    }

    fn parse(&mut self, expression: &str) -> Result<String, Error> {
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
                        if let Err(e) = error {
                            return Err(e);
                        };
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
                            return Err(Error::InvalidSyntax {
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

            if let Err(e) = match self.get_token() {
                Token::Apply => self.apply(),
                Token::List { paren_count: 0 } => self.reduce_quoted_list(),
                Token::Symbol => self.quote_symbol(),
                _ => Ok(()),
            } {
                return Err(e);
            }
        }

        match self.stack.pop() {
            Some(item) => Ok(item.data),
            None => Err(Error::StackExhausted),
        }
    }

    fn push_to_stack(&mut self, item: &str) -> Result<(), Error> {
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

    fn apply(&mut self) -> Result<(), Error> {
        let mut operands: Vec<String> = Vec::new();
        let mut procedure = String::new();
        loop {
            let item = match self.stack.pop() {
                Some(x) => x,
                None => return Err(Error::StackExhausted),
            };
            match item.token {
                Token::QuoteProc { state, .. } => {
                    match state {
                        State::Wait => procedure = item.data.clone(),
                        State::Args => operands.push(item.data.clone()),
                        _ => return Err(Error::InvalidSyntax { message: "unexpected token" }),
                    }
                },
                Token::Eval => break,
                Token::Procedure => procedure = item.data.clone(),
                Token::Value { .. } => operands.push(item.data.clone()),
                _ => (),
            }
        }
        self.eval(&procedure, &operands)
    }

    fn eval(&mut self, procedure: &str, operands: &[String]) -> Result<(), Error> {
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
            None => return Err(Error::Eval),
        };
        Ok(())
    }

    fn reduce_quoted_list(&mut self) -> Result<(), Error> {
        let mut operands: Vec<String> = Vec::new();
        loop {
            let item = match self.stack.pop() {
                Some(x) => x,
                None => return Err(Error::StackExhausted),
            };
            match item.token {
                Token::List { .. } => operands.push(item.data.clone()),
                Token::Quote => break,
                _ => return Err(Error::Eval),
            }
        }
        let operands: Vec<&String> = operands.iter().rev().collect();

        match evaluator::quote(&operands) {
            Some(res) => {
                self.push_to_stack(&res)
            },
            None => Err(Error::Eval),
        }
    }

    fn tokenize(&self, word: &str) -> Result<Token, Error> {
        let last_token = self.get_token();

        let token = match word {
            "(" => match last_token {
                Token::Quote => Token::List { paren_count: 1 },
                Token::QuoteProc { paren_count, state } => match state {
                    State::Args => {
                        if paren_count == 1 {
                            return Err(Error::InvalidSyntax {
                                message: "unexpected '('",
                            });
                        } else {
                            Token::QuoteProc {
                                paren_count: paren_count + 1,
                                state,
                            }
                        }
                    }
                    _ => Token::QuoteProc {
                        paren_count: paren_count + 1,
                        state: State::Args,
                    },
                },
                Token::List { paren_count } => Token::List {
                    paren_count: paren_count + 1,
                },
                Token::Lambda { paren_count, state } => {
                    let state = match state {
                        State::Wait => State::Args,
                        State::Args => State::Body,
                        State::Body => State::Body,
                    };
                    Token::Lambda {
                        paren_count: paren_count + 1,
                        state,
                    }
                }
                _ => Token::Eval,
            },
            ")" => match last_token {
                Token::Quote => {
                    return Err(Error::InvalidSyntax {
                        message: "unexpected ')'",
                    })
                }
                Token::QuoteProc { paren_count, state } => match state {
                    State::Args => {
                        if paren_count == 1 {
                            Token::Apply
                        } else {
                            Token::QuoteProc {
                                paren_count: paren_count - 1,
                                state,
                            }
                        }
                    }
                    _ => {
                        return Err(Error::InvalidSyntax {
                            message: "unexpected ')'",
                        })
                    }
                },
                Token::List { paren_count } => {
                    if paren_count == 1 {
                        Token::List { paren_count: 0 }
                    } else if paren_count == 0 {
                        Token::Apply
                    } else {
                        Token::List {
                            paren_count: paren_count - 1,
                        }
                    }
                }
                Token::Lambda { paren_count, state } => {
                    if paren_count == 1 {
                        match state {
                            State::Body => Token::Apply,
                            _ => {
                                return Err(Error::InvalidSyntax {
                                    message: "unexpected ')'",
                                })
                            }
                        }
                    } else {
                        Token::Lambda {
                            paren_count: paren_count - 1,
                            state,
                        }
                    }
                }
                _ => Token::Apply,
            },
            "lambda" => match last_token {
                Token::Eval => Token::Lambda {
                    paren_count: 1,
                    state: State::Wait,
                },
                Token::Quote => Token::Symbol,
                Token::List { paren_count } => Token::List { paren_count },
                _ => Token::Value {
                    item_type: Type::Name,
                },
            },
            "'" => Token::Quote,
            "quote" => Token::QuoteProc {
                paren_count: 1,
                state: State::Wait,
            },
            &_ => match last_token {
                Token::Quote => Token::Symbol,
                Token::QuoteProc { paren_count, .. } => Token::QuoteProc {
                    state: State::Args,
                    paren_count,
                },
                Token::List { paren_count } => Token::List { paren_count },
                Token::Lambda { paren_count, state } => match state {
                    State::Wait => {
                        return Err(Error::InvalidSyntax {
                            message: "unexpected token, expected parameter list",
                        })
                    }
                    State::Args => {
                        if paren_count == 1 {
                            Token::Lambda {
                                paren_count,
                                state: State::Body,
                            }
                        } else {
                            Token::Lambda { paren_count, state }
                        }
                    }
                    _ => Token::Lambda { paren_count, state },
                },
                Token::Eval => match get_item_type(word) {
                    Type::Name => Token::Procedure,
                    _ => {
                        return Err(Error::InvalidSyntax {
                            message: "Expected identifer",
                        })
                    }
                },
                _ => Token::Value {
                    item_type: get_item_type(word),
                },
            },
        };
        Ok(token)
    }

    fn quote_symbol(&mut self) -> Result<(), Error> {
        let mut item = match self.stack.pop() {
            Some(i) => i.data,
            None => return Err(Error::StackExhausted),
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
                Err(Error::InvalidSyntax { message }) => {
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
