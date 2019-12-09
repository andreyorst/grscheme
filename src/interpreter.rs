use std::collections::HashMap;
use std::io;
use std::io::Write;

// use crate::evaluator;
use crate::identifier::{Identifier, Type};

#[derive(Debug, Clone)]
pub enum Token {
    Value,
    Lambda,
    Procedure,
    Eval,
    Apply,
    Quote,
    QuoteProc,
    Symbol,
    List,
    None,
}

#[derive(Debug)]
pub enum FrameKind {
    Lambda {
        args: Vec<String>,
        body: Vec<String>,
    },
    Procedure {
        name: String,
        args: Vec<String>,
    },
    Quote {
        args: Vec<String>,
    },
    None,
}

#[derive(Debug)]
pub struct StackFrame {
    pub kind: FrameKind,
    pub paren_count: u32,
}

impl StackFrame {
    fn new() -> StackFrame {
        StackFrame {
            kind: FrameKind::None,
            paren_count: 0,
        }
    }
    fn procedure(name: &str) -> StackFrame {
        StackFrame {
            kind: FrameKind::Procedure {
                name: name.to_owned(),
                args: vec![],
            },
            paren_count: 1,
        }
    }
}

#[derive(Debug)]
pub struct Interpreter {
    pub stack: Vec<StackFrame>,
    pub scope_stack: Vec<HashMap<String, Identifier>>,
    pub last_token: Token,
}

enum Error {
    StackExhausted,
    _Eval,
    InvalidSyntax { message: &'static str },
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            stack: vec![],
            last_token: Token::None,
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
                        item.push(c);
                        inside_word = false;
                    }
                    ')' => {
                        if !item.is_empty() {
                            error = self.add_to_stack(&item);
                            if let Err(e) = error {
                                return Err(e);
                            };
                            item.clear();
                        }
                        item.push(c);
                        inside_word = false;
                    }
                    '\'' => {
                        if inside_word {
                            return Err(Error::InvalidSyntax {
                                message: "qoute is not a valid word character",
                            });
                        } else {
                            item.push(c);
                            inside_word = false;
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
                    let _ = self.add_to_stack(&item);
                    item.clear();
                }
            } else if inside_string {
                item.push(c);
                if c == '"' {
                    inside_string = false;
                    let _ = self.add_to_stack(&item);
                    item.clear();
                }
            } else if comment && c == '\n' {
                comment = false;
            }

            if let Err(e) = error {
                return Err(e);
            }
        }
        Ok("".to_owned())
    }

    fn add_to_stack(&mut self, item: &str) -> Result<(), Error> {
        if let Err(e) = self.tokenize(&item) {
            return Err(e);
        }
        match self.last_token {
            Token::Eval => {
                self.stack.push(StackFrame::new());
                println!("{:?}", self.stack);
            }
            Token::Apply => println!("reduce frame, update previous frame?"),
            Token::Procedure => {
                let frame = match self.stack.last_mut() {
                    Some(f) => f,
                    None => return Err(Error::StackExhausted),
                };
                match frame.kind {
                    FrameKind::None => {
                        *frame = StackFrame::procedure(item);
                        println!("{:?}", self.stack);
                    },
                    FrameKind::Procedure { .. } => println!("update argument list"),
                    _ => println!("error."),
                }
            }
            Token::Symbol | Token::Quote | Token::List | Token::QuoteProc => {
                println!("create or update quoted frame")
            }
            Token::Lambda => println!("change frame type to lambda, update, or reduce lambda"),
            Token::Value => println!("reduce value?"),
            Token::None => println!("error?"),
        }
        Ok(())
    }

    fn _reduce_quoted_list(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn tokenize(&mut self, word: &str) -> Result<(), Error> {
        let last_token = &self.last_token;

        let token = match word {
            "(" => match last_token {
                Token::Quote => Token::List,
                Token::QuoteProc => Token::QuoteProc,
                Token::List => Token::List,
                Token::Lambda => Token::Lambda,
                _ => Token::Eval,
            },
            ")" => match last_token {
                Token::Quote => {
                    return Err(Error::InvalidSyntax {
                        message: "unexpected ')'",
                    })
                }
                Token::QuoteProc => Token::QuoteProc,
                Token::List => Token::List,
                Token::Lambda => Token::Lambda,
                _ => Token::Apply,
            },
            "lambda" => match last_token {
                Token::Eval => Token::Lambda,
                Token::Quote => Token::Symbol,
                Token::List => Token::List,
                _ => Token::Value,
            },
            "'" => Token::Quote,
            "quote" => Token::QuoteProc,
            &_ => match last_token {
                Token::Quote => Token::Symbol,
                Token::QuoteProc => Token::QuoteProc,
                Token::List => Token::List,
                Token::Lambda => Token::Lambda,
                Token::Procedure => Token::Procedure,
                Token::Eval => match get_item_type(word) {
                    Type::Name => Token::Procedure,
                    _ => {
                        return Err(Error::InvalidSyntax {
                            message: "Expected identifer",
                        })
                    }
                },
                _ => Token::Value,
            },
        };

        self.last_token = token;

        Ok(())
    }

    // fn quote_symbol(&mut self) -> Result<(), Error> {
    //     let mut item = match self.stack.pop() {
    //         Some(i) => i.data,
    //         None => return Err(Error::StackExhausted),
    //     };
    //     let mut token = Token::None;
    //     while let Some(StackFrame {
    //         token: Token::Quote,
    //         ..
    //     }) = self.stack.last()
    //     {
    //         let item_type = get_item_type(&item);
    //         item = match item_type {
    //             Type::Name => format!("'{}", item),
    //             _ => {
    //                 token = Token::Value { item_type };
    //                 item.to_owned()
    //             }
    //         };
    //         self.stack.pop();
    //     }
    //     self.stack.push(StackFrame { token, data: item });
    //     Ok(())
    // }
}

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
    } else if s.starts_with('\'') {
        if &s[0..2] == "'(" {
            Type::List
        } else {
            Type::Symbol
        }
    } else {
        Type::Name
    }
}

#[test]
fn test_types() {
    assert_eq!(get_item_type("32"), Type::U32);
    assert_eq!(get_item_type("-32"), Type::I32);
    assert_eq!(get_item_type("32.0"), Type::F32);
    assert_eq!(get_item_type("-32.0"), Type::F32);
    assert_eq!(get_item_type("\"str\""), Type::Str);
    assert_eq!(get_item_type("'symbol"), Type::Symbol);
    assert_eq!(get_item_type("'(list list)"), Type::List);
    assert_eq!(get_item_type("name"), Type::Name);
}
