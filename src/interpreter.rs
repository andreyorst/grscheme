use std::io;
use std::io::Write;

// use crate::evaluator;
use crate::identifier::Type;
use crate::tree::Tree;

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
pub struct Interpreter {
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
            last_token: Token::None,
        }
    }

    fn parse(&mut self, expression: &str) -> Result<Tree<String>, Error> {
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
                            error = self.add_to_tree(&item);
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
                    let _ = self.add_to_tree(&item);
                    item.clear();
                }
            } else if inside_string {
                item.push(c);
                if c == '"' {
                    inside_string = false;
                    let _ = self.add_to_tree(&item);
                    item.clear();
                }
            } else if comment && c == '\n' {
                comment = false;
            }

            if let Err(e) = error {
                return Err(e);
            }
        }
        Err(Error::StackExhausted)
    }

    fn add_to_tree(&mut self, item: &str) -> Result<(), Error> {
        let _ = self.tokenize(item);
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
                Token::Eval => match _get_item_type(word) {
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
                Ok(_) => println!(""),
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

pub fn _get_item_type(s: &str) -> Type {
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
    assert_eq!(_get_item_type("32"), Type::U32);
    assert_eq!(_get_item_type("-32"), Type::I32);
    assert_eq!(_get_item_type("32.0"), Type::F32);
    assert_eq!(_get_item_type("-32.0"), Type::F32);
    assert_eq!(_get_item_type("\"str\""), Type::Str);
    assert_eq!(_get_item_type("'symbol"), Type::Symbol);
    assert_eq!(_get_item_type("'(list list)"), Type::List);
    assert_eq!(_get_item_type("name"), Type::Name);
}
