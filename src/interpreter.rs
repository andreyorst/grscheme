use crate::identifier::Type;
use crate::tree::{NodePtr, Tree};

#[derive(Debug, Clone)]
pub enum Token {
    Value,
    Lambda,
    Procedure,
    Eval,
    Apply,
    Quote,
    Symbol,
    List,
    None,
}

#[derive(Debug)]
pub struct Interpreter {
    pub last_token: Token,
}

pub enum InterpreterError {
    InvalidSyntax { message: &'static str },
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            last_token: Token::None,
        }
    }

    pub fn parse(
        &mut self,
        mut tree: NodePtr,
        expression: &str,
    ) -> Result<NodePtr, InterpreterError> {
        let mut comment = false;
        let mut inside_word = false;
        let mut inside_string = false;
        let mut item = String::new();

        for c in expression.chars() {
            if !comment && !inside_string {
                match c {
                    '(' => {
                        item.push(c);
                        inside_word = false;
                    }
                    ')' => {
                        if !item.is_empty() {
                            match self.add_to_tree(&tree, &item) {
                                Ok(t) => tree = t,
                                Err(e) => return Err(e),
                            };
                            item.clear();
                        }
                        item.push(c);
                        inside_word = false;
                    }
                    '\'' => {
                        if inside_word {
                            return Err(InterpreterError::InvalidSyntax {
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
                    match self.add_to_tree(&tree, &item) {
                        Ok(t) => tree = t,
                        Err(e) => return Err(e),
                    };
                    item.clear();
                }
            } else if inside_string {
                item.push(c);
                if c == '"' {
                    inside_string = false;
                    match self.add_to_tree(&tree, &item) {
                        Ok(t) => tree = t,
                        Err(e) => return Err(e),
                    };
                    item.clear();
                }
            } else if comment && c == '\n' {
                comment = false;
            }
        }
        Ok(tree)
    }

    fn add_to_tree(&mut self, node: &NodePtr, item: &str) -> Result<NodePtr, InterpreterError> {
        if let Err(e) = self.tokenize(item) {
            return Err(e);
        };
        match self.last_token {
            Token::Quote => Tree::add_child(node, "quote".to_owned()),
            Token::List => return Ok(Tree::add_child(node, "expr".to_owned())),
            _ => Tree::add_child(node, item.to_owned()),
        };
        Ok(node.clone())
    }

    fn tokenize(&mut self, word: &str) -> Result<(), InterpreterError> {
        let last_token = &self.last_token;

        let token = match word {
            "(" => match last_token {
                Token::Quote => Token::List,
                Token::List => Token::List,
                Token::Lambda => Token::Lambda,
                _ => Token::Eval,
            },
            ")" => match last_token {
                Token::Quote => {
                    return Err(InterpreterError::InvalidSyntax {
                        message: "unexpected ')'",
                    })
                }
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
            "'" | "quote" => Token::Quote,
            &_ => match last_token {
                Token::Quote => Token::Symbol,
                Token::List => Token::List,
                Token::Lambda => Token::Lambda,
                Token::Procedure => Token::Procedure,
                Token::Eval => match _get_item_type(word) {
                    Type::Name => Token::Procedure,
                    _ => {
                        return Err(InterpreterError::InvalidSyntax {
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
