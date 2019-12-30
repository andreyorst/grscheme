use crate::tree::{NodePtr, Tree};
use std::rc::Weak;

#[derive(Debug, Clone)]
pub enum Token {
    Value,
    Lambda,
    Eval,
    Apply,
    Quote { kind: String },
    Symbol,
    Args,
    Dot,
    None,
}

#[derive(Debug)]
pub struct Parser {
    pub last_token: Token,
}

pub enum ParseError {
    InvalidSyntax { message: &'static str },
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            last_token: Token::None,
        }
    }

    pub fn parse(&mut self, expression: &str) -> Result<NodePtr, ParseError> {
        let mut comment = false;
        let mut inside_word = false;
        let mut inside_string = false;
        let mut item = String::new();
        let mut tree = Tree::root("progn".to_owned());
        let root = tree.clone();

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
                    '\'' | '`' | ',' => {
                        if inside_word {
                            return Err(ParseError::InvalidSyntax {
                                message: "qoute is not a valid word character",
                            });
                        } else {
                            match c {
                                '\'' => item = "quote".to_owned(),
                                '`' => item = "quasiquote".to_owned(),
                                ',' => item = "unquote".to_owned(),
                                _ => (),
                            }
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

        Ok(root)
    }

    fn add_to_tree(&mut self, node: &NodePtr, item: &str) -> Result<NodePtr, ParseError> {
        match self.tokenize(item) {
            Err(e) => Err(e),
            Ok(t) => match t {
                Token::Quote { kind } => Ok(Tree::add_child(node, kind)),
                Token::Args => Ok(Tree::add_child(node, "args".to_owned())),
                Token::Eval => Ok(Tree::add_child(node, "eval".to_owned())),
                Token::Lambda => Ok(Tree::add_child(node, "lambda".to_owned())),
                Token::Symbol => {
                    Tree::add_child(node, item.to_owned());
                    Ok(Weak::upgrade(&node.borrow().parent.as_ref().unwrap()).unwrap())
                }
                Token::Apply => match &node.borrow().parent {
                    Some(p) => Ok(Weak::upgrade(&p).unwrap()),
                    None => Err(ParseError::InvalidSyntax {
                        message: "unexpected ')'",
                    }),
                },
                _ => {
                    Tree::add_child(node, item.to_owned());
                    Ok(node.clone())
                }
            },
        }
    }

    fn tokenize(&mut self, word: &str) -> Result<Token, ParseError> {
        let last_token = &self.last_token;

        let token = match word {
            "(" => match last_token {
                Token::Lambda => Token::Args,
                _ => Token::Eval,
            },
            ")" => match last_token {
                Token::Quote { .. } => {
                    return Err(ParseError::InvalidSyntax {
                        message: "unexpected ')'",
                    })
                }
                _ => Token::Apply,
            },
            "lambda" | "λ" => match last_token {
                Token::Eval => Token::Lambda,
                Token::Quote { .. } => Token::Symbol,
                _ => Token::Value,
            },
            "quote" | "quasiquote" | "unquote" => match last_token {
                Token::Dot => {
                    Token::Value
                }
                _ => Token::Quote {
                    kind: word.to_owned(),
                },
            },
            "." => Token::Dot,
            &_ => match last_token {
                Token::Quote { .. } => Token::Symbol,
                _ => Token::Value,
            },
        };

        self.last_token = token.clone();

        Ok(token)
    }
}
