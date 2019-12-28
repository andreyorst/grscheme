use crate::identifier::Type;
use crate::tree::{NodePtr, Tree};

#[derive(Debug, Clone)]
pub enum Token {
    Value,
    Lambda,
    Eval,
    Apply,
    Quote { kind: String },
    Symbol,
    List,
    Args,
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

    pub fn parse(&mut self, expression: &str) -> Result<NodePtr, InterpreterError> {
        let mut comment = false;
        let mut inside_word = false;
        let mut inside_string = false;
        let mut item = String::new();
        let mut tree = Tree::root("progn".to_owned());

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
        Ok(tree.clone().borrow().root.clone().unwrap())
    }

    fn add_to_tree(&mut self, node: &NodePtr, item: &str) -> Result<NodePtr, InterpreterError> {
        match self.tokenize(item) {
            Err(e) => return Err(e),
            Ok(t) => match t {
                Token::Quote { kind } => return Ok(Tree::add_child(node, kind)),
                Token::List => return Ok(Tree::add_child(node, "expr".to_owned())),
                Token::Args => return Ok(Tree::add_child(node, "args".to_owned())),
                Token::Eval => return Ok(Tree::add_child(node, "eval".to_owned())),
                Token::Lambda => return Ok(Tree::add_child(node, "lambda".to_owned())),
                Token::Symbol => {
                    Tree::add_child(node, item.to_owned());
                    return Ok(node.borrow().parent.clone().unwrap());
                }
                Token::Apply => match node.borrow().parent.clone() {
                    Some(p) => return Ok(p),
                    None => {
                        return Err(InterpreterError::InvalidSyntax {
                            message: "unexpected ')'",
                        })
                    }
                },
                _ => Tree::add_child(node, item.to_owned()),
            },
        };
        Ok(node.clone())
    }

    fn tokenize(&mut self, word: &str) -> Result<Token, InterpreterError> {
        let last_token = &self.last_token;

        let token = match word {
            "(" => match last_token {
                Token::Quote { .. } => Token::List,
                Token::List => Token::List,
                Token::Lambda => Token::Args,
                _ => Token::Eval,
            },
            ")" => match last_token {
                Token::Quote { .. } => {
                    return Err(InterpreterError::InvalidSyntax {
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
            "'" | "quote" => Token::Quote {
                kind: "quote".to_owned(),
            },
            "`" | "quasiquote" => Token::Quote {
                kind: "quasiquote".to_owned(),
            },
            "," | "unquote" => Token::Quote {
                kind: "unquote".to_owned(),
            },
            &_ => match last_token {
                Token::Eval => match item_type(word) {
                    Type::Name => Token::Value,
                    _ => {
                        return Err(InterpreterError::InvalidSyntax {
                            message: "Expected identifer",
                        })
                    }
                },
                Token::Lambda => {
                    return Err(InterpreterError::InvalidSyntax {
                        message: "Unexpected identifier",
                    })
                }
                Token::Quote { .. } => Token::Symbol,
                _ => Token::Value,
            },
        };

        self.last_token = token.clone();

        Ok(token)
    }
}
