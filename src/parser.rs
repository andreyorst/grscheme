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
    last_token: Token,
    extra_up: u32,
    line: u32,
    column: u32,
}

pub enum ParseError {
    InvalidSyntax { message: String },
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            last_token: Token::None,
            extra_up: 0,
            line: 1,
            column: 0,
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
            self.column += 1;
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
                                message: format!(
                                    "qoute is not a valid word character. line: {}, column: {}",
                                    self.line, self.column
                                ),
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
                    ' ' | '\t' => {
                        inside_word = false;
                    }
                    '\n' => {
                        self.line += 1;
                        self.column = 0;
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
                match c {
                    '"' => {
                        inside_string = false;
                        match self.add_to_tree(&tree, &item) {
                            Ok(t) => tree = t,
                            Err(e) => return Err(e),
                        };
                        item.clear();
                    }
                    '\n' => {
                        self.line += 1;
                        self.column = 0;
                    }
                    _ => (),
                }
            } else if comment && c == '\n' {
                self.line += 1;
                self.column = 0;
                comment = false;
            }
        }

        if let Err(e) = Self::remove_dots(&root) {
            return Err(e);
        }
        Ok(root)
    }

    fn add_to_tree(&mut self, node: &NodePtr, item: &str) -> Result<NodePtr, ParseError> {
        match self.tokenize(item) {
            Err(e) => Err(e),
            Ok(t) => match t {
                Token::Quote { kind } => {
                    let eval = Tree::add_child(node, "eval".to_owned());
                    self.extra_up += 1;
                    Tree::add_child(&eval, kind);
                    Ok(eval)
                }
                Token::Args => Ok(Tree::add_child(node, "args".to_owned())),
                Token::Eval => Ok(Tree::add_child(node, "eval".to_owned())),
                Token::Lambda => Ok(Tree::add_child(node, "lambda".to_owned())),
                Token::Symbol => {
                    Tree::add_child(node, item.to_owned());
                    let mut parent = node.clone().borrow().parent.as_ref().unwrap().clone();
                    while self.extra_up > 0 {
                        self.extra_up -= 1;
                        parent = node.clone().borrow().parent.as_ref().unwrap().clone();
                    }
                    Ok(Weak::upgrade(&parent).unwrap())
                }
                Token::Apply => match &node.borrow().parent {
                    Some(_) => {
                        let mut parent = node.clone().borrow().parent.as_ref().unwrap().clone();
                        while self.extra_up > 0 {
                            self.extra_up -= 1;
                            parent = node.clone().borrow().parent.as_ref().unwrap().clone();
                        }
                        Ok(Weak::upgrade(&parent).unwrap())
                    }
                    None => Err(ParseError::InvalidSyntax {
                        message: format!(
                            "unexpected `)'. line: {}, col: {}",
                            self.line, self.column
                        ),
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
                        message: format!(
                            "unexpected `)'. line: {}, col: {}",
                            self.line, self.column
                        ),
                    })
                }
                _ => Token::Apply,
            },
            "lambda" | "λ" => match last_token {
                Token::Eval => Token::Lambda,
                Token::Quote { .. } => Token::Symbol,
                _ => Token::Value,
            },
            "'" | "," | "`" => Token::Quote {
                kind: match word {
                    "'" => "quote",
                    "," => "unquote",
                    "`" => "quasiquote",
                    _ => {
                        return Err(ParseError::InvalidSyntax {
                            message: format!(
                                "unexpected quote type `{}'. line: {}, col: {}",
                                word, self.line, self.column
                            ),
                        })
                    }
                }
                .to_string(),
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

    fn remove_dots(tree: &NodePtr) -> Result<(), ParseError> {
        let len = tree.borrow().childs.len();
        let mut childs = tree.borrow_mut().childs.clone();
        for (n, child) in childs.iter_mut().enumerate() {
            if child.borrow().data == "." {
                if n != len - 2 {
                    return Err(ParseError::InvalidSyntax {
                        message: "illegal use of `.'".to_owned(),
                    });
                } else {
                    let last = tree.borrow().childs.last().unwrap().clone();
                    let data = last.borrow().data.clone();
                    if data == "eval" {
                        let list = tree.borrow_mut().childs.pop().unwrap(); // extract child list
                        tree.borrow_mut().childs.pop(); // remove the dot
                        tree.borrow_mut()
                            .childs
                            .append(&mut list.borrow_mut().childs); // append extracted list to current node childs
                                                                    // re-traverse current node
                        if let Err(e) = Self::remove_dots(&tree) {
                            return Err(e);
                        }
                    }
                }
            }
            if let Err(e) = Self::remove_dots(&child) {
                return Err(e);
            }
        }
        Ok(())
    }
}
