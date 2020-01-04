use crate::tree::{NodePtr, Tree};

#[derive(Debug, Clone)]
pub enum Token {
    Value,
    Eval,
    Apply,
    Quote { kind: String },
    Symbol,
    Dot,
    None,
}

#[derive(Debug)]
pub struct Parser {
    last_token: Token,
    line_num: u32,
    column_num: u32,
}

pub enum ParseError {
    InvalidSyntax { message: String },
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            last_token: Token::None,
            line_num: 1,
            column_num: 0,
        }
    }

    pub fn parse(&mut self, expression: &str) -> Result<NodePtr, ParseError> {
        let mut comment = false;
        let mut inside_word = false;
        let mut inside_string = false;
        let mut item = String::new();
        let mut tree = Tree::root("progn".to_owned());
        let root = tree.clone();

        self.line_num = 1;

        for c in expression.chars() {
            self.column_num += 1;
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
                    '\'' | '`' => {
                        if inside_word {
                            return Err(ParseError::InvalidSyntax {
                                message: format!(
                                    "\"{}\" is not a valid word character. line_num: {}, column_num: {}",
                                    c, self.line_num, self.column_num
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
                        self.line_num += 1;
                        self.column_num = 0;
                        inside_word = false;
                    }
                    ';' => {
                        comment = true;
                        continue;
                    }
                    _ => {
                        if c == '@' && item == "," {
                            item.push(c);
                            inside_word = false;
                        } else if c == ',' && inside_word {
                            return Err(ParseError::InvalidSyntax {
                                message: format!(
                                    "\"{}\" is not a valid word character. line_num: {}, column_num: {}",
                                    c, self.line_num, self.column_num
                                ),
                            });
                        } else {
                            inside_word = true;
                        }
                    }
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
                        self.line_num += 1;
                        self.column_num = 0;
                    }
                    _ => (),
                }
            } else if comment && c == '\n' {
                self.line_num += 1;
                self.column_num = 0;
                comment = false;
            }
        }
        if !item.is_empty() {
            if let Err(e) = self.add_to_tree(&tree, &item) {
                return Err(e);
            };
            item.clear();
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
                    let eval = Tree::add_child(node, "(".to_owned());
                    Tree::add_child(&eval, kind).borrow_mut().extra_up = true;
                    Ok(eval)
                }
                Token::Eval => Ok(Tree::add_child(node, "(".to_owned())),
                Token::Symbol => {
                    Tree::add_child(node, item.to_owned());
                    self.get_parent(node)
                }
                Token::Apply => match &node.borrow().parent {
                    Some(_) => self.get_parent(node),
                    None => Err(ParseError::InvalidSyntax {
                        message: format!(
                            "unexpected \")\". line_num: {}, col: {}",
                            self.line_num, self.column_num
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

    fn get_parent(&mut self, node: &NodePtr) -> Result<NodePtr, ParseError> {
        let mut parent = match Tree::get_parent(node) {
            Some(p) => p,
            None => {
                return Err(ParseError::InvalidSyntax {
                    message: format!(
                        "no parent found for expression at line {}, col {}",
                        self.line_num, self.column_num
                    ),
                })
            }
        };
        if parent.borrow().extra_up {
            parent = match self.get_parent(&parent) {
                Ok(p) => p,
                Err(e) => return Err(e),
            }
        }
        Ok(parent)
    }

    fn tokenize(&mut self, word: &str) -> Result<Token, ParseError> {
        let last_token = &self.last_token;

        let token = match word {
            "(" => Token::Eval,
            ")" => match last_token {
                Token::Quote { .. } => {
                    return Err(ParseError::InvalidSyntax {
                        message: format!(
                            "unexpected \")\". line_num: {}, col: {}",
                            self.line_num, self.column_num
                        ),
                    })
                }
                _ => Token::Apply,
            },
            "'" | "," | ",@" | "`" => Token::Quote {
                kind: match word {
                    "'" => "quote",
                    "," => "unquote",
                    "`" => "quasiquote",
                    ",@" => "unquote-splicing",
                    _ => {
                        return Err(ParseError::InvalidSyntax {
                            message: format!(
                                "unexpected quote type \"{}\". line_num: {}, col: {}",
                                word, self.line_num, self.column_num
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
                        message: "illegal use of \".\"".to_owned(),
                    });
                } else {
                    let last = tree.borrow().childs.last().unwrap().clone();
                    let data = last.borrow().data.clone();
                    if data == "(" {
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
