use crate::tree::{NodePtr, Tree};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct GRData {
    pub extra_up: bool,
    pub data: String,
    pub scope: HashMap<String, NodePtr<GRData>>,
}

impl PartialEq for GRData {
    fn eq(&self, other: &GRData) -> bool {
        self.data == other.data && self.scope == other.scope
    }
}

impl GRData {
    pub fn new() -> GRData {
        GRData {
            data: "".to_owned(),
            extra_up: false,
            scope: HashMap::new(),
        }
    }

    pub fn from(data: &str, extra_up: bool) -> GRData {
        GRData {
            data: data.to_owned(),
            extra_up,
            scope: HashMap::new(),
        }
    }

    pub fn from_str(data: &str) -> GRData {
        GRData {
            data: data.to_owned(),
            extra_up: false,
            scope: HashMap::new(),
        }
    }
}

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

#[derive(Debug)]
pub enum ParseError {
    InvalidSyntax {
        message: String,
    },
    UnmatchedParenthesis {
        line: u32,
        column: u32,
    },
    MismatchedParenthesis {
        line: u32,
        column: u32,
        expected: String,
        fact: String,
    },
    UnexpectedExpressionEnd {
        line: u32,
        column: u32,
    },
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            last_token: Token::None,
            line_num: 1,
            column_num: 0,
        }
    }

    pub fn parse(&mut self, expression: &str) -> Result<NodePtr<GRData>, ParseError> {
        let mut comment = false;
        let mut inside_word = false;
        let mut inside_string = false;
        let mut unquote = false;
        let mut item = String::new();
        let mut paren_stack = vec![];

        let mut tree = Tree::root(GRData::from_str("("));
        Tree::add_node(&tree, GRData::from_str("progn"));
        let root = tree.clone();

        self.line_num = 1;
        self.column_num = 0;

        for c in expression.chars() {
            self.column_num += 1;
            if !comment && !inside_string {
                match c {
                    '(' | '[' | '{' => {
                        paren_stack.push(c);
                        item.push(c);
                        inside_word = false;
                    }
                    ')' | ']' | '}' => {
                        let matching = match paren_stack.last() {
                            Some('(') => ')',
                            Some('[') => ']',
                            Some('{') => '}',
                            None | Some(_) => {
                                return Err(ParseError::UnmatchedParenthesis {
                                    line: self.line_num,
                                    column: self.column_num,
                                })
                            }
                        };
                        if c != matching {
                            return Err(ParseError::MismatchedParenthesis {
                                line: self.line_num,
                                column: self.column_num,
                                expected: matching.to_string(),
                                fact: c.to_string(),
                            });
                        }

                        paren_stack.pop();

                        if !item.is_empty() {
                            tree = self.add_to_tree(&tree, &item)?;
                            item.clear();
                        }
                        item.push(c);
                        inside_word = false;
                    }
                    '\'' | '`' => {
                        if unquote {
                            tree = self.add_to_tree(&tree, &item)?;
                            item.clear();
                            unquote = false;
                        } else if inside_word {
                            return Err(ParseError::InvalidSyntax {
                                    message: format!(
                                        "\"{}\" is not a valid word character. line: {}, column_num: {}",
                                        c, self.line_num, self.column_num
                                    ),
                                });
                        }
                        item.push(c);
                        inside_word = false;
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
                    '@' => {
                        item.push(c);
                        if unquote {
                            inside_word = false;
                            unquote = false;
                        }
                    }
                    ',' => {
                        if inside_word && item != "," {
                            return Err(ParseError::InvalidSyntax {
                                    message: format!(
                                        "\"{}\" is not a valid word character. line: {}, column_num: {}",
                                        c, self.line_num, self.column_num
                                    ),
                            });
                        }
                        if unquote {
                            tree = self.add_to_tree(&tree, &item)?;
                            item.clear();
                        }
                        unquote = true;
                        inside_word = true;
                    }
                    _ => {
                        if unquote {
                            tree = self.add_to_tree(&tree, &item)?;
                            item.clear();
                            unquote = false;
                        }
                        inside_word = true;
                    }
                }
                if inside_word {
                    item.push(c);
                } else if !item.is_empty() {
                    tree = self.add_to_tree(&tree, &item)?;
                    item.clear();
                    unquote = false;
                }
            } else if inside_string {
                item.push(c);
                match c {
                    '"' => {
                        inside_string = false;
                        tree = self.add_to_tree(&tree, &item)?;
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
            self.add_to_tree(&tree, &item)?;
            item.clear();
        }

        if inside_string {
            Err(ParseError::InvalidSyntax {
                message: "end of expression reached while parsing string".to_owned(),
            })
        } else {
            Self::remove_dots(&root)?;
            Ok(root)
        }
    }

    fn add_to_tree(
        &mut self,
        node: &NodePtr<GRData>,
        item: &str,
    ) -> Result<NodePtr<GRData>, ParseError> {
        let token = self.tokenize(item)?;
        match token {
            Token::Quote { kind } => {
                let eval = Tree::add_node(node, GRData::from("(", true));
                Tree::add_node(&eval, GRData::from_str(&kind));
                Ok(eval)
            }
            Token::Eval => Ok(Tree::add_node(node, GRData::from_str("("))),
            Token::Symbol => {
                Tree::add_node(node, GRData::from_str(item));
                self.get_parent(node)
            }
            Token::Apply => self.get_parent(node),
            _ => {
                Tree::add_node(node, GRData::from_str(item));
                Ok(node.clone())
            }
        }
    }

    fn get_parent(&mut self, node: &NodePtr<GRData>) -> Result<NodePtr<GRData>, ParseError> {
        match Tree::get_parent(node) {
            Some(parent) => {
                if parent.borrow().data.extra_up {
                    self.get_parent(&parent)
                } else {
                    Ok(parent)
                }
            }
            None => Err(ParseError::UnexpectedExpressionEnd {
                line: self.line_num,
                column: self.column_num,
            }),
        }
    }

    fn tokenize(&mut self, word: &str) -> Result<Token, ParseError> {
        let last_token = &self.last_token;

        let token = match word {
            "(" | "[" | "{" => Token::Eval,
            ")" | "]" | "}" => match last_token {
                Token::Quote { .. } => {
                    return Err(ParseError::UnexpectedExpressionEnd {
                        line: self.line_num,
                        column: self.column_num,
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
                                "unexpected quote type \"{}\". line: {}, column: {}",
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

    pub fn remove_dots(tree: &NodePtr<GRData>) -> Result<(), ParseError> {
        let mut has_dot = false;

        for (n, child) in tree.borrow().nodes.iter().enumerate() {
            Self::remove_dots(&child)?;
            if child.borrow().data.data == "." {
                if !has_dot || n != tree.borrow().nodes.len() - 2 {
                    has_dot = true;
                } else {
                    return Err(ParseError::InvalidSyntax {
                        message: "illegal use of \".\"".to_owned(),
                    });
                }
            }
        }
        if has_dot && tree.borrow().nodes.last().unwrap().borrow().data.data == "(" {
            let list = tree.borrow_mut().nodes.pop().unwrap(); // extract child list
            tree.borrow_mut().nodes.pop(); // remove the dot
            tree.borrow_mut().nodes.append(&mut list.borrow_mut().nodes); // append extracted list to current node nodes
                                                                          // re-traverse current node
            Self::remove_dots(&tree)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{GRData, ParseError, Parser};
    use crate::tree::{NodePtr, Tree};

    #[test]
    fn valid_tree_1() {
        let root = Tree::root(GRData::from_str("("));
        Tree::add_node(&root, GRData::from_str("progn"));
        let expr = Tree::add_node(&root, GRData::from_str("("));
        Tree::add_node(&expr, GRData::from_str("quote"));
        Tree::add_node(&expr, GRData::from_str("a"));

        test_parse("'a", &root);
        test_parse("(quote a)", &root);
    }

    #[test]
    fn valid_tree_2() {
        let root = Tree::root(GRData::from_str("("));
        Tree::add_node(&root, GRData::from_str("progn"));
        let expr = Tree::add_node(&root, GRData::from_str("("));
        Tree::add_node(&expr, GRData::from_str("quote"));
        let expr = Tree::add_node(&expr, GRData::from_str("("));
        Tree::add_node(&expr, GRData::from_str("a"));
        Tree::add_node(&expr, GRData::from_str("b"));

        test_parse("'(a b)", &root);
        test_parse("(quote (a b))", &root);
    }

    #[test]
    fn valid_tree_3() {
        let root = Tree::root(GRData::from_str("("));
        Tree::add_node(&root, GRData::from_str("progn"));
        let quasiquote = Tree::add_node(&root, GRData::from_str("("));
        Tree::add_node(&quasiquote, GRData::from_str("quasiquote"));
        let expr = Tree::add_node(&quasiquote, GRData::from_str("("));
        Tree::add_node(&expr, GRData::from_str("a"));
        let unquote = Tree::add_node(&expr, GRData::from_str("("));
        Tree::add_node(&unquote, GRData::from_str("unquote"));
        Tree::add_node(&unquote, GRData::from_str("b"));
        Tree::add_node(&expr, GRData::from_str("c"));

        test_parse("`(a ,b c)", &root);
        test_parse("`(a (unquote b) c)", &root);
        test_parse("(quasiquote (a ,b c))", &root);
        test_parse("(quasiquote (a (unquote b) c))", &root);
    }

    #[test]
    fn valid_tree_4() {
        let root = Tree::root(GRData::from_str("("));
        Tree::add_node(&root, GRData::from_str("progn"));
        let quasiquote = Tree::add_node(&root, GRData::from_str("("));
        Tree::add_node(&quasiquote, GRData::from_str("quasiquote"));
        let expr1 = Tree::add_node(&quasiquote, GRData::from_str("("));
        let quote = Tree::add_node(&expr1, GRData::from_str("("));
        Tree::add_node(&quote, GRData::from_str("quote"));
        Tree::add_node(&quote, GRData::from_str("a"));
        let unquote_splicing = Tree::add_node(&expr1, GRData::from_str("("));
        Tree::add_node(&unquote_splicing, GRData::from_str("unquote-splicing"));
        let expr2 = Tree::add_node(&unquote_splicing, GRData::from_str("("));
        Tree::add_node(&expr2, GRData::from_str("b"));
        Tree::add_node(&expr2, GRData::from_str("c"));
        Tree::add_node(&expr1, GRData::from_str("d"));

        test_parse("`('a ,@(b c) d)", &root);
        test_parse("`('a (unquote-splicing (b c)) d)", &root);
        test_parse("`((quote a) ,@(b c) d)", &root);
        test_parse("`((quote a) (unquote-splicing (b c)) d)", &root);
        test_parse("(quasiquote ((quote a) (unquote-splicing (b c)) d))", &root);
    }

    #[test]
    fn valid_tree_6() {
        let root = Tree::root(GRData::from_str("("));
        Tree::add_node(&root, GRData::from_str("progn"));
        let quote = Tree::add_node(&root, GRData::from_str("("));
        Tree::add_node(&quote, GRData::from_str("quote"));
        let quote = Tree::add_node(&quote, GRData::from_str("("));
        Tree::add_node(&quote, GRData::from_str("quote"));
        let quote = Tree::add_node(&quote, GRData::from_str("("));
        Tree::add_node(&quote, GRData::from_str("quote"));
        Tree::add_node(&quote, GRData::from_str("a"));

        test_parse("'''a", &root);
        test_parse("''(quote a)", &root);
        test_parse("'(quote (quote a))", &root);
        test_parse("(quote (quote (quote a)))", &root);
    }

    #[test]
    fn valid_tree_7() {
        let root = Tree::root(GRData::from_str("("));
        Tree::add_node(&root, GRData::from_str("progn"));
        let quasiquote = Tree::add_node(&root, GRData::from_str("("));
        Tree::add_node(&quasiquote, GRData::from_str("quasiquote"));
        let quote = Tree::add_node(&quasiquote, GRData::from_str("("));
        Tree::add_node(&quote, GRData::from_str("quote"));
        let unquote = Tree::add_node(&quote, GRData::from_str("("));
        Tree::add_node(&unquote, GRData::from_str("unquote"));
        let unquote = Tree::add_node(&unquote, GRData::from_str("("));
        Tree::add_node(&unquote, GRData::from_str("unquote"));
        let unquote_splicing = Tree::add_node(&unquote, GRData::from_str("("));
        Tree::add_node(&unquote_splicing, GRData::from_str("unquote-splicing"));
        Tree::add_node(&unquote_splicing, GRData::from_str("a"));

        test_parse("`',,,@a", &root);
        test_parse("(quasiquote ',,,@a)", &root);
        test_parse("(quasiquote (quote ,,,@a))", &root);
        test_parse("(quasiquote (quote (unquote (unquote ,@a))))", &root);
        test_parse(
            "(quasiquote (quote (unquote (unquote (unquote-splicing a)))))",
            &root,
        );
    }

    #[test]
    fn valid_tree_8() {
        let root = Tree::root(GRData::from_str("("));
        Tree::add_node(&root, GRData::from_str("progn"));
        let quasiquote = Tree::add_node(&root, GRData::from_str("("));
        Tree::add_node(&quasiquote, GRData::from_str("quasiquote"));
        let quote = Tree::add_node(&quasiquote, GRData::from_str("("));
        Tree::add_node(&quote, GRData::from_str("quote"));
        let unquote = Tree::add_node(&quote, GRData::from_str("("));
        Tree::add_node(&unquote, GRData::from_str("unquote"));
        let quasiquote = Tree::add_node(&unquote, GRData::from_str("("));
        Tree::add_node(&quasiquote, GRData::from_str("quasiquote"));
        let quasiquote = Tree::add_node(&quasiquote, GRData::from_str("("));
        Tree::add_node(&quasiquote, GRData::from_str("quasiquote"));
        let quote = Tree::add_node(&quasiquote, GRData::from_str("("));
        Tree::add_node(&quote, GRData::from_str("quote"));
        let unquote = Tree::add_node(&quote, GRData::from_str("("));
        Tree::add_node(&unquote, GRData::from_str("unquote"));
        let quasiquote = Tree::add_node(&unquote, GRData::from_str("("));
        Tree::add_node(&quasiquote, GRData::from_str("quasiquote"));
        let quasiquote = Tree::add_node(&quasiquote, GRData::from_str("("));
        Tree::add_node(&quasiquote, GRData::from_str("quasiquote"));
        let quote = Tree::add_node(&quasiquote, GRData::from_str("("));
        Tree::add_node(&quote, GRData::from_str("quote"));
        let quote = Tree::add_node(&quote, GRData::from_str("("));
        Tree::add_node(&quote, GRData::from_str("quote"));
        let unquote = Tree::add_node(&quote, GRData::from_str("("));
        Tree::add_node(&unquote, GRData::from_str("unquote"));
        let unquote = Tree::add_node(&unquote, GRData::from_str("("));
        Tree::add_node(&unquote, GRData::from_str("unquote"));
        let unquote_splicing = Tree::add_node(&unquote, GRData::from_str("("));
        Tree::add_node(&unquote_splicing, GRData::from_str("unquote-splicing"));
        Tree::add_node(&unquote_splicing, GRData::from_str("a"));

        test_parse("`',``',``'',,,@a", &root);
        test_parse("(quasiquote ',``',``'',,,@a)", &root);
        test_parse("(quasiquote (quote ,``',``'',,,@a))", &root);
        test_parse("(quasiquote (quote (unquote ``',``'',,,@a)))", &root);
        test_parse(
            "(quasiquote (quote (unquote (quasiquote `',``'',,,@a))))",
            &root,
        );
        test_parse(
            "(quasiquote (quote (unquote (quasiquote (quasiquote ',``'',,,@a)))))",
            &root,
        );
        test_parse(
            "(quasiquote (quote (unquote (quasiquote (quasiquote (quote ,``'',,,@a))))))",
            &root,
        );
        test_parse(
            "(quasiquote (quote (unquote (quasiquote (quasiquote (quote (unquote ``'',,,@a)))))))",
            &root,
        );
        test_parse("(quasiquote (quote (unquote (quasiquote (quasiquote (quote (unquote (quasiquote `'',,,@a))))))))", &root);
        test_parse("(quasiquote (quote (unquote (quasiquote (quasiquote (quote (unquote (quasiquote (quasiquote '',,,@a)))))))))", &root);
        test_parse("(quasiquote (quote (unquote (quasiquote (quasiquote (quote (unquote (quasiquote (quasiquote (quote ',,,@a))))))))))", &root);
        test_parse("(quasiquote (quote (unquote (quasiquote (quasiquote (quote (unquote (quasiquote (quasiquote (quote (quote ,,,@a)))))))))))", &root);
        test_parse("(quasiquote (quote (unquote (quasiquote (quasiquote (quote (unquote (quasiquote (quasiquote (quote (quote (unquote ,,@a))))))))))))", &root);
        test_parse("(quasiquote (quote (unquote (quasiquote (quasiquote (quote (unquote (quasiquote (quasiquote (quote (quote (unquote (unquote ,@a)))))))))))))", &root);
        test_parse("(quasiquote (quote (unquote (quasiquote (quasiquote (quote (unquote (quasiquote (quasiquote (quote (quote (unquote (unquote (unquote-splicing a)))))))))))))", &root);
    }

    #[test]
    fn valid_tree_9() {
        let root = Tree::root(GRData::from_str("("));
        Tree::add_node(&root, GRData::from_str("progn"));
        let lambda = Tree::add_node(&root, GRData::from_str("("));
        Tree::add_node(&lambda, GRData::from_str("lambda"));
        Tree::add_node(&lambda, GRData::from_str("x"));
        let body = Tree::add_node(&lambda, GRData::from_str("("));
        Tree::add_node(&body, GRData::from_str("length"));
        Tree::add_node(&body, GRData::from_str("x"));

        test_parse("(lambda x (length x))", &root);
    }

    #[test]
    fn valid_tree_10() {
        let root = Tree::root(GRData::from_str("("));
        Tree::add_node(&root, GRData::from_str("progn"));
        let lambda = Tree::add_node(&root, GRData::from_str("("));
        Tree::add_node(&lambda, GRData::from_str("lambda"));
        let args = Tree::add_node(&lambda, GRData::from_str("("));
        Tree::add_node(&args, GRData::from_str("x"));
        Tree::add_node(&args, GRData::from_str("y"));
        let body = Tree::add_node(&lambda, GRData::from_str("("));
        Tree::add_node(&body, GRData::from_str("+"));
        Tree::add_node(&body, GRData::from_str("x"));
        Tree::add_node(&body, GRData::from_str("y"));

        test_parse("(lambda (x y) (+ x y))", &root);
    }

    #[test]
    fn valid_tree_11() {
        let root = Tree::root(GRData::from_str("("));
        Tree::add_node(&root, GRData::from_str("progn"));
        let let_proc = Tree::add_node(&root, GRData::from_str("("));
        Tree::add_node(&let_proc, GRData::from_str("let"));
        let bindings = Tree::add_node(&let_proc, GRData::from_str("("));
        Tree::add_node(&bindings, GRData::from_str("a"));
        Tree::add_node(&bindings, GRData::from_str("220"));
        Tree::add_node(&bindings, GRData::from_str("b"));
        Tree::add_node(&bindings, GRData::from_str("8"));
        let body = Tree::add_node(&let_proc, GRData::from_str("("));
        Tree::add_node(&body, GRData::from_str("+"));
        Tree::add_node(&body, GRData::from_str("a"));
        Tree::add_node(&body, GRData::from_str("b"));

        test_parse("(let (a 220 b 8) (+ a b))", &root);
    }

    #[test]
    fn valid_tree_12() {
        let root = Tree::root(GRData::from_str("("));
        Tree::add_node(&root, GRData::from_str("progn"));
        let define = Tree::add_node(&root, GRData::from_str("("));
        Tree::add_node(&define, GRData::from_str("define"));
        Tree::add_node(&define, GRData::from_str("vaiv"));
        let quote = Tree::add_node(&define, GRData::from_str("("));
        Tree::add_node(&quote, GRData::from_str("quote"));
        Tree::add_node(&quote, GRData::from_str("daun"));

        test_parse("(define vaiv 'daun)", &root);
        test_parse("(define vaiv (quote daun)", &root);
    }

    #[test]
    fn invalid_tree_1() {
        let mut p = Parser::new();
        let inputs = vec!["'(1 2 3))", "'[1 2 3]]", "'{1 2 3}}"];
        for input in inputs.iter() {
            match p.parse(input) {
                Err(ParseError::UnmatchedParenthesis { line, column }) => {
                    assert_eq!((line, column), (1, 9))
                }
                Ok(_) => panic!("parsed correctly"),
                Err(e) => panic!("unexpected error {:?}", e),
            }
        }
    }

    #[test]
    fn invalid_tree_2() {
        let mut p = Parser::new();
        let inputs = vec!["(quote [a b c)]", "{quote [1 2 3}]", "{quote (1 2 3]}"];
        for input in inputs.iter() {
            match p.parse(input) {
                Err(ParseError::MismatchedParenthesis { line, column, .. }) => {
                    assert_eq!((line, column), (1, 14))
                }
                Ok(_) => panic!("parsed correctly"),
                Err(e) => panic!("unexpected error {:?}", e),
            }
        }
    }

    fn test_parse(input: &str, valid_tree: &NodePtr<GRData>) {
        let mut p = Parser::new();
        match p.parse(input) {
            Ok(res) => assert_eq!(res.clone(), valid_tree.clone(), "\n input: \"{}\"\n", input),
            Err(e) => panic!("{:?}", e),
        }
    }
}
