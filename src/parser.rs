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

    pub fn parse(&mut self, expression: &str) -> Result<NodePtr, ParseError> {
        let mut comment = false;
        let mut inside_word = false;
        let mut inside_string = false;
        let mut unquote = false;
        let mut item = String::new();
        let mut paren_stack = vec![];

        let mut tree = Tree::root("(".to_owned());
        Tree::add_child(&tree, "progn".to_owned());
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
            Err(ParseError::InvalidSyntax { message: "end of expression reached while parsing string".to_owned() })
        } else {
            Self::remove_dots(&root)?;
            Ok(root)
        }
    }

    fn add_to_tree(&mut self, node: &NodePtr, item: &str) -> Result<NodePtr, ParseError> {
        let token = self.tokenize(item)?;
        match token {
            Token::Quote { kind } => {
                let eval = Tree::add_child(node, "(".to_owned());
                eval.borrow_mut().extra_up = true;
                Tree::add_child(&eval, kind);
                Ok(eval)
            }
            Token::Eval => Ok(Tree::add_child(node, "(".to_owned())),
            Token::Symbol => {
                Tree::add_child(node, item.to_owned());
                self.get_parent(node)
            }
            Token::Apply => self.get_parent(node),
            _ => {
                Tree::add_child(node, item.to_owned());
                Ok(node.clone())
            }
        }
    }

    fn get_parent(&mut self, node: &NodePtr) -> Result<NodePtr, ParseError> {
        match Tree::get_parent(node) {
            Some(parent) => {
                if parent.borrow().extra_up {
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

    pub fn remove_dots(tree: &NodePtr) -> Result<(), ParseError> {
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
            Self::remove_dots(&child)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{ParseError, Parser};
    use crate::tree::{NodePtr, Tree};

    #[test]
    fn valid_tree_1() {
        let root = Tree::root("(".to_owned());
        Tree::add_child(&root, "progn".to_owned());
        let expr = Tree::add_child(&root, "(".to_owned());
        Tree::add_child(&expr, "quote".to_owned());
        Tree::add_child(&expr, "a".to_owned());

        test_parse("'a", &root);
        test_parse("(quote a)", &root);
    }

    #[test]
    fn valid_tree_2() {
        let root = Tree::root("(".to_owned());
        Tree::add_child(&root, "progn".to_owned());
        let expr = Tree::add_child(&root, "(".to_owned());
        Tree::add_child(&expr, "quote".to_owned());
        let expr = Tree::add_child(&expr, "(".to_owned());
        Tree::add_child(&expr, "a".to_owned());
        Tree::add_child(&expr, "b".to_owned());

        test_parse("'(a b)", &root);
        test_parse("(quote (a b))", &root);
    }

    #[test]
    fn valid_tree_3() {
        let root = Tree::root("(".to_owned());
        Tree::add_child(&root, "progn".to_owned());
        let quasiquote = Tree::add_child(&root, "(".to_owned());
        Tree::add_child(&quasiquote, "quasiquote".to_owned());
        let expr = Tree::add_child(&quasiquote, "(".to_owned());
        Tree::add_child(&expr, "a".to_owned());
        let unquote = Tree::add_child(&expr, "(".to_owned());
        Tree::add_child(&unquote, "unquote".to_owned());
        Tree::add_child(&unquote, "b".to_owned());
        Tree::add_child(&expr, "c".to_owned());

        test_parse("`(a ,b c)", &root);
        test_parse("`(a (unquote b) c)", &root);
        test_parse("(quasiquote (a ,b c))", &root);
        test_parse("(quasiquote (a (unquote b) c))", &root);
    }

    #[test]
    fn valid_tree_4() {
        let root = Tree::root("(".to_owned());
        Tree::add_child(&root, "progn".to_owned());
        let quasiquote = Tree::add_child(&root, "(".to_owned());
        Tree::add_child(&quasiquote, "quasiquote".to_owned());
        let expr1 = Tree::add_child(&quasiquote, "(".to_owned());
        let quote = Tree::add_child(&expr1, "(".to_owned());
        Tree::add_child(&quote, "quote".to_owned());
        Tree::add_child(&quote, "a".to_owned());
        let unquote_splicing = Tree::add_child(&expr1, "(".to_owned());
        Tree::add_child(&unquote_splicing, "unquote-splicing".to_owned());
        let expr2 = Tree::add_child(&unquote_splicing, "(".to_owned());
        Tree::add_child(&expr2, "b".to_owned());
        Tree::add_child(&expr2, "c".to_owned());
        Tree::add_child(&expr1, "d".to_owned());

        test_parse("`('a ,@(b c) d)", &root);
        test_parse("`('a (unquote-splicing (b c)) d)", &root);
        test_parse("`((quote a) ,@(b c) d)", &root);
        test_parse("`((quote a) (unquote-splicing (b c)) d)", &root);
        test_parse("(quasiquote ((quote a) (unquote-splicing (b c)) d))", &root);
    }

    #[test]
    fn valid_tree_6() {
        let root = Tree::root("(".to_owned());
        Tree::add_child(&root, "progn".to_owned());
        let quote = Tree::add_child(&root, "(".to_owned());
        Tree::add_child(&quote, "quote".to_owned());
        let quote = Tree::add_child(&quote, "(".to_owned());
        Tree::add_child(&quote, "quote".to_owned());
        let quote = Tree::add_child(&quote, "(".to_owned());
        Tree::add_child(&quote, "quote".to_owned());
        Tree::add_child(&quote, "a".to_owned());

        test_parse("'''a", &root);
        test_parse("''(quote a)", &root);
        test_parse("'(quote (quote a))", &root);
        test_parse("(quote (quote (quote a)))", &root);
    }

    #[test]
    fn valid_tree_7() {
        let root = Tree::root("(".to_owned());
        Tree::add_child(&root, "progn".to_owned());
        let quasiquote = Tree::add_child(&root, "(".to_owned());
        Tree::add_child(&quasiquote, "quasiquote".to_owned());
        let quote = Tree::add_child(&quasiquote, "(".to_owned());
        Tree::add_child(&quote, "quote".to_owned());
        let unquote = Tree::add_child(&quote, "(".to_owned());
        Tree::add_child(&unquote, "unquote".to_owned());
        let unquote = Tree::add_child(&unquote, "(".to_owned());
        Tree::add_child(&unquote, "unquote".to_owned());
        let unquote_splicing = Tree::add_child(&unquote, "(".to_owned());
        Tree::add_child(&unquote_splicing, "unquote-splicing".to_owned());
        Tree::add_child(&unquote_splicing, "a".to_owned());

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
        let root = Tree::root("(".to_owned());
        Tree::add_child(&root, "progn".to_owned());
        let quasiquote = Tree::add_child(&root, "(".to_owned());
        Tree::add_child(&quasiquote, "quasiquote".to_owned());
        let quote = Tree::add_child(&quasiquote, "(".to_owned());
        Tree::add_child(&quote, "quote".to_owned());
        let unquote = Tree::add_child(&quote, "(".to_owned());
        Tree::add_child(&unquote, "unquote".to_owned());
        let quasiquote = Tree::add_child(&unquote, "(".to_owned());
        Tree::add_child(&quasiquote, "quasiquote".to_owned());
        let quasiquote = Tree::add_child(&quasiquote, "(".to_owned());
        Tree::add_child(&quasiquote, "quasiquote".to_owned());
        let quote = Tree::add_child(&quasiquote, "(".to_owned());
        Tree::add_child(&quote, "quote".to_owned());
        let unquote = Tree::add_child(&quote, "(".to_owned());
        Tree::add_child(&unquote, "unquote".to_owned());
        let quasiquote = Tree::add_child(&unquote, "(".to_owned());
        Tree::add_child(&quasiquote, "quasiquote".to_owned());
        let quasiquote = Tree::add_child(&quasiquote, "(".to_owned());
        Tree::add_child(&quasiquote, "quasiquote".to_owned());
        let quote = Tree::add_child(&quasiquote, "(".to_owned());
        Tree::add_child(&quote, "quote".to_owned());
        let quote = Tree::add_child(&quote, "(".to_owned());
        Tree::add_child(&quote, "quote".to_owned());
        let unquote = Tree::add_child(&quote, "(".to_owned());
        Tree::add_child(&unquote, "unquote".to_owned());
        let unquote = Tree::add_child(&unquote, "(".to_owned());
        Tree::add_child(&unquote, "unquote".to_owned());
        let unquote_splicing = Tree::add_child(&unquote, "(".to_owned());
        Tree::add_child(&unquote_splicing, "unquote-splicing".to_owned());
        Tree::add_child(&unquote_splicing, "a".to_owned());

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
        let root = Tree::root("(".to_owned());
        Tree::add_child(&root, "progn".to_owned());
        let lambda = Tree::add_child(&root, "(".to_owned());
        Tree::add_child(&lambda, "lambda".to_owned());
        Tree::add_child(&lambda, "x".to_owned());
        let body = Tree::add_child(&lambda, "(".to_owned());
        Tree::add_child(&body, "length".to_owned());
        Tree::add_child(&body, "x".to_owned());

        test_parse("(lambda x (length x))", &root);
    }

    #[test]
    fn valid_tree_10() {
        let root = Tree::root("(".to_owned());
        Tree::add_child(&root, "progn".to_owned());
        let lambda = Tree::add_child(&root, "(".to_owned());
        Tree::add_child(&lambda, "lambda".to_owned());
        let args = Tree::add_child(&lambda, "(".to_owned());
        Tree::add_child(&args, "x".to_owned());
        Tree::add_child(&args, "y".to_owned());
        let body = Tree::add_child(&lambda, "(".to_owned());
        Tree::add_child(&body, "+".to_owned());
        Tree::add_child(&body, "x".to_owned());
        Tree::add_child(&body, "y".to_owned());

        test_parse("(lambda (x y) (+ x y))", &root);
    }

    #[test]
    fn valid_tree_11() {
        let root = Tree::root("(".to_owned());
        Tree::add_child(&root, "progn".to_owned());
        let let_proc = Tree::add_child(&root, "(".to_owned());
        Tree::add_child(&let_proc, "let".to_owned());
        let bindings = Tree::add_child(&let_proc, "(".to_owned());
        Tree::add_child(&bindings, "a".to_owned());
        Tree::add_child(&bindings, "220".to_owned());
        Tree::add_child(&bindings, "b".to_owned());
        Tree::add_child(&bindings, "8".to_owned());
        let body = Tree::add_child(&let_proc, "(".to_owned());
        Tree::add_child(&body, "+".to_owned());
        Tree::add_child(&body, "a".to_owned());
        Tree::add_child(&body, "b".to_owned());

        test_parse("(let (a 220 b 8) (+ a b))", &root);
    }

    #[test]
    fn valid_tree_12() {
        let root = Tree::root("(".to_owned());
        Tree::add_child(&root, "progn".to_owned());
        let define = Tree::add_child(&root, "(".to_owned());
        Tree::add_child(&define, "define".to_owned());
        Tree::add_child(&define, "vaiv".to_owned());
        let quote = Tree::add_child(&define, "(".to_owned());
        Tree::add_child(&quote, "quote".to_owned());
        Tree::add_child(&quote, "daun".to_owned());

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

    fn test_parse(input: &str, valid_tree: &NodePtr) {
        let mut p = Parser::new();
        match p.parse(input) {
            Ok(res) => assert_eq!(
                Tree::tree_to_string(&res),
                Tree::tree_to_string(valid_tree),
                "\n input: \"{}\"\n",
                input
            ),
            Err(e) => panic!("{:?}", e),
        }
    }
}
