use crate::tree::{self, Tree};
use rug::{Integer, Rational};
use std::collections::HashMap;
use std::fmt;
use std::io::Write;

pub type NodePtr = tree::NodePtr<GRData>;

#[derive(Debug, Clone, PartialEq)]
pub enum Data {
    Integer { data: Integer },
    Float { data: f64 },
    Rational { data: Rational },
    String { data: String },
}

impl ToString for Data {
    fn to_string(&self) -> String {
        match self {
            Data::Integer { data } => data.to_string(),
            Data::Float { data } => data.to_string(),
            Data::Rational { data } => data.to_string(),
            Data::String { data } => data.clone(),
        }
    }
}

impl Data {
    pub fn deduce_type_and_convert(data: &str) -> Data {
        if let Ok(data) = data.trim().parse::<Integer>() {
            Data::Integer { data }
        } else if let Ok(data) = data.trim().parse::<f64>() {
            Data::Float { data }
        } else if let Ok(data) = data.trim().parse::<Rational>() {
            Data::Rational { data }
        } else {
            Data::String {
                data: data.to_owned(),
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct GRData {
    pub data: Data,
    pub extra_up: bool,
    pub user_defined_procedure: bool,
    pub scope: HashMap<String, NodePtr>,
}

impl PartialEq for GRData {
    fn eq(&self, other: &GRData) -> bool {
        self.data == other.data && self.scope == other.scope
    }
}

impl GRData {
    pub fn from(data: &str, extra_up: bool) -> GRData {
        GRData {
            data: Data::deduce_type_and_convert(data),
            extra_up,
            user_defined_procedure: false,
            scope: HashMap::new(),
        }
    }

    pub fn from_proc_str(data: &str) -> GRData {
        GRData {
            data: Data::deduce_type_and_convert(data),
            extra_up: false,
            user_defined_procedure: true,
            scope: HashMap::new(),
        }
    }

    pub fn from_str(data: &str) -> GRData {
        GRData {
            data: Data::deduce_type_and_convert(data),
            extra_up: false,
            user_defined_procedure: false,
            scope: HashMap::new(),
        }
    }
}

impl ToString for GRData {
    fn to_string(&self) -> String {
        let data = self.data.to_string();
        if data == "(" {
            "open_paren".to_owned()
        } else {
            data
        }
    }
}

#[derive(Debug, Clone)]
pub enum Token {
    Value,
    Eval,
    Apply,
    Quote { kind: &'static str },
    Symbol,
    None,
}

#[derive(Debug)]
pub struct Reader {
    last_token: Token,
    line_num: u32,
    column_num: u32,
}

#[derive(Debug)]
pub enum ReadError {
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
    InvalidInput {
        character: char,
        line: u32,
        column: u32,
    },
}

impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let msg = match self {
            Self::InvalidSyntax { message } => message.to_owned(),
            Self::UnmatchedParenthesis { line, column } => {
                format!("unmatched parenthesis at line {}, column {}", line, column)
            }
            Self::MismatchedParenthesis {
                line,
                column,
                expected,
                fact,
            } => format!(
                "mismatched parenthesis: expected {}, got {} at line {}, column {}",
                expected, fact, line, column
            ),
            Self::UnexpectedExpressionEnd { line, column } => format!(
                "unexpected expression end reached at line: {}, column: {}",
                line, column
            ),
            Self::InvalidInput {
                character,
                line,
                column,
            } => format!(
                "unexpected character {} at line {}, column: {}",
                character, line, column
            ),
        };
        write!(f, "{}", msg)
    }
}

impl Reader {
    pub fn new() -> Reader {
        Reader {
            last_token: Token::None,
            line_num: 1,
            column_num: 0,
        }
    }

    pub fn parse(&mut self, expression: &str) -> Result<NodePtr, ReadError> {
        let mut comment = false;
        let mut inside_word = false;
        let mut inside_string = false;
        let mut unquote = false;
        let mut escaped = false;
        let mut paren_stack = vec![];

        let mut item = String::new();

        let mut tree = Tree::new(GRData::from_str("("));
        Tree::push_child(&tree, GRData::from_str("progn"));
        let root = tree.clone();

        self.line_num = 1;
        self.column_num = 0;

        for c in expression.chars() {
            self.column_num += 1;
            if !comment && !inside_string {
                match c {
                    '(' | '[' | '{' => {
                        if !item.is_empty() {
                            tree = self.add_to_tree(&tree, &item)?;
                            item.clear();
                        }
                        paren_stack.push(c);
                        item.push(c);
                        inside_word = false;
                    }
                    ')' | ']' | '}' => {
                        let matching = match paren_stack.pop() {
                            Some('(') => ')',
                            Some('[') => ']',
                            Some('{') => '}',
                            None | Some(_) => {
                                return Err(ReadError::UnmatchedParenthesis {
                                    line: self.line_num,
                                    column: self.column_num,
                                })
                            }
                        };

                        if c != matching {
                            return Err(ReadError::MismatchedParenthesis {
                                line: self.line_num,
                                column: self.column_num,
                                expected: matching.to_string(),
                                fact: c.to_string(),
                            });
                        }

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
                            return Err(ReadError::InvalidSyntax {
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
                            return Err(ReadError::InvalidSyntax {
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
                    '\\' => escaped = true,
                    '"' if !escaped => {
                        inside_string = false;
                        tree = self.add_to_tree(&tree, &item)?;
                        item.clear();
                        escaped = false;
                    }
                    '\n' => {
                        escaped = false;
                        self.line_num += 1;
                        self.column_num = 0;
                    }
                    _ => escaped = false,
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
            Err(ReadError::InvalidSyntax {
                message: "end of expression reached while parsing string".to_owned(),
            })
        } else {
            Ok(root)
        }
    }

    pub fn balanced_read(prompt: &str) -> Result<String, ReadError> {
        let mut paren_count: i32 = 0;
        let mut bracket_count: i32 = 0;
        let mut curly_count: i32 = 0;
        let mut escaped = false;
        let mut inside_string = false;
        let mut comment = false;
        let mut line = String::new();
        let mut expression = String::new();

        print!("{}", prompt);
        std::io::stdout().flush().ok();

        let mut line_n = 0;
        loop {
            line_n += 1;
            std::io::stdin().read_line(&mut line).unwrap_or_default();
            for (column_n, c) in line.chars().enumerate() {
                if !inside_string && !comment {
                    match c {
                        '(' => paren_count += 1,
                        ')' => paren_count -= 1,
                        '[' => bracket_count += 1,
                        ']' => bracket_count -= 1,
                        '{' => curly_count += 1,
                        '}' => curly_count -= 1,
                        '"' if !escaped => inside_string = true,
                        '\\' => escaped = true,
                        ';' => {
                            comment = true;
                            continue;
                        }
                        _ => (),
                    }
                } else if inside_string {
                    match c {
                        '\\' => escaped = true,
                        '"' => {
                            if !escaped {
                                inside_string = false;
                            }
                            escaped = false;
                        }
                        _ => escaped = false,
                    }
                }
                if paren_count < 0 || curly_count < 0 || bracket_count < 0 {
                    return Err(ReadError::InvalidInput {
                        character: c,
                        line: line_n,
                        column: column_n as u32 + 1,
                    });
                }
            }
            comment = false;
            if !line.is_empty() {
                expression.push_str(&line);
                line.clear();
            }
            if paren_count == 0 && curly_count == 0 && bracket_count == 0 && !inside_string {
                break;
            } else {
                for _ in 0..prompt.len() {
                    print!(" ");
                }
                std::io::stdout().flush().ok();
            }
        }
        Ok(expression)
    }

    fn add_to_tree(&mut self, node: &NodePtr, item: &str) -> Result<NodePtr, ReadError> {
        match self.tokenize(item)? {
            Token::Quote { kind } => {
                let eval = Tree::push_child(node, GRData::from("(", true));
                Tree::push_child(&eval, GRData::from_str(&kind));
                Ok(eval)
            }
            Token::Eval => Ok(Tree::push_child(node, GRData::from_str("("))),
            Token::Symbol => {
                Tree::push_child(node, GRData::from_str(item));
                self.parent(node)
            }
            Token::Apply => self.parent(node),
            _ => {
                Tree::push_child(node, GRData::from_str(item));
                Ok(node.clone())
            }
        }
    }

    fn parent(&mut self, node: &NodePtr) -> Result<NodePtr, ReadError> {
        let mut current = node.clone();
        while let Some(parent) = Tree::parent(&current) {
            if parent.borrow().data.extra_up {
                current = parent;
                current.borrow_mut().data.extra_up = false;
            } else {
                return Ok(parent);
            }
        }
        Err(ReadError::UnexpectedExpressionEnd {
            line: self.line_num,
            column: self.column_num,
        })
    }

    fn tokenize(&mut self, word: &str) -> Result<Token, ReadError> {
        let last_token = &self.last_token;

        let token = match word {
            "(" | "[" | "{" => Token::Eval,
            ")" | "]" | "}" => match last_token {
                Token::Quote { .. } => {
                    return Err(ReadError::UnexpectedExpressionEnd {
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
                        return Err(ReadError::InvalidSyntax {
                            message: format!(
                                "unexpected quote type \"{}\". line: {}, column: {}",
                                word, self.line_num, self.column_num
                            ),
                        })
                    }
                },
            },
            &_ => match last_token {
                Token::Quote { .. } => Token::Symbol,
                _ => Token::Value,
            },
        };

        self.last_token = token.clone();
        Ok(token)
    }
}

#[cfg(test)]
mod tests {
    use crate::reader::{GRData, NodePtr, ReadError, Reader};
    use crate::tree::Tree;

    #[test]
    fn valid_tree_1() {
        let root = Tree::new(GRData::from_str("("));
        Tree::push_child(&root, GRData::from_str("progn"));
        let expr = Tree::push_child(&root, GRData::from_str("("));
        Tree::push_child(&expr, GRData::from_str("quote"));
        Tree::push_child(&expr, GRData::from_str("a"));

        test_parse("'a", &root);
        test_parse("(quote a)", &root);
    }

    #[test]
    fn valid_tree_2() {
        let root = Tree::new(GRData::from_str("("));
        Tree::push_child(&root, GRData::from_str("progn"));
        let expr = Tree::push_child(&root, GRData::from_str("("));
        Tree::push_child(&expr, GRData::from_str("quote"));
        let expr = Tree::push_child(&expr, GRData::from_str("("));
        Tree::push_child(&expr, GRData::from_str("a"));
        Tree::push_child(&expr, GRData::from_str("b"));

        test_parse("'(a b)", &root);
        test_parse("(quote (a b))", &root);
    }

    #[test]
    fn valid_tree_3() {
        let root = Tree::new(GRData::from_str("("));
        Tree::push_child(&root, GRData::from_str("progn"));
        let quasiquote = Tree::push_child(&root, GRData::from_str("("));
        Tree::push_child(&quasiquote, GRData::from_str("quasiquote"));
        let expr = Tree::push_child(&quasiquote, GRData::from_str("("));
        Tree::push_child(&expr, GRData::from_str("a"));
        let unquote = Tree::push_child(&expr, GRData::from_str("("));
        Tree::push_child(&unquote, GRData::from_str("unquote"));
        Tree::push_child(&unquote, GRData::from_str("b"));
        Tree::push_child(&expr, GRData::from_str("c"));

        test_parse("`(a ,b c)", &root);
        test_parse("`(a (unquote b) c)", &root);
        test_parse("(quasiquote (a ,b c))", &root);
        test_parse("(quasiquote (a (unquote b) c))", &root);
    }

    #[test]
    fn valid_tree_4() {
        let root = Tree::new(GRData::from_str("("));
        Tree::push_child(&root, GRData::from_str("progn"));
        let quasiquote = Tree::push_child(&root, GRData::from_str("("));
        Tree::push_child(&quasiquote, GRData::from_str("quasiquote"));
        let expr1 = Tree::push_child(&quasiquote, GRData::from_str("("));
        let quote = Tree::push_child(&expr1, GRData::from_str("("));
        Tree::push_child(&quote, GRData::from_str("quote"));
        Tree::push_child(&quote, GRData::from_str("a"));
        let unquote_splicing = Tree::push_child(&expr1, GRData::from_str("("));
        Tree::push_child(&unquote_splicing, GRData::from_str("unquote-splicing"));
        let expr2 = Tree::push_child(&unquote_splicing, GRData::from_str("("));
        Tree::push_child(&expr2, GRData::from_str("b"));
        Tree::push_child(&expr2, GRData::from_str("c"));
        Tree::push_child(&expr1, GRData::from_str("d"));

        test_parse("`('a ,@(b c) d)", &root);
        test_parse("`('a (unquote-splicing (b c)) d)", &root);
        test_parse("`((quote a) ,@(b c) d)", &root);
        test_parse("`((quote a) (unquote-splicing (b c)) d)", &root);
        test_parse("(quasiquote ((quote a) (unquote-splicing (b c)) d))", &root);
    }

    #[test]
    fn valid_tree_6() {
        let root = Tree::new(GRData::from_str("("));
        Tree::push_child(&root, GRData::from_str("progn"));
        let quote = Tree::push_child(&root, GRData::from_str("("));
        Tree::push_child(&quote, GRData::from_str("quote"));
        let quote = Tree::push_child(&quote, GRData::from_str("("));
        Tree::push_child(&quote, GRData::from_str("quote"));
        let quote = Tree::push_child(&quote, GRData::from_str("("));
        Tree::push_child(&quote, GRData::from_str("quote"));
        Tree::push_child(&quote, GRData::from_str("a"));

        test_parse("'''a", &root);
        test_parse("''(quote a)", &root);
        test_parse("'(quote (quote a))", &root);
        test_parse("(quote (quote (quote a)))", &root);
    }

    #[test]
    fn valid_tree_7() {
        let root = Tree::new(GRData::from_str("("));
        Tree::push_child(&root, GRData::from_str("progn"));
        let quasiquote = Tree::push_child(&root, GRData::from_str("("));
        Tree::push_child(&quasiquote, GRData::from_str("quasiquote"));
        let quote = Tree::push_child(&quasiquote, GRData::from_str("("));
        Tree::push_child(&quote, GRData::from_str("quote"));
        let unquote = Tree::push_child(&quote, GRData::from_str("("));
        Tree::push_child(&unquote, GRData::from_str("unquote"));
        let unquote = Tree::push_child(&unquote, GRData::from_str("("));
        Tree::push_child(&unquote, GRData::from_str("unquote"));
        let unquote_splicing = Tree::push_child(&unquote, GRData::from_str("("));
        Tree::push_child(&unquote_splicing, GRData::from_str("unquote-splicing"));
        Tree::push_child(&unquote_splicing, GRData::from_str("a"));

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
        let root = Tree::new(GRData::from_str("("));
        Tree::push_child(&root, GRData::from_str("progn"));
        let quasiquote = Tree::push_child(&root, GRData::from_str("("));
        Tree::push_child(&quasiquote, GRData::from_str("quasiquote"));
        let quote = Tree::push_child(&quasiquote, GRData::from_str("("));
        Tree::push_child(&quote, GRData::from_str("quote"));
        let unquote = Tree::push_child(&quote, GRData::from_str("("));
        Tree::push_child(&unquote, GRData::from_str("unquote"));
        let quasiquote = Tree::push_child(&unquote, GRData::from_str("("));
        Tree::push_child(&quasiquote, GRData::from_str("quasiquote"));
        let quasiquote = Tree::push_child(&quasiquote, GRData::from_str("("));
        Tree::push_child(&quasiquote, GRData::from_str("quasiquote"));
        let quote = Tree::push_child(&quasiquote, GRData::from_str("("));
        Tree::push_child(&quote, GRData::from_str("quote"));
        let unquote = Tree::push_child(&quote, GRData::from_str("("));
        Tree::push_child(&unquote, GRData::from_str("unquote"));
        let quasiquote = Tree::push_child(&unquote, GRData::from_str("("));
        Tree::push_child(&quasiquote, GRData::from_str("quasiquote"));
        let quasiquote = Tree::push_child(&quasiquote, GRData::from_str("("));
        Tree::push_child(&quasiquote, GRData::from_str("quasiquote"));
        let quote = Tree::push_child(&quasiquote, GRData::from_str("("));
        Tree::push_child(&quote, GRData::from_str("quote"));
        let quote = Tree::push_child(&quote, GRData::from_str("("));
        Tree::push_child(&quote, GRData::from_str("quote"));
        let unquote = Tree::push_child(&quote, GRData::from_str("("));
        Tree::push_child(&unquote, GRData::from_str("unquote"));
        let unquote = Tree::push_child(&unquote, GRData::from_str("("));
        Tree::push_child(&unquote, GRData::from_str("unquote"));
        let unquote_splicing = Tree::push_child(&unquote, GRData::from_str("("));
        Tree::push_child(&unquote_splicing, GRData::from_str("unquote-splicing"));
        Tree::push_child(&unquote_splicing, GRData::from_str("a"));

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
        let root = Tree::new(GRData::from_str("("));
        Tree::push_child(&root, GRData::from_str("progn"));
        let lambda = Tree::push_child(&root, GRData::from_str("("));
        Tree::push_child(&lambda, GRData::from_str("lambda"));
        Tree::push_child(&lambda, GRData::from_str("x"));
        let body = Tree::push_child(&lambda, GRData::from_str("("));
        Tree::push_child(&body, GRData::from_str("length"));
        Tree::push_child(&body, GRData::from_str("x"));

        test_parse("(lambda x (length x))", &root);
    }

    #[test]
    fn valid_tree_10() {
        let root = Tree::new(GRData::from_str("("));
        Tree::push_child(&root, GRData::from_str("progn"));
        let lambda = Tree::push_child(&root, GRData::from_str("("));
        Tree::push_child(&lambda, GRData::from_str("lambda"));
        let args = Tree::push_child(&lambda, GRData::from_str("("));
        Tree::push_child(&args, GRData::from_str("x"));
        Tree::push_child(&args, GRData::from_str("y"));
        let body = Tree::push_child(&lambda, GRData::from_str("("));
        Tree::push_child(&body, GRData::from_str("+"));
        Tree::push_child(&body, GRData::from_str("x"));
        Tree::push_child(&body, GRData::from_str("y"));

        test_parse("(lambda (x y) (+ x y))", &root);
    }

    #[test]
    fn valid_tree_11() {
        let root = Tree::new(GRData::from_str("("));
        Tree::push_child(&root, GRData::from_str("progn"));
        let let_proc = Tree::push_child(&root, GRData::from_str("("));
        Tree::push_child(&let_proc, GRData::from_str("let"));
        let bindings = Tree::push_child(&let_proc, GRData::from_str("("));
        Tree::push_child(&bindings, GRData::from_str("a"));
        Tree::push_child(&bindings, GRData::from_str("220"));
        Tree::push_child(&bindings, GRData::from_str("b"));
        Tree::push_child(&bindings, GRData::from_str("8"));
        let body = Tree::push_child(&let_proc, GRData::from_str("("));
        Tree::push_child(&body, GRData::from_str("+"));
        Tree::push_child(&body, GRData::from_str("a"));
        Tree::push_child(&body, GRData::from_str("b"));

        test_parse("(let (a 220 b 8) (+ a b))", &root);
    }

    #[test]
    fn valid_tree_12() {
        let root = Tree::new(GRData::from_str("("));
        Tree::push_child(&root, GRData::from_str("progn"));
        let define = Tree::push_child(&root, GRData::from_str("("));
        Tree::push_child(&define, GRData::from_str("define"));
        Tree::push_child(&define, GRData::from_str("vaiv"));
        let quote = Tree::push_child(&define, GRData::from_str("("));
        Tree::push_child(&quote, GRData::from_str("quote"));
        Tree::push_child(&quote, GRData::from_str("daun"));

        test_parse("(define vaiv 'daun)", &root);
        test_parse("(define vaiv (quote daun)", &root);
    }

    #[test]
    fn invalid_tree_1() {
        let mut p = Reader::new();
        let inputs = vec!["'(1 2 3))", "'[1 2 3]]", "'{1 2 3}}"];
        for input in inputs.iter() {
            match p.parse(input) {
                Err(ReadError::UnmatchedParenthesis { line, column }) => {
                    assert_eq!((line, column), (1, 9))
                }
                Ok(_) => panic!("parsed correctly"),
                Err(e) => panic!("unexpected error {:?}", e),
            }
        }
    }

    #[test]
    fn invalid_tree_2() {
        let mut p = Reader::new();
        let inputs = vec!["(quote [a b c)]", "{quote [1 2 3}]", "{quote (1 2 3]}"];
        for input in inputs.iter() {
            match p.parse(input) {
                Err(ReadError::MismatchedParenthesis { line, column, .. }) => {
                    assert_eq!((line, column), (1, 14))
                }
                Ok(_) => panic!("parsed correctly"),
                Err(e) => panic!("unexpected error {:?}", e),
            }
        }
    }

    fn test_parse(input: &str, valid_tree: &NodePtr) {
        let mut p = Reader::new();
        match p.parse(input) {
            Ok(res) => assert_eq!(res, valid_tree.clone(), "\n input: \"{}\"\n", input),
            Err(e) => panic!("{:?}", e),
        }
    }
}
