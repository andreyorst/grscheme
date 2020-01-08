use crate::parser::Parser;
use crate::tree::{NodePtr, Tree};

#[derive(Debug)]
pub struct Identifier {
    data: String,
    pattern: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    U32,
    I32,
    F32,
    Name,
    Str,
    Symbol,
    List,
    Procedure,
    Pattern,
}

pub struct Evaluator {
    _global_scope: Vec<String>,
}

#[derive(Debug)]
pub enum EvalError {
    GeneralError {
        message: String,
    },
    UnknownProc {
        name: String,
    },
    WrongArgAmount {
        procedure: String,
        expected: u32,
        fact: u32,
    },
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            _global_scope: vec![],
        }
    }

    pub fn eval(&mut self, expression: &NodePtr) -> Result<NodePtr, EvalError> {
        match Self::expression_type(expression) {
            Type::Procedure | Type::List | Type::Symbol => {
                let proc = match Self::first_expression(expression) {
                    Ok(res) => match Self::expression_type(&res) {
                        Type::Procedure => match self.eval(&res) {
                            Ok(res) => match Self::expression_type(&res) {
                                Type::Name => res,
                                _ => {
                                    return Err(EvalError::GeneralError {
                                        message: format!(
                                            "wrong type to apply: \"{}\"",
                                            Self::tree_to_string(&res)
                                        ),
                                    })
                                }
                            },
                            Err(e) => return Err(e),
                        },
                        Type::Name => res,
                        _ => {
                            return Err(EvalError::GeneralError {
                                message: format!(
                                    "wrong type to apply: \"{}\"",
                                    Self::tree_to_string(&res)
                                ),
                            })
                        }
                    },
                    Err(e) => return Err(e),
                };

                let args = match Self::rest_expressions(expression) {
                    Ok(res) => res,
                    Err(e) => return Err(e),
                };

                self.apply(&proc, &args)
            }
            Type::Name => {
                // TODO: lookup
                Ok(expression.clone())
            }
            _ => Ok(expression.clone()),
        }
    }

    fn apply(&mut self, proc: &NodePtr, args: &NodePtr) -> Result<NodePtr, EvalError> {
        match proc.borrow().data.as_ref() {
            "quote" => Self::quote(args),
            _ => {
                let evaled_args = Tree::root("(".to_owned());
                for sub in args.borrow().childs.iter() {
                    match self.eval(sub) {
                        Ok(res) => {
                            Tree::adopt_node(&evaled_args, &res);
                        }
                        Err(e) => return Err(e),
                    }
                }
                match proc.borrow().data.as_ref() {
                    "car" => Self::car(&evaled_args),
                    "cdr" => Self::cdr(&evaled_args),
                    "cons" => Self::cons(&evaled_args),
                    _ => Err(EvalError::UnknownProc {
                        name: proc.borrow().data.clone(),
                    }),
                }
            }
        }
    }

    pub fn print(expression: &NodePtr) {
        match Self::expression_type(expression) {
            Type::Pattern => match expression.borrow().data.as_ref() {
                "#void" => (),
                _ => println!("{}", Self::tree_to_string(expression)),
            },
            _ => println!("{}", Self::tree_to_string(expression)),
        }
    }

    fn tree_to_string(expression: &NodePtr) -> String {
        let mut vector: Vec<String> = vec![];
        Self::tree_to_vec(expression, &mut vector);
        vector.join("").trim().to_owned()
    }

    fn tree_to_vec(expression: &NodePtr, vec_repr: &mut Vec<String>) {
        let mut print_closing = false;
        let data = expression.borrow().data.clone();
        if let "(" = data.as_ref() {
            print_closing = true;
        }
        vec_repr.push(data);
        for child in expression.borrow().childs.iter() {
            let data = child.borrow().data.clone();
            match data.as_ref() {
                "quote" | "unquote" | "unquote-splicing" | "quasiquote" => {
                    if vec_repr.last().unwrap() == "(" {
                        vec_repr.pop();
                        vec_repr.push(
                            match data.as_ref() {
                                "quote" => "'",
                                "unquote" => ",",
                                "unquote-splicing" => ",@",
                                "quasiquote" => "`",
                                _ => "",
                            }
                            .to_owned(),
                        );
                        print_closing = false;
                    } else {
                        vec_repr.push(data);
                        vec_repr.push(" ".to_owned());
                    }
                }
                "(" => Self::tree_to_vec(child, vec_repr),
                _ => {
                    vec_repr.push(data);
                    vec_repr.push(" ".to_owned());
                }
            }
        }
        if print_closing {
            if vec_repr.last().unwrap() == " " {
                vec_repr.pop();
            }
            vec_repr.push(")".to_owned());
            vec_repr.push(" ".to_owned());
        }
    }

    fn expression_type(s: &NodePtr) -> Type {
        if !s.borrow().childs.is_empty() {
            if s.borrow().childs[0].borrow().data == "quote" {
                if s.borrow().childs[1].borrow().childs.is_empty() {
                    Type::Symbol
                } else {
                    Type::List
                }
            } else {
                Type::Procedure
            }
        } else if s.borrow().data.starts_with('#') {
            Type::Pattern
        } else if s.borrow().data.trim().parse::<i32>().is_ok() {
            Type::I32
        } else if s.borrow().data.trim().parse::<u32>().is_ok() {
            Type::U32
        } else if s.borrow().data.trim().parse::<f32>().is_ok() {
            Type::F32
        } else if s.borrow().data.starts_with('"') && s.borrow().data.ends_with('"') {
            Type::Str
        } else {
            Type::Name
        }
    }

    fn quote(tree: &NodePtr) -> Result<NodePtr, EvalError> {
        if tree.borrow().childs.len() > 1 {
            return Err(EvalError::WrongArgAmount {
                procedure: "quote".to_owned(),
                expected: 1,
                fact: tree.borrow().childs.len() as u32,
            });
        }

        match Self::first_expression(&tree) {
            Ok(res) => match Self::expression_type(&res) {
                Type::Procedure | Type::Name => {
                    let root = Tree::root("(".to_owned());
                    Tree::add_child(&root, "quote".to_owned());
                    Tree::adopt_node(&root, &res);
                    Ok(root)
                }
                _ => Ok(res),
            },
            Err(e) => Err(e),
        }
    }

    fn car(tree: &NodePtr) -> Result<NodePtr, EvalError> {
        if tree.borrow().childs.len() > 1 {
            return Err(EvalError::WrongArgAmount {
                procedure: "car".to_owned(),
                expected: 1,
                fact: tree.borrow().childs.len() as u32,
            });
        }

        match Self::first_expression(&tree) {
            Ok(res) => match Self::expression_type(&res) {
                Type::List => match Self::rest_expressions(&res) {
                    Ok(res) => match Self::first_expression(&res) {
                        Ok(res) => match Self::first_expression(&res) {
                            Ok(res) => match Self::expression_type(&res) {
                                Type::Name => {
                                    let root = Tree::root("(".to_owned());
                                    Tree::add_child(&root, "quote".to_owned());
                                    Tree::adopt_node(&root, &res);
                                    Ok(root)
                                }
                                _ => Ok(res),
                            },
                            Err(e) => Err(e),
                        },
                        Err(e) => Err(e),
                    },
                    Err(e) => Err(e),
                },
                _ => Err(EvalError::GeneralError {
                    message: format!("car: expected pair, got {}", Self::tree_to_string(&res)),
                }),
            },
            Err(e) => Err(e),
        }
    }

    fn first_expression(expression: &NodePtr) -> Result<NodePtr, EvalError> {
        if !expression.borrow().childs.is_empty() {
            Ok(Tree::clone_tree(&expression.borrow().childs[0]))
        } else {
            Err(EvalError::GeneralError {
                message: format!(
                    "first: expected pair, got \"{}\"",
                    Self::tree_to_string(expression)
                ),
            })
        }
    }

    fn cdr(tree: &NodePtr) -> Result<NodePtr, EvalError> {
        if tree.borrow().childs.len() > 1 {
            return Err(EvalError::WrongArgAmount {
                procedure: "cdr".to_owned(),
                expected: 1,
                fact: tree.borrow().childs.len() as u32,
            });
        }

        match Self::first_expression(&tree) {
            Ok(res) => match Self::expression_type(&res) {
                Type::List => match Self::rest_expressions(&res) {
                    Ok(res) => match Self::first_expression(&res) {
                        Ok(res) => match Self::rest_expressions(&res) {
                            Ok(res) => match Self::expression_type(&res) {
                                Type::Procedure | Type::Name => {
                                    let root = Tree::root("(".to_owned());
                                    Tree::add_child(&root, "quote".to_owned());
                                    Tree::adopt_node(&root, &res);
                                    Ok(root)
                                }
                                _ => Ok(res),
                            },
                            Err(e) => Err(e),
                        },
                        Err(e) => Err(e),
                    },
                    Err(e) => Err(e),
                },
                _ => Err(EvalError::GeneralError {
                    message: format!("cdr: expected pair, got {}", Self::tree_to_string(&res)),
                }),
            },
            Err(e) => Err(e),
        }
    }

    fn rest_expressions(expression: &NodePtr) -> Result<NodePtr, EvalError> {
        if !expression.borrow().childs.is_empty() {
            Ok(
                if expression.borrow().childs.len() > 2
                    && expression.borrow().childs[1].borrow().data == "."
                {
                    Tree::clone_tree(&expression.borrow().childs[2])
                } else {
                    let rest = Tree::clone_tree(expression);
                    rest.borrow_mut().childs.remove(0);
                    rest
                },
            )
        } else {
            Err(EvalError::GeneralError {
                message: format!(
                    "rest: expected pair, got \"{}\"",
                    Self::tree_to_string(expression)
                ),
            })
        }
    }

    fn cons(args: &NodePtr) -> Result<NodePtr, EvalError> {
        if args.borrow().childs.len() != 2 {
            return Err(EvalError::WrongArgAmount {
                procedure: "cons".to_owned(),
                expected: 2,
                fact: args.borrow().childs.len() as u32,
            });
        }

        let first = match Self::first_expression(&args) {
            Ok(res) => match Self::expression_type(&res) {
                Type::List | Type::Symbol => match Self::rest_expressions(&res) {
                    Ok(res) => match Self::first_expression(&res) {
                        Ok(res) => res,
                        Err(e) => return Err(e),
                    },
                    Err(e) => return Err(e),
                },
                _ => res,
            },
            Err(e) => return Err(e),
        };

        let second = match Self::rest_expressions(args) {
            Ok(res) => match Self::first_expression(&res) {
                Ok(res) => match Self::expression_type(&res) {
                    Type::List | Type::Symbol => match Self::rest_expressions(&res) {
                        Ok(res) => match Self::first_expression(&res) {
                            Ok(res) => res,
                            Err(e) => return Err(e),
                        },
                        Err(e) => return Err(e),
                    },
                    _ => res,
                },
                Err(e) => return Err(e),
            },
            Err(e) => return Err(e),
        };

        let quote = Tree::root("(".to_owned());
        Tree::add_child(&quote, "quote".to_owned());

        let pair = Tree::add_child(&quote, "(".to_owned());
        Tree::adopt_node(&pair, &first);
        Tree::add_child(&pair, ".".to_owned());
        Tree::adopt_node(&pair, &second);

        if Parser::remove_dots(&quote).is_err() {
            return Err(EvalError::GeneralError {
                message: "vaiv".to_owned(),
            });
        }

        Ok(quote)
    }
}

#[cfg(test)]
mod tests {
    use crate::evaluator::{Evaluator, Type};
    use crate::parser::Parser;

    fn test_inputs_with_outputs(tests: Vec<&'static str>, results: Vec<String>) {
        let mut parser = Parser::new();
        let mut evaluator = Evaluator::new();
        for (test, correct) in tests.iter().zip(results) {
            match parser.parse(test) {
                Ok(res) => {
                    for subexpr in res.borrow().childs.iter() {
                        match evaluator.eval(&subexpr) {
                            Ok(res) => {
                                assert_eq!(Evaluator::tree_to_string(&res), correct);
                            }
                            Err(e) => panic!("{:?}", e),
                        }
                    }
                }
                Err(e) => panic!("{:?}", e),
            }
        }
    }

    #[test]
    fn test_car() {
        let tests = vec![
            "(car '(1 2 3))",
            "(car '(2))",
            "(car '(a b c))",
            "(car '(3 . 4))",
            "(car '(b . c))",
        ];
        let results = vec![
            "1".to_owned(),
            "2".to_owned(),
            "'a".to_owned(),
            "3".to_owned(),
            "'b".to_owned(),
        ];
        test_inputs_with_outputs(tests, results);
    }

    #[test]
    fn test_cdr() {
        let tests = vec![
            "(cdr '(1 2 3))",
            "(cdr '(1))",
            "(cdr '(a b c))",
            "(cdr '(3 . 4))",
            "(cdr '(b . c))",
        ];
        let results = vec![
            "'(2 3)".to_owned(),
            "'()".to_owned(),
            "'(b c)".to_owned(),
            "4".to_owned(),
            "'c".to_owned(),
        ];
        test_inputs_with_outputs(tests, results);
    }

    #[test]
    fn test_cons() {
        let tests = vec![
            "(cons 1 2)",
            "(cons 'a 'b)",
            "(cons 1 '(2 3))",
            "(cons '(1) '(2 3))",
            "(cons 'a '(b c))",
            "(cons '(a) '(b c))",
        ];
        let results = vec![
            "'(1 . 2)".to_owned(),
            "'(a . b)".to_owned(),
            "'(1 2 3)".to_owned(),
            "'((1) 2 3)".to_owned(),
            "'(a b c)".to_owned(),
            "'((a) b c)".to_owned(),
        ];
        test_inputs_with_outputs(tests, results);
    }

    #[test]
    fn test_quote() {
        let tests = vec![
            "'a",
            "'1",
            "'(a)",
            "'(a 'b)",
            "'(1)",
            "'(1 '2)",
            "'\"str\"",
            "(quote a)",
            "(quote 1)",
            "(quote (a))",
            "(quote (a 'b))",
            "(quote (1))",
            "(quote (1 '2))",
            "(quote \"str\")",
        ];
        let results = vec![
            "'a".to_owned(),
            "1".to_owned(),
            "'(a)".to_owned(),
            "'(a 'b)".to_owned(),
            "'(1)".to_owned(),
            "'(1 '2)".to_owned(),
            "\"str\"".to_owned(),
            "'a".to_owned(),
            "1".to_owned(),
            "'(a)".to_owned(),
            "'(a 'b)".to_owned(),
            "'(1)".to_owned(),
            "'(1 '2)".to_owned(),
            "\"str\"".to_owned(),
        ];
        test_inputs_with_outputs(tests, results);
    }

    #[test]
    fn test_types() {
        let mut parser = Parser::new();
        assert_eq!(
            Evaluator::expression_type(&parser.parse("32").ok().unwrap().borrow().childs[0]),
            Type::I32
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("-32").ok().unwrap().borrow().childs[0]),
            Type::I32
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("32.0").ok().unwrap().borrow().childs[0]),
            Type::F32
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("-32.0").ok().unwrap().borrow().childs[0]),
            Type::F32
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("\"str\"").ok().unwrap().borrow().childs[0]),
            Type::Str
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("name").ok().unwrap().borrow().childs[0]),
            Type::Name
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("'name").ok().unwrap().borrow().childs[0]),
            Type::Symbol
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("(name)").ok().unwrap().borrow().childs[0]),
            Type::Procedure
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("'(name)").ok().unwrap().borrow().childs[0]),
            Type::List
        );
    }
}
