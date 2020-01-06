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
}

pub struct Evaluator {
    _global_scope: Vec<String>,
}

pub enum EvalError {
    Vaiv {
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
    pub fn eval(&mut self, expression: &NodePtr) -> Result<NodePtr, EvalError> {
        match Self::expression_type(expression) {
            Type::Procedure => {
                let proc = match Self::get_first_subexpr(expression) {
                    Ok(res) => match Self::expression_type(&res) {
                        Type::Procedure => match self.eval(&res) {
                            Ok(res) => match Self::expression_type(&res) {
                                Type::Name => res,
                                _ => {
                                    return Err(EvalError::Vaiv {
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
                            return Err(EvalError::Vaiv {
                                message: format!(
                                    "wrong type to apply: \"{}\"",
                                    Self::tree_to_string(&res)
                                ),
                            })
                        }
                    },
                    Err(e) => return Err(e),
                };

                let args = match Self::get_rest_subexpr(expression) {
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

    pub fn apply(&mut self, proc: &NodePtr, args: &NodePtr) -> Result<NodePtr, EvalError> {
        match proc.borrow().data.as_ref() {
            "quote" => Ok(args.clone()),
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
    pub fn new() -> Evaluator {
        Evaluator {
            _global_scope: vec![],
        }
    }

    pub fn print(expression: &NodePtr) {
        println!("{}", Self::tree_to_string(expression));
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

    fn _quote(expr: &NodePtr) -> Result<NodePtr, EvalError> {
        if expr.borrow().childs.len() > 1 {
            return Err(EvalError::WrongArgAmount {
                procedure: "quote".to_owned(),
                expected: 1,
                fact: expr.borrow().childs.len() as u32,
            });
        }
        Self::car(expr)
    }

    fn car(tree: &NodePtr) -> Result<NodePtr, EvalError> {
        if tree.borrow().childs.len() > 1 {
            return Err(EvalError::WrongArgAmount {
                procedure: "car".to_owned(),
                expected: 1,
                fact: tree.borrow().childs.len() as u32,
            });
        }
        match Self::get_first_subexpr(&tree) {
            Ok(res) => match Self::expression_type(&res) {
                Type::List => {
                    let first = match Self::get_rest_subexpr(&res) {
                        Ok(res) => match Self::get_first_subexpr(&res) {
                            Ok(res) => match Self::get_first_subexpr(&res) {
                                Ok(res) => res,
                                Err(e) => return Err(e),
                            },
                            Err(e) => return Err(e),
                        },
                        Err(e) => return Err(e),
                    };
                    match Self::expression_type(&first) {
                        Type::Name => {
                            let root = Tree::root("(".to_owned());
                            Tree::add_child(&root, "quote".to_owned());
                            Tree::adopt_node(&root, &first);
                            Ok(root)
                        }
                        _ => Ok(first.clone()),
                    }
                }
                _ => Err(EvalError::Vaiv {
                    message: format!("car: expected pair, got {}", Self::tree_to_string(&res)),
                }),
            },
            Err(e) => Err(e),
        }
    }

    fn get_first_subexpr(expression: &NodePtr) -> Result<NodePtr, EvalError> {
        if !expression.borrow().childs.is_empty() {
            Ok(Tree::clone_tree(&expression.borrow().childs[0]))
        } else {
            Err(EvalError::Vaiv {
                message: format!(
                    "car: expected pair, got \"{}\"",
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
        match Self::get_first_subexpr(&tree) {
            Ok(res) => match Self::expression_type(&res) {
                Type::List => {
                    let rest = match Self::get_rest_subexpr(&res) {
                        Ok(res) => match Self::get_first_subexpr(&res) {
                            Ok(res) => match Self::get_rest_subexpr(&res) {
                                Ok(res) => res,
                                Err(e) => return Err(e),
                            },
                            Err(e) => return Err(e),
                        },
                        Err(e) => return Err(e),
                    };
                    match Self::expression_type(&rest) {
                        Type::Procedure | Type::Name => {
                            let root = Tree::root("(".to_owned());
                            Tree::add_child(&root, "quote".to_owned());
                            Tree::adopt_node(&root, &rest);
                            Ok(root)
                        }
                        _ => Ok(rest.clone()),
                    }
                }
                _ => Err(EvalError::Vaiv {
                    message: format!("cdr: expected pair, got {}", Self::tree_to_string(&res)),
                }),
            },
            Err(e) => Err(e),
        }
    }

    fn get_rest_subexpr(expression: &NodePtr) -> Result<NodePtr, EvalError> {
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
            Err(EvalError::Vaiv {
                message: format!(
                    "cdr: expected pair, got \"{}\"",
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
        let first = match Self::get_first_subexpr(&args) {
            Ok(res) => match Self::expression_type(&res) {
                Type::List | Type::Symbol => match Self::get_rest_subexpr(&res) {
                    Ok(res) => match Self::get_first_subexpr(&res) {
                        Ok(res) => res,
                        Err(e) => return Err(e),
                    },
                    Err(e) => return Err(e),
                },
                _ => res,
            },
            Err(e) => return Err(e),
        };
        let second = match Self::get_rest_subexpr(args) {
            Ok(res) => match Self::get_first_subexpr(&res) {
                Ok(res) => match Self::expression_type(&res) {
                    Type::List | Type::Symbol => match Self::get_rest_subexpr(&res) {
                        Ok(res) => match Self::get_first_subexpr(&res) {
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
        let root = Tree::root("(".to_owned());
        Tree::add_child(&root, "quote".to_owned());
        let root2 = Tree::add_child(&root, "(".to_owned());
        Tree::adopt_node(&root2, &first);
        Tree::add_child(&root2, ".".to_owned());
        Tree::adopt_node(&root2, &second);
        if Parser::remove_dots(&root).is_err() {
            return Err(EvalError::Vaiv {
                message: "vaiv".to_owned(),
            });
        }
        Ok(root)
    }
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
