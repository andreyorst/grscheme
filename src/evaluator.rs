use crate::parser::Parser;
use crate::tree::{NodePtr, Tree};

#[derive(Debug)]
pub struct Identifier {
    data: String,
    pattern: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    _U32,
    _I32,
    _F32,
    _Name,
    _Str,
    _Symbol,
    _List,
    _Procedure
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
        if !expression.borrow().childs.is_empty() {
            let proc = match Self::car(&expression) {
                Ok(res) => {
                    if !res.borrow().childs.is_empty() {
                        match self.eval(&res) {
                            Ok(res) => res,
                            Err(e) => return Err(e),
                        }
                    } else {
                        res
                    }
                }
                Err(e) => return Err(e),
            };

            let args = match Self::cdr(&expression) {
                Ok(res) => res,
                Err(e) => return Err(e),
            };

            self.apply(&proc, &args)
        } else {
            Ok(expression.clone())
        }
    }

    fn apply(&mut self, proc: &NodePtr, args: &NodePtr) -> Result<NodePtr, EvalError> {
        println!(
            "apply {} to {}",
            Self::tree_to_string(&proc),
            Self::tree_to_string(&args)
        );
        match proc.borrow().data.as_ref() {
            "quote" => Self::quote(&args),
            _ => {
                let args = match self.eval(&args) {
                    Ok(res) => res,
                    Err(e) => return Err(e),
                };
                match proc.borrow().data.as_ref() {
                    "cons" => Self::cons(&args),
                    "car" => Self::car(&args),
                    "cdr" => Self::cdr(&args),
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

    fn _item_type(s: &NodePtr) -> Type {
        if !s.borrow().childs.is_empty() {
            if s.borrow().childs[0].borrow().data == "quote" {
                if !s.borrow().childs[1].borrow().childs.is_empty() {
                     Type::_Symbol
                } else {
                     Type::_List
                }
            } else {
                 Type::_Procedure
            }
        } else if s.borrow().data.trim().parse::<i32>().is_ok() {
            Type::_I32
        } else if s.borrow().data.trim().parse::<u32>().is_ok() {
            Type::_U32
        } else if s.borrow().data.trim().parse::<f32>().is_ok() {
            Type::_F32
        } else if s.borrow().data.starts_with('"') && s.borrow().data.ends_with('"') {
            Type::_Str
        } else {
            Type::_Name
        }
    }

    fn quote(expr: &NodePtr) -> Result<NodePtr, EvalError> {
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
        // if tree.borrow().childs.len() > 1 {
        //     return Err(EvalError::WrongArgAmount {
        //         procedure: "car".to_owned(),
        //         expected: 1,
        //         fact: tree.borrow().childs.len() as u32,
        //     });
        // }
        if !tree.borrow().childs.is_empty() {
            let first = Tree::clone_tree(&tree.borrow().childs[0]);
            Ok(first)
        } else {
            Err(EvalError::Vaiv {
                message: format!("car: expected pair, got \"{}\"", Self::tree_to_string(tree)),
            })
        }
    }

    fn cdr(tree: &NodePtr) -> Result<NodePtr, EvalError> {
        // if tree.borrow().childs.len() > 1 {
        //     return Err(EvalError::WrongArgAmount {
        //         procedure: "cdr".to_owned(),
        //         expected: 1, fact: tree.borrow().childs.len() as u32,
        //     });
        // }
        if !tree.borrow().childs.is_empty() {
            let rest =
                if tree.borrow().childs.len() > 2 && tree.borrow().childs[1].borrow().data == "." {
                    Tree::clone_tree(&tree.borrow().childs[2])
                } else {
                    let rest = Tree::clone_tree(tree);
                    rest.borrow_mut().childs.remove(0);
                    rest
                };
            Ok(rest)
        } else {
            Err(EvalError::Vaiv {
                message: format!("cdr: expected pair, got \"{}\"", Self::tree_to_string(tree)),
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

        let root = Tree::root("(".to_owned());
        let first = match Self::car(&args) {
            Ok(res) => res,
            Err(e) => return Err(e),
        };
        let second = match Self::cdr(&args) {
            Ok(res) => match Self::car(&res) {
                Ok(res) => res,
                Err(e) => return Err(e),
            },
            Err(e) => return Err(e),
        };
        Tree::adopt_node(&root, &first);
        Tree::add_child(&root, ".".to_owned());
        Tree::adopt_node(&root, &second);
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
    assert_eq!(Evaluator::_item_type(&parser.parse("32").ok().unwrap().borrow().childs[0]), Type::_I32);
    assert_eq!(Evaluator::_item_type(&parser.parse("-32").ok().unwrap().borrow().childs[0]), Type::_I32);
    assert_eq!(Evaluator::_item_type(&parser.parse("32.0").ok().unwrap().borrow().childs[0]), Type::_F32);
    assert_eq!(Evaluator::_item_type(&parser.parse("-32.0").ok().unwrap().borrow().childs[0]), Type::_F32);
    assert_eq!(Evaluator::_item_type(&parser.parse("\"str\"").ok().unwrap().borrow().childs[0]), Type::_Str);
    assert_eq!(Evaluator::_item_type(&parser.parse("name").ok().unwrap().borrow().childs[0]), Type::_Name);
}
