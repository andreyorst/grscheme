use crate::parser::Parser;
use crate::repl::{read_balanced_input, ReplError};
use crate::tree::{NodePtr, Tree};
use std::collections::HashMap;

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

enum ArgAmount {
    MoreThan(usize),
    LessThan(usize),
    NotEqual(usize),
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
        expected: usize,
        fact: usize,
    },
    UnboundIdentifier {
        name: String,
    },
}

pub struct Evaluator {
    pub global_scope: HashMap<String, NodePtr>,
}

/**
 * Internal methods
 */
impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            global_scope: HashMap::new(),
        }
    }

    pub fn eval(&mut self, expression: &NodePtr) -> Result<NodePtr, EvalError> {
        match Self::expression_type(expression) {
            Type::Procedure | Type::List | Type::Symbol => {
                let proc = Self::first_expression(expression)?;
                let proc = match Self::expression_type(&proc) {
                    Type::Procedure => {
                        let res = self.eval(&proc)?;
                        match Self::expression_type(&res) {
                            Type::Name => match self.lookup(&res) {
                                Ok(res) => res,
                                Err(_) => res,
                            },
                            _ => res,
                        }
                    }
                    Type::Name => match self.lookup(&proc) {
                        Ok(res) => res,
                        Err(_) => proc,
                    },
                    _ => proc,
                };

                let args = Self::rest_expressions(expression)?;
                let res = self.apply(&proc, &args)?;

                Tree::replace_node(expression, res);
                Ok(expression.clone())
            }
            Type::Name => {
                let value = self.lookup(expression)?;
                Tree::replace_node(expression, value);
                Ok(expression.clone())
            }
            _ => Ok(expression.clone()),
        }
    }

    fn lookup(&mut self, expression: &NodePtr) -> Result<NodePtr, EvalError> {
        let mut current = expression.clone();
        while let Some(p) = Tree::get_parent(&current) {
            current = p.clone();
            for c in current.borrow().childs.iter() {
                if let Some(v) = c.borrow().scope.get(&expression.borrow().data) {
                    return Ok(v.clone());
                }
            }
        }
        if let Some(v) = self.global_scope.get(&expression.borrow().data) {
            return Ok(v.clone());
        }
        Err(EvalError::UnboundIdentifier {
            name: Tree::get_data(expression),
        })
    }

    fn apply(&mut self, proc: &NodePtr, args: &NodePtr) -> Result<NodePtr, EvalError> {
        match Self::expression_type(&proc) {
            Type::Name => (),
            Type::Pattern => match Tree::get_data(proc).as_ref() {
                "#procedure" => return self.apply_lambda(&proc, &args),
                _ => {
                    return Err(EvalError::GeneralError {
                        message: format!(
                            "wrong type to apply: \"{}\"",
                            Self::tree_to_string(&proc)
                        ),
                    })
                }
            },
            _ => {
                return Err(EvalError::GeneralError {
                    message: format!("wrong type to apply: \"{}\"", Self::tree_to_string(&proc)),
                })
            }
        }
        match proc.borrow().data.as_ref() {
            "quote" => Self::quote(args),
            "newline" => Self::newline(&args),
            "read" => Self::read(&args),
            "progn" => self.progn(&args),
            "define" => self.define(&args),
            "lambda" => Self::lambda(&args),
            _ => {
                for sub in args.borrow().childs.iter() {
                    self.eval(sub)?;
                }
                match proc.borrow().data.as_ref() {
                    "car" => Self::car(&args),
                    "cdr" => Self::cdr(&args),
                    "cons" => Self::cons(&args),
                    "display" => Self::display(&args),
                    "eval" => self.eval_proc(&args),
                    _ => Err(EvalError::UnknownProc {
                        name: Tree::get_data(proc),
                    }),
                }
            }
        }
    }

    fn apply_lambda(&mut self, expression: &NodePtr, args: &NodePtr) -> Result<NodePtr, EvalError> {
        let copy = Tree::clone_node(expression);
        let proc_args = Self::first_expression(&copy)?;
        let proc_body = Self::rest_expressions(&copy)?;
        let proc_body = Self::first_expression(&proc_body)?;

        let bindings = Tree::root("#bindings".to_owned());
        for sub in args.borrow().childs.iter() {
            self.eval(sub)?;
        }
        match Self::expression_type(&proc_args) {
            Type::Procedure => {
                Self::check_argument_count(
                    "#lambda",
                    ArgAmount::NotEqual(Tree::child_count(&proc_args)),
                    args,
                )?;
                for (n, v) in proc_args.borrow().childs.iter().zip(&args.borrow().childs) {
                    bindings
                        .borrow_mut()
                        .scope
                        .insert(Tree::get_data(n), v.clone());
                }
            }
            Type::Name => {
                let quoted_args = Tree::root("(".to_owned());
                Tree::add_child(&quoted_args, "quote".to_owned());
                let list = Tree::add_child(&quoted_args, "(".to_owned());
                for c in args.borrow().childs.iter() {
                    let res = match Self::expression_type(&c) {
                        Type::List | Type::Symbol => {
                            let res = Self::rest_expressions(&c)?;
                            Self::first_expression(&res)?
                        }
                        _ => c.clone(),
                    };
                    Tree::adopt_node(&list, res);
                }
                bindings
                    .borrow_mut()
                    .scope
                    .insert(Tree::get_data(&proc_args), quoted_args);
            }
            _ => {
                return Err(EvalError::GeneralError {
                    message: "wrong binding type".to_owned(),
                })
            }
        };

        proc_body.borrow_mut().childs.insert(1, bindings);
        self.eval(&proc_body)
    }

    pub fn print(expression: &NodePtr) {
        match Self::expression_type(expression) {
            Type::Pattern => match expression.borrow().data.as_ref() {
                "#void" => (),
                _ => println!("{}", Tree::get_data(expression)),
            },
            _ => println!("{}", Self::tree_to_string(expression)),
        }
    }

    fn tree_to_string(expression: &NodePtr) -> String {
        let mut string = String::new();
        Self::parse_tree(expression, &mut string);
        while string.ends_with(' ') {
            string.pop();
        }
        string
    }

    fn parse_tree(expression: &NodePtr, string: &mut String) {
        let mut print_closing = false;
        let data = Tree::get_data(expression);
        if let "(" = data.as_ref() {
            print_closing = true;
        }
        string.push_str(&data);
        for child in expression.borrow().childs.iter() {
            let data = Tree::get_data(child);
            match data.as_ref() {
                "quote" | "unquote" | "unquote-splicing" | "quasiquote" => {
                    if string.ends_with('(') {
                        string.pop();
                        string.push_str(match data.as_ref() {
                            "quote" => "'",
                            "unquote" => ",",
                            "unquote-splicing" => ",@",
                            "quasiquote" => "`",
                            _ => "",
                        });
                        print_closing = false;
                    } else {
                        string.push_str(&data);
                        string.push_str(" ");
                    }
                }
                "(" => Self::parse_tree(child, string),
                _ => {
                    string.push_str(&data);
                    string.push_str(" ");
                }
            }
        }
        if print_closing {
            if string.ends_with(' ') {
                string.pop();
            }
            string.push_str(")");
            string.push_str(" ");
        }
    }

    fn expression_type(s: &NodePtr) -> Type {
        if s.borrow().data.starts_with('#') {
            Type::Pattern
        } else if !s.borrow().childs.is_empty() {
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

    fn check_argument_count(
        proc: &str,
        amount: ArgAmount,
        args: &NodePtr,
    ) -> Result<(), EvalError> {
        let len = Tree::child_count(args);
        if match amount {
            ArgAmount::NotEqual(n) => len != n,
            ArgAmount::MoreThan(n) => len > n,
            ArgAmount::LessThan(n) => len < n,
        } {
            Err(EvalError::WrongArgAmount {
                procedure: proc.to_owned(),
                expected: match amount {
                    ArgAmount::NotEqual(n) => n,
                    ArgAmount::MoreThan(n) => n,
                    ArgAmount::LessThan(n) => n,
                },
                fact: len,
            })
        } else {
            Ok(())
        }
    }

    fn first_expression(expression: &NodePtr) -> Result<NodePtr, EvalError> {
        if !expression.borrow().childs.is_empty() {
            Ok(expression.borrow().childs[0].clone())
        } else {
            Err(EvalError::GeneralError {
                message: format!(
                    "first: expected pair, got \"{}\"",
                    Self::tree_to_string(expression)
                ),
            })
        }
    }

    fn rest_expressions(expression: &NodePtr) -> Result<NodePtr, EvalError> {
        if !expression.borrow().childs.is_empty() {
            Ok(
                if expression.borrow().childs.len() > 2
                    && expression.borrow().childs[1].borrow().data == "."
                {
                    expression.borrow().childs[2].clone()
                } else {
                    let rest = expression.clone();
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
}

/**
 * Language procedures
 */
impl Evaluator {
    fn eval_proc(&mut self, expression: &NodePtr) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("eval", ArgAmount::MoreThan(1), expression)?;

        let expr = Self::first_expression(&expression)?;
        let expr = match Self::expression_type(&expr) {
            Type::Symbol | Type::List => Self::rest_expressions(&expr)?,
            _ => expression.clone(),
        };
        let expr = Self::first_expression(&expr)?;

        self.eval(&expr)
    }

    fn progn(&mut self, expressions: &NodePtr) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("progn", ArgAmount::LessThan(1), expressions)?;
        for child in expressions.borrow().childs.iter() {
            self.eval(child)?;
        }
        Ok(expressions.borrow().childs.last().unwrap().clone())
    }

    fn display(args: &NodePtr) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("display", ArgAmount::NotEqual(1), args)?;

        let res = Self::first_expression(&args)?;
        match Self::expression_type(&res) {
            Type::Pattern => print!("{}", Tree::get_data(&res)),
            _ => print!("{}", Self::tree_to_string(&res)),
        }

        Ok(Tree::root("#void".to_owned()))
    }

    fn newline(args: &NodePtr) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("newline", ArgAmount::NotEqual(0), args)?;
        println!();
        Ok(Tree::root("#void".to_owned()))
    }

    fn read(args: &NodePtr) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("read", ArgAmount::NotEqual(0), args)?;

        let input = match read_balanced_input("read > ") {
            Ok(res) => res,
            Err(ReplError::InvalidInput {
                character,
                line,
                column,
            }) => {
                return Err(EvalError::GeneralError {
                    message: format!(
                        "read error: unexpected character {} at line {}, column: {}",
                        character, line, column
                    ),
                })
            }
        };

        let mut parser = Parser::new();
        match parser.parse(&input) {
            Ok(res) => {
                if res.borrow().childs.len() > 1 {
                    Ok(res)
                } else {
                    Ok(Tree::root("#void".to_owned()))
                }
            }
            Err(_) => Err(EvalError::GeneralError {
                message: "error parsing expression".to_owned(),
            }),
        }
    }

    fn lambda(args: &NodePtr) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("lambda", ArgAmount::LessThan(1), args)?;

        let arg_list = Self::first_expression(&args)?;
        let body = Self::rest_expressions(&args)?;

        match Self::expression_type(&arg_list) {
            Type::Procedure | Type::Name => (),
            _ => {
                return Err(EvalError::GeneralError {
                    message: "lambda: wrong argument type".to_owned(),
                })
            }
        }

        let res = Tree::root("#procedure".to_owned());
        Tree::adopt_node(&res, arg_list);

        let progn = Tree::root("(".to_owned());
        Tree::add_child(&progn, "progn".to_owned());

        for child in body.borrow().childs.iter() {
            Tree::adopt_node(&progn, child.clone());
        }

        Tree::adopt_node(&res, progn);
        Ok(res)
    }

    fn define(&mut self, args: &NodePtr) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("define", ArgAmount::NotEqual(2), args)?;
        let name = Self::first_expression(&args)?;

        let name = match Self::expression_type(&name) {
            Type::Name => Tree::get_data(&name),
            _ => {
                return Err(EvalError::GeneralError {
                    message: "define: wrong type of first argument".to_owned(),
                })
            }
        };

        let value = Self::rest_expressions(&args)?;
        let value = Self::first_expression(&value)?;
        let value = self.eval(&value)?;

        let res = Tree::root("#void".to_owned());
        if Tree::get_parent(args).is_some() {
            res.borrow_mut().scope.insert(name, value);
        } else {
            self.global_scope.insert(name, value);
        }

        Ok(res)
    }

    fn quote(tree: &NodePtr) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("quote", ArgAmount::MoreThan(1), tree)?;

        let res = Self::first_expression(&tree)?;
        match Self::expression_type(&res) {
            Type::Procedure | Type::Name => {
                let root = Tree::root("(".to_owned());
                Tree::add_child(&root, "quote".to_owned());
                Tree::adopt_node(&root, res);
                Ok(root)
            }
            _ => Ok(res),
        }
    }

    fn car(tree: &NodePtr) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("car", ArgAmount::MoreThan(1), tree)?;

        let res = Self::first_expression(&tree)?;
        match Self::expression_type(&res) {
            Type::List => {
                let res = Self::rest_expressions(&res)?;
                let res = Self::first_expression(&res)?;
                let res = Self::first_expression(&res)?;
                match Self::expression_type(&res) {
                    Type::Name => {
                        let root = Tree::root("(".to_owned());
                        Tree::add_child(&root, "quote".to_owned());
                        Tree::adopt_node(&root, res);
                        Ok(root)
                    }
                    _ => Ok(res),
                }
            }
            _ => Err(EvalError::GeneralError {
                message: format!("car: expected pair, got {}", Self::tree_to_string(&res)),
            }),
        }
    }

    fn cdr(tree: &NodePtr) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("cdr", ArgAmount::MoreThan(1), tree)?;

        let res = Self::first_expression(&tree)?;
        match Self::expression_type(&res) {
            Type::List => {
                let res = Self::rest_expressions(&res)?;
                let res = Self::first_expression(&res)?;
                let res = Self::rest_expressions(&res)?;
                match Self::expression_type(&res) {
                    Type::Procedure | Type::Name => {
                        let root = Tree::root("(".to_owned());
                        Tree::add_child(&root, "quote".to_owned());
                        Tree::adopt_node(&root, res);
                        Ok(root)
                    }
                    _ => Ok(res),
                }
            }
            _ => Err(EvalError::GeneralError {
                message: format!("cdr: expected pair, got {}", Self::tree_to_string(&res)),
            }),
        }
    }

    fn cons(args: &NodePtr) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("cons", ArgAmount::NotEqual(2), args)?;

        let res = Self::first_expression(&args)?;
        let first = match Self::expression_type(&res) {
            Type::List | Type::Symbol => {
                let res = Self::rest_expressions(&res)?;
                Self::first_expression(&res)?
            }
            _ => res,
        };

        let res = Self::rest_expressions(args)?;
        let res = Self::first_expression(&res)?;
        let second = match Self::expression_type(&res) {
            Type::List | Type::Symbol => {
                let res = Self::rest_expressions(&res)?;
                Self::first_expression(&res)?
            }
            _ => res,
        };

        let quote = Tree::root("(".to_owned());
        Tree::add_child(&quote, "quote".to_owned());

        let pair = Tree::add_child(&quote, "(".to_owned());
        Tree::adopt_node(&pair, first);
        Tree::add_child(&pair, ".".to_owned());
        Tree::adopt_node(&pair, second);

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
                    // for subexpr in res.borrow().childs.iter().skip(1) {
                    match evaluator.eval(&res) {
                        Ok(res) => {
                            assert_eq!(Evaluator::tree_to_string(&res), correct);
                        }
                        Err(e) => panic!("{:?}", e),
                    }
                    // }
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
            Evaluator::expression_type(&parser.parse("32").ok().unwrap().borrow().childs[1]),
            Type::I32
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("-32").ok().unwrap().borrow().childs[1]),
            Type::I32
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("32.0").ok().unwrap().borrow().childs[1]),
            Type::F32
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("-32.0").ok().unwrap().borrow().childs[1]),
            Type::F32
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("\"str\"").ok().unwrap().borrow().childs[1]),
            Type::Str
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("name").ok().unwrap().borrow().childs[1]),
            Type::Name
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("'name").ok().unwrap().borrow().childs[1]),
            Type::Symbol
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("(name)").ok().unwrap().borrow().childs[1]),
            Type::Procedure
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("'(name)").ok().unwrap().borrow().childs[1]),
            Type::List
        );
    }
}
