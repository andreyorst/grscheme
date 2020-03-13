use crate::reader::{Data, GRData, NodePtr, Reader};
use crate::tree::Tree;
use rug::{Integer, Rational};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Type {
    Integer,
    Rational,
    Float,
    Name,
    Str,
    Symbol,
    List,
    Procedure,
    Pattern,
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Integer => "integer",
            Type::Float => "float",
            Type::Rational => "rational",
            Type::Name => "name",
            Type::Str => "string",
            Type::Symbol => "symbol",
            Type::List => "list",
            Type::Procedure => "procedure",
            Type::Pattern => "pattern",
        }
        .to_owned()
    }
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

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let msg = match self {
            EvalError::GeneralError { message } => message.to_owned(),
            EvalError::UnknownProc { name } => format!("unknown procedure \"{}\"", name),
            EvalError::WrongArgAmount {
                procedure,
                expected,
                fact,
            } => format!(
                "wrong amount of arguments to \"{}\": expected {}, got {}",
                procedure, expected, fact
            ),
            EvalError::UnboundIdentifier { name } => format!("unbound identifier \"{}\"", name),
        };
        write!(f, "{}", msg)
    }
}

const BUILTINS: &[&str] = &[
    "car", "cdr", "cons", "display", "eval", "empty?", "+", "-", "*", "/", "<", ">", "<=", ">=",
    "=", "length", "newline", "progn",
];

const BUILTINS_NOEVAL: &[&str] = &["quote", "read", "define", "lambda", "if", "cond", "let"];

pub struct Evaluator {
    global_scope: HashMap<String, NodePtr>,
}

/*
 * Internal methods
 */
impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            global_scope: HashMap::new(),
        }
    }

    pub fn eval(&mut self, expression: &NodePtr) -> Result<NodePtr, EvalError> {
        let mut stack = vec![expression.clone()];
        let mut res = None;
        while let Some(expr) = stack.last() {
            res = match Self::expression_type(&expr) {
                Type::Procedure => {
                    let tmp = expr.clone();
                    let proc = Self::first_expression(&expr)?;
                    let proc = match Self::expression_type(&proc) {
                        Type::Procedure => {
                            stack.push(proc);
                            continue;
                        }
                        Type::Name => {
                            let name = self.lookup(&proc)?;
                            Tree::replace_tree(&proc, name);
                            proc
                        }
                        _ => proc,
                    };

                    let mut args = Self::rest_expressions(&expr)?;
                    let proc_name = proc.borrow().data.data.to_string();

                    if proc_name.len() > 11 {
                        let name = &proc_name[11..];
                        let args_to_eval: Vec<NodePtr> = match name {
                            "if" => Self::pre_if(&args)?,
                            "let" => Self::pre_let(&args)?,
                            "define" => Self::pre_define(&args)?,
                            &_ => {
                                if name == "progn" {
                                    args.reverse();
                                }
                                if proc.borrow().data.user_defined_procedure
                                    || !BUILTINS_NOEVAL.contains(&name)
                                    || Self::expression_type(&proc) == Type::List
                                {
                                    args.iter()
                                        .cloned()
                                        .filter(|arg| match Self::expression_type(arg) {
                                            Type::Name | Type::Procedure => true,
                                            _ => false,
                                        })
                                        .collect()
                                } else {
                                    vec![]
                                }
                            }
                        };
                        if !args_to_eval.is_empty() {
                            stack.extend_from_slice(&args_to_eval);
                            continue;
                        }
                    }

                    if proc.borrow().data.user_defined_procedure {
                        if let Some(p) = Tree::parent(&tmp) {
                            if p.borrow().siblings.last().unwrap() == &tmp
                                && p.borrow()
                                    .siblings
                                    .first()
                                    .unwrap()
                                    .borrow()
                                    .data
                                    .data
                                    .to_string()
                                    == "#procedure:progn"
                            {
                                // possible tail call
                                let res = self.apply_lambda(&tmp, true)?;
                                if res.0 {
                                    // valid tail call
                                    Tree::replace_tree(&p, res.1);
                                    stack.pop();
                                } else {
                                    Tree::replace_tree(&tmp, res.1);
                                }
                                continue;
                            }
                        }
                        Tree::replace_tree(&tmp, self.apply_lambda(&tmp, false)?.1);
                    } else {
                        Tree::replace_tree(&tmp, self.apply(&tmp)?);
                    }

                    continue;
                }
                Type::List | Type::Symbol => {
                    let res = Self::quote(&Self::rest_expressions(&expr)?)?;
                    Tree::replace_tree(&expr, res);
                    stack.pop()
                }
                Type::Name => {
                    let value = self.lookup(&expr)?;
                    Tree::replace_tree(&expr, value);
                    stack.pop()
                }
                _ => stack.pop(),
            };
        }
        Ok(res.unwrap())
    }

    fn apply(&mut self, expression: &NodePtr) -> Result<NodePtr, EvalError> {
        let proc = Self::first_expression(&expression)?;
        let args = Self::rest_expressions(&expression)?;

        match Self::expression_type(&proc) {
            Type::Pattern => {
                if !&proc
                    .borrow()
                    .data
                    .data
                    .to_string()
                    .starts_with("#procedure")
                {
                    return Err(EvalError::GeneralError {
                        message: format!(
                            "wrong type to apply: \"{}\"",
                            Self::tree_to_string(&proc)
                        ),
                    });
                }
            }
            Type::List => {
                return Self::list_get(&proc, &args);
            }
            _ => {
                return Err(EvalError::GeneralError {
                    message: format!("wrong type to apply: \"{}\"", Self::tree_to_string(&proc)),
                })
            }
        }
        let proc = proc.borrow().data.data.to_string()[11..].to_owned();
        match proc.as_ref() {
            "quote" => Self::quote(&args),
            "newline" => Self::newline(&args),
            "read" => Self::read(&args),
            "define" => self.define(&expression, &args),
            "lambda" => Self::lambda(&args),
            "if" => Self::if_proc(&args),
            "cond" => Self::cond(&args),
            "progn" => Self::progn(&args),
            "let" => Self::let_proc(&args),
            "car" => Self::car(&args),
            "cdr" => Self::cdr(&args),
            "cons" => Self::cons(&args),
            "display" => Self::display(&args),
            "eval" => Self::eval_proc(&args),
            "empty?" => Self::is_empty(&args),
            "length" => Self::length(&args),
            "+" | "-" | "*" | "/" | "%" | "remainder" => Self::math(&proc, &args),
            "<" | ">" | "<=" | ">=" | "=" => Self::compare(&proc, &args),
            _ => Err(EvalError::UnknownProc { name: proc }),
        }
    }

    fn apply_lambda(
        &mut self,
        expression: &NodePtr,
        tail_call: bool,
    ) -> Result<(bool, NodePtr), EvalError> {
        let lambda = Self::first_expression(expression)?;
        let args = Self::rest_expressions(expression)?;
        let lambda_args = Self::first_expression(&lambda)?;
        let lambda_body = Self::rest_expressions(&lambda)?[0].clone();
        let mut can_optimize = tail_call;

        if tail_call {
            if let Some(p) = Tree::parent(&expression) {
                let args: Vec<String> = lambda_args
                    .borrow()
                    .siblings
                    .iter()
                    .map(|a| a.borrow().data.data.to_string())
                    .collect();
                let mut body = lambda_body.borrow_mut();
                for (key, val) in p.borrow().data.scope.iter() {
                    if !args.contains(key) {
                        can_optimize = false;
                    } else {
                        body.data.scope.insert(key.clone(), val.clone());
                    }
                }
            }
        }

        match Self::expression_type(&lambda_args) {
            Type::Procedure => {
                Self::check_argument_count(
                    "#lambda",
                    ArgAmount::NotEqual(lambda_args.borrow().siblings.len()),
                    &args,
                )?;
                let mut lambda_body = lambda_body.borrow_mut();
                for (key, val) in lambda_args.borrow().siblings.iter().zip(&args) {
                    lambda_body
                        .data
                        .scope
                        .insert(key.borrow().data.to_string(), val.clone());
                }
            }
            Type::Name => {
                let quoted_args = Tree::new(GRData::from_str("("));
                Tree::push_child(&quoted_args, GRData::from_str("quote"));
                let list = Tree::push_child(&quoted_args, GRData::from_str("("));
                for c in args.iter() {
                    let res = match Self::expression_type(&c) {
                        Type::List | Type::Symbol => {
                            let res = Self::rest_expressions(&c)?;
                            res[0].clone()
                        }
                        _ => c.clone(),
                    };
                    Tree::push_tree(&list, res);
                }
                lambda_body
                    .borrow_mut()
                    .data
                    .scope
                    .insert(lambda_args.borrow().data.to_string(), quoted_args);
            }
            _ => {
                return Err(EvalError::GeneralError {
                    message: "wrong binding type".to_owned(),
                })
            }
        };

        Ok((can_optimize, lambda_body))
    }

    fn lookup(&mut self, expression: &NodePtr) -> Result<NodePtr, EvalError> {
        let name = expression.borrow().data.data.to_string();

        if BUILTINS.contains(&&name.as_ref()) || BUILTINS_NOEVAL.contains(&&name.as_ref()) {
            return Ok(Tree::new(GRData {
                data: Data::deduce_type_and_convert(&format!("#procedure:{}", name)),
                user_defined_procedure: false,
                extra_up: false,
                scope: expression.borrow().data.scope.clone(),
            }));
        }

        let mut current = expression.clone();
        while let Some(p) = Tree::parent(&current) {
            current = p.clone();
            if let Some(v) = current.borrow().data.scope.get(&name) {
                return Ok(Tree::clone_tree(v));
            }
        }

        if let Some(v) = self.global_scope.get(&name) {
            return Ok(Tree::clone_tree(v));
        }

        Err(EvalError::UnboundIdentifier { name })
    }

    pub fn print(expression: &NodePtr) {
        match Self::expression_type(expression) {
            Type::Pattern => match expression.borrow().data.data.to_string().as_ref() {
                "#void" => (),
                _ => println!("{}", expression.borrow().data.data.to_string()),
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
        let data = &expression.borrow().data.data.to_string();
        if let "(" = data.as_ref() {
            print_closing = true;
        }
        string.push_str(data);
        for child in expression.borrow().siblings.iter() {
            let data = &child.borrow().data.data.to_string();
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
                        string.push_str(data);
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
        let data = &s.borrow().data.data;
        match data {
            Data::String { data } => {
                if data.starts_with('#') {
                    Type::Pattern
                } else if !s.borrow().siblings.is_empty() {
                    match &s.borrow().siblings[0].borrow().data.data {
                        Data::String { data } if data == "quote" => {
                            match &s.borrow().siblings[1].borrow().data.data {
                                Data::String { data } if data == "(" => Type::List,
                                _ => Type::Symbol,
                            }
                        }
                        _ => Type::Procedure,
                    }
                } else if data.starts_with('"') && data.ends_with('"') {
                    Type::Str
                } else {
                    Type::Name
                }
            }
            Data::Integer { .. } => Type::Integer,
            Data::Float { .. } => Type::Float,
            Data::Rational { .. } => Type::Rational,
        }
    }

    fn check_argument_count(
        proc: &str,
        amount: ArgAmount,
        args: &[NodePtr],
    ) -> Result<(), EvalError> {
        let len = args.len();
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
        if !expression.borrow().siblings.is_empty() {
            Ok(expression.borrow().siblings[0].clone())
        } else {
            panic!(
                "first: expected pair, got \"{}\"",
                Self::tree_to_string(expression)
            )
        }
    }

    fn rest_expressions(expression: &NodePtr) -> Result<Vec<NodePtr>, EvalError> {
        if expression.borrow().siblings.len() >= 1 {
            Ok(expression.borrow().siblings[1..].to_vec())
        } else {
            panic!(
                "rest: expected list, got \"{}\"",
                Self::tree_to_string(expression)
            )
        }
    }
}

/*
 * Language procedures
 */
impl Evaluator {
    fn eval_proc(expression: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("eval", ArgAmount::MoreThan(1), expression)?;

        let expr = expression[0].clone();
        let expr = match Self::expression_type(&expr) {
            Type::Symbol | Type::List => Self::rest_expressions(&expr)?[0].clone(),
            _ => expression[0].clone(),
        };

        Ok(expr)
    }

    fn display(args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("display", ArgAmount::NotEqual(1), args)?;

        let res = args[0].clone();
        match Self::expression_type(&res) {
            Type::Pattern => print!("{}", &res.borrow().data.data.to_string()),
            _ => print!("{}", Self::tree_to_string(&res)),
        }

        Ok(Tree::new(GRData::from_str("#void")))
    }

    fn newline(args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("newline", ArgAmount::NotEqual(0), args)?;
        println!();
        Ok(Tree::new(GRData::from_str("#void")))
    }

    fn progn(args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("progn", ArgAmount::LessThan(1), args)?;
        Ok(args.last().unwrap().clone())
    }

    fn list_get(list: &NodePtr, args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        if args.len() < 1 || args.len() > 2 {
            Err(EvalError::GeneralError {
                message: format!(
                    "wrong amount of arguments to list expression: expected 1 or 2, got {}",
                    args.len()
                ),
            })
        } else {
            let items = Self::rest_expressions(list)?[0].borrow().siblings.clone();
            match &args[0].borrow().data.data {
                Data::Integer { data } => {
                    let start = if *data >= 0 && *data < items.len() {
                        data.to_usize_wrapping()
                    } else {
                        return Err(EvalError::GeneralError {
                            message: format!(
                                "index {} is out of bounds of a list of len {}",
                                data,
                                items.len()
                            ),
                        });
                    };
                    let end = if args.len() > 1 {
                        match &args[1].borrow().data.data {
                            Data::Integer { data } => {
                                if *data >= 0 && *data < items.len() {
                                    data.to_usize_wrapping()
                                } else {
                                    return Err(EvalError::GeneralError {
                                        message: format!(
                                            "index {} is out of bounds of a list of len {}",
                                            data,
                                            items.len()
                                        ),
                                    });
                                }
                            }
                            _ => {
                                return Err(EvalError::GeneralError {
                                    message: format!(
                                        "can't take slice of a list with index of type {}",
                                        Self::expression_type(&args[1]).to_string(),
                                    ),
                                })
                            }
                        }
                    } else {
                        start.clone()
                    };

                    let quote = Tree::new(GRData::from_str("("));
                    Tree::push_child(&quote, GRData::from_str("quote"));
                    let list = if args.len() > 1 {
                        Tree::push_child(&quote, GRData::from_str("("))
                    } else {
                        quote.clone()
                    };

                    for item in items[start..end + 1].iter() {
                        Tree::push_tree(&list, item.clone());
                    }
                    Ok(quote)
                }
                _ => Err(EvalError::GeneralError {
                    message: format!(
                        "can't index a list with index of type {}",
                        Self::expression_type(&args[0]).to_string(),
                    ),
                }),
            }
        }
    }

    fn read(args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("read", ArgAmount::NotEqual(0), args)?;

        let input = match Reader::balanced_read("read > ") {
            Ok(res) => res,
            Err(e) => {
                return Err(EvalError::GeneralError {
                    message: e.to_string(),
                })
            }
        };

        let mut parser = Reader::new();
        match parser.parse(&input) {
            Ok(res) => {
                if res.borrow().siblings.len() > 1 {
                    Ok(res)
                } else {
                    Ok(Tree::new(GRData::from_str("#void")))
                }
            }
            Err(_) => Err(EvalError::GeneralError {
                message: "error parsing expression".to_owned(),
            }),
        }
    }

    fn lambda(args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("lambda", ArgAmount::LessThan(1), args)?;

        let arg_list = args[0].clone();
        let body = args[1..].to_vec();

        match Self::expression_type(&arg_list) {
            Type::Procedure | Type::Name => (),
            _ => {
                return Err(EvalError::GeneralError {
                    message: "lambda: wrong argument type".to_owned(),
                })
            }
        }

        let res = Tree::new(GRData::from_proc_str("#procedure:anonymous"));
        Tree::push_tree(&res, arg_list.clone());

        let progn = Tree::new(GRData::from_str("("));
        Tree::push_child(&progn, GRData::from_str("progn"));

        let mut current = Tree::parent(&body[0]);
        while let Some(p) = current {
            if !p.borrow().data.scope.is_empty() {
                for (key, val) in p.borrow().data.scope.iter() {
                    progn
                        .borrow_mut()
                        .data
                        .scope
                        .insert(key.clone(), val.clone());
                }
                break;
            }
            current = Tree::parent(&p);
        }

        for child in body.iter() {
            Tree::push_tree(&progn, child.clone());
        }

        Tree::push_tree(&res, progn);

        Ok(res)
    }

    fn pre_define(args: &[NodePtr]) -> Result<Vec<NodePtr>, EvalError> {
        Self::check_argument_count("define", ArgAmount::NotEqual(2), args)?;

        match Self::expression_type(&args[1]) {
            Type::Name | Type::Procedure => Ok(vec![args[1].clone()]),
            _ => Ok(vec![]),
        }
    }

    fn define(&mut self, expression: &NodePtr, args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        let name = args[0].clone();

        let name = match Self::expression_type(&name) {
            Type::Name => name.borrow().data.data.clone(),
            _ => {
                return Err(EvalError::GeneralError {
                    message: "define: wrong type of first argument".to_owned(),
                })
            }
        };

        let value = args[1].clone();

        if let Type::Pattern = Self::expression_type(&value) {
            if value.borrow().data.data.to_string() == "#procedure:anonymous" {
                value.borrow_mut().data.data = Data::String {
                    data: format!("#procedure:{}", name.to_string()),
                };
                value.borrow_mut().data.user_defined_procedure = true;
            }
        }

        let res = Tree::new(GRData::from_str("#void"));
        if let Some(p) = Tree::parent(&expression) {
            p.borrow_mut().data.scope.insert(name.to_string(), value);
        } else {
            self.global_scope.insert(name.to_string(), value);
        }

        Ok(res)
    }

    fn quote(args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("quote", ArgAmount::MoreThan(1), args)?;

        let res = args[0].clone();
        match Self::expression_type(&res) {
            Type::Procedure | Type::Name => {
                let root = Tree::new(GRData::from_str("("));
                Tree::push_child(&root, GRData::from_str("quote"));
                Tree::push_tree(&root, res);
                Ok(root)
            }
            _ => Ok(res),
        }
    }

    fn car(tree: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("car", ArgAmount::MoreThan(1), tree)?;
        Self::check_argument_count("car", ArgAmount::LessThan(1), tree)?;

        let list = tree[0].clone();
        match Self::expression_type(&list) {
            Type::List => {
                let res = Self::rest_expressions(&list)?;
                let res = res[0].clone();
                if res.borrow().siblings.is_empty() {
                    return Err(EvalError::GeneralError {
                        message: "car: expected pair, got \'()".to_owned(),
                    });
                }
                let res = Self::first_expression(&res)?;
                match Self::expression_type(&res) {
                    Type::Name => {
                        let root = Tree::new(GRData::from_str("("));
                        Tree::push_child(&root, GRData::from_str("quote"));
                        Tree::push_tree(&root, res);
                        Ok(root)
                    }
                    _ => Ok(res),
                }
            }
            _ => Err(EvalError::GeneralError {
                message: format!("car: expected pair, got {}", Self::tree_to_string(&list)),
            }),
        }
    }

    fn cdr(tree: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("cdr", ArgAmount::MoreThan(1), tree)?;
        Self::check_argument_count("cdr", ArgAmount::LessThan(1), tree)?;

        let res = tree[0].clone();
        match Self::expression_type(&res) {
            Type::List => {
                let res = Self::rest_expressions(&res)?;
                let res = res[0].clone();

                let res = Self::rest_expressions(&res)?;

                let root = Tree::new(GRData::from_str("("));
                Tree::push_child(&root, GRData::from_str("quote"));
                let list = Tree::push_child(&root, GRData::from_str("("));
                for item in res.iter() {
                    Tree::push_tree(&list, item.clone());
                }
                Ok(root)
            }
            _ => Err(EvalError::GeneralError {
                message: format!("cdr: expected pair, got {}", Self::tree_to_string(&res)),
            }),
        }
    }

    fn cons(args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("cons", ArgAmount::NotEqual(2), args)?;

        let res = args[0].clone();
        let first = match Self::expression_type(&res) {
            Type::List | Type::Symbol => Self::rest_expressions(&res)?[0].clone(),
            _ => res,
        };

        let res = args[1].clone();
        let second = match Self::expression_type(&res) {
            Type::List => Self::rest_expressions(&res)?[0].clone(),
            _ => {
                return Err(EvalError::GeneralError {
                    message: format!(
                        "Expected list as a second argument, got {}",
                        Self::tree_to_string(&res)
                    ),
                })
            }
        };

        let quote = Tree::new(GRData::from_str("("));
        Tree::push_child(&quote, GRData::from_str("quote"));

        let pair = Tree::push_child(&quote, GRData::from_str("("));
        Tree::push_tree(&pair, first);

        for n in second.borrow().siblings.iter() {
            Tree::push_tree(&pair, n.clone());
        }

        Ok(quote)
    }

    fn pre_if(args: &[NodePtr]) -> Result<Vec<NodePtr>, EvalError> {
        Self::check_argument_count("if", ArgAmount::LessThan(2), args)?;

        match Self::expression_type(&args[0]) {
            Type::Name | Type::Procedure => Ok(vec![args[0].clone()]),
            _ => Ok(vec![]),
        }
    }
    fn if_proc(args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        let condition = match Self::expression_type(&args[0]) {
            Type::Pattern => match args[0].borrow().data.data.to_string().as_ref() {
                "#f" => false,
                _ => true,
            },
            _ => true,
        };

        Ok(if condition {
            args[1].clone()
        } else {
            args[2].clone()
        })
    }

    fn cond(args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("cond", ArgAmount::LessThan(2), args)?;
        if args.len() % 2 != 0 {
            return Err(EvalError::GeneralError {
                message: "cond: wrong amount of bindings".to_owned(),
            });
        }
        let root = Tree::new(GRData::from_str("("));
        Tree::push_child(&root, GRData::from_str("if"));
        let mut tmp = root.clone();
        for (predicate, branch) in args.iter().step_by(2).zip(args.iter().skip(1).step_by(2)) {
            Tree::push_tree(&tmp, predicate.clone());
            Tree::push_tree(&tmp, branch.clone());
            tmp = Tree::push_tree(&tmp, Tree::new(GRData::from_str("(")));
            Tree::push_child(&tmp, GRData::from_str("if"));
        }
        Tree::push_child(&tmp, GRData::from_str("#t"));
        Tree::push_child(&tmp, GRData::from_str("#void"));
        Tree::push_child(&tmp, GRData::from_str("#void"));

        Ok(root.clone())
    }

    fn is_empty(tree: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("empty?", ArgAmount::NotEqual(1), tree)?;
        let first = tree[0].clone();
        let first = match Self::expression_type(&first) {
            Type::List => Self::rest_expressions(&first)?[0].clone(),
            _ => {
                return Err(EvalError::GeneralError {
                    message: format!(
                        "empty?: expected list, got {}",
                        Self::tree_to_string(&first)
                    ),
                })
            }
        };
        let res = if first.borrow().siblings.is_empty() {
            Tree::new(GRData::from_str("#t"))
        } else {
            Tree::new(GRData::from_str("#f"))
        };
        Ok(res)
    }

    fn length(tree: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("length", ArgAmount::NotEqual(1), tree)?;
        let first = tree[0].clone();
        let first = match Self::expression_type(&first) {
            Type::List => Self::rest_expressions(&first)?[0].clone(),
            _ => {
                return Err(EvalError::GeneralError {
                    message: format!(
                        "empty?: expected list, got {}",
                        Self::tree_to_string(&first)
                    ),
                })
            }
        };
        let res = Tree::new(GRData::from_str(&first.borrow().siblings.len().to_string()));
        Ok(res)
    }

    fn math(operation: &str, args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        match Self::dominant_type(args) {
            Type::Rational => {
                let operands = Self::convert_to_rational(&args)?;
                let data = match operation {
                    "+" => Self::add(&operands)?,
                    "-" => {
                        if operands.len() > 1 {
                            Self::substract(&operands)?
                        } else {
                            Self::negate(&operands)?
                        }
                    }
                    "*" => Self::multiply(&operands)?,
                    "/" => Self::divide(&operands)?,
                    "%" | "remainder" => {
                        return Err(EvalError::GeneralError {
                            message: "remainder expects integer, got rational".to_string(),
                        })
                    }
                    _ => {
                        return Err(EvalError::GeneralError {
                            message: "wrong operation".to_owned(),
                        })
                    }
                };
                Ok(Tree::new(GRData {
                    data: Data::Rational { data },
                    extra_up: false,
                    user_defined_procedure: false,
                    scope: HashMap::new(),
                }))
            }
            Type::Float => {
                let operands = Self::convert_to_f64(&args)?;
                let data = match operation {
                    "+" => Self::add(&operands)?,
                    "-" => {
                        if operands.len() > 1 {
                            Self::substract(&operands)?
                        } else {
                            Self::negate(&operands)?
                        }
                    }
                    "*" => Self::multiply(&operands)?,
                    "/" => Self::divide(&operands)?,
                    "%" | "remainder" => {
                        return Err(EvalError::GeneralError {
                            message: "remainder expects integer, got float".to_string(),
                        })
                    }
                    _ => {
                        return Err(EvalError::GeneralError {
                            message: "wrong operation".to_owned(),
                        })
                    }
                };
                Ok(Tree::new(GRData {
                    data: Data::Float { data },
                    extra_up: false,
                    user_defined_procedure: false,
                    scope: HashMap::new(),
                }))
            }
            Type::Integer => {
                if operation == "/" {
                    return Ok(Tree::new(GRData {
                        data: Data::Rational {
                            data: Self::divide(&Self::convert_to_rational(&args)?)?,
                        },
                        user_defined_procedure: false,
                        extra_up: false,
                        scope: HashMap::new(),
                    }));
                } else {
                    let operands = Self::convert_to_integer(&args)?;
                    let data = match operation {
                        "+" => Self::add(&operands)?,
                        "-" => {
                            if operands.len() > 1 {
                                Self::substract(&operands)?
                            } else {
                                Self::negate(&operands)?
                            }
                        }
                        "*" => Self::multiply(&operands)?,
                        "%" | "remainder" => Self::remainder(&operands)?,
                        _ => {
                            return Err(EvalError::GeneralError {
                                message: "wrong operation".to_owned(),
                            })
                        }
                    };
                    Ok(Tree::new(GRData {
                        data: Data::Integer { data },
                        extra_up: false,
                        user_defined_procedure: false,
                        scope: HashMap::new(),
                    }))
                }
            }
            _ => Err(EvalError::GeneralError {
                message: format!(
                    "can't apply '{}' to argument with type of '{}'",
                    operation,
                    Self::dominant_type(args).to_string()
                ),
            }),
        }
    }

    fn convert_to_f64(args: &[NodePtr]) -> Result<Vec<f64>, EvalError> {
        let mut converted = vec![];
        for c in args.iter() {
            let res = match &c.borrow().data.data {
                Data::Float { data } => Ok(*data),
                Data::Integer { data } => Ok(data.to_f64()),
                Data::Rational { data } => Ok(data.to_f64()),
                _ => Err(()),
            };
            if let Ok(res) = res {
                converted.push(res);
            } else {
                return Err(EvalError::GeneralError {
                    message: format!(
                        "can't convert '{}', to type 'f64'",
                        c.borrow().data.data.to_string(),
                    ),
                });
            }
        }
        Ok(converted)
    }

    fn convert_to_integer(args: &[NodePtr]) -> Result<Vec<Integer>, EvalError> {
        let mut converted = vec![];
        for c in args.iter() {
            let res = match &c.borrow().data.data {
                Data::Float { data } => Integer::from_f64(*data),
                Data::Integer { data } => Some(data.clone()),
                Data::Rational { data } => Integer::from_f64(data.to_f64()),
                _ => None,
            };
            if let Some(res) = res {
                converted.push(res);
            } else {
                return Err(EvalError::GeneralError {
                    message: format!(
                        "can't convert '{}', to type 'Integer'",
                        c.borrow().data.data.to_string(),
                    ),
                });
            }
        }
        Ok(converted)
    }

    fn convert_to_rational(args: &[NodePtr]) -> Result<Vec<Rational>, EvalError> {
        let mut converted = vec![];
        for c in args.iter() {
            let res = match &c.borrow().data.data {
                Data::Float { data } => Rational::from_f64(*data),
                Data::Integer { data } => Some(Rational::new() + data),
                Data::Rational { data } => Some(data.clone()),
                _ => None,
            };
            if let Some(res) = res {
                converted.push(res);
            } else {
                return Err(EvalError::GeneralError {
                    message: format!(
                        "can't convert '{}', to type 'Rational'",
                        c.borrow().data.data.to_string(),
                    ),
                });
            }
        }
        Ok(converted)
    }

    fn add<T>(args: &[T]) -> Result<T, EvalError>
    where
        T: std::ops::AddAssign + Clone,
    {
        let mut res = args[0].clone();
        for i in args.iter().skip(1) {
            res += i.clone();
        }
        Ok(res)
    }

    fn multiply<T>(args: &[T]) -> Result<T, EvalError>
    where
        T: std::ops::MulAssign + Clone,
    {
        let mut res = args[0].clone();
        for i in args.iter().skip(1) {
            res *= i.clone();
        }
        Ok(res)
    }

    fn substract<T>(args: &[T]) -> Result<T, EvalError>
    where
        T: std::ops::SubAssign + Clone,
    {
        let mut res = args[0].clone();
        for i in args.iter().skip(1) {
            res -= i.clone();
        }
        Ok(res)
    }

    fn negate<T>(args: &[T]) -> Result<T, EvalError>
    where
        T: std::ops::Neg<Output = T> + Clone,
    {
        Ok(-args[0].clone())
    }

    fn divide<T>(args: &[T]) -> Result<T, EvalError>
    where
        T: std::ops::DivAssign + Clone + PartialEq + std::str::FromStr,
    {
        let mut res = args[0].clone();

        if args[1..].contains(&"0".parse::<T>().ok().unwrap()) {
            return Err(EvalError::GeneralError {
                message: "division by zero".to_owned(),
            });
        }
        for i in args.iter().skip(1) {
            res /= i.clone();
        }
        Ok(res)
    }

    fn remainder<T>(args: &[T]) -> Result<T, EvalError>
    where
        T: std::ops::Rem<Output = T> + Clone,
    {
        if args.len() != 2 {
            Err(EvalError::WrongArgAmount {
                procedure: "remainder".to_owned(),
                expected: 2,
                fact: args.len(),
            })
        } else {
            Ok(args[0].clone() % args[1].clone())
        }
    }

    fn dominant_type(args: &[NodePtr]) -> Type {
        let mut t = Type::Integer;
        for c in args.iter() {
            if Self::expression_type(c) > t {
                t = Self::expression_type(c);
            }
        }
        t
    }

    fn compare(operation: &str, args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        let res = match Self::dominant_type(args) {
            Type::Rational => {
                let operands = Self::convert_to_rational(&args)?;
                match operation {
                    "<" => Self::less_than(&operands),
                    "<=" => Self::less_than(&operands) | Self::equal(&operands),
                    "=" => Self::equal(&operands),
                    ">" => !Self::less_than(&operands),
                    ">=" => !Self::less_than(&operands) | Self::equal(&operands),
                    _ => {
                        return Err(EvalError::GeneralError {
                            message: "wrong operation".to_owned(),
                        })
                    }
                }
            }
            Type::Float => {
                let operands = Self::convert_to_f64(&args)?;
                match operation {
                    "<" => Self::less_than(&operands),
                    "<=" => Self::less_than(&operands) | Self::equal(&operands),
                    "=" => Self::equal(&operands),
                    ">" => !Self::less_than(&operands),
                    ">=" => !Self::less_than(&operands) | Self::equal(&operands),
                    _ => {
                        return Err(EvalError::GeneralError {
                            message: "wrong operation".to_owned(),
                        })
                    }
                }
            }
            Type::Integer => {
                let operands = Self::convert_to_integer(&args)?;
                match operation {
                    "<" => Self::less_than(&operands),
                    "<=" => Self::less_than(&operands) | Self::equal(&operands),
                    "=" => Self::equal(&operands),
                    ">" => !Self::less_than(&operands),
                    ">=" => !Self::less_than(&operands) | Self::equal(&operands),
                    _ => {
                        return Err(EvalError::GeneralError {
                            message: "wrong operation".to_owned(),
                        })
                    }
                }
            }
            _ => {
                return Err(EvalError::GeneralError {
                    message: format!(
                        "cant apply '{}' to argument with type of '{}'",
                        operation,
                        Self::dominant_type(args).to_string()
                    ),
                })
            }
        };

        Ok(Tree::new(GRData::from_str(if res { "#t" } else { "#f" })))
    }

    fn less_than<T>(args: &[T]) -> bool
    where
        T: std::cmp::PartialOrd + Clone,
    {
        let mut res = false;
        let mut left = args[0].clone();
        for i in args.iter().skip(1) {
            let right = i.clone();
            res = left < right;
            if !res {
                break;
            }
            left = right;
        }
        res
    }

    fn equal<T>(args: &[T]) -> bool
    where
        T: std::cmp::PartialOrd + Clone,
    {
        let mut res = false;
        let mut left = args[0].clone();
        for i in args.iter().skip(1) {
            let right = i.clone();
            res = left == right;
            if !res {
                break;
            }
            left = right;
        }
        res
    }

    fn pre_let(args: &[NodePtr]) -> Result<Vec<NodePtr>, EvalError> {
        Self::check_argument_count("let", ArgAmount::LessThan(2), args)?;
        let binding_list = args[0].clone();
        if binding_list.borrow().siblings.len() % 2 != 0 {
            return Err(EvalError::GeneralError {
                message: "let: wrong amount of bindings".to_owned(),
            });
        }
        let res = binding_list
            .borrow()
            .siblings
            .iter()
            .cloned()
            .skip(1)
            .step_by(2)
            .filter(|arg| match Self::expression_type(arg) {
                Type::Name | Type::Procedure => true,
                _ => false,
            })
            .collect::<Vec<NodePtr>>();
        Ok(res.clone())
    }

    fn let_proc(args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        let binding_list = args[0].clone();
        let progn = Tree::new(GRData::from_str("("));

        for (n, v) in binding_list
            .borrow()
            .siblings
            .iter()
            .step_by(2)
            .zip(binding_list.borrow().siblings.iter().skip(1).step_by(2))
        {
            progn
                .borrow_mut()
                .data
                .scope
                .insert(n.borrow().data.to_string(), v.clone());
        }

        Tree::push_child(&progn, GRData::from_str("progn"));

        let body = &args[1..];
        for expr in body.iter() {
            Tree::push_tree(&progn, expr.clone());
        }

        Ok(progn)
    }
}

#[cfg(test)]
mod tests {
    use crate::evaluator::{Evaluator, Type};
    use crate::reader::Reader;

    fn test_inputs_with_outputs(tests: &[&'static str], results: &[&'static str]) {
        let mut parser = Reader::new();
        let mut evaluator = Evaluator::new();
        for (test, correct) in tests.iter().zip(results) {
            match parser.parse(test) {
                Ok(res) => match evaluator.eval(&res) {
                    Ok(res) => {
                        assert_eq!(Evaluator::tree_to_string(&res), correct.to_owned());
                    }
                    Err(e) => panic!("{:?}", e),
                },
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
            "(car '(3 4))",
            "(car '(b c))",
        ];
        let results = vec!["1", "2", "'a", "3", "'b"];
        test_inputs_with_outputs(&tests, &results);
    }

    #[test]
    fn test_cdr() {
        let tests = vec![
            "(cdr '(1 2 3))",
            "(cdr '(1))",
            "(cdr '(a b c))",
            "(cdr '(3 4))",
            "(cdr '(b c))",
        ];
        let results = vec!["'(2 3)", "'()", "'(b c)", "'(4)", "'(c)"];
        test_inputs_with_outputs(&tests, &results);
    }

    #[test]
    fn test_cons() {
        let tests = vec![
            "(cons 1 '(2))",
            "(cons 'a '(b))",
            "(cons 1 '(2 3))",
            "(cons '(1) '(2 3))",
            "(cons 'a '(b c))",
            "(cons '(a) '(b c))",
        ];
        let results = vec![
            "'(1 2)",
            "'(a b)",
            "'(1 2 3)",
            "'((1) 2 3)",
            "'(a b c)",
            "'((a) b c)",
        ];
        test_inputs_with_outputs(&tests, &results);
    }

    #[test]
    fn test_cond() {
        let tests = vec![
            "(cond (= 1 2) 1 (= 2 2) 2)",
            "(cond (= 1 2) 1 (= 1 2) 2 #t 3)",
            "(cond (= 1 2) 1)",
            "(cond (cond (= 1 1) #t) 1)",
        ];
        let results = vec!["2", "3", "#void"];
        test_inputs_with_outputs(&tests, &results);
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
            "'a", "1", "'(a)", "'(a 'b)", "'(1)", "'(1 '2)", "\"str\"", "'a", "1", "'(a)",
            "'(a 'b)", "'(1)", "'(1 '2)", "\"str\"",
        ];
        test_inputs_with_outputs(&tests, &results);
    }

    #[test]
    fn behavior_tests() {
        let inputs = [
            "(empty? '())",
            "(empty? '(a))",
            "(if (empty? '()) 'a 'b)",
            "(if (empty? '(a)) 'a 'b)",
            "(if (empty? '(a)) 'a (progn 'b 'c))",
            "(define true #t)
             (if true 'then 'else)",
            "(define false #f)
             (if false 'then 'else)",
            "(define a 22)
             a",
            "(define cadr (lambda (x) (car (cdr x))))
             (cadr '(1 2 3))",
            "(define f (lambda x x))
                       (car (f 3 2 1))",
            "(define f (lambda x x))
                       (cdr (f 3 2 1))",
            "(define f (lambda (x)
                       (if (empty? x)
                           '()
                           (cons (car x) (f (cdr x))))))
                     (f '(1 2 3 4))",
            "(define map (lambda (f x)
               (if (empty? x)
                   '()
                   (cons (f (car x)) (map f (cdr x))))))
             (define list (lambda x x))
             (map list (list 1 2 3 4 5))",
            "(define a (lambda (x)
               (if (empty? x)
                   '()
                   (cons 'a (b (cdr x))))))
             (define b (lambda (x)
               (if (empty? x)
                   '()
                   (cons 'b (a (cdr x))))))
             (a '(1 2 3 4))",
            "(+ 1 2)",
            "(+ 1 2.1)",
            "(* 1 2)",
            "(* 1 2.1)",
            "(/ 1 2)",
            "(/ 1 2.0)",
            "(- 1 2)",
            "(- 1 1.9)",
            "(- 1)",
            "(- 1.1)",
            "(define map-add (lambda (x)
               (if (empty? (cdr x))
                      (car x)
                      (+ (car x) (map-add (cdr x))))))
             (map-add '(1 2 3 4))",
            "(define seq (lambda (x y)
               (if (= x y)
                   '()
                   (cons x (seq (+ x 1) y)))))
             (seq 0 4)",
            "(define f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))
             (f 6)",
            "(define f (lambda (n) (define fi (lambda (m n) (if (= n 0) m (fi (* m n) (- n 1))))) (fi 1 n)))
             (f 5)",
             "(define list (lambda x x))
              (define add-gen (lambda (n) (lambda (x) (+ x n))))
              (define add2 (add-gen 2))
              (define add15 (add-gen 15))
              (define add-5 (add-gen -5))
              (list (add2 3) (add2 1) (add15 -15) (add-5 3))",
            "(let (a 1 b (+ 1 2)) (+ a b))",
            "(define append (lambda (l1 l2)
               (if (empty? l1)
                   l2
                   (cons (car l1) (append (cdr l1) l2)))))
             (define reverse (lambda (lis)
               (if (empty? lis)
                   '()
                   (append (reverse (cdr lis))
                           (list (car lis))))))
             (define list (lambda x x))
             (reverse '(1 2 3))",
            "(define a '(1 2 3))
             (define list (lambda x x))
             (list (a 0) (a 0 1) (a 1 2) (a 0 2))"
        ];

        let outputs = [
            "#t",
            "#f",
            "'a",
            "'b",
            "'c",
            "'then",
            "'else",
            "22",
            "2",
            "3",
            "'(2 1)",
            "'(1 2 3 4)",
            "'((1) (2) (3) (4) (5))",
            "'(a b a b)",
            "3",
            "3.1",
            "2",
            "2.1",
            "1/2",
            "0.5",
            "-1",
            "-0.8999999999999999",
            "-1",
            "-1.1",
            "10",
            "'(0 1 2 3)",
            "720",
            "120",
            "'(5 3 0 -2)",
            "4",
            "'(3 2 1)",
            "'(1 (1 2) (2 3) (1 2 3))",
        ];

        for (input, output) in inputs.iter().zip(outputs.iter()) {
            test_behavior(input, output);
        }
    }

    #[test]
    fn closure_test() {
        let input = "(define expt
               (lambda (x p)
                 (if (>= p 1)
                     (* x (expt x (- p 1)))
                     1)))

             (define abs
               (lambda (x)
                 (if (< x 0) (- x) x)))

             (define tolerance 0.00001)

             (define fixed-point
               (lambda (f first-guess)
                 (define close-enough?
                   (lambda (v1 v2)
                     (< (abs (- v1 v2)) tolerance)))
                 (define try
                   (lambda (guess)
                     (let (next (f guess))
                       (if (close-enough? guess next)
                           next
                           (try next)))))
                 (try first-guess)))

             (define average-damp
               (lambda (f)
                 (define average
                   (lambda (x y) (/ (+ x y) 2)))
                 (lambda (x) (average x (f x)))))

             (define compose
               (lambda (f1 f2)
                 (lambda (x) (f1 (f2 x)))))

             (define repeated
               (lambda (f n)
                 (if (< n 1)
                     (lambda (x) x)
                     (compose f (repeated f (- n 1))))))

             ((lambda (x)
                (fixed-point
                 (average-damp
                  (lambda (y) (/ x (* y y))))
                 1.0)) 8)

             ((lambda (x)
                (fixed-point
                 (average-damp
                  (average-damp
                   (lambda (y) (/ x (* y y y)))))
                 1.0)) 16)

             ((lambda (x)
                (fixed-point
                 (average-damp
                  (average-damp
                   (average-damp
                    (lambda (y) (/ x (* y y y y))))))
                 1.0)) 32)

             (define create-nth-root (lambda (n) (lambda (x) (fixed-point ((repeated average-damp (- n 2)) (lambda (y) (/ x (expt y (- n 1))))) 1.0))))

             (define octa-root (create-nth-root 6))
             (octa-root 64)";
        let output = "2.000011071925238";

        test_behavior(input, output);
    }

    #[test]
    #[ignore]
    fn cps_test() {
        let input = "(define list (lambda x x))
             (define *& (lambda (x y k) (k (* x y))))
             (define -& (lambda (x y k) (k (- x y))))
             (define =& (lambda (x y k) (k (= x y))))
             (define fact&
               (lambda (n k)
                 (=& n 0 (lambda (b)
                           (if b                    ; growing continuation
                               (k 1)                ; in the recursive call
                               (-& n 1 (lambda (nm1)
                                         (fact& nm1 (lambda (f)
                                                      (*& n f k))))))))))
             (define fact-iter&
               (lambda (n k) (f-aux& n 1 k)))
             (define f-aux&
               (lambda (n a k)
                 (=& n 0 (lambda (b)
                           (if b                    ; unmodified continuation
                               (k a)                ; in the recursive call
                               (-& n 1 (lambda (nm1)
                                         (*& n a (lambda (nta)
                                                   (f-aux& nm1 nta k))))))))))
             (list
              (fact& 6 (lambda (x) x))
              (fact-iter& 7 (lambda (x) x)))";

        let output = "'(720 5040)";

        test_behavior(input, output);
    }

    fn test_behavior(input: &str, output: &str) {
        let mut parser = Reader::new();
        let mut evaluator = Evaluator::new();
        match parser.parse(input) {
            Ok(t) => match evaluator.eval(&t) {
                Ok(res) => assert_eq!(output, Evaluator::tree_to_string(&res)),
                Err(e) => panic!("{:?}\nexpression: \"{}\"", e, input),
            },
            Err(e) => panic!("{:?}\nexpression: \"{}\"", e, input),
        }
    }
    #[test]
    fn test_types() {
        let mut parser = Reader::new();
        assert_eq!(
            Evaluator::expression_type(&parser.parse("32").ok().unwrap().borrow().siblings[1]),
            Type::Integer,
            "32"
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("-32").ok().unwrap().borrow().siblings[1]),
            Type::Integer,
            "-32"
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("32.0").ok().unwrap().borrow().siblings[1]),
            Type::Float,
            "32.0"
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("1/2").ok().unwrap().borrow().siblings[1]),
            Type::Rational,
            "1/2"
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("-32.0").ok().unwrap().borrow().siblings[1]),
            Type::Float,
            "-32.0"
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("\"str\"").ok().unwrap().borrow().siblings[1]),
            Type::Str,
            "\"str\""
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("name").ok().unwrap().borrow().siblings[1]),
            Type::Name,
            "name"
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("'name").ok().unwrap().borrow().siblings[1]),
            Type::Symbol,
            "'name"
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("(name)").ok().unwrap().borrow().siblings[1]),
            Type::Procedure,
            "(name)"
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("'(name)").ok().unwrap().borrow().siblings[1]),
            Type::List,
            "'(name)"
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("'()").ok().unwrap().borrow().siblings[1]),
            Type::List,
            "'()"
        );
    }
}
