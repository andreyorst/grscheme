use crate::reader::{GRData, NodePtr, Reader};
use crate::tree::Tree;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Type {
    I32,
    F32,
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
            Type::I32 => "i32",
            Type::F32 => "f32",
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
    "car",
    "cdr",
    "cons",
    "display",
    "eval",
    "empty?",
    "+",
    "-",
    "*",
    "/",
    "<",
    ">",
    "<=",
    ">=",
    "=",
    "length",
    "newline",
    "anonymous",
    "progn",
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

    #[allow(dead_code)]
    fn examine_stack(stack: &[NodePtr]) {
        print!("stack: [ ");
        for item in stack.iter() {
            if item.borrow().data.data == "(" {
                print!("{} ", item.borrow().siblings[0].borrow().data.data);
            } else {
                print!("{} ", item.borrow().data.data);
            }
        }
        println!("]");
        std::thread::sleep(std::time::Duration::from_millis(100));
    }

    pub fn eval(&mut self, expression: &NodePtr) -> Result<NodePtr, EvalError> {
        let mut stack = vec![expression.clone()];
        let mut res = None;
        while let Some(expr) = stack.last() {
            // Self::examine_stack(&stack);
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

                    if proc.borrow().data.data.ends_with("progn") {
                        args.reverse();
                    }

                    if proc.borrow().data.data.len() > 11
                        && !BUILTINS_NOEVAL.contains(&&proc.borrow().data.data[11..])
                    {
                        let args_to_eval: Vec<NodePtr> = args
                            .iter()
                            .cloned()
                            .filter(|arg| match Self::expression_type(arg) {
                                Type::Name | Type::Procedure => true,
                                _ => false,
                            })
                            .collect();

                        if !args_to_eval.is_empty() {
                            stack.extend_from_slice(&args_to_eval);
                            continue;
                        }
                    }

                    if proc.borrow().data.data == "#procedure:anonymous" {
                        let res = self.apply_lambda(&tmp)?;
                        if let Some(p) = Tree::parent(&tmp) {
                            if p.borrow().siblings.last().unwrap() == &tmp
                                && p.borrow().siblings.first().unwrap().borrow().data.data
                                    == "#procedure:progn"
                            {
                                Tree::replace_tree(&p, res);
                                stack.pop();
                                continue;
                            }
                        }
                        Tree::replace_tree(&tmp, res);
                    } else {
                        let res = self.apply(&tmp)?;
                        Tree::replace_tree(&tmp, res);
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
        Ok(res.unwrap().clone())
    }

    fn apply(&mut self, expression: &NodePtr) -> Result<NodePtr, EvalError> {
        let proc = Self::first_expression(&expression)?;
        let args = Self::rest_expressions(&expression)?;

        match Self::expression_type(&proc) {
            Type::Pattern => {
                if !&proc.borrow().data.data.starts_with("#procedure") {
                    return Err(EvalError::GeneralError {
                        message: format!(
                            "wrong type to apply: \"{}\"",
                            Self::tree_to_string(&proc)
                        ),
                    });
                }
            }
            _ => {
                return Err(EvalError::GeneralError {
                    message: format!("wrong type to apply: \"{}\"", Self::tree_to_string(&proc)),
                })
            }
        }
        let proc = proc.borrow().data.data[11..].to_owned();
        match proc.as_ref() {
            "quote" => Self::quote(&args),
            "newline" => Self::newline(&args),
            "read" => Self::read(&args),
            "define" => self.define(&expression, &args),
            "lambda" => Self::lambda(&args),
            "if" => self.if_proc(&args),
            "cond" => self.cond(&args),
            "progn" => Self::progn(&args),
            // "let" => self.let_proc(&args),
            "car" => Self::car(&args),
            "cdr" => Self::cdr(&args),
            "cons" => Self::cons(&args),
            "display" => Self::display(&args),
            "eval" => self.eval_proc(&args),
            "empty?" => Self::is_empty(&args),
            "length" => Self::length(&args),
            "+" | "-" | "*" | "/" => Self::math(&proc, &args),
            "<" | ">" | "<=" | ">=" | "=" => Self::compare(&proc, &args),
            _ => Err(EvalError::UnknownProc { name: proc }),
        }
    }

    fn apply_lambda(&mut self, expression: &NodePtr) -> Result<NodePtr, EvalError> {
        let lambda = Self::first_expression(expression)?;
        let args = Self::rest_expressions(expression)?;
        let lambda_args = Self::first_expression(&lambda)?;
        let lambda_body = Self::rest_expressions(&lambda)?[0].clone();
        if let Some(p) = Tree::parent(&expression) {
            for (key, val) in p.borrow().data.scope.iter() {
                lambda_body
                    .borrow_mut()
                    .data
                    .scope
                    .insert(key.clone(), val.clone());
            }
        };
        match Self::expression_type(&lambda_args) {
            Type::Procedure => {
                Self::check_argument_count(
                    "#lambda",
                    ArgAmount::NotEqual(lambda_args.borrow().siblings.len()),
                    &args,
                )?;
                for (n, v) in lambda_args.borrow().siblings.iter().zip(&args) {
                    lambda_body
                        .borrow_mut()
                        .data
                        .scope
                        .insert(n.borrow().data.to_string(), v.clone());
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

        Ok(lambda_body)
    }

    fn lookup(&mut self, expression: &NodePtr) -> Result<NodePtr, EvalError> {
        let mut current = expression.clone();
        let name = expression.borrow().data.data.clone();
        if BUILTINS.contains(&&name.as_ref()) || BUILTINS_NOEVAL.contains(&&name.as_ref()) {
            return Ok(Tree::new(GRData {
                data: format!("#procedure:{}", &expression.borrow().data.data),
                extra_up: false,
                scope: expression.borrow().data.scope.clone(),
            }));
        }

        while let Some(p) = Tree::parent(&current) {
            current = p.clone();
            if let Some(v) = current.borrow().data.scope.get(&name) {
                return Ok(Tree::clone_tree(v));
            }
        }

        if let Some(v) = self.global_scope.get(&expression.borrow().data.data) {
            return Ok(Tree::clone_tree(v));
        }

        Err(EvalError::UnboundIdentifier {
            name: expression.borrow().data.data.clone(),
        })
    }

    pub fn print(expression: &NodePtr) {
        match Self::expression_type(expression) {
            Type::Pattern => match expression.borrow().data.data.as_ref() {
                "#void" => (),
                _ => println!("{}", expression.borrow().data.data),
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
        let data = &expression.borrow().data.data;
        if let "(" = data.as_ref() {
            print_closing = true;
        }
        string.push_str(data);
        for child in expression.borrow().siblings.iter() {
            let data = &child.borrow().data.data;
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
        if s.borrow().data.data.starts_with('#') {
            Type::Pattern
        } else if !s.borrow().siblings.is_empty() {
            if s.borrow().siblings[0].borrow().data.data == "quote" {
                if s.borrow().siblings[1].borrow().data.data == "(" {
                    Type::List
                } else {
                    Type::Symbol
                }
            } else {
                Type::Procedure
            }
        } else if s.borrow().data.data.trim().parse::<i32>().is_ok() {
            Type::I32
        } else if s.borrow().data.data.trim().parse::<f32>().is_ok() {
            Type::F32
        } else if s.borrow().data.data.starts_with('"') && s.borrow().data.data.ends_with('"') {
            Type::Str
        } else {
            Type::Name
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
    fn eval_proc(&mut self, expression: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("eval", ArgAmount::MoreThan(1), expression)?;

        let expr = expression[0].clone();
        let expr = match Self::expression_type(&expr) {
            Type::Symbol | Type::List => Self::rest_expressions(&expr)?[0].clone(),
            _ => expression[0].clone(),
        };

        self.eval(&expr)
    }

    fn display(args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("display", ArgAmount::NotEqual(1), args)?;

        let res = args[0].clone();
        match Self::expression_type(&res) {
            Type::Pattern => print!("{}", &res.borrow().data.data),
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

        let res = Tree::new(GRData::from_str("#procedure:anonymous"));
        Tree::push_tree(&res, arg_list);

        let progn = Tree::new(GRData::from_str("("));
        Tree::push_child(&progn, GRData::from_str("progn"));

        for child in body.iter() {
            Tree::push_tree(&progn, child.clone());
        }

        Tree::push_tree(&res, progn);

        Ok(res)
    }

    fn define(&mut self, expression: &NodePtr, args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("define", ArgAmount::NotEqual(2), args)?;
        let name = args[0].clone();

        let name = match Self::expression_type(&name) {
            Type::Name => name.borrow().data.data.clone(),
            _ => {
                return Err(EvalError::GeneralError {
                    message: "define: wrong type of first argument".to_owned(),
                })
            }
        };

        let value = self.eval(&args[1])?;

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

    fn if_proc(&mut self, args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("if", ArgAmount::LessThan(2), args)?;

        let condition = self.eval(&args[0])?;
        let condition = match Self::expression_type(&condition) {
            Type::Pattern => match condition.borrow().data.data.as_ref() {
                "#t" => true,
                _ => false,
            },
            _ => {
                return Err(EvalError::GeneralError {
                    message: "wrong condition type".to_owned(),
                });
            }
        };

        let res = if condition {
            args[1].clone()
        } else {
            args[2].clone()
        };

        Ok(res)
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
        let res = match Self::dominant_type(args) {
            Type::F32 => {
                let operands = Self::convert_to_type::<f32>(&args);
                match operation {
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
                    _ => {
                        return Err(EvalError::GeneralError {
                            message: "wrong operation".to_owned(),
                        })
                    }
                }
            }
            Type::I32 => {
                let operands = Self::convert_to_type::<i32>(&args);
                match operation {
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
        let res = Tree::new(GRData::from_str(&res));
        Ok(res)
    }

    fn convert_to_type<T>(args: &[NodePtr]) -> Vec<T>
    where
        T: std::str::FromStr,
    {
        let mut res = vec![];
        for c in args.iter() {
            res.push(c.borrow().data.data.trim().parse::<T>().ok().unwrap())
        }
        res
    }

    fn add<T>(args: &[T]) -> Result<String, EvalError>
    where
        T: std::ops::AddAssign + std::string::ToString + Clone,
    {
        let mut res = args[0].clone();
        for i in args.iter().skip(1) {
            res += i.clone();
        }
        Ok(res.to_string())
    }

    fn multiply<T>(args: &[T]) -> Result<String, EvalError>
    where
        T: std::ops::MulAssign + std::string::ToString + Clone,
    {
        let mut res = args[0].clone();
        for i in args.iter().skip(1) {
            res *= i.clone();
        }
        Ok(res.to_string())
    }

    fn substract<T>(args: &[T]) -> Result<String, EvalError>
    where
        T: std::ops::SubAssign + std::string::ToString + Clone,
    {
        let mut res = args[0].clone();
        for i in args.iter().skip(1) {
            res -= i.clone();
        }
        Ok(res.to_string())
    }

    fn negate<T>(args: &[T]) -> Result<String, EvalError>
    where
        T: std::ops::Neg<Output = T> + std::string::ToString + Clone,
    {
        Ok((-args[0].clone()).to_string())
    }

    fn divide<T>(args: &[T]) -> Result<String, EvalError>
    where
        T: std::ops::DivAssign + std::string::ToString + Clone + PartialEq + std::str::FromStr,
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
        Ok(res.to_string())
    }

    fn dominant_type(args: &[NodePtr]) -> Type {
        let mut t = Type::I32;
        for c in args.iter() {
            if Self::expression_type(c) > t {
                t = Self::expression_type(c);
            }
        }
        t
    }

    fn compare(operation: &str, args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        let res = match Self::dominant_type(args) {
            Type::F32 => {
                let operands = Self::convert_to_type::<f32>(&args);
                match operation {
                    "<" => Self::less_than(&operands),
                    "<=" => Self::less_equal(&operands),
                    "=" => Self::equal(&operands),
                    ">" => Self::greater_than(&operands),
                    ">=" => Self::greater_equal(&operands),
                    _ => {
                        return Err(EvalError::GeneralError {
                            message: "wrong operation".to_owned(),
                        })
                    }
                }
            }
            Type::I32 => {
                let operands = Self::convert_to_type::<i32>(&args);
                match operation {
                    "<" => Self::less_than(&operands),
                    "<=" => Self::less_equal(&operands),
                    "=" => Self::equal(&operands),
                    ">" => Self::greater_than(&operands),
                    ">=" => Self::greater_equal(&operands),
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

        let res = if res { "#t" } else { "#f" };

        let res = Tree::new(GRData::from_str(res));
        Ok(res)
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

    fn less_equal<T>(args: &[T]) -> bool
    where
        T: std::cmp::PartialOrd + Clone,
    {
        let mut res = false;
        let mut left = args[0].clone();
        for i in args.iter().skip(1) {
            let right = i.clone();
            res = left <= right;
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

    fn greater_than<T>(args: &[T]) -> bool
    where
        T: std::cmp::PartialOrd + Clone,
    {
        let mut res = false;
        let mut left = args[0].clone();
        for i in args.iter().skip(1) {
            let right = i.clone();
            res = left > right;
            if !res {
                break;
            }
            left = right;
        }
        res
    }

    fn greater_equal<T>(args: &[T]) -> bool
    where
        T: std::cmp::PartialOrd + Clone,
    {
        let mut res = false;
        let mut left = args[0].clone();
        for i in args.iter().skip(1) {
            let right = i.clone();
            res = left >= right;
            if !res {
                break;
            }
            left = right;
        }
        res
    }

    // fn let_proc(&mut self, args: &NodePtr) -> Result<NodePtr, EvalError> {
    //     Self::check_argument_count("let", ArgAmount::LessThan(2), args)?;
    //     let binding_list = Self::first_expression(args)?;
    //     if binding_list.borrow().siblings.len() % 2 != 0 {
    //         return Err(EvalError::GeneralError {
    //             message: "let: wrong amount of bindings".to_owned(),
    //         });
    //     }
    //     let bindings = Tree::new(GRData::from_str("#bindings"));
    //     for (n, v) in binding_list
    //         .borrow()
    //         .siblings
    //         .iter()
    //         .step_by(2)
    //         .zip(binding_list.borrow().siblings.iter().skip(1).step_by(2))
    //     {
    //         bindings
    //             .borrow_mut()
    //             .data
    //             .scope
    //             .insert(n.borrow().data.to_string(), self.eval(v)?);
    //     }

    //     let progn = Tree::new(GRData::from_str("("));
    //     Tree::push_child(&progn, GRData::from_str("progn"));
    //     Tree::push_tree(&progn, bindings);

    //     let body = Self::rest_expressions(&args)?;
    //     for expr in body.borrow().siblings.iter() {
    //         Tree::push_tree(&progn, expr.clone());
    //     }

    //     Ok(progn)
    // }

    fn cond(&mut self, args: &[NodePtr]) -> Result<NodePtr, EvalError> {
        Self::check_argument_count("cond", ArgAmount::LessThan(2), args)?;
        if args.len() % 2 != 0 {
            return Err(EvalError::GeneralError {
                message: "cond: wrong amount of conditionals".to_owned(),
            });
        }
        for (cond, body) in args.iter().step_by(2).zip(args.iter().skip(1).step_by(2)) {
            let cond = self.eval(cond)?;
            let cond = match Self::expression_type(&cond) {
                Type::Pattern => match cond.borrow().data.data.as_ref() {
                    "#t" => true,
                    _ => false,
                },
                _ => {
                    return Err(EvalError::GeneralError {
                        message: "wrong condition type".to_owned(),
                    })
                }
            };
            if cond {
                return Ok(body.clone());
            }
        }
        Err(EvalError::GeneralError {
            message: "".to_owned(),
        })
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
            "0",
            "0.5",
            "-1",
            "-0.9",
            "-1",
            "-1.1",
            "10",
            "'(0 1 2 3)",
            "720",
            "120",
        ];

        for (input, output) in inputs.iter().zip(outputs.iter()) {
            test_behavior(input, output);
        }
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
            Type::I32
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("-32").ok().unwrap().borrow().siblings[1]),
            Type::I32
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("32.0").ok().unwrap().borrow().siblings[1]),
            Type::F32
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("-32.0").ok().unwrap().borrow().siblings[1]),
            Type::F32
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("\"str\"").ok().unwrap().borrow().siblings[1]),
            Type::Str
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("name").ok().unwrap().borrow().siblings[1]),
            Type::Name
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("'name").ok().unwrap().borrow().siblings[1]),
            Type::Symbol
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("(name)").ok().unwrap().borrow().siblings[1]),
            Type::Procedure
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("'(name)").ok().unwrap().borrow().siblings[1]),
            Type::List
        );
        assert_eq!(
            Evaluator::expression_type(&parser.parse("'()").ok().unwrap().borrow().siblings[1]),
            Type::List
        );
    }
}
