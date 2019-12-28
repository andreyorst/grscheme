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
    List,
    Symbol,
    Str,
}

pub struct Evaluator {
    global_scope: Vec<String>,
}

impl Evaluator {
    pub fn eval(&mut self, program: &NodePtr) {
        Tree::print_tree(program);
    }

    pub fn new() -> Evaluator {
        Evaluator {
            global_scope: vec![],
        }
    }

    fn _calculate(operands: &[&String], op: &str) -> Option<String> {
        let mut res: i32;
        if operands.len() >= 2 || (operands.len() == 1 && op == "-") {
            res = match operands[0].trim().parse() {
                Ok(x) => x,
                Err(_) => return None,
            };
        } else {
            return None;
        }
        if operands.len() == 1 && op == "-" {
            res = -res;
        } else {
            for value in operands.iter().skip(1) {
                let val: i32 = match value.trim().parse() {
                    Ok(x) => x,
                    Err(_) => return None,
                };

                match op {
                    "+" => res += val,
                    "-" => {
                        if operands.len() == 1 {
                            res = -val;
                        } else {
                            res -= val;
                        }
                    }
                    "*" => res *= val,
                    "/" => {
                        if val != 0 {
                            res /= val;
                        } else {
                            return None;
                        }
                    }
                    &_ => return None,
                }
            }
        }
        Some(res.to_string())
    }

    fn _compare(operands: &[&String], op: &str) -> Option<String> {
        if operands.is_empty() {
            return None;
        }
        let mut res = false;
        let mut left: i32 = match operands[0].trim().parse() {
            Ok(x) => x,
            Err(_) => return None,
        };
        for value in operands.iter().skip(1) {
            let right: i32 = match value.trim().parse() {
                Ok(x) => x,
                Err(_) => return None,
            };
            res = match op {
                "=" => left == right,
                "<" => left < right,
                "<=" => left <= right,
                ">" => left > right,
                ">=" => left >= right,
                &_ => panic!("wrong operator"),
            };
            left = right;
        }

        if res {
            Some("#t".to_string())
        } else {
            Some("#f".to_string())
        }
    }

    fn _list_impl(operands: &[&String]) -> Option<String> {
        if operands.is_empty() {
            return Some("'()".to_owned());
        }
        let mut res = String::from("'(");
        let list = operands
            .iter()
            .map(|&s| s.to_owned())
            .collect::<Vec<String>>()
            .join(" ");
        res.push_str(&list);
        res.push(')');
        Some(res)
    }

    fn _first(list: &[&String]) -> Option<String> {
        if list.len() > 1 {
            return None;
        }
        let list = list[0];
        let mut item = String::new();
        let mut inside_string = false;
        for c in list.chars().skip(2) {
            if !inside_string {
                match c {
                    '"' => {
                        item.push(c);
                        inside_string = true;
                    }
                    ' ' | ')' => return Self::_quote(&[&item]),
                    _ => item.push(c),
                }
            } else {
                item.push(c);
                if c == '"' {
                    inside_string = false;
                }
            }
        }
        None
    }

    fn _rest(list: &[&String]) -> Option<String> {
        if list.len() > 1 {
            return None;
        }
        let list = list[0];
        let mut rest: Vec<String> = Vec::new();
        let mut item = String::new();
        let mut inside_string = false;
        let mut skip = true;
        for c in list.chars().skip(2) {
            if !inside_string {
                match c {
                    '"' => {
                        if !skip {
                            item.push(c);
                        }
                        inside_string = true;
                    }
                    ' ' | ')' => {
                        skip = false;
                        if !skip && !item.is_empty() {
                            rest.push(item.clone());
                            item.clear();
                        }
                    }
                    _ => {
                        if !skip {
                            item.push(c);
                        }
                    }
                }
            } else {
                if !skip {
                    item.push(c);
                }
                if c == '"' {
                    inside_string = false;
                }
            }
        }
        let rest: Vec<&String> = rest.iter().collect();
        Self::_list_impl(&rest)
    }

    fn _quote(operands: &[&String]) -> Option<String> {
        let mut res = String::from("'");
        if operands.starts_with(&[&"(".to_owned()]) {
            if !operands.ends_with(&[&")".to_owned()]) {
                return None;
            }
            for (i, item) in operands.iter().enumerate() {
                res.push_str(item);
                if i + 1 < operands.len() {
                    let next = operands[i + 1];
                    match next.to_owned().as_ref() {
                        ")" => (),
                        &_ => match item.to_owned().as_ref() {
                            "(" => (),
                            &_ => res.push(' '),
                        },
                    }
                }
            }
        } else if operands.len() == 1 {
            let item_type = Self::item_type(&operands[0]);
            res = match item_type {
                Type::Name => format!("'{}", operands[0]),
                _ => operands[0].to_owned(),
            };
        } else {
            return None;
        }
        Some(res)
    }

    fn item_type(s: &str) -> Type {
        if s.trim().parse::<u32>().is_ok() {
            Type::U32
        } else if s.trim().parse::<i32>().is_ok() {
            Type::I32
        } else if s.trim().parse::<f32>().is_ok() {
            Type::F32
        } else if s.starts_with('"') && s.ends_with('"') {
            Type::Str
        } else if s.starts_with('\'') {
            if &s[0..2] == "'(" {
                Type::List
            } else {
                Type::Symbol
            }
        } else {
            Type::Name
        }
    }
}

#[test]
fn test_types() {
    assert_eq!(Evaluator::item_type("32"), Type::U32);
    assert_eq!(Evaluator::item_type("-32"), Type::I32);
    assert_eq!(Evaluator::item_type("32.0"), Type::F32);
    assert_eq!(Evaluator::item_type("-32.0"), Type::F32);
    assert_eq!(Evaluator::item_type("\"str\""), Type::Str);
    assert_eq!(Evaluator::item_type("'symbol"), Type::Symbol);
    assert_eq!(Evaluator::item_type("'(list list)"), Type::List);
    assert_eq!(Evaluator::item_type("name"), Type::Name);
}
