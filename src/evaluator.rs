use crate::tree::NodePtr;

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
    _List,
    _Symbol,
    _Str,
}

pub struct Evaluator {
    _global_scope: Vec<String>,
}

impl Evaluator {
    pub fn eval(&mut self, program: &NodePtr) {
        Self::print(program);
    }

    pub fn new() -> Evaluator {
        Evaluator {
            _global_scope: vec![],
        }
    }

    fn print(expression: &NodePtr) {
        for expr in expression.borrow().childs.iter() {
            let mut vector: Vec<String> = vec![];
            Self::print_recursive(expr, &mut vector);
            println!("{}", vector.join(""));
        }
    }

    fn print_recursive(expression: &NodePtr, vec_repr: &mut Vec<String>) {
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
                    }
                }
                "(" => Self::print_recursive(child, vec_repr),
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
            let _item_type = Self::_item_type(&operands[0]);
            res = match _item_type {
                Type::_Name => format!("'{}", operands[0]),
                _ => operands[0].to_owned(),
            };
        } else {
            return None;
        }
        Some(res)
    }

    fn _item_type(s: &str) -> Type {
        if s.trim().parse::<u32>().is_ok() {
            Type::_U32
        } else if s.trim().parse::<i32>().is_ok() {
            Type::_I32
        } else if s.trim().parse::<f32>().is_ok() {
            Type::_F32
        } else if s.starts_with('"') && s.ends_with('"') {
            Type::_Str
        } else if s.starts_with('\'') {
            if &s[0..2] == "'(" {
                Type::_List
            } else {
                Type::_Symbol
            }
        } else {
            Type::_Name
        }
    }
}

#[test]
fn test_types() {
    assert_eq!(Evaluator::_item_type("32"), Type::_U32);
    assert_eq!(Evaluator::_item_type("-32"), Type::_I32);
    assert_eq!(Evaluator::_item_type("32.0"), Type::_F32);
    assert_eq!(Evaluator::_item_type("-32.0"), Type::_F32);
    assert_eq!(Evaluator::_item_type("\"str\""), Type::_Str);
    assert_eq!(Evaluator::_item_type("'symbol"), Type::_Symbol);
    assert_eq!(Evaluator::_item_type("'(list list)"), Type::_List);
    assert_eq!(Evaluator::_item_type("name"), Type::_Name);
}
