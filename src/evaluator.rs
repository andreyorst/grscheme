use crate::identifier::Type;
use crate::interpreter::_get_item_type;

pub fn _calculate(operands: &[&String], op: &str) -> Option<String> {
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

pub fn _compare(operands: &[&String], op: &str) -> Option<String> {
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

pub fn _list_impl(operands: &[&String]) -> Option<String> {
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

pub fn _first(list: &[&String]) -> Option<String> {
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
                ' ' | ')' => return _quote(&[&item]),
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

pub fn _rest(list: &[&String]) -> Option<String> {
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
    _list_impl(&rest)
}

pub fn _quote(operands: &[&String]) -> Option<String> {
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
        let item_type = _get_item_type(&operands[0]);
        res = match item_type {
            Type::Name => format!("'{}", operands[0]),
            _ => operands[0].to_owned(),
        };
    } else {
        return None;
    }
    Some(res)
}

#[test]
fn test_quote() {
    assert_eq!(
        _quote(&[
            &"(".to_owned(),
            &"1".to_owned(),
            &"2".to_owned(),
            &")".to_owned()
        ]),
        Some("'(1 2)".to_owned())
    );
    assert_eq!(
        _quote(&[&"(".to_owned(), &")".to_owned()]),
        Some("'()".to_owned())
    );
    assert_eq!(_quote(&[&"(".to_owned()]), None);
    assert_eq!(_quote(&[&"quote".to_owned()]), Some("'quote".to_owned()));
    assert_eq!(_quote(&[&"quote".to_owned(), &"quote".to_owned()]), None);
}
