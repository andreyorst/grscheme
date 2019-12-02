use crate::identifier::Type;
use crate::interpreter::get_item_type;

pub fn calculate(operands: &[&String], op: &str) -> Option<String> {
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

pub fn compare(operands: &[&String], op: &str) -> Option<String> {
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

pub fn list(_operands: &[&String]) -> Option<String> {
    Some("".to_owned())
}

pub fn quote(operands: &[&String]) -> Option<String> {
    let mut res = String::from("'");
    if operands.starts_with(&[&"(".to_owned()]) {
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
        let item_type = get_item_type(&operands[0]);
        res = match item_type {
            Type::Name => format!("'{}", operands[0]),
            _ => {
                operands[0].to_owned()
            }
        };
    } else {
        return None;
    }
    Some(res)
}
