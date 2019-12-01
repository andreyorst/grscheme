#![warn(clippy::all)]

#[derive(Debug)]
pub struct Identifier {
    data: String,
    pattern: Pattern,
}

#[derive(Debug)]
enum Pattern {
    _Procedure {
        arguments: Vec<String>,
        body: Vec<String>,
    },
    _Value { r#type: Type },
    _True,
    _False,
    _Any,
}

#[derive(Debug, Clone)]
pub enum Type {
    U32,
    I32,
    F32,
    Name,
    Str,
}

impl Identifier {
    pub fn _new() -> Identifier {
        Identifier {
            data: String::new(),
            pattern: Pattern::_Any,
        }
    }

    pub fn _procedure(args: &[String], body: &[String]) -> Identifier {
        Identifier {
            data: String::from("#procedure:anonymous"),
            pattern: Pattern::_Procedure {
                arguments: args.to_vec(),
                body: body.to_vec(),
            }
        }
    }
}
