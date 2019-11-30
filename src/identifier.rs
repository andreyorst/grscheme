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
    _NumU32,
    _NumI32,
    _NumF32,
    _True,
    _False,
    _Any,
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
