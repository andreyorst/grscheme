use crate::identifier::Type;

#[derive(Debug, Clone)]
pub enum Token {
    Procedure,
    Id,
    Value { r#type: Type },
    Lambda { paren_count: u32, state: State },
    Eval,
    Apply,
    Quote,
    List { paren_count: u32, state: State },
    Symbol,
    None,
}

#[derive(Debug, Clone)]
pub enum State {
    Args,
    Body,
    Wait,
}
