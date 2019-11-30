#[derive(Debug, Clone)]
pub enum Token {
    Procedure,
    Id,
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
