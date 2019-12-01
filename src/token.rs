#![warn(clippy::all)]

use crate::identifier::Type;

#[derive(Debug, Clone)]
pub enum Token {
    Procedure,
    Id,
    Value { item_type: Type },
    Lambda { paren_count: u32, state: State },
    Eval,
    Apply,
    Quote { procedure: bool },
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
