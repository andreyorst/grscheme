use crate::interpreter::Token;

#[derive(Debug)]
pub enum State {
    ExprStart,
    ExprEnd,
    Arg,
    Body,
}

#[derive(Debug)]
pub struct StackItem {
    pub token: Token,
    pub state: State,
    pub data: String,
}

impl StackItem {
    pub fn from(token: Token, data: &str) -> StackItem {
        StackItem {
            token,
            data: String::from(data),
            state: State::Arg,
        }
    }
}
