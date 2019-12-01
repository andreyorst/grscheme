#![warn(clippy::all)]

use crate::token::Token;

#[derive(Debug)]
pub struct StackItem {
    pub token: Token,
    pub data: String,
}

impl StackItem {
    pub fn from(token: Token, data: &str) -> StackItem {
        StackItem {
            token,
            data: String::from(data),
        }
    }
}
