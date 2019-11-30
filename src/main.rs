mod interpreter;
mod identifier;
mod stack_item;
mod token;

use crate::interpreter::Interpreter;

fn main() {
    Interpreter::repl();
}
