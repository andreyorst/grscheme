mod interpreter;
mod identifier;
mod stack_item;
mod token;
mod evaluator;

use crate::interpreter::Interpreter;

fn main() {
    Interpreter::repl();
}
