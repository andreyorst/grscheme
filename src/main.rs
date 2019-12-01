mod interpreter;
mod identifier;
mod stack_item;
mod evaluator;

use crate::interpreter::repl;

fn main() {
    repl();
}
