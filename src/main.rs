mod interpreter;
mod identifier;
mod evaluator;
mod tree;

use crate::interpreter::repl;

fn main() {
    repl();
}
