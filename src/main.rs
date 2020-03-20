mod evaluator;
mod reader;
mod runner;
mod tree;

fn main() -> Result<(), u32> {
    runner::run()
}
