mod repl;
mod lexer;
mod ast;
mod parser;
mod evaluator;

fn main() {
    crate::repl::start();
}
