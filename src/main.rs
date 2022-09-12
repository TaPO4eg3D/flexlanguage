mod repl;
mod lexer;
mod ast;
mod parser;

fn main() {
    crate::repl::start();
}
