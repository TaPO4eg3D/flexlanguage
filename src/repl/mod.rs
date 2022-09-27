use std::io;
use std::io::prelude::*;

use crate::ast::Node;

use crate::lexer::Lexer;
use crate::parser::Parser;

use crate::evaluator::eval;

const PROMPT: &str = ">> ";

pub fn start() {
    let mut buffer = String::new();

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        io::stdin().read_line(&mut buffer).unwrap();

        let mut chars = buffer.chars().collect();
        let mut lexer = Lexer::new(buffer.len(), &mut chars);
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            for err in parser.errors {
                println!("{:?}", err);
            }
        }

        let evaluated = eval(Node::Program(program));
        println!("{}", evaluated);

        buffer.clear();
    }
}
