use std::io;
use std::io::prelude::*;

use std::rc::Rc;
use std::cell::RefCell;

use flexcore::ast::Node;

use flexcore::lexer::Lexer;
use flexcore::parser::Parser;

use flexcore::evaluator::eval;
use flexcore::evaluator::environment::Environment;
use flexcore::evaluator::builtin::construct_builtins;

const PROMPT: &str = ">> ";

pub fn main() {
    let mut buffer = String::new();
    let env = Rc::new(RefCell::new(Environment::new()));
    env.borrow_mut().outer = Some(Rc::new(RefCell::new(Environment {
        store: construct_builtins(),
        outer: None,
    })));

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

        let evaluated = eval(Node::Program(program), Rc::clone(&env));
        println!("{}", evaluated);

        buffer.clear();
    }
}
