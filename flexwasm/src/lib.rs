use std::rc::Rc;
use std::cell::RefCell;

use wasm_bindgen::prelude::*;

use flexcore::ast::Node;

use flexcore::lexer::Lexer;
use flexcore::parser::Parser;

use flexcore::evaluator::eval;
use flexcore::evaluator::environment::Environment;
use flexcore::evaluator::builtin::construct_builtins;

#[wasm_bindgen]
pub fn wasm_eval(input: String) -> String {
    let env = Rc::new(RefCell::new(Environment::new()));
    env.borrow_mut().outer = Some(Rc::new(RefCell::new(Environment {
        store: construct_builtins(),
        outer: None,
    })));

    let mut chars = input.chars().collect();
    let mut lexer = Lexer::new(input.len(), &mut chars);
    let mut parser = Parser::new(&mut lexer);

    let program = parser.parse_program();

    if parser.errors.len() > 0 {
        for err in parser.errors {
            println!("{:?}", err);
        }
    }

    let evaluated = eval(Node::Program(program), Rc::clone(&env));
    format!("{}", evaluated)
}
