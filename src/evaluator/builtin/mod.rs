use std::collections::HashMap;

use super::objects::{EvalObject, ErrorKind};

pub fn construct_builtins() -> HashMap<String, EvalObject> {
    let mut env = HashMap::new();

    env.insert(format!("len"), EvalObject::Builtin(len));

    env
}

fn len(args: Vec<EvalObject>) -> EvalObject {
    if args.len() != 1 {
        return EvalObject::Error {
            kind: ErrorKind::WrongArgNumber,
            details: format!("expect to have 1, instead got {}", args.len()),
        };
    }

    match args[0] {
        EvalObject::String(ref s) => {
            EvalObject::Int(s.len() as i32)
        },
        _ => EvalObject::Error {
            kind: ErrorKind::TypeMismatch,
            details: format!("len() does not support '{}'", args[0].kind()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::construct_builtins;

    use std::rc::Rc;
    use std::cell::RefCell;

    use crate::ast::Node;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use crate::evaluator::eval;
    use crate::evaluator::objects::EvalObject;
    use crate::evaluator::environment::Environment;

    fn run_eval(input: String) -> EvalObject {
        let mut chars: Vec<char> = input.chars().collect();
        let mut lexer = Lexer::new(input.len(), &mut chars);
        let mut parser = Parser::new(&mut lexer);
        let env = Rc::new(RefCell::new(Environment::new()));
        env.borrow_mut().outer = Some(Rc::new(RefCell::new(Environment {
            store: construct_builtins(),
            outer: None,
        })));

        return eval(Node::Program(parser.parse_program()), Rc::clone(&env));
    }

    #[test]
    fn test_funtion_object() {
        let input = "len(\"hi!\");";

        let evaluated = run_eval(input.to_string());
        assert_eq!(evaluated, EvalObject::Int(3));
    }
}
