pub mod objects;

use objects::*;
use crate::ast::*;

pub fn eval(node: Node) -> EvalObject {
    match node {
        Node::Program(program) => {
            for stmt in program.statements {
                // TODO: Change it
                return eval(Node::Stmt(stmt))
            }
        },
        Node::Stmt(stmt) => {
            match stmt {
                Stmt::Expr(Expr::Literal(Literal::Int(i))) => {
                    return EvalObject::Int(i.try_into().unwrap());
                },
                Stmt::Expr(Expr::Literal(Literal::Boolean(b))) => {
                    return EvalObject::Boolean(b);
                },
                _ => unimplemented!(),
            }
        }
        _ => panic!("Unknown Node!")
    };

    return EvalObject::Int(-9999);
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn run_eval(input: String) -> EvalObject {
        let mut chars: Vec<char> = input.chars().collect();
        let mut lexer = Lexer::new(input.len(), &mut chars);
        let mut parser = Parser::new(&mut lexer);

        return eval(Node::Program(parser.parse_program()));
    }

    #[test]
    fn test_eval_integer() {
        let test_cases = vec![
            ("5;", 5),
            ("10;", 10),
        ];

        for (input, expected_result) in test_cases {
            match run_eval(input.to_string()) {
                EvalObject::Int(i) => assert_eq!(i, expected_result),
                _ => panic!("Expect an Integer type!")
            }
        }
    }

    #[test]
    fn test_eval_boolean() {
        let test_cases = vec![
            ("true;", true),
            ("false;", false),
        ];

        for (input, expected_result) in test_cases {
            match run_eval(input.to_string()) {
                EvalObject::Boolean(b) => assert_eq!(b, expected_result),
                _ => panic!("Expect a Boolean type!")
            }
        }
    }
}
