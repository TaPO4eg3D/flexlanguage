pub mod objects;

use objects::*;
use crate::ast::*;

fn eval_minus_expression(right: EvalObject) -> EvalObject {
    match right {
        EvalObject::Int(i) => {
            EvalObject::Int(-i)
        },
        _ => EvalObject::Null
    }
}

fn eval_bang_expression(right: EvalObject) -> EvalObject {
    match right {
        EvalObject::Boolean(b) => {
            match b {
                true => EvalObject::Boolean(false),
                false => EvalObject::Boolean(true)
            }
        },
        EvalObject::Null => EvalObject::Boolean(true),
        _ => EvalObject::Boolean(false)
    }
}

fn eval_prefix_expression(prefix: Prefix, right: EvalObject) -> EvalObject {
    match prefix {
        Prefix::Bang => eval_bang_expression(right),
        Prefix::Minus => eval_minus_expression(right),
        _ => unimplemented!()
    }
}

fn eval_infix_expression(infix: Infix, left: EvalObject, right: EvalObject) -> EvalObject {
    match (left, right) {
        (EvalObject::Int(lnum), EvalObject::Int(rnum)) => {
            match infix {
                Infix::Plus => EvalObject::Int(lnum + rnum),
                Infix::Minus => EvalObject::Int(lnum - rnum),
                Infix::Asterisk => EvalObject::Int(lnum * rnum),
                Infix::Lt => EvalObject::Boolean(lnum < rnum),
                Infix::Gt => EvalObject::Boolean(lnum > rnum),
                Infix::Eq => EvalObject::Boolean(lnum == rnum),
                Infix::NotEq => EvalObject::Boolean(lnum != rnum),
                Infix::Slash => unimplemented!(),
            }
        },
        _ => EvalObject::Null
    }
}

pub fn eval(node: Node) -> EvalObject {
    match node {
        Node::Program(program) => {
            for stmt in program.statements {
                // TODO: Change it
                return eval(Node::Stmt(stmt))
            }
        },
        Node::Expr(expr) => {
            match expr {
                Expr::Literal(Literal::Int(i)) => {
                    return EvalObject::Int(i.try_into().unwrap());
                },
                Expr::Literal(Literal::Boolean(b)) => {
                    return EvalObject::Boolean(b);
                },
                Expr::Prefix(prefix, pexpr) => {
                    let right = eval(Node::Expr(*pexpr));

                    return eval_prefix_expression(prefix, right)
                },
                Expr::Infix(infix, lexpr, rexpr) => {
                    let left = eval(Node::Expr(*lexpr));
                    let right = eval(Node::Expr(*rexpr));

                    return eval_infix_expression(infix, left, right)
                },
                _ => unimplemented!()
            }
        },
        Node::Stmt(stmt) => {
            match stmt {
                Stmt::Expr(expr) => {
                    return eval(Node::Expr(expr))
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
            ("-5;", -5),
            ("-10;", -10),
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

    #[test]
    fn test_bang_operator() {
        let test_cases = vec![
            ("!true;", false),
            ("!false;", true),
            ("!5;", false),
            ("!!true;", true),
            ("!!false;", false),
            ("!!5;", true),
        ];

        for (input, expected_result) in test_cases {
            match run_eval(input.to_string()) {
                EvalObject::Boolean(b) => assert_eq!(b, expected_result, "{}", input),
                _ => panic!("Expect a Boolean type!")
            }
        }
    }

    #[test]
    fn test_eval_infix_int() {
        let test_cases = vec![
            ("1 + 1", 2),
            ("4 + 2 * 2", 8),
            ("(4 + 2) * 2", 12),
        ];

        for (input, expected_result) in test_cases {
            match run_eval(input.to_string()) {
                EvalObject::Int(i) => assert_eq!(i, expected_result, "{}", input),
                _ => panic!("Expect an Int type!")
            }
        }
    }

    #[test]
    fn test_eval_infix_boolean() {
        let test_cases = vec![
            ("1 > 1", false),
            ("1 < 1", false),
            ("1 == 1", true),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 == 2", false),
            ("1 != 2", true),
        ];

        for (input, expected_result) in test_cases {
            match run_eval(input.to_string()) {
                EvalObject::Boolean(b) => assert_eq!(b, expected_result, "{}", input),
                _ => panic!("Expect a Boolean type!")
            }
        }
    }
}
