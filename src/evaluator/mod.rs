pub mod objects;

use objects::*;
use crate::ast::*;

fn is_truthy(obj: EvalObject) -> bool {
    match obj {
        EvalObject::Int(i) => {
            if i == 0 { false } else { true }
        }
        EvalObject::Boolean(b) => b,
        _ => false
    }
}

fn eval_minus_expression(right: EvalObject) -> EvalObject {
    match right {
        EvalObject::Int(i) => {
            EvalObject::Int(-i)
        },
        _ => EvalObject::Null
    }
}

fn eval_prefix_expression(prefix: Prefix, right: EvalObject) -> EvalObject {
    match prefix {
        Prefix::Bang => EvalObject::Boolean(!is_truthy(right)),
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
        (EvalObject::Boolean(lbool), EvalObject::Boolean(rbool)) => {
            match infix {
                Infix::Eq => EvalObject::Boolean(lbool == rbool),
                Infix::NotEq => EvalObject::Boolean(lbool != rbool),
                _ => EvalObject::Null,
            }
        }
        _ => EvalObject::Null
    }
}

pub fn eval(node: Node) -> EvalObject {
    match node {
        Node::Program(program) => {
            let mut result = EvalObject::Null;

            for stmt in program.statements {
                result = eval(Node::Stmt(stmt));

                if let EvalObject::Return(result) = result {
                    return *result;
                }
            }

            return result;
        },
        Node::Stmt(Stmt::Block(block)) => {
            let mut result = EvalObject::Null;

            for stmt in block {
                result = eval(Node::Stmt(stmt));

                // Bubble up the return statement
                if let EvalObject::Return(_) = result {
                    return result;
                }
            }

            return result;
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
                Expr::IfExpr { condition, conseq, alternative } => {
                    let rcond = eval(Node::Expr(*condition));

                    return if is_truthy(rcond) {
                        eval(Node::Stmt(*conseq))
                    } else if let Some(alternative) = alternative {
                        eval(Node::Stmt(*alternative))
                    } else {
                        EvalObject::Null
                    }
                },
                _ => unimplemented!()
            }
        },
        Node::Stmt(stmt) => {
            match stmt {
                Stmt::Expr(expr) => {
                    return eval(Node::Expr(expr))
                },
                Stmt::Return(expr) => {
                    let value = eval(Node::Expr(expr));
                    return EvalObject::Return(Box::new(value));
                },
                _ => unimplemented!(),
            }
        }
    };
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
            ("(1 == 2) == true", false),
            ("(1 == 1) == true", true),
            ("(1 == 2) == false", true),
            ("true == false", false),
            ("true == true", true),
            ("false == false", true),
        ];

        for (input, expected_result) in test_cases {
            match run_eval(input.to_string()) {
                EvalObject::Boolean(b) => assert_eq!(b, expected_result, "{}", input),
                _ => panic!("Expect a Boolean type!")
            }
        }
    }

    #[test]
    fn test_eval_if_expressions() {
        let test_cases = vec![
            ("if (true) { 10; }", EvalObject::Int(10)),
            ("if (false) { 10; }", EvalObject::Null),
            ("if (1) { 10; }", EvalObject::Int(10)),
            ("if (1 < 2) { 10; }", EvalObject::Int(10)),
            ("if (1 > 2) { 10; }", EvalObject::Null),
            ("if (1 > 2) { 10; } else { 20; }", EvalObject::Int(20)),
        ];

        for (input, expected_result) in test_cases {
            let evaluated = run_eval(input.to_string());
            assert_eq!(evaluated, expected_result);
        }
    }

    #[test]
    fn test_return() {
        let input = r#"
            if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }

                return 1;
            }
        "#;

        let evaluated = run_eval(input.to_string());
        assert_eq!(evaluated, EvalObject::Int(10));
    }
}
