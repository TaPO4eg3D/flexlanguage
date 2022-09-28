pub mod objects;
pub mod environment;

use objects::*;
use crate::ast::*;

use std::rc::Rc;
use std::cell::RefCell;

use std::iter::zip;

use environment::Environment;

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
        _ => EvalObject::Error {
            kind: ErrorKind::UnknownOp,
            details: format!("-{} ({})", right, right.kind())
        }
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
    match (&left, &right) {
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
                _ => EvalObject::Error {
                    kind: ErrorKind::UnknownOp,
                    details: format!("{} ({}) {} {} ({})", left, left.kind(), infix, right, right.kind()),
                }
            }
        },
        (EvalObject::String(s1), EvalObject::String(s2)) => {
            match infix {
                Infix::Plus => EvalObject::String(format!("{}{}", s1, s2)),
                _ => EvalObject::Error {
                    kind: ErrorKind::UnknownOp,
                    details: format!("{} ({}) {} {} ({})", left, left.kind(), infix, right, right.kind()),
                }
            }
        },
        _ => EvalObject::Error {
            kind: ErrorKind::TypeMismatch,
            details: format!("{} ({}) {} {} ({})", left, left.kind(), infix, right, right.kind()),
        }
    }
}

fn apply_function(func: EvalObject, args: Vec<EvalObject>) -> EvalObject {
    if let EvalObject::Function {params, body, env} = func {
        let func_env = Rc::new(RefCell::new(Environment::new()));

        for (arg, param) in zip(args, params) {
            func_env.borrow_mut().set(param.0, arg);
        }

        func_env.borrow_mut().outer = Some(Rc::clone(&env));
        let value = eval(Node::Stmt(body), Rc::clone(&func_env));

        if let EvalObject::Return(ret) = value {
            *ret
        } else { value }
    } else {
        EvalObject::Error {
            kind: ErrorKind::NotFunc,
            details: format!("TODO: ADD DETAILS"),
        }
    }
}

pub fn eval(node: Node, env: Rc<RefCell<Environment>>) -> EvalObject {
    match node {
        Node::Program(program) => {
            let mut result = EvalObject::Null;

            for stmt in program.statements {
                result = eval(Node::Stmt(stmt), Rc::clone(&env));

                match result {
                    EvalObject::Return(return_result) => {
                        return *return_result;
                    },
                    EvalObject::Error { .. } => {
                        return result;
                    },
                    _ => {}
                }
            }

            return result;
        },
        Node::Stmt(Stmt::Block(block)) => {
            let mut result = EvalObject::Null;

            for stmt in block {
                result = eval(Node::Stmt(stmt), Rc::clone(&env));

                // Bubble up returns and errors
                match result {
                    EvalObject::Return(_) | EvalObject::Error { .. } => {
                        return result;
                    },
                    _ => {}
                }
            }

            return result;
        },
        Node::Expr(expr) => {
            match expr {
                Expr::Ident(ident) => {
                    let env = env.borrow_mut();
                    let value = env.get(&ident.0);

                    match value {
                        Some(val) => return val,
                        None => return EvalObject::Error {
                            kind: ErrorKind::UnknownIdent,
                            details: format!("{}", &ident.0),
                        }
                    }
                },
                Expr::CallFunc(func, params) => {
                    let func = eval(Node::Expr(*func), Rc::clone(&env));

                    if let EvalObject::Error {..} = func {
                        return func;
                    }

                    let mut args = Vec::new();
                    for param_expr in params {
                        let value = eval(Node::Expr(param_expr), Rc::clone(&env));

                        if let EvalObject::Error {..} = func {
                            return value;
                        }
                        
                        args.push(value);
                    }

                    return apply_function(func, args);
                },
                Expr::Literal(Literal::Func(params, body)) => {
                    return EvalObject::Function {
                        params,
                        body: *body,
                        env: Rc::clone(&env),
                    };
                },
                Expr::Literal(Literal::Int(i)) => {
                    return EvalObject::Int(i.try_into().unwrap());
                },
                Expr::Literal(Literal::Boolean(b)) => {
                    return EvalObject::Boolean(b);
                },
                Expr::Literal(Literal::String(s)) => {
                    return EvalObject::String(s);
                },
                Expr::Prefix(prefix, pexpr) => {
                    let right = eval(Node::Expr(*pexpr), Rc::clone(&env));

                    if let EvalObject::Error {..} = right {
                        return right;
                    }

                    return eval_prefix_expression(prefix, right)
                },
                Expr::Infix(infix, lexpr, rexpr) => {
                    let left = eval(Node::Expr(*lexpr), Rc::clone(&env));
                    let right = eval(Node::Expr(*rexpr), Rc::clone(&env));

                    if let EvalObject::Error {..} = left {
                        return left;
                    }

                    if let EvalObject::Error {..} = right {
                        return right;
                    }

                    return eval_infix_expression(infix, left, right)
                },
                Expr::IfExpr { condition, conseq, alternative } => {
                    let rcond = eval(Node::Expr(*condition), Rc::clone(&env));

                    if let EvalObject::Error {..} = rcond {
                        return rcond;
                    }

                    return if is_truthy(rcond) {
                        eval(Node::Stmt(*conseq), Rc::clone(&env))
                    } else if let Some(alternative) = alternative {
                        eval(Node::Stmt(*alternative), Rc::clone(&env))
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
                    return eval(Node::Expr(expr), Rc::clone(&env))
                },
                Stmt::Let(indent, expr) => {
                    let value = eval(Node::Expr(expr), Rc::clone(&env));

                    if let EvalObject::Error {..} = value {
                        return value;
                    }

                    (*env.borrow_mut()).set(indent.0, value);

                    return EvalObject::Null;
                },
                Stmt::Return(expr) => {
                    let value = eval(Node::Expr(expr), Rc::clone(&env));

                    if let EvalObject::Error {..} = value {
                        return value;
                    }

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
        let env = Rc::new(RefCell::new(Environment::new()));

        return eval(Node::Program(parser.parse_program()), Rc::clone(&env));
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

    #[test]
    fn test_errors() {
        let test_cases = vec![
            (
                "5 + true;",
                EvalObject::Error {
                    kind: ErrorKind::TypeMismatch,
                    details: format!("5 (int) + true (bool)")
                },
            ),
            (
                "5 + true; 5;",
                EvalObject::Error {
                    kind: ErrorKind::TypeMismatch,
                    details: format!("5 (int) + true (bool)")
                },
            ),
            (
                "-true",
                EvalObject::Error {
                    kind: ErrorKind::UnknownOp,
                    details: format!("-true (bool)")
                },
            ),
            (
                "5; true + true; 10;",
                EvalObject::Error {
                    kind: ErrorKind::UnknownOp,
                    details: format!("true (bool) + true (bool)")
                },
            ),
            (
                r#"
                    if (10 > 1) {
                        if (10 > 1) {
                            return true + true;
                        }

                        return 10 + 1;
                    }
                "#,
                EvalObject::Error {
                    kind: ErrorKind::UnknownOp,
                    details: format!("true (bool) + true (bool)")
                },
            ),
        ];

        for (input, expected_result) in test_cases {
            let evaluated = run_eval(input.to_string());
            assert_eq!(evaluated, expected_result, "{}", input);
        }
    }

    #[test]
    fn test_let_statements() {
        let test_cases = vec![
            ("let a = 5; a", 5),
            ("let a = 6 * 5; a", 30),
            ("let a = 5; let b = a + 1; b", 6),
        ];

        for (input, expected_result) in test_cases {
            let evaluated = run_eval(input.to_string());
            assert_eq!(evaluated, EvalObject::Int(expected_result));
        }
    }

    #[test]
    fn test_strings() {
        let test_cases = vec![
            ("\"hello world!\"", format!("hello world!")),
            ("\"hello world!\" + \" hi!\"", format!("hello world! hi!")),
        ];

        for (input, expected_result) in test_cases {
            let evaluated = run_eval(input.to_string());
            assert_eq!(evaluated, EvalObject::String(expected_result));
        }
    }

    #[test]
    fn test_funtion_object() {
        let input = "fn(x) { x + 2; }";

        let evaluated = run_eval(input.to_string());
        if let EvalObject::Function { params, body, env } = evaluated {
            assert_eq!(params[0].0, "x");
            // TODO: Continue the test case
        } else {
            panic!("Expect a function!")
        }
    }

    #[test]
    fn test_func_application() {
        let test_cases = vec![
            (
                r#"
                    let id = fn(x) {
                        x;
                    };

                    id(5);
                "#,5
            ),
            (
                r#"
                    let double = fn(x) {
                        return x * 2;
                    };

                    double(5);
                "#, 10
            ),
            (
                r#"
                    fn(x) {
                        return x * 2;
                    }(5);
                "#, 10
            ),
            (
                r#"
                    let a = fn(x) {
                        return fn(y) {
                            x * y;
                        };
                    };
                    let b = a(10);
                    b(2);
                "#, 20
            )
        ];

        for (input, expected_result) in test_cases {
            let evaluated = run_eval(input.to_string());
            assert_eq!(evaluated, EvalObject::Int(expected_result));
        }
    }
}
