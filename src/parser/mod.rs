use crate::ast::*;
use crate::lexer::*;
use crate::lexer::tokens::*;

use std::mem;

struct ParserError {
    row: usize,
    column: usize,
    text: String,
}

struct Parser<'a> {
    l: &'a mut Lexer<'a>,
    errors: Vec<ParserError>,

    cur_token: Token<'a>,
    peek_token: Token<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Parser<'a> {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Parser {
            l: lexer,
            errors: Vec::new(),
            cur_token,
            peek_token,
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.cur_token.kind != EOF {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }

        program
    }

    fn next_token(&mut self) {
        mem::swap(&mut self.cur_token, &mut self.peek_token);
        self.peek_token = self.l.next_token();
    }

    fn parse_statement(&mut self) -> Option<Stmt> {
        match self.cur_token.kind {
            LET => self.parse_let_statement(),
            RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Stmt> {
        if !self.expect_peek(IDENT) {
            let cur_token = self.cur_token.clone();

            // TODO: Support for precice positions
            let error = ParserError {
                row: 0,
                column: 0,
                text: format!(
                    "Expected to have an identifier, got '{}' instead!",
                    cur_token.kind
                ),
            };
            self.errors.push(error);

            return None;
        }

        let cur_token = self.cur_token.clone();

        if !self.expect_peek(ASSIGN) { 
            let error = ParserError {
                row: 0,
                column: 0,
                text: format!(
                    "Expected to have '=', got '{}' instead!",
                    cur_token.kind
                ),
            };
            self.errors.push(error);

            return None;
        }

        // TODO: we're skipping until semilicon
        while self.cur_token.kind != SEMICOLON {
            self.next_token();
        }

        let ident = Ident(cur_token.literal);
        Some(Stmt::Let(ident, Expr::Empty))
    }

    fn parse_return_statement(&mut self) -> Option<Stmt> {
        self.next_token();

        // TODO: we're skipping until semilicon
        while self.cur_token.kind != SEMICOLON {
            self.next_token();
        }

        Some(Stmt::Return(Expr::Empty))
    }

    fn parse_expression_statement(&mut self) -> Option<Stmt> {
        let statement = match self.prefix_parse_token() {
            Some(expr) => Some(Stmt::Expr(expr)),
            _ => None,
        };

        if self.peek_token.kind == SEMICOLON {
            self.next_token();
        }

        statement
    }

    fn prefix_parse_token(&mut self) -> Option<Expr> {
        match self.cur_token.kind {
            IDENT => Some(self.parse_identifier()),
            INT => self.parse_integer_literal(),
            BANG | MINUS => self.parse_prefix_expression(),
            _ => {
                let error = ParserError {
                    row: 0,
                    column: 0,
                    text: format!("No prefix parse function for '{}'", self.cur_token.kind),
                };
                self.errors.push(error); None
            },
        }
    }

    fn parse_identifier(&mut self) -> Expr {
        Expr::Ident(Ident(self.cur_token.clone().literal))
    }

    fn parse_integer_literal(&mut self) -> Option<Expr> {
        match self.cur_token.literal.parse::<i64>() {
            Ok(num) => {
                let num = Literal::Int(num);
                Some(Expr::Literal(num))
            },
            Err(_) => {
                let error = ParserError {
                    row: 0,
                    column: 0,
                    text: String::from("Could not parse the integer!"),
                };
                self.errors.push(error); None
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expr> {
        let cur_token = self.cur_token.clone();
        self.next_token();

        let prefix = match cur_token.kind {
            BANG => Prefix::Bang,
            MINUS => Prefix::Minus,
            PLUS => Prefix::Plus,
            _ => {
                let error = ParserError {
                    row: 0,
                    column: 0,
                    text: String::from("Unknown prefix!"),
                };
                self.errors.push(error); return None
            }
        };

        match self.prefix_parse_token() {
            Some(expr) => Some(
                Expr::Prefix(prefix, Box::new(expr))
            ),
            _ => None
        }
    }

    fn expect_peek(&mut self, kind: &str) -> bool {
        if self.peek_token.kind == kind {
            self.next_token(); true
        } else { false }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_iter_tokens() {
        let input = r#"
            let x = 5;
        "#;

        let expected_tokens = vec![
            (
                Token {kind: LET, literal: "let".to_string()},
                Token {kind: IDENT, literal: "x".to_string()},
            ),
            (
                Token {kind: IDENT, literal: "x".to_string()},
                Token {kind: ASSIGN, literal: "=".to_string()},
            ),
            (
                Token {kind: ASSIGN, literal: "=".to_string()},
                Token {kind: INT, literal: "5".to_string()},
            ),
            (
                Token {kind: INT, literal: "5".to_string()},
                Token {kind: SEMICOLON, literal: ";".to_string()},
            ),
        ];

        let mut chars: Vec<char> = input.chars().collect();
        let mut lexer = Lexer::new(input.len(), &mut chars);
        let mut parser = Parser::new(&mut lexer);
        expect_no_errors(&parser.errors);

        for t in expected_tokens {
            assert_eq!(t.0, parser.cur_token);
            assert_eq!(t.1, parser.peek_token);

            parser.next_token()
        }
    }

    #[test]
    fn test_let_statements() {
        let input = r#"
            let x = 5;
            let foobar = 228;
        "#;

        let mut chars: Vec<char> = input.chars().collect();
        let mut lexer = Lexer::new(input.len(), &mut chars);
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program();
        expect_no_errors(&parser.errors);
        assert_eq!(program.statements.len(), 2);

        let expected_identifiers = vec![
            String::from("x"),
            String::from("foobar"),
        ];

        for (i, expect_ident) in expected_identifiers.iter().enumerate() {
            let stmt = &program.statements[i];
            
            if let Stmt::Let(ident, expr) = stmt {
                assert_eq!(&ident.0, expect_ident);
                // TODO: Compare value
            } else {
                panic!("Expect to have only LET statements");
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
            return 5;
            return 200;
        "#;

        let mut chars: Vec<char> = input.chars().collect();
        let mut lexer = Lexer::new(input.len(), &mut chars);
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 2);

        for stmt in program.statements {
            if let Stmt::Return(Expr::Empty) = stmt {
                // TODO: Should be integer literal
            } else {
                panic!("Expect Return Statement!");
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let mut chars: Vec<char> = input.chars().collect();
        let mut lexer = Lexer::new(input.len(), &mut chars);
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program();
        expect_no_errors(&parser.errors);
        assert_eq!(program.statements.len(), 1);

        if let Stmt::Expr(Expr::Ident(expr)) = &program.statements[0] {
            assert_eq!(expr.0, "foobar");
        } else {
            panic!("Expect an expression statement!");
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let mut chars: Vec<char> = input.chars().collect();
        let mut lexer = Lexer::new(input.len(), &mut chars);
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program();
        expect_no_errors(&parser.errors);
        assert_eq!(program.statements.len(), 1);

        if let Stmt::Expr(Expr::Literal(Literal::Int(i))) = &program.statements[0] {
            assert_eq!(i, &5);
        } else {
            panic!("Expect an INT expression statement!");
        }
    }

    #[test]
    fn test_prefix_expressions() {
        let test_cases = vec![
            ("!5", "!", 5),
            ("-15", "-", 15),
        ];

        for test_case in test_cases {
            let (input, exp_prefix, result) = test_case;

            let mut chars: Vec<char> = input.chars().collect();
            let mut lexer = Lexer::new(input.len(), &mut chars);
            let mut parser = Parser::new(&mut lexer);

            let program = parser.parse_program();
            expect_no_errors(&parser.errors);
            assert_eq!(program.statements.len(), 1);

            match &program.statements[0] {
                Stmt::Expr(Expr::Prefix(prefix, expr)) => {
                    let p = match exp_prefix {
                        "!" => Prefix::Bang,
                        "-" => Prefix::Minus,
                        _ => panic!(),
                    };

                    assert_eq!(&p, prefix);

                    if let Expr::Literal(Literal::Int(i)) = **expr {
                        assert_eq!(i, result);
                    } else {
                        panic!("Expect to have an INT");
                    }
                }
                _ => panic!("Expect to have prefx expression"),
            };
        }
    }

    fn expect_no_errors(errors: &Vec<ParserError>) {
        if errors.len() > 0 {
            for error in errors {
                println!("ParserError: {}", error.text);
            }
            panic!("Expect to not have errors!");
        }
    }
}
