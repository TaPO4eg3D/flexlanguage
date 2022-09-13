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

    fn parse_statement(&mut self) -> Option<StatementKind<'a>> {
        match self.cur_token.kind {
            LET => self.parse_let_statement(),
            _ => None
        }
    }

    fn parse_let_statement(&mut self) -> Option<StatementKind<'a>> {
        if !self.expect_peek(IDENT) {
            // TODO: Somehow avoid cloning?
            let current_tok = self.cur_token.clone();

            // TODO: Support for precice positions
            let error = ParserError {
                row: 0,
                column: 0,
                text: format!(
                    "Expected to have an identifier, got '{}' instead!",
                    current_tok.kind
                ),
            };
            self.errors.push(error);

            return None;
        }

        // TODO: Somehow avoid cloning?
        let name_tok = self.cur_token.clone();

        if !self.expect_peek(ASSIGN) { 
            // TODO: Support for precice positions
            let error = ParserError {
                row: 0,
                column: 0,
                text: format!(
                    "Expected to have '=', got '{}' instead!",
                    name_tok.kind
                ),
            };
            self.errors.push(error);

            return None;
        }

        // TODO: we're skipping until semilicon
        while self.cur_token.kind != SEMICOLON {
            self.next_token();
        }

        Some(StatementKind::LET(LetStatement {
            name: name_tok,
        }))
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

        for t in expected_tokens {
            assert_eq!(t.0, parser.cur_token);
            assert_eq!(t.1, parser.peek_token);

            parser.next_token()
        }

        expect_no_errors(&parser.errors);
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
        assert_eq!(program.statements.len(), 2);

        let expected_identifiers = vec![
            String::from("x"),
            String::from("foobar"),
        ];

        for (i, ident) in expected_identifiers.iter().enumerate() {
            let stmt = &program.statements[i];
            
            if let StatementKind::LET(t) = stmt {
                assert_eq!(&t.name.literal, ident);
            } else {
                panic!("Expect to have only LET statements");
            }
        }

        expect_no_errors(&parser.errors);
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
