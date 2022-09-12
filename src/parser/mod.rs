use crate::ast::*;
use crate::lexer::*;
use crate::lexer::tokens::*;

use std::mem;

struct Parser<'a> {
    l: &'a mut Lexer<'a>,

    cur_token: Token<'a>,
    peek_token: Token<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Parser<'a> {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Parser {
            l: lexer,
            cur_token,
            peek_token,
        }
    }

    pub fn parse_program(&mut self) -> Program {
        Program {}
    }

    fn next_token(&mut self) {
        mem::swap(&mut self.cur_token, &mut self.peek_token);
        self.peek_token = self.l.next_token();
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
    }

    #[test]
    fn test_let_statements() {
    }
}
