use std::str::Chars;

use phf::phf_map;

const ILLEGAL: &str = "ILLEAGAL";
const EOF: &str = "EOF";

const IDENT: &str = "IDENT";
const INT: &str = "INT";

const EQ: &str = "EQ";
const NOT_EQ: &str = "NOT_EQ";

// Operators
const ASSIGN: &str = "=";
const PLUS: &str = "+";
const MINUS: &str = "-";
const BANG: &str = "!";
const ASTERISK: &str = "*";
const SLASH: &str = "/";

const LT: &str = "<";
const GT: &str = ">";

const COMMA: &str = ",";
const SEMICOLON: &str = ";";

const LPAREN: &str = "(";
const RPAREN: &str = ")";

const LBRACE: &str = "{";
const RBRACE: &str = "}";

const LBRACKET: &str = "[";
const RBRACKET: &str = "]";

// Keywords
const FUNCTION: &str = "FUNC";
const LET: &str = "LET";
const TRUE: &str = "TRUE";
const FALSE: &str = "FALSE";
const IF: &str = "IF";
const ELSE: &str = "ELSE";
const RETURN: &str = "RETURN";

static KEYWORDS: phf::Map<&'static str, &'static str> = phf_map! {
    "fn" => FUNCTION,
    "let" => LET,
    "true" => TRUE,
    "false" => FALSE,
    "if" => IF,
    "else" => ELSE,
    "return" => RETURN,
};

#[derive(Debug, Eq, PartialEq)]
struct Token<'a> {
    kind: &'a str,
    literal: String,
}

struct Lexer<'a> {
    ch: char, // current char
    input: &'a mut Vec<char>, // TODO: Do not use iterator, collect to Vector
    length: usize,
    position: usize, // position of current char
    read_position: usize, // position next to current char
}

impl<'a> Lexer<'a> {
    fn new(length: usize, input: &'a mut Vec<char>) -> Lexer<'a> {
        let mut lexer = Lexer {
            length,
            input,
            ch: 0 as char,
            position: 0,
            read_position: 0,
        };
        lexer.read_char();
        
        return lexer;
    }

    fn read_char(&mut self) {
        if self.read_position >= self.length {
            self.ch = 0 as char;
        } else {
            match self.input.get(self.read_position) {
                Some(ch) => {
                    self.ch = *ch;
                },
                None => {}
            }
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let literal = self.ch.to_string();

        if self.ch == (0 as char) {
            return Token{ kind: EOF, literal: String::from("") };
        }

        let tok = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();

                    Token{ kind: EQ, literal: String::from("==") }
                } else {
                    Token{ kind: ASSIGN, literal }
                }
            },
            '+' => Token{ kind: PLUS, literal },
            '-' => Token{ kind: MINUS, literal },
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();

                    Token{ kind: NOT_EQ, literal: String::from("!=") }
                } else {
                    Token{ kind: BANG, literal }
                }
            },
            '/' => Token{ kind: SLASH, literal },
            '*' => Token{ kind: ASTERISK, literal },
            '<' => Token{ kind: LT, literal },
            '>' => Token{ kind: GT, literal },
            ';' => Token{ kind: SEMICOLON, literal },
            '(' => Token{ kind: LPAREN, literal },
            ')' => Token{ kind: RPAREN, literal },
            ',' => Token{ kind: COMMA, literal },
            '{' => Token{ kind: LBRACE, literal },
            '}' => Token{ kind: RBRACE, literal },
            _ => {
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let kind = match KEYWORDS.get(&literal) {
                        Some(kw) => kw,
                        None => IDENT,
                    };

                    return Token {
                        kind,
                        literal,
                    }
                } else if is_digit(self.ch) {
                    let literal = self.read_number();
                    return Token {
                        kind: INT,
                        literal,
                    }
                }

                Token{ kind: ILLEGAL, literal }
            }
        };

        self.read_char();
        tok
    }

    fn read_number(&mut self) -> String {
        // TODO: Squash read_number and read_identifier
        let pos = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }

        self.input[pos..self.position].into_iter().collect()
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }

        self.input[pos..self.position].into_iter().collect()
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.length {
            return 0 as char;
        } else {
            return self.input[self.read_position];
        }
    }
}

fn is_letter(ch: char) -> bool {
    return 'a' <= ch && ch <= 'z'
        || 'A' <= ch && ch <= 'Z'
        || ch == '_'
}

fn is_digit(ch: char) -> bool {
    return '0' <= ch && ch <= '9';
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_nex_token() {
        let input = r#"let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
        "#;

        let expected_tokens = vec![
            Token {
                kind: LET,
                literal: String::from("let"),
            },
            Token {
                kind: IDENT,
                literal: String::from("five"),
            },
            Token {
                kind: ASSIGN,
                literal: String::from("="),
            },
            Token {
                kind: INT,
                literal: String::from("5"),
            },
            Token {
                kind: SEMICOLON,
                literal: String::from(";"),
            },
            Token {
                kind: LET,
                literal: String::from("let"),
            },
            Token {
                kind: IDENT,
                literal: String::from("ten"),
            },
            Token {
                kind: ASSIGN,
                literal: String::from("="),
            },
            Token {
                kind: INT,
                literal: String::from("10"),
            },
            Token {
                kind: SEMICOLON,
                literal: String::from(";"),
            },
            Token {
                kind: LET,
                literal: String::from("let"),
            },
            Token {
                kind: IDENT,
                literal: String::from("add"),
            },
            Token {
                kind: ASSIGN,
                literal: String::from("="),
            },
            Token {
                kind: FUNCTION,
                literal: String::from("fn"),
            },
            Token {
                kind: LPAREN,
                literal: String::from("("),
            },
            Token {
                kind: IDENT,
                literal: String::from("x"),
            },
            Token {
                kind: COMMA,
                literal: String::from(","),
            },
            Token {
                kind: IDENT,
                literal: String::from("y"),
            },
            Token {
                kind: RPAREN,
                literal: String::from(")"),
            },
            Token {
                kind: LBRACE,
                literal: String::from("{"),
            },
            Token {
                kind: IDENT,
                literal: String::from("x"),
            },
            Token {
                kind: PLUS,
                literal: String::from("+"),
            },
            Token {
                kind: IDENT,
                literal: String::from("y"),
            },
            Token {
                kind: SEMICOLON,
                literal: String::from(";"),
            },
            Token {
                kind: RBRACE,
                literal: String::from("}"),
            },
            Token {
                kind: SEMICOLON,
                literal: String::from(";"),
            },
            Token {
                kind: LET,
                literal: String::from("let"),
            },
            Token {
                kind: IDENT,
                literal: String::from("result"),
            },
            Token {
                kind: ASSIGN,
                literal: String::from("="),
            },
            Token {
                kind: IDENT,
                literal: String::from("add"),
            },
            Token {
                kind: LPAREN,
                literal: String::from("("),
            },
            Token {
                kind: IDENT,
                literal: String::from("five"),
            },
            Token {
                kind: COMMA,
                literal: String::from(","),
            },
            Token {
                kind: IDENT,
                literal: String::from("ten"),
            },
            Token {
                kind: RPAREN,
                literal: String::from(")"),
            },
            Token {
                kind: SEMICOLON,
                literal: String::from(";"),
            },
            Token {
                kind: BANG,
                literal: String::from("!"),
            },
            Token {
                kind: MINUS,
                literal: String::from("-"),
            },
            Token {
                kind: SLASH,
                literal: String::from("/"),
            },
            Token {
                kind: ASTERISK,
                literal: String::from("*"),
            },
            Token {
                kind: INT,
                literal: String::from("5"),
            },
            Token {
                kind: SEMICOLON,
                literal: String::from(";"),
            },
            Token {
                kind: INT,
                literal: String::from("5"),
            },
            Token {
                kind: LT,
                literal: String::from("<"),
            },
            Token {
                kind: INT,
                literal: String::from("10"),
            },
            Token {
                kind: GT,
                literal: String::from(">"),
            },
            Token {
                kind: INT,
                literal: String::from("5"),
            },
            Token {
                kind: SEMICOLON,
                literal: String::from(";"),
            },
            Token {
                kind: IF,
                literal: String::from("if"),
            },
            Token {
                kind: LPAREN,
                literal: String::from("("),
            },
            Token {
                kind: INT,
                literal: String::from("5"),
            },
            Token {
                kind: LT,
                literal: String::from("<"),
            },
            Token {
                kind: INT,
                literal: String::from("10"),
            },
            Token {
                kind: RPAREN,
                literal: String::from(")"),
            },
            Token {
                kind: LBRACE,
                literal: String::from("{"),
            },
            Token {
                kind: RETURN,
                literal: String::from("return"),
            },
            Token {
                kind: TRUE,
                literal: String::from("true"),
            },
            Token {
                kind: SEMICOLON,
                literal: String::from(";"),
            },
            Token {
                kind: RBRACE,
                literal: String::from("}"),
            },
            Token {
                kind: ELSE,
                literal: String::from("else"),
            },
            Token {
                kind: LBRACE,
                literal: String::from("{"),
            },
            Token {
                kind: RETURN,
                literal: String::from("return"),
            },
            Token {
                kind: FALSE,
                literal: String::from("false"),
            },
            Token {
                kind: SEMICOLON,
                literal: String::from(";"),
            },
            Token {
                kind: RBRACE,
                literal: String::from("}"),
            },
            Token {
                kind: INT,
                literal: String::from("10"),
            },
            Token {
                kind: EQ,
                literal: String::from("=="),
            },
            Token {
                kind: INT,
                literal: String::from("10"),
            },
            Token {
                kind: SEMICOLON,
                literal: String::from(";"),
            },
            Token {
                kind: INT,
                literal: String::from("10"),
            },
            Token {
                kind: NOT_EQ,
                literal: String::from("!="),
            },
            Token {
                kind: INT,
                literal: String::from("9"),
            },
            Token {
                kind: SEMICOLON,
                literal: String::from(";"),
            },
            Token {
                kind: EOF,
                literal: String::from(""),
            },
        ];

        let length = input.len();
        let mut chars: Vec<char> = input.chars().collect();

        let mut lexer = Lexer::new(length, &mut chars);

        for expected_token in expected_tokens {
            let token = lexer.next_token();

            assert_eq!(expected_token, token);
        }
    }
}
