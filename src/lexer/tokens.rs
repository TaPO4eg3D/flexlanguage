use phf::phf_map;

#[derive(Debug, Eq, PartialEq)]
pub struct Token<'a> {
    pub kind: &'a str,
    pub literal: String,
}

pub const ILLEGAL: &str = "ILLEAGAL";
pub const EOF: &str = "EOF";

pub const IDENT: &str = "IDENT";
pub const INT: &str = "INT";

pub const EQ: &str = "EQ";
pub const NOT_EQ: &str = "NOT_EQ";

// Operators
pub const ASSIGN: &str = "=";
pub const PLUS: &str = "+";
pub const MINUS: &str = "-";
pub const BANG: &str = "!";
pub const ASTERISK: &str = "*";
pub const SLASH: &str = "/";

pub const LT: &str = "<";
pub const GT: &str = ">";

pub const COMMA: &str = ",";
pub const SEMICOLON: &str = ";";

pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";

pub const LBRACE: &str = "{";
pub const RBRACE: &str = "}";

pub const LBRACKET: &str = "[";
pub const RBRACKET: &str = "]";

// Keywords
pub const FUNCTION: &str = "FUNC";
pub const LET: &str = "LET";
pub const TRUE: &str = "TRUE";
pub const FALSE: &str = "FALSE";
pub const IF: &str = "IF";
pub const ELSE: &str = "ELSE";
pub const RETURN: &str = "RETURN";

pub static KEYWORDS: phf::Map<&'static str, &'static str> = phf_map! {
    "fn" => FUNCTION,
    "let" => LET,
    "true" => TRUE,
    "false" => FALSE,
    "if" => IF,
    "else" => ELSE,
    "return" => RETURN,
};
