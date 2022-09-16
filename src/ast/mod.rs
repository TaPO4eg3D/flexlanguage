use crate::lexer::tokens::*;

#[derive(Debug, Eq, PartialEq)]
pub struct Ident(pub String);

#[derive(Debug, Eq, PartialEq)]
pub enum Prefix {
    Plus,
    Minus,
    Bang,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Infix {
    Eq,
    NotEq,
    Lt,
    Gt,
    Plus,
    Minus,
    Slash,
    Asterisk,
}

pub fn str_to_infix(str: &str) -> Option<Infix> {
    match str {
        EQ => Some(Infix::Eq),
        NOT_EQ => Some(Infix::NotEq),
        LT => Some(Infix::Lt),
        GT => Some(Infix::Gt),
        PLUS => Some(Infix::Plus),
        MINUS => Some(Infix::Minus),
        SLASH => Some(Infix::Slash),
        ASTERISK => Some(Infix::Asterisk),
        _ => None,
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Literal {
    Int(i64),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Empty,
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expr>),
    Infix(Infix, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Stmt {
    Empty,
    Let(Ident, Expr),
    Return(Expr),
    Expr(Expr),
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Program {
    pub statements: Vec<Stmt>,
}
