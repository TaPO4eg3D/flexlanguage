use std::fmt;
use std::fmt::Write;
use crate::lexer::tokens::*;

#[derive(Debug, Eq, PartialEq)]
pub struct Ident(pub String);

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Prefix {
    Plus,
    Minus,
    Bang,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            Prefix::Plus => "+",
            Prefix::Minus => "-",
            Prefix::Bang => "!",
        };
        write!(f, "{}", str)
    }
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

impl Infix {
    pub fn get(str: &str) -> Option<Infix> {
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
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            Infix::Eq => "==",
            Infix::NotEq => "!=",
            Infix::Lt => "<",
            Infix::Gt => ">",
            Infix::Plus => "+",
            Infix::Minus => "-",
            Infix::Slash => "/",
            Infix::Asterisk => "*",
        };
        write!(f, "{}", str)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Literal {
    Int(i64),
    Boolean(bool),
    Func(Vec<Ident>, Box<Stmt>)
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            Literal::Int(elem) => {
                let mut s = String::new();
                write!(&mut s, "{}", elem).unwrap(); s
            },
            Literal::Boolean(elem) => {
                let mut s = String::new();
                write!(&mut s, "{}", elem).unwrap(); s
            },
            _ => String::from("")
        };
        write!(f, "{}", str)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Empty,
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expr>),
    Infix(Infix, Box<Expr>, Box<Expr>),
    IfExpr {
        condition: Box<Expr>,
        conseq: Box<Stmt>,
        alternative: Option<Box<Stmt>>,
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            Expr::Prefix(pref, expr) => {
                let mut s = String::new();
                write!(&mut s, "({}{})", pref, expr).unwrap(); s
            },
            Expr::Literal(literal) => {
                let mut s = String::new();
                write!(&mut s, "{}", literal).unwrap(); s
            },
            Expr::Ident(ident) => {
                let mut s = String::new();
                write!(&mut s, "{}", ident).unwrap(); s
            },
            Expr::Infix(infix, lexpr, rexpr) => {
                let mut s = String::new();
                write!(
                    &mut s,
                    "({} {} {})",
                    lexpr, infix, rexpr,
                ).unwrap(); s
            },
            Expr::IfExpr {condition, conseq, alternative} => {
                let mut s = String::new();
                write!(
                    &mut s, "if ({}) {}",
                    condition, conseq,
                ).unwrap();

                if let Some(alternative) = alternative {
                    write!(&mut s, "else {}", alternative).unwrap();
                }; s
            }
            _ => String::from(""),
        };
        write!(f, "{}", str)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Stmt {
    Empty,
    Let(Ident, Expr),
    Return(Expr),
    Expr(Expr),
    Block(Vec<Stmt>),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            Stmt::Let(ident, expr) => {
                let mut s = String::new();
                write!(
                    &mut s,
                    "let {} = {};",
                    ident, expr
                ).unwrap(); s
            },
            Stmt::Return(expr) => {
                let mut s = String::new();
                write!(
                    &mut s,
                    "return {};",
                    expr,
                ).unwrap(); s
            },
            Stmt::Expr(expr) => {
                let mut s = String::new();
                write!(
                    &mut s,
                    "{};",
                    expr,
                ).unwrap(); s
            },
            _ => String::from(""),
        };
        write!(f, "{}", str)
    }
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

impl Precedence {
    pub fn from_token(tok: &Token) -> Precedence {
        match tok.kind {
            EQ => Precedence::Equals,
            NOT_EQ => Precedence::Equals,
            LT => Precedence::LessGreater,
            GT => Precedence::LessGreater,
            PLUS => Precedence::Sum,
            MINUS => Precedence::Sum,
            SLASH => Precedence::Product,
            ASTERISK => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
}

pub struct Program {
    pub statements: Vec<Stmt>,
}
