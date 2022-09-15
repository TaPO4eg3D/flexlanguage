#[derive(Debug)]
pub struct Ident(pub String);

#[derive(Debug, Eq, PartialEq)]
pub enum Prefix {
    Plus,
    Minus,
    Bang,
}

#[derive(Debug)]
pub enum Literal {
    Int(i64),
}

#[derive(Debug)]
pub enum Expr {
    Empty,
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expr>),
}

#[derive(Debug)]
pub enum Stmt {
    Empty,
    Let(Ident, Expr),
    Return(Expr),
    Expr(Expr),
}

pub struct Program {
    pub statements: Vec<Stmt>,
}
