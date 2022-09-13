use crate::lexer::tokens::*;

pub struct Expression {}

pub struct LetStatement<'a> {
    pub name: Token<'a>,
    // pub value: Expression,
}

pub enum StatementKind<'a> {
    NotLet,
    LET(LetStatement<'a>),
}

pub struct Program<'a> {
    pub statements: Vec<StatementKind<'a>>,
}
