use crate::lexer::tokens::*;

pub struct Expression {}

pub struct Identifier<'a> {
    token: Token<'a>,
    value: String,
}

pub struct LetStatement<'a> {
    pub token: Token<'a>,
    pub name: Identifier<'a>,
    pub value: Expression,
}

pub struct Program {}
