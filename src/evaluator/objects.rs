use std::fmt::{Display, Formatter, Result};

use crate::ast::*;
use super::environment::Environment;

use std::rc::Rc;
use std::cell::RefCell;


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ErrorKind {
    TypeMismatch,
    UnknownOp,
    UnknownIdent,
    NotFunc,
}

impl Display for ErrorKind {
   fn fmt(&self, f: &mut Formatter<'_>) -> Result {
       match self {
           ErrorKind::TypeMismatch => {
               write!(f, "Type Mismatch")
           },
           ErrorKind::UnknownOp => {
               write!(f, "Unknown Operator")
           },
           ErrorKind::UnknownIdent => {
               write!(f, "Identifier not found")
           },
           ErrorKind::NotFunc => {
               write!(f, "Not a function")
           },
       }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum EvalObject {
    Int(i32),
    Boolean(bool),
    Return(Box<EvalObject>),
    String(String),
    Error { kind: ErrorKind, details: String },
    Function {
        params: Vec<Ident>,
        body: Stmt,
        env: Rc<RefCell<Environment>>,
    },
    Null,
}

impl EvalObject {
    pub fn kind(&self) -> &str {
        match self {
            EvalObject::Int(_) => "int",
            EvalObject::Boolean(_) => "bool",
            EvalObject::String(_) => "string",
            EvalObject::Return(_) => "return",
            EvalObject::Error { .. } => "err",
            EvalObject::Function { .. } => "func",
            EvalObject::Null => "null",
        }
    }
}

impl Display for EvalObject {
   fn fmt(&self, f: &mut Formatter<'_>) -> Result {
       match self {
           EvalObject::Int(i) => {
               write!(f, "{}", i)
           },
           EvalObject::Boolean(b) => {
               write!(f, "{}", b)
           },
           EvalObject::String(s) => {
               write!(f, "{}", s)
           },
           EvalObject::Error { kind, details } => {
               write!(f, "{}: {}", kind, details)
           },
           EvalObject::Null => {
               write!(f, "NULL")
           },
           _ => unimplemented!()
       }
    }
}
