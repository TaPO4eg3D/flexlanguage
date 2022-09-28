use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Eq, PartialEq)]
pub enum EvalObject {
    Int(i32),
    Boolean(bool),
    Return(Box<EvalObject>),
    Null,
}

impl EvalObject {
    pub fn kind(&self) -> &str {
        match self {
            EvalObject::Int(_) => "int",
            EvalObject::Boolean(_) => "bool",
            EvalObject::Return(_) => "return",
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
           EvalObject::Null => {
               write!(f, "NULL")
           },
           _ => unimplemented!()
       }
    }
}
